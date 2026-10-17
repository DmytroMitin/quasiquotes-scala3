package quasiquotes.definitions.dotty

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser

import quasiquotes.definitions.{DefinitionName, DefinitionSemanticError, SemanticDefinition}
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.{Capture, EditPlan, Failure, Result}
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class ExistingGeneratedStrictValueInitializationOrderTyperRuntimeTest
    extends munit.FunSuite:
  private enum Family:
    case MethodChain, ValueChain, MethodReadsValue, ValueCallsMethod, ValueReadsExisting, MethodReadsExisting

  private enum Negative:
    case MissingGeneratedValue, DuplicateGeneratedValue, OmittedExistingValue

  test("M0 generated method bodies remain order-independent and delayed until invocation") {
    val dependency = compilePositive(Family.MethodChain, forward = false)
    val forward = compilePositive(Family.MethodChain, forward = true)

    assertSuccessful(dependency, expectedRuntime = 3)
    assertSuccessful(forward, expectedRuntime = 3)
    assertEquals(dependency.warnings, Vector.empty)
    assertEquals(forward.warnings, Vector.empty)
  }

  test("V1 dependency-first generated strict values typecheck emit TASTy and initialize to seven") {
    val outcome = compilePositive(Family.ValueChain, forward = false)

    assertSuccessful(outcome, expectedRuntime = 7)
    assertEquals(outcome.warnings, Vector.empty)
  }

  test("V2 forward generated strict value reference typechecks warning-free emits TASTy and reads zero") {
    val outcome = compilePositive(Family.ValueChain, forward = true)

    assertSuccessful(outcome, expectedRuntime = 0)
    assertEquals(outcome.warnings, Vector.empty)
  }

  test("V3 generated method reads generated value correctly after construction in both orders") {
    val dependency = compilePositive(Family.MethodReadsValue, forward = false)
    val forward = compilePositive(Family.MethodReadsValue, forward = true)

    assertSuccessful(dependency, expectedRuntime = 7)
    assertSuccessful(forward, expectedRuntime = 7)
    assertEquals(dependency.warnings, Vector.empty)
    assertEquals(forward.warnings, Vector.empty)
  }

  test("V4 strict value invokes generated method correctly in both append orders") {
    val dependency = compilePositive(Family.ValueCallsMethod, forward = false)
    val forward = compilePositive(Family.ValueCallsMethod, forward = true)

    assertSuccessful(dependency, expectedRuntime = 3)
    assertSuccessful(forward, expectedRuntime = 3)
    assertEquals(dependency.warnings, Vector.empty)
    assertEquals(forward.warnings, Vector.empty)
  }

  test("V5 generated strict value and delayed method both resolve retained untouched value") {
    val strict = compilePositive(Family.ValueReadsExisting, forward = false)
    val delayed = compilePositive(Family.MethodReadsExisting, forward = false)

    assertSuccessful(strict, expectedRuntime = 7)
    assertSuccessful(delayed, expectedRuntime = 7)
    assertEquals(strict.warnings, Vector.empty)
    assertEquals(delayed.warnings, Vector.empty)
  }

  test("missing generated strict value target is admitted pre-Typer then rejected at generated origin") {
    assertNegative(
      Negative.MissingGeneratedValue,
      expectedText = "seed",
      expectedPath = "generated/U070MissingBonus.scala"
    )
  }

  test("duplicate generated strict values are admitted pre-Typer then rejected without TASTy") {
    assertNegative(
      Negative.DuplicateGeneratedValue,
      expectedText = "Conflicting definitions",
      expectedPath = None
    )
  }

  test("generated strict value reference to omitted retained target fails at generated origin") {
    assertNegative(
      Negative.OmittedExistingValue,
      expectedText = "untouched",
      expectedPath = "generated/U070OmittedBonus.scala"
    )
  }

  private final case class DiagnosticEvidence(message: String, path: String)

  private final case class Outcome(
      family: Family,
      forward: Boolean,
      firstGeneratedPath: String,
      errors: Vector[DiagnosticEvidence],
      warnings: Vector[DiagnosticEvidence],
      emittedFiles: Vector[String],
      runtime: Option[Int],
      publicTransactionSucceeded: Boolean,
      finalGeneratedOrder: Vector[String],
      distinctGeneratedSources: Boolean,
      untouchedIdentityPreserved: Boolean,
      preTyperClean: Boolean
  )

  private def assertSuccessful(outcome: Outcome, expectedRuntime: Int): Unit =
    assertEquals(outcome.errors, Vector.empty)
    assert(outcome.publicTransactionSucceeded)
    assert(outcome.distinctGeneratedSources)
    assert(outcome.untouchedIdentityPreserved)
    assert(outcome.preTyperClean)
    assert(outcome.emittedFiles.exists(_.endsWith(s"${className(outcome.family, outcome.forward)}.class")))
    assert(outcome.emittedFiles.exists(_.endsWith(s"${className(outcome.family, outcome.forward)}.tasty")))
    assertEquals(outcome.runtime, Some(expectedRuntime))
    assertEquals(
      outcome.finalGeneratedOrder,
      expectedGeneratedNames(outcome.family, outcome.forward)
    )

  private def compilePositive(family: Family, forward: Boolean): Outcome =
    val owner = className(family, forward)
    val use = owner + "Use"
    val temporary = Files.createTempDirectory("u070-generated-value-positive-")
    try
      val source = temporary.resolve(owner + ".scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(source, positiveProgram(owner, use, family), StandardCharsets.UTF_8)
      val driver = new PositiveDriver(owner, family, forward)
      val reporter = driver.process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))
      val errors = reporter.allErrors.toVector.map(diagnostic)
      val warnings = reporter.allWarnings.toVector.map(diagnostic)
      val files = emitted(output)
      val runtime =
        if reporter.hasErrors then None
        else
          val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
          try Some(invoke(loader, use))
          finally loader.close()
      Outcome(
        family,
        forward,
        driver.firstGeneratedPath,
        errors,
        warnings,
        files,
        runtime,
        driver.publicTransactionSucceeded,
        driver.finalGeneratedOrder,
        driver.distinctGeneratedSources,
        driver.untouchedIdentityPreserved,
        driver.preTyperClean
      )
    finally deleteRecursively(temporary)

  private final class PositiveDriver(owner: String, family: Family, forward: Boolean)
      extends Driver:
    @volatile var firstGeneratedPath = ""
    @volatile var publicTransactionSucceeded = false
    @volatile var finalGeneratedOrder = Vector.empty[String]
    @volatile var distinctGeneratedSources = false
    @volatile var untouchedIdentityPreserved = false
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new PositiveRewriteBeforeTyper(
            owner,
            family,
            forward,
            PositiveDriver.this
          )) :: super.frontendPhases.tail

  private final class PositiveRewriteBeforeTyper(
      owner: String,
      family: Family,
      forward: Boolean,
      evidence: PositiveDriver
  ) extends Phase:
    def phaseName: String = "u070GeneratedStrictValuePositive"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString == owner =>
              rewritePositive(root, family, forward) match
                case Left(problem) =>
                  report.error(problem)
                  root
                case Right(proof) =>
                  evidence.firstGeneratedPath = proof.firstGeneratedPath
                  evidence.publicTransactionSucceeded = true
                  evidence.finalGeneratedOrder = proof.finalGeneratedOrder
                  evidence.distinctGeneratedSources = proof.distinctGeneratedSources
                  evidence.untouchedIdentityPreserved = proof.untouchedIdentityPreserved
                  evidence.preTyperClean = allTrees(proof.result.tree).forall(node =>
                    node.symbol == NoSymbol && !node.isInstanceOf[untpd.TypedSplice]
                  )
                  proof.result.tree
            case other => other
          }
          summon[Context].compilationUnit.untpdTree =
            untpd.cpy.PackageDef(packageDef)(packageDef.pid, rewritten)
        case other => report.error("expected PackageDef before Typer, found " + other.getClass.getSimpleName)

  private final case class PositiveProof(
      result: Result,
      firstGeneratedPath: String,
      finalGeneratedOrder: Vector[String],
      distinctGeneratedSources: Boolean,
      untouchedIdentityPreserved: Boolean
  )

  private def rewritePositive(
      root: untpd.TypeDef,
      family: Family,
      forward: Boolean
  )(using Context): Either[String, PositiveProof] =
    val original = template(root).body.toVector
    val definitions = generatedDefinitions(family)
    val ordered = if forward then definitions.reverse else definitions
    val paths = ordered.map(definition =>
      s"generated/${root.name}${definition.name.source.capitalize}.scala"
    )
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      plan <- appendAll(captured.emptyPlan, ordered.zip(paths)).left.map(_.message)
      result <- ExistingClassUntypedRewrite(captured, plan).left.map(_.message)
      members = template(result.tree).body.toVector
      generated = members.drop(original.size).map(_.asInstanceOf[untpd.MemberDef])
      names = generated.map(_.name.toString)
      _ <- Either.cond(names == ordered.map(_.name.source), (), "unexpected generated member order: " + names)
      _ <- Either.cond(rawTopology(family, generated), (), "unexpected generated raw topology")
      distinct = generated.indices.forall(i =>
        generated.indices.forall(j => i == j || !generated(i).source.eq(generated(j).source))
      )
      provenance = generated.zip(paths).forall { (member, path) =>
        member.source.path == path && member.span.start == 0 &&
          member.span.end == member.source.content.length
      }
      originalDistinct = generated.forall(member => !member.source.eq(root.source))
      untouched = members(1).eq(original(1)) &&
        result.directMemberIdentities(1).sameObjectAs(captured.members(1).identity)
    yield PositiveProof(result, paths.head, names, distinct && provenance && originalDistinct, untouched)

  private def generatedDefinitions(family: Family): Vector[SemanticDefinition] =
    family match
      case Family.MethodChain =>
        Vector(generatedApplyMethod("relay", "sum"), generatedIdentifierMethod("bonus", "relay"))
      case Family.ValueChain =>
        Vector(generatedLiteralValue("seed", "7"), generatedIdentifierValue("bonus", "seed"))
      case Family.MethodReadsValue =>
        Vector(generatedLiteralValue("seed", "7"), generatedIdentifierMethod("relay", "seed"))
      case Family.ValueCallsMethod =>
        Vector(generatedApplyMethod("relay", "sum"), generatedIdentifierValue("bonus", "relay"))
      case Family.ValueReadsExisting =>
        Vector(generatedIdentifierValue("bonus", "untouched"))
      case Family.MethodReadsExisting =>
        Vector(generatedIdentifierMethod("bonus", "untouched"))

  private def expectedGeneratedNames(family: Family, forward: Boolean): Vector[String] =
    val names = generatedDefinitions(family).map(_.name.source)
    if forward then names.reverse else names

  private def appendAll(
      initial: EditPlan,
      definitions: Vector[(SemanticDefinition, String)]
  ): Either[Failure, EditPlan] =
    definitions.foldLeft[Either[Failure, EditPlan]](Right(initial)) {
      case (plan, (definition, path)) => plan.flatMap(_.append(definition, path))
    }

  private def rawTopology(family: Family, generated: Vector[untpd.MemberDef])(using Context): Boolean =
    def rhs(member: untpd.MemberDef): untpd.Tree = member match
      case value: untpd.ValDef => value.rhs
      case method: untpd.DefDef => method.rhs
    def identifier(member: untpd.MemberDef, expected: String): Boolean = rhs(member) match
      case value: untpd.Ident => value.name.toString == expected
      case _ => false
    def literal(member: untpd.MemberDef, expected: String): Boolean = rhs(member) match
      case untpd.Number(digits, untpd.NumberKind.Whole(10)) => digits == expected
      case _ => false
    def apply(member: untpd.MemberDef, expected: String): Boolean = rhs(member) match
      case untpd.Apply(function: untpd.Ident, arguments) =>
        function.name.toString == expected && arguments.size == 3
      case _ => false
    val byName = generated.map(member => member.name.toString -> member).toMap
    family match
      case Family.MethodChain => apply(byName("relay"), "sum") && identifier(byName("bonus"), "relay")
      case Family.ValueChain => literal(byName("seed"), "7") && identifier(byName("bonus"), "seed")
      case Family.MethodReadsValue => literal(byName("seed"), "7") && identifier(byName("relay"), "seed")
      case Family.ValueCallsMethod => apply(byName("relay"), "sum") && identifier(byName("bonus"), "relay")
      case Family.ValueReadsExisting => identifier(byName("bonus"), "untouched")
      case Family.MethodReadsExisting => identifier(byName("bonus"), "untouched")

  private def assertNegative(
      mode: Negative,
      expectedText: String,
      expectedPath: Option[String] | String
  ): Unit =
    val owner = "U070Negative" + mode.toString
    val temporary = Files.createTempDirectory("u070-generated-value-negative-")
    try
      val source = temporary.resolve(owner + ".scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(source, negativeProgram(owner), StandardCharsets.UTF_8)
      val driver = new NegativeDriver(owner, mode)
      val reporter = driver.process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))
      val errors = reporter.allErrors.toVector.map(diagnostic)

      assert(driver.publicTransactionSucceeded)
      assert(driver.preTyperClean)
      assert(reporter.hasErrors)
      assert(errors.exists(_.message.contains(expectedText)), clues(errors))
      expectedPath match
        case path: String => assert(errors.exists(_.path == path), clues(errors))
        case _ => assert(errors.exists(_.path == source.toString), clues(errors))
      assert(!emitted(output).exists(_.endsWith(owner + ".class")))
      assert(!emitted(output).exists(_.endsWith(owner + ".tasty")))
    finally deleteRecursively(temporary)

  private final class NegativeDriver(owner: String, mode: Negative) extends Driver:
    @volatile var publicTransactionSucceeded = false
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new NegativeRewriteBeforeTyper(
            owner,
            mode,
            NegativeDriver.this
          )) :: super.frontendPhases.tail

  private final class NegativeRewriteBeforeTyper(
      owner: String,
      mode: Negative,
      evidence: NegativeDriver
  ) extends Phase:
    def phaseName: String = "u070GeneratedStrictValueNegative"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString == owner =>
              val transaction = for
                captured <- ExistingClassUntypedRewrite.capture(root)
                plan <- mode match
                  case Negative.MissingGeneratedValue =>
                    captured.emptyPlan.append(
                      generatedIdentifierValue("bonus", "seed"),
                      "generated/U070MissingBonus.scala"
                    )
                  case Negative.DuplicateGeneratedValue =>
                    for
                      p1 <- captured.emptyPlan.append(
                        generatedLiteralValue("duplicate", "1"),
                        "generated/U070DuplicateOne.scala"
                      )
                      p2 <- p1.append(
                        generatedLiteralValue("duplicate", "2"),
                        "generated/U070DuplicateTwo.scala"
                      )
                    yield p2
                  case Negative.OmittedExistingValue =>
                    for
                      p1 <- captured.emptyPlan.omit(captured.members(1).ref)
                      p2 <- p1.append(
                        generatedIdentifierValue("bonus", "untouched"),
                        "generated/U070OmittedBonus.scala"
                      )
                    yield p2
                result <- ExistingClassUntypedRewrite(captured, plan)
              yield result
              transaction match
                case Left(problem) =>
                  report.error(problem.message)
                  root
                case Right(result) =>
                  evidence.publicTransactionSucceeded = true
                  evidence.preTyperClean = allTrees(result.tree).forall(node =>
                    node.symbol == NoSymbol && !node.isInstanceOf[untpd.TypedSplice]
                  )
                  result.tree
            case other => other
          }
          summon[Context].compilationUnit.untpdTree =
            untpd.cpy.PackageDef(packageDef)(packageDef.pid, rewritten)
        case other => report.error("expected PackageDef before Typer, found " + other.getClass.getSimpleName)

  private def generatedLiteralValue(label: String, literal: String): SemanticDefinition =
    semantic(SemanticDefinition.immutableValue(
      semantic(DefinitionName.fromSource(label)),
      IntType,
      TermShape.Literal(literal)
    ))

  private def generatedIdentifierValue(label: String, sibling: String): SemanticDefinition =
    semantic(SemanticDefinition.immutableValue(
      semantic(DefinitionName.fromSource(label)),
      IntType,
      TermShape.Identifier(sibling, false)
    ))

  private def generatedApplyMethod(label: String, sibling: String): SemanticDefinition =
    semantic(SemanticDefinition.concreteMethod(
      semantic(DefinitionName.fromSource(label)),
      Vector.empty,
      IntType
    )(_ => Right(TermShape.Apply(
      TermShape.Identifier(sibling, false),
      List("10", "20", "30").map(TermShape.Literal(_))
    ))))

  private def generatedIdentifierMethod(label: String, sibling: String): SemanticDefinition =
    semantic(SemanticDefinition.concreteMethod(
      semantic(DefinitionName.fromSource(label)),
      Vector.empty,
      IntType
    )(_ => Right(TermShape.Identifier(sibling, false))))

  private def positiveProgram(owner: String, use: String, family: Family): String =
    val resultName = family match
      case Family.MethodReadsValue => "relay"
      case _ => "bonus"
    s"""class $owner:
       |  def sum(values: Int*): Int = values.size
       |  val untouched: Int = 7
       |
       |object $use:
       |  def result: Int = new $owner().$resultName
       |""".stripMargin

  private def negativeProgram(owner: String): String =
    s"""class $owner:
       |  def sum(values: Int*): Int = values.size
       |  val untouched: Int = 7
       |""".stripMargin

  private def className(family: Family, forward: Boolean): String =
    "U070" + family.toString + (if forward then "Forward" else "Dependency")

  private def diagnostic(problem: dotty.tools.dotc.reporting.Diagnostic): DiagnosticEvidence =
    val path = Option(problem.pos)
      .flatMap(position => Option(position.source))
      .map(_.path)
      .getOrElse("")
    DiagnosticEvidence(problem.message, path)

  private def template(root: untpd.TypeDef)(using Context): untpd.Template =
    root.rhs.asInstanceOf[untpd.Template]

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ExistingUntpdClassMemberFilter.allTrees(tree)

  private def semantic[A](value: Either[DefinitionSemanticError, A]): A =
    value.fold(problem => throw new IllegalArgumentException(problem.message), identity)

  private def invoke(loader: ClassLoader, use: String): Int =
    val moduleClass = loader.loadClass(use + "$")
    val module = moduleClass.getField("MODULE$").get(null)
    moduleClass.getMethod("result").invoke(module).asInstanceOf[Integer].intValue

  private def emitted(output: Path): Vector[String] =
    val stream = Files.walk(output)
    try stream.filter(Files.isRegularFile(_)).iterator().asScala.map(_.toString).toVector
    finally stream.close()

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ExistingClassUntypedRewrite.type],
      getClass
    ).flatMap(value =>
      Option(value.getProtectionDomain)
        .flatMap(domain => Option(domain.getCodeSource))
        .map(_.getLocation.toURI)
    ).map(Path.of(_).toString).distinct.mkString(java.io.File.pathSeparator)

  private def deleteRecursively(root: Path): Unit =
    if Files.exists(root) then
      val stream = Files.walk(root)
      try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally stream.close()

  private val IntType = TypeNormalForm.STypeIdent("Int")
