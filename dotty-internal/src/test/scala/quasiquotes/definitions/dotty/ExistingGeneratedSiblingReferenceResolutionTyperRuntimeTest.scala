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
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.{Failure, Result}
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class ExistingGeneratedSiblingReferenceResolutionTyperRuntimeTest
    extends munit.FunSuite:
  private enum NegativeMode:
    case OmittedSibling, DuplicateName

  test("retained rewritten and value siblings resolve through Typer TASTy and runtime") {
    val temporary = Files.createTempDirectory("u063-generated-sibling-positive-")
    try
      val source = temporary.resolve("U063GeneratedSibling.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(source, PositiveProgram, StandardCharsets.UTF_8)

      val baseline = new Driver().process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))
      assert(baseline.hasErrors)
      deleteRecursively(output)
      Files.createDirectories(output)

      val driver = new PositiveDriver
      val reporter = driver.process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(driver.rewritten, Set("U063A", "U063B", "U063C", "U063D1", "U063D2", "U063E"))
      assertEquals(driver.freeApplyTopology, 5)
      assertEquals(driver.freeIdentifierTopology, 1)
      assertEquals(driver.repeatedModesPreserved, 4)
      assertEquals(driver.byNameModesPreserved, 3)
      assertEquals(driver.omissionsApplied, 2)
      assertEquals(driver.untouchedIdentitiesPreserved, 6)
      assert(driver.preTyperClean)

      val files = emitted(output)
      driver.rewritten.foreach(name => assert(files.exists(_.endsWith(s"$name.tasty")), clues(name)))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        assertEquals(invoke(loader, "appendOnly"), 3)
        assertEquals(invoke(loader, "rewrittenRepeated"), 3)
        assertEquals(invoke(loader, "bothModes"), 3032)
        assertEquals(invoke(loader, "editsFirst"), 10)
        assertEquals(invoke(loader, "omitFirst"), 10)
        assertEquals(invoke(loader, "retainedValue"), 7)
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  test("reference to an omitted sibling is admitted pre-Typer and rejected by ordinary Typer") {
    assertNegative(
      NegativeMode.OmittedSibling,
      "obsolete",
      "generated/U063OmittedSiblingBonus.scala"
    )
  }

  test("duplicate generated member name is admitted pre-Typer and rejected as a duplicate declaration") {
    assertNegative(
      NegativeMode.DuplicateName,
      "untouched",
      "generated/U063DuplicateUntouched.scala"
    )
  }

  private def assertNegative(
      mode: NegativeMode,
      expectedName: String,
      generatedPath: String
  ): Unit =
    val temporary = Files.createTempDirectory("u063-generated-sibling-negative-")
    try
      val source = temporary.resolve("U063Negative.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(source, NegativeProgram, StandardCharsets.UTF_8)
      val driver = new NegativeDriver(mode)
      val reporter = driver.process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))

      assert(driver.publicTransactionSucceeded)
      assert(driver.preTyperClean)
      assert(reporter.hasErrors)
      assert(reporter.allErrors.exists(_.message.contains(expectedName)), clues(reporter.allErrors))
      mode match
        case NegativeMode.OmittedSibling =>
          assert(reporter.allErrors.exists(_.message.contains("Not found")), clues(reporter.allErrors))
        case NegativeMode.DuplicateName =>
          assert(reporter.allErrors.exists(_.message.contains("Conflicting definitions")), clues(reporter.allErrors))
      val expectedDiagnosticPath = mode match
        case NegativeMode.OmittedSibling => generatedPath
        case NegativeMode.DuplicateName => source.toString
      assert(reporter.allErrors.exists(problem =>
        problem.pos != null && problem.pos.source != null && problem.pos.source.path == expectedDiagnosticPath
      ), clues(reporter.allErrors))
      assert(!emitted(output).exists(_.endsWith("U063Negative.tasty")))
    finally deleteRecursively(temporary)

  private final class PositiveDriver extends Driver:
    @volatile var rewritten = Set.empty[String]
    @volatile var freeApplyTopology = 0
    @volatile var freeIdentifierTopology = 0
    @volatile var repeatedModesPreserved = 0
    @volatile var byNameModesPreserved = 0
    @volatile var omissionsApplied = 0
    @volatile var untouchedIdentitiesPreserved = 0
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new PositiveRewriteBeforeTyper(PositiveDriver.this)) ::
            super.frontendPhases.tail

  private final class PositiveRewriteBeforeTyper(evidence: PositiveDriver) extends Phase:
    def phaseName: String = "u063GeneratedSiblingPositive"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString.matches("U063(A|B|C|D1|D2|E)") =>
              replace(root, evidence)
            case other => other
          }
          summon[Context].compilationUnit.untpdTree =
            untpd.cpy.PackageDef(packageDef)(packageDef.pid, rewritten)
          evidence.preTyperClean = rewritten.forall(tree => allTrees(tree).forall(node =>
            node.symbol == NoSymbol && !node.isInstanceOf[untpd.TypedSplice]
          ))
        case other => report.error("expected PackageDef before Typer, found " + other.getClass.getSimpleName)

    private def replace(root: untpd.TypeDef, evidence: PositiveDriver)(using Context): untpd.TypeDef =
      rewritePositive(root) match
        case Left(problem) =>
          report.error(problem)
          root
        case Right(proof) =>
          evidence.rewritten += root.name.toString
          if proof.freeApply then evidence.freeApplyTopology += 1
          if proof.freeIdentifier then evidence.freeIdentifierTopology += 1
          if proof.repeatedPreserved then evidence.repeatedModesPreserved += 1
          if proof.byNamePreserved then evidence.byNameModesPreserved += 1
          if proof.omitted then evidence.omissionsApplied += 1
          if proof.untouchedIdentity then evidence.untouchedIdentitiesPreserved += 1
          proof.result.tree

  private final case class PositiveProof(
      result: Result,
      freeApply: Boolean,
      freeIdentifier: Boolean,
      repeatedPreserved: Boolean,
      byNamePreserved: Boolean,
      omitted: Boolean,
      untouchedIdentity: Boolean
  )

  private def rewritePositive(root: untpd.TypeDef)(using Context): Either[String, PositiveProof] =
    val className = root.name.toString
    val original = template(root).body.toVector
    val path = s"generated/${className}Bonus.scala"
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      eval <- captured.method(captured.members(0).ref).left.map(_.message)
      sum <- captured.method(captured.members(1).ref).left.map(_.message)
      evalParameter = eval.parameterClauses.head.parameters.head
      sumParameter = sum.parameterClauses.head.parameters.head
      plan <- buildPositivePlan(className, captured, evalParameter.ref, sumParameter.ref, path)
        .left.map(_.message)
      result <- ExistingClassUntypedRewrite(captured, plan).left.map(_.message)
      members = template(result.tree).body.toVector
      names = members.map(_.asInstanceOf[untpd.MemberDef].name.toString)
      generated = members.last.asInstanceOf[untpd.DefDef]
      generatedTopology = generated.rhs match
        case untpd.Apply(function: untpd.Ident, arguments) =>
          (function.name.toString == "sum" && arguments.size == 3, false)
        case identifier: untpd.Ident =>
          (false, identifier.name.toString == "untouched")
        case _ => (false, false)
      repeatedExpected = Set("U063B", "U063C", "U063D1", "U063D2").contains(className)
      byNameExpected = Set("U063C", "U063D1", "U063D2").contains(className)
      omittedExpected = Set("U063D1", "U063D2").contains(className)
      rewrittenSum = members(1).asInstanceOf[untpd.DefDef]
      repeatedPreserved = !repeatedExpected || parameter(rewrittenSum).tpt.isInstanceOf[untpd.PostfixOp]
      rewrittenEval = members(0).asInstanceOf[untpd.DefDef]
      byNamePreserved = !byNameExpected || parameter(rewrittenEval).tpt.isInstanceOf[untpd.ByNameTypeTree]
      untouchedResultIndex = if omittedExpected then 2 else 3
      expectedNames = Vector("eval", "sum") ++
        (if omittedExpected then Vector.empty else Vector("obsolete")) ++
        Vector("untouched", "bonus")
      _ <- Either.cond(names == expectedNames, (), "unexpected final member order: " + names)
      _ <- Either.cond(generated.source.path == path, (), "generated source path mismatch")
    yield PositiveProof(
      result,
      generatedTopology._1,
      generatedTopology._2,
      repeatedExpected && repeatedPreserved,
      byNameExpected && byNamePreserved,
      omittedExpected,
      members(untouchedResultIndex).eq(original(3)) &&
        result.directMemberIdentities(untouchedResultIndex).sameObjectAs(captured.members(3).identity)
    )

  private def buildPositivePlan(
      className: String,
      captured: ExistingClassUntypedRewrite.Capture,
      evalParameter: ExistingClassUntypedRewrite.ParameterRef,
      sumParameter: ExistingClassUntypedRewrite.ParameterRef,
      path: String
  ): Either[Failure, ExistingClassUntypedRewrite.EditPlan] =
    className match
      case "U063A" =>
        captured.emptyPlan.append(generatedApplyMethod("bonus", "sum"), path)
      case "U063B" =>
        for
          p1 <- captured.emptyPlan.replaceParameterType(sumParameter, IntType)
          p2 <- p1.append(generatedApplyMethod("bonus", "sum"), path)
        yield p2
      case "U063C" =>
        for
          p1 <- captured.emptyPlan.replaceParameterType(evalParameter, IntType)
          p2 <- p1.replaceParameterType(sumParameter, IntType)
          p3 <- p2.append(generatedApplyMethod("bonus", "sum"), path)
        yield p3
      case "U063D1" =>
        for
          p1 <- captured.emptyPlan.replaceParameterType(evalParameter, IntType)
          p2 <- p1.replaceParameterType(sumParameter, IntType)
          p3 <- p2.omit(captured.members(2).ref)
          p4 <- p3.append(generatedApplyMethod("bonus", "sum"), path)
        yield p4
      case "U063D2" =>
        for
          p1 <- captured.emptyPlan.omit(captured.members(2).ref)
          p2 <- p1.append(generatedApplyMethod("bonus", "sum"), path)
          p3 <- p2.replaceParameterType(sumParameter, IntType)
          p4 <- p3.replaceParameterType(evalParameter, IntType)
        yield p4
      case "U063E" =>
        captured.emptyPlan.append(generatedIdentifierMethod("bonus", "untouched"), path)
      case other => Left(Failure("INTERNAL_INVARIANT_FAILED", "unexpected positive fixture " + other))

  private final class NegativeDriver(mode: NegativeMode) extends Driver:
    @volatile var publicTransactionSucceeded = false
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new NegativeRewriteBeforeTyper(mode, NegativeDriver.this)) ::
            super.frontendPhases.tail

  private final class NegativeRewriteBeforeTyper(mode: NegativeMode, evidence: NegativeDriver)
      extends Phase:
    def phaseName: String = "u063GeneratedSiblingNegative"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString == "U063Negative" =>
              val transaction = for
                captured <- ExistingClassUntypedRewrite.capture(root)
                plan <- mode match
                  case NegativeMode.OmittedSibling =>
                    for
                      p1 <- captured.emptyPlan.omit(captured.members(2).ref)
                      p2 <- p1.append(
                        generatedIdentifierMethod("bonus", "obsolete"),
                        "generated/U063OmittedSiblingBonus.scala"
                      )
                    yield p2
                  case NegativeMode.DuplicateName =>
                    captured.emptyPlan.append(
                      generatedLiteralMethod("untouched", "5"),
                      "generated/U063DuplicateUntouched.scala"
                    )
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

  private def generatedLiteralMethod(label: String, literal: String): SemanticDefinition =
    semantic(SemanticDefinition.concreteMethod(
      semantic(DefinitionName.fromSource(label)),
      Vector.empty,
      IntType
    )(_ => Right(TermShape.Literal(literal))))

  private def semantic[A](value: Either[DefinitionSemanticError, A]): A =
    value.fold(problem => throw new IllegalArgumentException(problem.message), identity)

  private def template(root: untpd.TypeDef)(using Context): untpd.Template =
    root.rhs.asInstanceOf[untpd.Template]

  private def parameter(method: untpd.DefDef): untpd.ValDef =
    method.paramss.head.head.asInstanceOf[untpd.ValDef]

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ExistingUntpdClassMemberFilter.allTrees(tree)

  private def emitted(output: Path): Vector[String] =
    val stream = Files.walk(output)
    try stream.filter(Files.isRegularFile(_)).iterator().asScala.map(_.toString).toVector
    finally stream.close()

  private def invoke(loader: ClassLoader, methodName: String): Int =
    val moduleClass = loader.loadClass("U063GeneratedSiblingUse$")
    val module = moduleClass.getField("MODULE$").get(null)
    moduleClass.getMethod(methodName).invoke(module).asInstanceOf[Integer].intValue

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

  private val PositiveProgram =
    """class U063A:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |
      |class U063B:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |
      |class U063C:
      |  def eval(lazyValue: => AnyVal): Int = lazyValue.hashCode + lazyValue.hashCode
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |
      |class U063D1:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |
      |class U063D2:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |
      |class U063E:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |
      |object U063GeneratedSiblingUse:
      |  def appendOnly: Int = new U063A().bonus
      |  def rewrittenRepeated: Int = new U063B().bonus
      |  def bothModes: Int =
      |    var counter = 0
      |    val instance = new U063C()
      |    instance.eval({ counter += 1; counter }) * 1000 + instance.bonus * 10 + counter
      |  def editsFirst: Int =
      |    var counter = 0
      |    val instance = new U063D1()
      |    instance.eval({ counter += 1; counter }) + instance.bonus + instance.untouched + counter
      |  def omitFirst: Int =
      |    var counter = 0
      |    val instance = new U063D2()
      |    instance.eval({ counter += 1; counter }) + instance.bonus + instance.untouched + counter
      |  def retainedValue: Int = new U063E().bonus
      |""".stripMargin

  private val NegativeProgram =
    """class U063Negative:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |""".stripMargin
