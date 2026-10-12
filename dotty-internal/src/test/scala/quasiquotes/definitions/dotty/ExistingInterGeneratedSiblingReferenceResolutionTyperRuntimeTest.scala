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

final class ExistingInterGeneratedSiblingReferenceResolutionTyperRuntimeTest
    extends munit.FunSuite:
  private enum NegativeMode:
    case MissingGeneratedTarget, DuplicateGeneratedName, OmittedExistingTarget

  test("both append orders typecheck emit TASTy and execute through two generated method levels") {
    val temporary = Files.createTempDirectory("u064-inter-generated-positive-")
    try
      val source = temporary.resolve("U064InterGenerated.scala")
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
      assertEquals(
        driver.rewritten,
        Set("U064A", "U064B", "U064C1", "U064C2", "U064D1", "U064D2", "U064D3", "U064D4", "U064E")
      )
      assertEquals(driver.dependencyFirstOrders, 5)
      assertEquals(driver.forwardOrders, 4)
      assertEquals(driver.callingModesPreserved, 6)
      assertEquals(driver.omissionsApplied, 4)
      assertEquals(driver.untouchedIdentitiesPreserved, 9)
      assertEquals(driver.distinctGeneratedSources, 9)
      assert(driver.preTyperClean)

      val files = emitted(output)
      driver.rewritten.foreach(name => assert(files.exists(_.endsWith(s"$name.tasty")), clues(name)))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        assertEquals(invoke(loader, "dependencyFirst"), 3)
        assertEquals(invoke(loader, "forwardFirst"), 3)
        assertEquals(invoke(loader, "editedDependencyFirst"), 3032)
        assertEquals(invoke(loader, "editedForwardFirst"), 3032)
        assertEquals(invoke(loader, "dependencyEditsFirst"), 10)
        assertEquals(invoke(loader, "dependencyAppendsFirst"), 10)
        assertEquals(invoke(loader, "forwardEditsFirst"), 10)
        assertEquals(invoke(loader, "forwardAppendsFirst"), 10)
        assertEquals(invoke(loader, "retainedValueTwoHop"), 7)
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  test("bonus without generated relay is admitted pre-Typer then rejected at its virtual source") {
    assertNegative(
      NegativeMode.MissingGeneratedTarget,
      "relay",
      "generated/U064MissingBonus.scala"
    )
  }

  test("two generated methods with the same fresh name are admitted pre-Typer then rejected as duplicates") {
    assertNegative(
      NegativeMode.DuplicateGeneratedName,
      "generatedDuplicate",
      "generated/U064DuplicateOne.scala"
    )
  }

  test("relay reference to an omitted existing member is rejected at relay source and emits no TASTy") {
    assertNegative(
      NegativeMode.OmittedExistingTarget,
      "obsolete",
      "generated/U064OmittedRelay.scala"
    )
  }

  private def assertNegative(
      mode: NegativeMode,
      expectedName: String,
      generatedDiagnosticPath: String
  ): Unit =
    val temporary = Files.createTempDirectory("u064-inter-generated-negative-")
    try
      val source = temporary.resolve("U064Negative.scala")
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
        case NegativeMode.DuplicateGeneratedName =>
          assert(reporter.allErrors.exists(_.message.contains("Conflicting definitions")), clues(reporter.allErrors))
        case _ =>
          assert(reporter.allErrors.exists(_.message.contains("Not found")), clues(reporter.allErrors))
      val expectedDiagnosticPath = mode match
        case NegativeMode.DuplicateGeneratedName => source.toString
        case _ => generatedDiagnosticPath
      assert(reporter.allErrors.exists(problem =>
        problem.pos != null && problem.pos.source != null &&
          problem.pos.source.path == expectedDiagnosticPath
      ), clues(reporter.allErrors))
      assert(!emitted(output).exists(_.endsWith("U064Negative.tasty")))
    finally deleteRecursively(temporary)

  private final class PositiveDriver extends Driver:
    @volatile var rewritten = Set.empty[String]
    @volatile var dependencyFirstOrders = 0
    @volatile var forwardOrders = 0
    @volatile var callingModesPreserved = 0
    @volatile var omissionsApplied = 0
    @volatile var untouchedIdentitiesPreserved = 0
    @volatile var distinctGeneratedSources = 0
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new PositiveRewriteBeforeTyper(PositiveDriver.this)) ::
            super.frontendPhases.tail

  private final class PositiveRewriteBeforeTyper(evidence: PositiveDriver) extends Phase:
    def phaseName: String = "u064InterGeneratedPositive"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString.matches("U064(A|B|C1|C2|D1|D2|D3|D4|E)") =>
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
          if proof.forward then evidence.forwardOrders += 1 else evidence.dependencyFirstOrders += 1
          if proof.callingModesPreserved then evidence.callingModesPreserved += 1
          if proof.omitted then evidence.omissionsApplied += 1
          if proof.untouchedIdentity then evidence.untouchedIdentitiesPreserved += 1
          if proof.distinctSources then evidence.distinctGeneratedSources += 1
          proof.result.tree

  private final case class PositiveProof(
      result: Result,
      forward: Boolean,
      callingModesPreserved: Boolean,
      omitted: Boolean,
      untouchedIdentity: Boolean,
      distinctSources: Boolean
  )

  private def rewritePositive(root: untpd.TypeDef)(using Context): Either[String, PositiveProof] =
    val className = root.name.toString
    val original = template(root).body.toVector
    val forward = Set("U064B", "U064C2", "U064D3", "U064D4").contains(className)
    val editModes = Set("U064C1", "U064C2", "U064D1", "U064D2", "U064D3", "U064D4").contains(className)
    val omit = Set("U064D1", "U064D2", "U064D3", "U064D4").contains(className)
    val appendsFirst = Set("U064D2", "U064D4").contains(className)
    val valueControl = className == "U064E"
    val relayPath = s"generated/${className}Relay.scala"
    val bonusPath = s"generated/${className}Bonus.scala"
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      eval <- captured.method(captured.members(0).ref).left.map(_.message)
      sum <- captured.method(captured.members(1).ref).left.map(_.message)
      plan <- buildPositivePlan(
        captured,
        eval.parameterClauses.head.parameters.head.ref,
        sum.parameterClauses.head.parameters.head.ref,
        forward,
        editModes,
        omit,
        appendsFirst,
        valueControl,
        relayPath,
        bonusPath
      ).left.map(_.message)
      result <- ExistingClassUntypedRewrite(captured, plan).left.map(_.message)
      members = template(result.tree).body.toVector
      generated = members.takeRight(2).map(_.asInstanceOf[untpd.DefDef])
      generatedByName = generated.map(value => value.name.toString -> value).toMap
      relay <- generatedByName.get("relay").toRight("missing generated relay")
      bonus <- generatedByName.get("bonus").toRight("missing generated bonus")
      relayTopology = relay.rhs match
        case untpd.Apply(function: untpd.Ident, arguments) =>
          !valueControl && function.name.toString == "sum" && arguments.size == 3
        case identifier: untpd.Ident =>
          valueControl && identifier.name.toString == "untouched"
        case _ => false
      bonusTopology = bonus.rhs match
        case identifier: untpd.Ident => identifier.name.toString == "relay"
        case _ => false
      _ <- Either.cond(relayTopology && bonusTopology, (), "unexpected generated raw topology")
      generatedNames = if forward then Vector("bonus", "relay") else Vector("relay", "bonus")
      expectedNames = Vector("eval", "sum") ++
        (if omit then Vector.empty else Vector("obsolete")) ++ Vector("untouched") ++ generatedNames
      names = members.map(_.asInstanceOf[untpd.MemberDef].name.toString)
      _ <- Either.cond(names == expectedNames, (), "unexpected final member order: " + names)
      callingModes = !editModes || (
        parameter(members(0).asInstanceOf[untpd.DefDef]).tpt.isInstanceOf[untpd.ByNameTypeTree] &&
          parameter(members(1).asInstanceOf[untpd.DefDef]).tpt.isInstanceOf[untpd.PostfixOp]
      )
      untouchedIndex = if omit then 2 else 3
      untouchedExact = members(untouchedIndex).eq(original(3)) &&
        result.directMemberIdentities(untouchedIndex).sameObjectAs(captured.members(3).identity)
      distinctSources = relay.source.path == relayPath && bonus.source.path == bonusPath &&
        !relay.source.eq(bonus.source) && relay.span.start == 0 && bonus.span.start == 0 &&
        relay.span.end == relay.source.content.length && bonus.span.end == bonus.source.content.length
    yield PositiveProof(result, forward, editModes && callingModes, omit, untouchedExact, distinctSources)

  private def buildPositivePlan(
      captured: Capture,
      evalParameter: ExistingClassUntypedRewrite.ParameterRef,
      sumParameter: ExistingClassUntypedRewrite.ParameterRef,
      forward: Boolean,
      editModes: Boolean,
      omit: Boolean,
      appendsFirst: Boolean,
      valueControl: Boolean,
      relayPath: String,
      bonusPath: String
  ): Either[Failure, EditPlan] =
    def appendPair(plan: EditPlan): Either[Failure, EditPlan] =
      val relay = if valueControl then generatedIdentifierMethod("relay", "untouched")
        else generatedApplyMethod("relay", "sum")
      if forward then
        for
          p1 <- plan.append(generatedIdentifierMethod("bonus", "relay"), bonusPath)
          p2 <- p1.append(relay, relayPath)
        yield p2
      else
        for
          p1 <- plan.append(relay, relayPath)
          p2 <- p1.append(generatedIdentifierMethod("bonus", "relay"), bonusPath)
        yield p2

    if appendsFirst then
      for
        p1 <- appendPair(captured.emptyPlan)
        p2 <- if editModes then p1.replaceParameterType(evalParameter, IntType) else Right(p1)
        p3 <- if editModes then p2.replaceParameterType(sumParameter, IntType) else Right(p2)
        p4 <- if omit then p3.omit(captured.members(2).ref) else Right(p3)
      yield p4
    else
      for
        p1 <- if editModes then captured.emptyPlan.replaceParameterType(evalParameter, IntType)
          else Right(captured.emptyPlan)
        p2 <- if editModes then p1.replaceParameterType(sumParameter, IntType) else Right(p1)
        p3 <- if omit then p2.omit(captured.members(2).ref) else Right(p2)
        p4 <- appendPair(p3)
      yield p4

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
    def phaseName: String = "u064InterGeneratedNegative"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString == "U064Negative" =>
              val transaction = for
                captured <- ExistingClassUntypedRewrite.capture(root)
                plan <- mode match
                  case NegativeMode.MissingGeneratedTarget =>
                    captured.emptyPlan.append(
                      generatedIdentifierMethod("bonus", "relay"),
                      "generated/U064MissingBonus.scala"
                    )
                  case NegativeMode.DuplicateGeneratedName =>
                    for
                      p1 <- captured.emptyPlan.append(
                        generatedLiteralMethod("generatedDuplicate", "1"),
                        "generated/U064DuplicateOne.scala"
                      )
                      p2 <- p1.append(
                        generatedLiteralMethod("generatedDuplicate", "2"),
                        "generated/U064DuplicateTwo.scala"
                      )
                    yield p2
                  case NegativeMode.OmittedExistingTarget =>
                    for
                      p1 <- captured.emptyPlan.omit(captured.members(2).ref)
                      p2 <- p1.append(
                        generatedIdentifierMethod("relay", "obsolete"),
                        "generated/U064OmittedRelay.scala"
                      )
                      p3 <- p2.append(
                        generatedIdentifierMethod("bonus", "relay"),
                        "generated/U064OmittedBonus.scala"
                      )
                    yield p3
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
    val moduleClass = loader.loadClass("U064InterGeneratedUse$")
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
    """class U064A:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |
      |class U064B:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |
      |class U064C1:
      |  def eval(lazyValue: => AnyVal): Int = lazyValue.hashCode + lazyValue.hashCode
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |
      |class U064C2:
      |  def eval(lazyValue: => AnyVal): Int = lazyValue.hashCode + lazyValue.hashCode
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |
      |class U064D1:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |
      |class U064D2:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |
      |class U064D3:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |
      |class U064D4:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |
      |class U064E:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |
      |object U064InterGeneratedUse:
      |  def dependencyFirst: Int = new U064A().bonus
      |  def forwardFirst: Int = new U064B().bonus
      |  def editedDependencyFirst: Int =
      |    var counter = 0
      |    val instance = new U064C1()
      |    instance.eval({ counter += 1; counter }) * 1000 + instance.bonus * 10 + counter
      |  def editedForwardFirst: Int =
      |    var counter = 0
      |    val instance = new U064C2()
      |    instance.eval({ counter += 1; counter }) * 1000 + instance.bonus * 10 + counter
      |  def dependencyEditsFirst: Int =
      |    val instance = new U064D1()
      |    instance.eval(1) + instance.bonus + instance.untouched
      |  def dependencyAppendsFirst: Int =
      |    val instance = new U064D2()
      |    instance.eval(1) + instance.bonus + instance.untouched
      |  def forwardEditsFirst: Int =
      |    val instance = new U064D3()
      |    instance.eval(1) + instance.bonus + instance.untouched
      |  def forwardAppendsFirst: Int =
      |    val instance = new U064D4()
      |    instance.eval(1) + instance.bonus + instance.untouched
      |  def retainedValueTwoHop: Int = new U064E().bonus
      |""".stripMargin

  private val NegativeProgram =
    """class U064Negative:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |""".stripMargin
