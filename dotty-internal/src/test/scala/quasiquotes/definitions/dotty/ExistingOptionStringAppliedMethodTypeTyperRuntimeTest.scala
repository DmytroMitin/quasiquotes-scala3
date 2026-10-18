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

import quasiquotes.types.{
  ResolvedTypeNameId,
  ResolvedTypeOwnerKind,
  ResolvedTypeOwnerSegment,
  TypeNormalForm
}

class ExistingOptionStringAppliedMethodTypeTyperRuntimeTest extends munit.FunSuite:
  import ExistingClassUntypedRewrite.*

  private final case class ProductionProof(
      result: Result,
      positionEvidence: Boolean,
      untouchedIdentityAndOrder: Boolean
  )

  private val optionString: TypeNormalForm =
    val pkg = (name: String) =>
      ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, name)
    TypeNormalForm.STypeApply(
      TypeNormalForm.STypeResolved(ResolvedTypeNameId(Vector(pkg("scala")), "Option")),
      List(
        TypeNormalForm.STypeResolved(
          ResolvedTypeNameId(Vector(pkg("java"), pkg("lang")), "String")
        )
      )
    )

  test("public production facade P1-P5 reaches ordinary Typer, TASTy and runtime with canonical Option[String]"):
    val temporary = Files.createTempDirectory("u071-option-runtime-")
    try
      val source = temporary.resolve("U071OptionRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(source, positiveProgram, StandardCharsets.UTF_8)

      val baseline = new Driver().process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))
      assert(baseline.hasErrors, "the source call sites must require the pre-Typer Option[String] rewrite")
      deleteRecursively(output)
      Files.createDirectories(output)

      val driver = new RewriteDriver
      val reporter = driver.process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))

      assert(!reporter.hasErrors, clues(reporter.allErrors.toVector.map(_.message)))
      assertEquals(driver.rewritten.sorted, Vector("U071CombinedBody", "U071CombinedUnchanged", "U071MixedOwner", "U071ParameterOnly", "U071ResultOnly"))
      assert(driver.preTyperClean)
      assert(driver.positionEvidence)
      assert(driver.untouchedIdentityAndOrder)
      val files = emitted(output)
      driver.rewritten.foreach(name => assert(files.exists(_.endsWith(name + ".tasty")), clues(name, files)))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        assertEquals(invoke(loader, "u071.runtime.U071Use$", "combinedBody"), Some("ok"))
        assertEquals(invoke(loader, "u071.runtime.U071Use$", "combinedUnchanged"), Some("ok"))
        assertEquals(invoke(loader, "u071.runtime.U071Use$", "mixed"), Some("ok"))
      finally loader.close()
    finally deleteRecursively(temporary)

  test("production reconstruction does not hide an incompatible unchanged body from ordinary Typer"):
    val outcome = compileNegative("U071IncompatibleBody.scala", incompatibleBodyProgram, "U071IncompatibleBody")
    assert(outcome.rewriteSucceeded)
    assert(outcome.errors.nonEmpty)
    assert(outcome.errors.exists(message => message.contains("value trim is not a member") || message.contains("Found:") || message.contains("Required:")), clues(outcome.errors))
    assert(!outcome.emitted.exists(_.endsWith("U071IncompatibleBody.tasty")))

  test("canonical resolved pair survives imported Option String and scala aliases through runtime"):
    val outcome = compileNegative("U071ImportedAlias.scala", importedAliasProgram, "U071ImportedAlias")
    assert(outcome.rewriteSucceeded)
    assertEquals(outcome.errors, Vector.empty)
    assert(outcome.emitted.exists(_.endsWith("U071ImportedAlias.tasty")), clues(outcome.emitted))
    assertEquals(outcome.runtimeValue, Some(Some("ok")))

  private final case class NegativeOutcome(
      rewriteSucceeded: Boolean,
      errors: Vector[String],
      emitted: Vector[String],
      runtimeValue: Option[Option[String]]
  )

  private final class RewriteDriver extends Driver:
    @volatile var rewritten = Vector.empty[String]
    @volatile var preTyperClean = true
    @volatile var positionEvidence = true
    @volatile var untouchedIdentityAndOrder = true

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this, None)) :: super.frontendPhases.tail

  private final class NegativeDriver(owner: String) extends Driver:
    @volatile var rewriteSucceeded = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(null, Some(owner -> NegativeDriver.this))) :: super.frontendPhases.tail

  private final class RewriteBeforeTyper(
      positive: RewriteDriver,
      negative: Option[(String, NegativeDriver)]
  ) extends Phase:
    def phaseName: String = "u071OptionStringAppliedMethodTypePrototype"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val transformed = new untpd.UntypedTreeMap:
        override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match
          case root: untpd.TypeDef if selected(root.name.toString) =>
            rewrite(root, mode(root.name.toString)) match
              case Left(problem) =>
                report.error(problem)
                root
              case Right(proof) =>
                if positive != null then
                  positive.rewritten = positive.rewritten :+ root.name.toString
                  positive.preTyperClean &&= allTrees(proof.result.tree).forall(clean)
                  positive.positionEvidence &&= proof.positionEvidence
                  positive.untouchedIdentityAndOrder &&= proof.untouchedIdentityAndOrder
                negative.foreach(_._2.rewriteSucceeded = true)
                proof.result.tree
          case other => super.transform(other)
      summon[Context].compilationUnit.untpdTree = transformed.transform(unitTree)

    private def selected(name: String): Boolean =
      negative.fold(name.startsWith("U071") && name != "U071Use")(_._1 == name)

    private def mode(name: String): Mode = name match
      case "U071ParameterOnly" => Mode.ParameterOnly
      case "U071ResultOnly" => Mode.ResultOnly
      case "U071CombinedBody" | "U071MixedOwner" => Mode.BothBody
      case _ => Mode.BothUnchanged

  private enum Mode:
    case ParameterOnly, ResultOnly, BothBody, BothUnchanged

  private def rewrite(root: untpd.TypeDef, mode: Mode)(using Context): Either[String, ProductionProof] =
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      methodIndex <- captured.members.indexWhere(_.kind == MemberKind.Method) match
        case -1 => Left("fixture method was not captured")
        case value => Right(value)
      method <- captured.method(captured.members(methodIndex).ref).left.map(_.message)
      parameter <- method.parameterClauses.headOption.flatMap(_.parameters.headOption).toRight("fixture parameter was not captured")
      body <- method.parameterScope.reference(parameter.ref).left.map(_.message)
      plan1 <- mode match
        case Mode.ResultOnly => Right(captured.emptyPlan)
        case _ => captured.emptyPlan.replaceParameterType(parameter.ref, optionString).left.map(_.message)
      plan2 <- mode match
        case Mode.ParameterOnly => Right(plan1)
        case _ => plan1.replaceResultType(method.ref, optionString).left.map(_.message)
      plan3 <- mode match
        case Mode.BothBody => plan2.replaceBody(method.ref, body).left.map(_.message)
        case _ => Right(plan2)
      result <- ExistingClassUntypedRewrite(captured, plan3).left.map(_.message)
      rewrittenMethod <- result.tree.rhs.asInstanceOf[untpd.Template].body.lift(methodIndex) match
        case Some(value: untpd.DefDef) => Right(value)
        case other => Left(s"rewritten method was unavailable: $other")
      oldParameter = method.ref.methodIdentity.paramss.head.head.asInstanceOf[untpd.ValDef]
      rewrittenParameter = rewrittenMethod.paramss.head.head.asInstanceOf[untpd.ValDef]
      positionEvidence = Vector(
        Option.when(mode != Mode.ResultOnly)(positionedEvidence(rewrittenParameter.tpt, oldParameter.tpt)),
        Option.when(mode != Mode.ParameterOnly)(positionedEvidence(rewrittenMethod.tpt, method.ref.methodIdentity.tpt))
      ).flatten.forall(identity)
      untouchedIdentity = captured.members.indices.forall(index =>
        index == methodIndex || result.directMemberIdentities(index).sameObjectAs(captured.members(index).identity)
      )
      selectedFresh = !result.directMemberIdentities(methodIndex).sameObjectAs(captured.members(methodIndex).identity)
    yield ProductionProof(
      result,
      positionEvidence,
      result.directMemberIdentities.size == captured.members.size && untouchedIdentity && selectedFresh
    )

  private def positionedEvidence(positioned: untpd.Tree, oldSite: untpd.Tree)(using Context): Boolean =
    val nodes = allTrees(positioned)
    nodes.size == 8 && positioned.source == oldSite.source &&
      positioned.span == oldSite.span &&
      nodes.drop(1).forall(node => node.source.exists && node.span.exists) &&
      nodes.drop(1).forall(_.source.path == "generated/CanonicalResolvedOptionStringMethodType.scala")

  private def clean(tree: untpd.Tree)(using Context): Boolean =
    tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]

  private def compileNegative(fileName: String, program: String, owner: String): NegativeOutcome =
    val temporary = Files.createTempDirectory("u071-option-negative-")
    try
      val source = temporary.resolve(fileName)
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(source, program, StandardCharsets.UTF_8)
      val driver = new NegativeDriver(owner)
      val reporter = driver.process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))
      val errors = reporter.allErrors.toVector.map(_.message)
      val runtime =
        if errors.isEmpty && owner == "U071ImportedAlias" then
          val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
          try Some(invoke(loader, "u071.aliasnegative.U071ImportedAliasUse$", "value"))
          finally loader.close()
        else None
      NegativeOutcome(
        driver.rewriteSucceeded,
        errors,
        emitted(output),
        runtime
      )
    finally deleteRecursively(temporary)

  private def positiveProgram: String =
    """package u071.runtime
      |
      |class U071ParameterOnly:
      |  val untouchedBefore: Int = 1
      |  def echo(value: String): String = throw new RuntimeException("not invoked")
      |  val untouchedAfter: Int = 2
      |
      |class U071ResultOnly:
      |  val untouchedBefore: Int = 1
      |  def echo(value: String): String = throw new RuntimeException("not invoked")
      |  val untouchedAfter: Int = 2
      |
      |class U071CombinedBody:
      |  val untouchedBefore: Int = 1
      |  def echo(value: String): String = value
      |  val untouchedAfter: Int = 2
      |
      |class U071CombinedUnchanged:
      |  val untouchedBefore: Int = 1
      |  def echo(value: String): String = value
      |  val untouchedAfter: Int = 2
      |
      |class U071MixedOwner:
      |  val untouchedBefore: Int = 1
      |  def echo(value: String): String = value
      |  val untouchedAfter: Int = 2
      |
      |object U071Use:
      |  def signatureChecks(): Unit =
      |    val parameterOnly: String = new U071ParameterOnly().echo(Some("ok"))
      |    val resultOnly: Option[String] = new U071ResultOnly().echo("ok")
      |  def combinedBody: Option[String] = new U071CombinedBody().echo(Some("ok"))
      |  def combinedUnchanged: Option[String] = new U071CombinedUnchanged().echo(Some("ok"))
      |  def mixed: Option[String] =
      |    val service = new U071MixedOwner()
      |    assert(service.untouchedBefore == 1)
      |    assert(service.untouchedAfter == 2)
      |    service.echo(Some("ok"))
      |""".stripMargin

  private def incompatibleBodyProgram: String =
    """package u071.negative
      |class U071IncompatibleBody:
      |  def echo(value: String): String = value.trim
      |object U071IncompatibleBodyUse:
      |  val value: Option[String] = new U071IncompatibleBody().echo(Some("ok"))
      |""".stripMargin

  private def importedAliasProgram: String =
    """package aliases {
      |  object Types {
      |    type Option[A] = List[A]
      |    type List[A] = Vector[A]
      |    type String = Int
      |  }
      |  object Names {
      |    object scala {
      |      type Option[A] = List[A]
      |    }
      |  }
      |}
      |package u071.aliasnegative {
      |  import aliases.Types.{Option, List, String}
      |  import aliases.Names.scala
      |  class U071ImportedAlias {
      |    def echo(value: String): String = value
      |  }
      |  object U071ImportedAliasUse {
      |    def value: _root_.scala.Option[_root_.java.lang.String] =
      |      new U071ImportedAlias().echo(_root_.scala.Some("ok"))
      |  }
      |}
      |""".stripMargin

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ExistingUntpdClassMemberFilter.allTrees(tree)

  private def emitted(output: Path): Vector[String] =
    val stream = Files.walk(output)
    try stream.filter(Files.isRegularFile(_)).iterator().asScala.map(_.toString).toVector
    finally stream.close()

  private def invoke(loader: URLClassLoader, moduleName: String, method: String): Option[String] =
    val moduleClass = loader.loadClass(moduleName)
    val module = moduleClass.getField("MODULE$").get(null)
    moduleClass.getMethod(method).invoke(module).asInstanceOf[Option[String]]

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
