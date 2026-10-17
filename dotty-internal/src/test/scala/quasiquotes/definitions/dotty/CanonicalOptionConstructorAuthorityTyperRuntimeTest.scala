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

final class CanonicalOptionConstructorAuthorityTyperRuntimeTest extends munit.FunSuite:
  import ExistingClassUntypedRewrite.*
  import U071OptionStringAppliedMethodTypeOracle.optionString
  import U072CanonicalOptionConstructorAuthorityOracle.*

  test("root-qualified constructor survives imported Option and scala aliases through Typer TASTy and runtime"):
    val rooted = compileCase(
      "U072Rooted.scala",
      program("import aliases.Types.Option\n  import aliases.Names.scala", "U072Rooted", Candidate.Rooted),
      "U072Rooted",
      Candidate.Rooted
    )
    assert(rooted.oracleSucceeded)
    assertEquals(rooted.errors, Vector.empty)
    assert(rooted.preTyperClean)
    assert(rooted.positionEvidence)
    assert(rooted.emitted.exists(_.endsWith("U072Rooted.tasty")), clues(rooted.emitted))
    assertEquals(rooted.runtimeValue, Some(Some("ok")))

  test("fully rooted constructor and String survive all imported aliases through Typer TASTy and runtime"):
    val rootedPair = compileCase(
      "U072RootedPair.scala",
      program(
        "import aliases.Types.{Option, String}\n  import aliases.Names.scala",
        "U072RootedPair",
        Candidate.RootedPair
      ),
      "U072RootedPair",
      Candidate.RootedPair
    )
    assert(rootedPair.oracleSucceeded)
    assertEquals(rootedPair.errors, Vector.empty)
    assert(rootedPair.preTyperClean)
    assert(rootedPair.positionEvidence)
    assert(rootedPair.emitted.exists(_.endsWith("U072RootedPair.tasty")), clues(rootedPair.emitted))
    assertEquals(rootedPair.runtimeValue, Some(Some("ok")))

  test("unqualified Option is captured by enclosing imported Option alias"):
    val outcome = compileCase(
      "U072Unqualified.scala",
      program("import aliases.Types.Option", "U072Unqualified", Candidate.Unqualified),
      "U072Unqualified",
      Candidate.Unqualified
    )
    assert(outcome.oracleSucceeded)
    assert(outcome.errors.nonEmpty)
    assert(outcome.errors.exists(message => message.contains("List") || message.contains("Found:") || message.contains("Required:")), clues(outcome.errors))
    assert(!outcome.emitted.exists(_.endsWith("U072Unqualified.tasty")))

  test("non-rooted scala.Option is captured by an imported scala namespace"):
    val outcome = compileCase(
      "U072Qualified.scala",
      program("import aliases.Names.scala", "U072Qualified", Candidate.Qualified),
      "U072Qualified",
      Candidate.Qualified
    )
    assert(outcome.oracleSucceeded)
    assert(outcome.errors.nonEmpty)
    assert(outcome.errors.exists(message => message.contains("List") || message.contains("Found:") || message.contains("Required:")), clues(outcome.errors))
    assert(!outcome.emitted.exists(_.endsWith("U072Qualified.tasty")))

  test("rooted scala.Option with bare String remains captured by imported String alias"):
    val outcome = compileCase(
      "U072RootedBareString.scala",
      program("import aliases.Types.String", "U072RootedBareString", Candidate.Rooted),
      "U072RootedBareString",
      Candidate.Rooted
    )
    assert(outcome.oracleSucceeded)
    assert(outcome.errors.nonEmpty)
    assert(outcome.errors.exists(message => message.contains("Int") || message.contains("Found:") || message.contains("Required:")), clues(outcome.errors))
    assert(!outcome.emitted.exists(_.endsWith("U072RootedBareString.tasty")))

  test("ordinary source confirms term Option does not shadow the Type namespace while Type binders do"):
    val termOutcome = compileOrdinary(
      "U072TermOption.scala",
      """package u072.ordinary
        |class TermOption:
        |  def echo(value: _root_.scala.Option[_root_.java.lang.String]): _root_.scala.Option[_root_.java.lang.String] =
        |    val Option = 1
        |    value
        |object TermOptionUse:
        |  val value = new TermOption().echo(_root_.scala.Some("ok"))
        |""".stripMargin
    )
    assertEquals(termOutcome, Vector.empty)

    val binderOutcome = compileOrdinary(
      "U072BareStringBinder.scala",
      """package u072.ordinary
        |class BareStringBinder:
        |  def echo[String](value: _root_.scala.Option[String]): _root_.scala.Option[String] = value
        |object BareStringBinderUse:
        |  val value: _root_.scala.Option[_root_.java.lang.String] =
        |    new BareStringBinder().echo[Int](_root_.scala.Some("ok"))
        |""".stripMargin
    )
    assert(binderOutcome.nonEmpty)

  test("ordinary source resolves all four spellings and rooted pair survives local Type shadows"):
    val outcome = compileOrdinary(
      "U072OrdinaryResolutionMatrix.scala",
      """package u072.sourcealiases {
        |  object Types {
        |    type Option[A] = List[A]
        |    type String = Int
        |  }
        |  object Names {
        |    object scala {
        |      type Option[A] = List[A]
        |    }
        |  }
        |}
        |package u072.ordinarymatrix {
        |  object UnqualifiedScope {
        |    import u072.sourcealiases.Types.Option
        |    def echo(value: Option[String]): Option[String] = value
        |    val value: List[String] = echo(List("ok"))
        |  }
        |  object QualifiedScope {
        |    import u072.sourcealiases.Names.scala
        |    def echo(value: scala.Option[String]): scala.Option[String] = value
        |    val value: List[String] = echo(List("ok"))
        |  }
        |  object RootedScope {
        |    import u072.sourcealiases.Types.Option
        |    import u072.sourcealiases.Names.scala
        |    def echo(value: _root_.scala.Option[String]): _root_.scala.Option[String] = value
        |    val value: _root_.scala.Option[String] = echo(_root_.scala.Some("ok"))
        |  }
        |  object RootedPairScope {
        |    import u072.sourcealiases.Types.{Option, String}
        |    import u072.sourcealiases.Names.scala
        |    def echo(value: _root_.scala.Option[_root_.java.lang.String]): _root_.scala.Option[_root_.java.lang.String] = value
        |    val value = echo(_root_.scala.Some("ok"))
        |  }
        |  class LocalShadows {
        |    type Option[A] = List[A]
        |    type String = Int
        |    def echo[Option, String](value: _root_.scala.Option[_root_.java.lang.String]): _root_.scala.Option[_root_.java.lang.String] =
        |      val Option = 1
        |      value
        |  }
        |  object LocalShadowsUse {
        |    val value = new LocalShadows().echo[Int, Boolean](_root_.scala.Some("ok"))
        |  }
        |}
        |""".stripMargin
    )
    assertEquals(outcome, Vector.empty)

  private final case class Outcome(
      oracleSucceeded: Boolean,
      preTyperClean: Boolean,
      positionEvidence: Boolean,
      errors: Vector[String],
      emitted: Vector[String],
      runtimeValue: Option[Option[String]]
  )

  private final class RewriteDriver(owner: String, candidate: Candidate) extends Driver:
    @volatile var oracleSucceeded = false
    @volatile var preTyperClean = false
    @volatile var positionEvidence = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(owner, candidate, RewriteDriver.this)) :: super.frontendPhases.tail

  private final class RewriteBeforeTyper(owner: String, candidate: Candidate, evidence: RewriteDriver) extends Phase:
    def phaseName: String = "u072CanonicalOptionConstructorAuthorityPrototype"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val transformed = new untpd.UntypedTreeMap:
        override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match
          case root: untpd.TypeDef if root.name.toString == owner =>
            rewrite(root, candidate) match
              case Left(problem) =>
                report.error(problem)
                root
              case Right(proof) =>
                evidence.oracleSucceeded = true
                evidence.preTyperClean = allTrees(proof.reconstructed.root).forall(clean)
                evidence.positionEvidence = Vector(proof.parameterType, proof.resultType).flatten.forall(positionedEvidence)
                proof.reconstructed.root
          case other => super.transform(other)
      summon[Context].compilationUnit.untpdTree = transformed.transform(unitTree)

  private def rewrite(root: untpd.TypeDef, candidate: Candidate)(using Context): Either[String, Proof] =
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      methodIndex <- captured.members.indexWhere(_.kind == MemberKind.Method) match
        case -1 => Left("fixture method was not captured")
        case value => Right(value)
      method <- captured.method(captured.members(methodIndex).ref).left.map(_.message)
      parameter <- method.parameterClauses.headOption.flatMap(_.parameters.headOption).toRight("fixture parameter was not captured")
      body <- method.parameterScope.reference(parameter.ref).left.map(_.message)
      plan1 <- captured.emptyPlan.replaceParameterType(parameter.ref, optionString).left.map(_.message)
      plan2 <- plan1.replaceResultType(method.ref, optionString).left.map(_.message)
      plan3 <- plan2.replaceBody(method.ref, body).left.map(_.message)
      proof <- U072CanonicalOptionConstructorAuthorityOracle(captured, plan3, candidate).left.map(_.message)
    yield proof

  private def positionedEvidence(value: PositionedType)(using Context): Boolean =
    val nodes = allTrees(value.positioned)
    value.positioned.source == value.oldSite.source && value.positioned.span == value.oldSite.span &&
      nodes.drop(1).forall(node => node.source.exists && node.source.path == value.candidate.generatedPath && node.span.exists)

  private def clean(tree: untpd.Tree)(using Context): Boolean =
    tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]

  private def program(imports: String, owner: String, candidate: Candidate): String =
    s"""package aliases {
       |  object Types {
       |    type Option[A] = List[A]
       |    type String = Int
       |  }
       |  object Names {
       |    object scala {
       |      type Option[A] = List[A]
       |    }
       |  }
       |}
       |package u072.runtime {
       |  $imports
       |  class $owner {
       |    val untouchedBefore: Int = 1
       |    def echo(value: String): String = value
       |    val untouchedAfter: Int = 2
       |  }
       |  object ${owner}Use {
       |    def value: _root_.scala.Option[_root_.java.lang.String] =
       |      val service = new $owner()
       |      assert(service.untouchedBefore == 1)
       |      assert(service.untouchedAfter == 2)
       |      service.echo(_root_.scala.Some("ok"))
       |  }
       |}
       |""".stripMargin

  private def compileCase(fileName: String, sourceText: String, owner: String, candidate: Candidate): Outcome =
    val temporary = Files.createTempDirectory("u072-option-authority-")
    try
      val source = temporary.resolve(fileName)
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(source, sourceText, StandardCharsets.UTF_8)
      val driver = new RewriteDriver(owner, candidate)
      val reporter = driver.process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))
      val errors = reporter.allErrors.toVector.map(_.message)
      val runtime =
        if errors.isEmpty then
          val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
          try Some(invoke(loader, s"u072.runtime.${owner}Use$$", "value"))
          finally loader.close()
        else None
      Outcome(
        driver.oracleSucceeded,
        driver.preTyperClean,
        driver.positionEvidence,
        errors,
        emitted(output),
        runtime
      )
    finally deleteRecursively(temporary)

  private def compileOrdinary(fileName: String, sourceText: String): Vector[String] =
    val temporary = Files.createTempDirectory("u072-option-ordinary-")
    try
      val source = temporary.resolve(fileName)
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(source, sourceText, StandardCharsets.UTF_8)
      new Driver().process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      )).allErrors.toVector.map(_.message)
    finally deleteRecursively(temporary)

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
