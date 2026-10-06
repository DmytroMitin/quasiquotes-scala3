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
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.terms.{ConstructedTerm, TermBindingInternals}
import quasiquotes.terms.dotty.ConstructedTermUntypedBackend
import quasiquotes.types.TypeNormalForm
import quasiquotes.types.dotty.TypeUntypedLowering
import dotty.tools.dotc.parsing.Parser

final class ExistingCurriedMethodCaptureRewriteModelFitTyperRuntimeTest extends munit.FunSuite:
  test("test-only two-clause model fit survives U023 insertion Typer TASTy and runtime") {
    val temporary = Files.createTempDirectory("u052-existing-curried-model-fit-")
    try
      val source = temporary.resolve("U052ExistingCurriedRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(source, Program, StandardCharsets.UTF_8)

      val baseline = new Driver().process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))
      assert(baseline.hasErrors)
      deleteRecursively(output)
      Files.createDirectories(output)

      val driver = new RewriteDriver
      val reporter = driver.process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assert(driver.rewritten)
      assert(driver.clausesPreserved)
      assert(driver.untouchedMemberPreserved)
      assert(driver.preTyperClean)
      assert(emitted(output).exists(_.endsWith("U052ExistingCurried.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try assertEquals(invokeValue(loader, "U052Use$"), 42)
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class RewriteDriver extends Driver:
    @volatile var rewritten = false
    @volatile var clausesPreserved = false
    @volatile var untouchedMemberPreserved = false
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(evidence: RewriteDriver) extends Phase:
    def phaseName: String = "u052ExistingCurriedModelFit"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString == "U052ExistingCurried" =>
              ExistingCurriedMethodCaptureRewriteRuntimeProbe.rewrite(root) match
                case Left(problem) =>
                  report.error(problem)
                  root
                case Right(result) =>
                  evidence.rewritten = true
                  evidence.clausesPreserved = result.method.paramss.map(_.size) == List(1, 1)
                  evidence.untouchedMemberPreserved = result.untouchedMemberPreserved
                  result.root
            case other => other
          }
          summon[Context].compilationUnit.untpdTree =
            untpd.cpy.PackageDef(packageDef)(packageDef.pid, rewritten)
          evidence.preTyperClean = rewritten.forall(tree =>
            ExistingUntpdClassMemberFilter.allTrees(tree).forall(node =>
              node.symbol == NoSymbol && !node.isInstanceOf[untpd.TypedSplice]
            )
          )
        case other => report.error(s"expected PackageDef before Typer, found ${other.getClass.getSimpleName}")

  private def emitted(output: Path): Vector[String] =
    val stream = Files.walk(output)
    try stream.filter(Files.isRegularFile(_)).iterator().asScala.map(_.toString).toVector
    finally stream.close()

  private def invokeValue(loader: ClassLoader, moduleName: String): Int =
    val moduleClass = loader.loadClass(moduleName)
    val module = moduleClass.getField("MODULE$").get(null)
    moduleClass.getMethod("value").invoke(module).asInstanceOf[Integer].intValue

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ExistingUntpdClassMemberFilter.type],
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

  private val Program =
    """class U052ExistingCurried:
      |  def combine(a: AnyVal)(b: AnyVal): AnyVal = a
      |  val untouched: Int = 7
      |
      |object U052Use:
      |  def value: Int = new U052ExistingCurried().combine(20)(22)
      |""".stripMargin

private object ExistingCurriedMethodCaptureRewriteRuntimeProbe:
  final case class Result(
      root: untpd.TypeDef,
      method: untpd.DefDef,
      untouchedMemberPreserved: Boolean
  )

  def rewrite(root: untpd.TypeDef)(using Context): Either[String, Result] =
    for
      captured <- ExistingUntpdClassMemberFilter.capture(root).left.map(_.message)
      _ <- ExistingUntpdOrdinaryMethodDescriptor.capture(captured, 0) match
        case Left(problem) if problem.code == "UNSUPPORTED_PARAMETER_TOPOLOGY" => Right(())
        case Left(problem) => Left(s"unexpected current descriptor failure: ${problem.message}")
        case Right(_) => Left("current production descriptor unexpectedly admitted the curried method")
      descriptor <- ExistingCurriedMethodCaptureRewriteModelFitProbe.assemble(captured, 0)
      first <- prepareParameter(descriptor.parameterClauses(0)(0))
      second <- prepareParameter(descriptor.parameterClauses(1)(0))
      resultType <- prepareType(descriptor.resultType)
      body <- prepareBody(descriptor)
      method <- reconstructMethod(descriptor.method, List(List(first), List(second)), resultType, body)
      rebuilt <- ExistingUntpdClassMemberFilter
        .reconstruct(captured, captured.originalTemplate.body.toVector.updated(0, method))
        .left.map(_.message)
    yield Result(
      rebuilt.root,
      method,
      captured.members(1).tree.eq(rebuilt.template.body(1))
    )

  private def prepareParameter(
      parameter: ExistingUntpdOrdinaryMethodDescriptor.Parameter
  )(using Context): Either[String, untpd.ValDef] =
    for
      lowered <- lowerInt()
      positionedType = lowered.cloneIn(parameter.tpt.source).withSpan(parameter.tpt.span)
      positionedParameter <- positionParameter(parameter.tree, positionedType)
    yield positionedParameter

  private def prepareType(oldSite: untpd.Tree)(using Context): Either[String, untpd.Tree] =
    lowerInt().map(_.cloneIn(oldSite.source).withSpan(oldSite.span))

  private def lowerInt()(using Context): Either[String, untpd.Tree] =
    TypeUntypedLowering
      .lower(TypeNormalForm.STypeIdent("Int"))
      .left.map(_.message)

  private def positionParameter(
      original: untpd.ValDef,
      positionedType: untpd.Tree
  )(using Context): Either[String, untpd.ValDef] =
    given SourceFile = NoSource
    val sourceFree = untpd.ValDef(original.name, positionedType, untpd.EmptyTree)
      .withMods(original.mods)
    val positioned = untpd.cpy.ValDef(sourceFree)(
      sourceFree.name,
      positionedType,
      sourceFree.rhs
    ).cloneIn(original.source).withSpan(original.span)
    Either.cond(
      !positioned.eq(original) && positioned.tpt.eq(positionedType),
      positioned,
      "parameter preparation did not allocate the expected fresh shell"
    )

  private def prepareBody(
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor
  )(using Context): Either[String, untpd.Tree] =
    val names = descriptor.parameterClauses.map(_.map(_.diagnosticName))
    for
      parameters <- TermBindingInternals.persistentParameters(names).left.map(_.message)
      first <- parameters.referenceAt(0, 0).left.map(_.message)
      second <- parameters.referenceAt(1, 0).left.map(_.message)
      shape = TermShape.Apply(
        TermShape.Select(TermShape.Identifier("Math", false), "addExact"),
        List(first, second)
      )
      completed <- parameters.complete(shape).left.map(_.message)
      checked <- parameters.validateDefinitionBody(Vector(1, 1), completed).left.map(_.message)
      firstBinding <- binding(first, names(0)(0))
      secondBinding <- binding(second, names(1)(0))
      bindings = Vector(firstBinding, secondBinding)
      constructed <- ConstructedTerm
        .fromShapeInScope(checked, bindings.map(_._1))
        .left.map(_.message)
      lowered <- ConstructedTermUntypedBackend
        .lowerInScopes(constructed, bindings)
        .left.map(_.message)
      family <- ExistingUntpdSingleParameterMethodRhsRewriter
        .classify(lowered, "U052")
        .left.map(_.message)
      positioned <- ExistingUntpdMethodBodyRewriteOriginAdapter
        .prepareReplacement(lowered, descriptor.rhs, family)
        .left.map(_.message)
    yield positioned

  private def binding(shape: TermShape, name: String): Either[String, (BinderId, String)] =
    shape match
      case TermShape.BoundReference(id, _) => Right(id -> name)
      case other => Left(s"expected a bound reference for $name, found $other")

  private def reconstructMethod(
      original: untpd.DefDef,
      parameterClauses: List[List[untpd.ValDef]],
      resultType: untpd.Tree,
      body: untpd.Tree
  )(using Context): Either[String, untpd.DefDef] =
    given SourceFile = NoSource
    val sourceFree = untpd.DefDef(original.name, parameterClauses, resultType, body)
      .withMods(original.mods)
    val positioned = untpd.cpy.DefDef(sourceFree)(
      sourceFree.name,
      sourceFree.paramss,
      sourceFree.tpt,
      sourceFree.rhs
    ).cloneIn(original.source).withSpan(original.span)
    Either.cond(
      !positioned.eq(original) && positioned.paramss.map(_.size) == List(1, 1) &&
        positioned.tpt.eq(resultType) && positioned.rhs.eq(body),
      positioned,
      "method reconstruction did not preserve the exact two-clause prepared topology"
    )
