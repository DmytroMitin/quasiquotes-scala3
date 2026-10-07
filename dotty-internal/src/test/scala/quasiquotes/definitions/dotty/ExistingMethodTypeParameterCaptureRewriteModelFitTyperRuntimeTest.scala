package quasiquotes.definitions.dotty

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser

final class ExistingMethodTypeParameterCaptureRewriteModelFitTyperRuntimeTest
    extends munit.FunSuite:
  test("G1 and opaque-preservation G2 survive U023 insertion Typer TASTy and runtime") {
    val temporary = Files.createTempDirectory("u055-existing-generic-model-fit-")
    try
      val source = temporary.resolve("U055ExistingGenericRuntime.scala")
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
      assertEquals(driver.rewritten, Set("U055Phantom", "U055Identity"))
      assert(driver.phantomGenericPreserved)
      assert(driver.identityGenericPreserved)
      assertEquals(driver.untouchedMembersPreserved, 2)
      assert(driver.preTyperClean)
      val files = emitted(output)
      assert(files.exists(_.endsWith("U055Phantom.tasty")))
      assert(files.exists(_.endsWith("U055Identity.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try assertEquals(invokeValue(loader, "U055Use$"), 43)
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class RewriteDriver extends Driver:
    @volatile var rewritten = Set.empty[String]
    @volatile var phantomGenericPreserved = false
    @volatile var identityGenericPreserved = false
    @volatile var untouchedMembersPreserved = 0
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(evidence: RewriteDriver) extends Phase:
    def phaseName: String = "u055ExistingGenericModelFit"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString == "U055Phantom" =>
              rewrite(root, rewriteTypes = true) match
                case Left(problem) =>
                  report.error(problem)
                  root
                case Right(result) =>
                  evidence.rewritten += root.name.toString
                  evidence.phantomGenericPreserved =
                    genericShape(result.method, "A", "Int")
                  if result.untouchedMemberPreserved then
                    evidence.untouchedMembersPreserved += 1
                  result.root
            case root: untpd.TypeDef if root.name.toString == "U055Identity" =>
              rewrite(root, rewriteTypes = false) match
                case Left(problem) =>
                  report.error(problem)
                  root
                case Right(result) =>
                  evidence.rewritten += root.name.toString
                  evidence.identityGenericPreserved =
                    genericShape(result.method, "A", "A") &&
                      result.method.paramss(1).head.asInstanceOf[untpd.ValDef].tpt
                        .eq(result.originalParameterType) &&
                      result.method.tpt.eq(result.originalResultType)
                  if result.untouchedMemberPreserved then
                    evidence.untouchedMembersPreserved += 1
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
        case other =>
          report.error("expected PackageDef before Typer, found " + other.getClass.getSimpleName)

  private final case class RewriteResult(
      root: untpd.TypeDef,
      method: untpd.DefDef,
      originalParameterType: untpd.Tree,
      originalResultType: untpd.Tree,
      untouchedMemberPreserved: Boolean
  )

  private def rewrite(
      root: untpd.TypeDef,
      rewriteTypes: Boolean
  )(using Context): Either[String, RewriteResult] =
    for
      captured <- ExistingUntpdClassMemberFilter.capture(root).left.map(_.message)
      _ <- ExistingUntpdOrdinaryMethodDescriptor.capture(captured, 0) match
        case Left(problem) if problem.code == "UNSUPPORTED_PARAMETER_TOPOLOGY" => Right(())
        case Left(problem) => Left("unexpected descriptor failure: " + problem.message)
        case Right(_) => Left("production descriptor unexpectedly admitted generic method")
      carrier <- ExistingMethodTypeParameterCaptureRewriteModelFitProbe.assemble(captured, 0)
      prepared <- ExistingMethodTypeParameterCaptureRewriteModelFitProbe
        .prepare(carrier, rewriteTypes)
      rebuilt <- ExistingUntpdClassMemberFilter
        .reconstruct(captured, captured.originalTemplate.body.toVector.updated(0, prepared.method))
        .left.map(_.message)
    yield RewriteResult(
      rebuilt.root,
      prepared.method,
      carrier.parameterType,
      carrier.resultType,
      captured.members(1).tree.eq(rebuilt.template.body(1))
    )

  private def genericShape(
      method: untpd.DefDef,
      expectedTypeParameter: String,
      expectedTermType: String
  ): Boolean =
    method.paramss.map(_.size) == List(1, 1) &&
      method.paramss.head.head.isInstanceOf[untpd.TypeDef] &&
      method.paramss.head.head.asInstanceOf[untpd.TypeDef].name.toString == expectedTypeParameter &&
      method.paramss.head.head.asInstanceOf[untpd.TypeDef].mods.is(Flags.Param) &&
      method.paramss(1).head.asInstanceOf[untpd.ValDef].tpt
        .asInstanceOf[untpd.Ident].name.toString == expectedTermType &&
      method.tpt.asInstanceOf[untpd.Ident].name.toString == expectedTermType

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
    """class U055Phantom:
      |  def keep[A](x: AnyVal): AnyVal = x
      |  val untouched: Int = 7
      |
      |class U055Identity:
      |  def id[A](x: A): A = throw new RuntimeException("baseline")
      |  val untouched: Int = 9
      |
      |object U055Use:
      |  def phantomValue: Int = new U055Phantom().keep[String](41)
      |  def identityValue: String = new U055Identity().id[String]("ok")
      |  def value: Int = phantomValue + identityValue.length
      |""".stripMargin
