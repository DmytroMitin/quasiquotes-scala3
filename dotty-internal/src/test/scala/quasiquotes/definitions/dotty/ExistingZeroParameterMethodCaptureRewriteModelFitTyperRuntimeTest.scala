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

import quasiquotes.parser.TermShape

final class ExistingZeroParameterMethodCaptureRewriteModelFitTyperRuntimeTest extends munit.FunSuite:
  test("both zero-parameter forms survive U023 insertion Typer TASTy and runtime") {
    val temporary = Files.createTempDirectory("u053-existing-zero-parameter-model-fit-")
    try
      val source = temporary.resolve("U053ExistingZeroParameterRuntime.scala")
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
      assertEquals(driver.rewritten, Set("U053Parameterless", "U053EmptyParen"))
      assert(driver.parameterlessPreserved)
      assert(driver.emptyParenPreserved)
      assertEquals(driver.untouchedMembersPreserved, 2)
      assert(driver.preTyperClean)
      val files = emitted(output)
      assert(files.exists(_.endsWith("U053Parameterless.tasty")))
      assert(files.exists(_.endsWith("U053EmptyParen.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try assertEquals(invokeValue(loader, "U053Use$"), 42)
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class RewriteDriver extends Driver:
    @volatile var rewritten = Set.empty[String]
    @volatile var parameterlessPreserved = false
    @volatile var emptyParenPreserved = false
    @volatile var untouchedMembersPreserved = 0
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(evidence: RewriteDriver) extends Phase:
    def phaseName: String = "u053ExistingZeroParameterModelFit"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString == "U053Parameterless" =>
              rewrite(root, "20") match
                case Left(problem) =>
                  report.error(problem)
                  root
                case Right(result) =>
                  evidence.rewritten += root.name.toString
                  evidence.parameterlessPreserved = result.method.paramss.isEmpty
                  if result.untouchedMemberPreserved then evidence.untouchedMembersPreserved += 1
                  result.root
            case root: untpd.TypeDef if root.name.toString == "U053EmptyParen" =>
              rewrite(root, "22") match
                case Left(problem) =>
                  report.error(problem)
                  root
                case Right(result) =>
                  evidence.rewritten += root.name.toString
                  evidence.emptyParenPreserved = result.method.paramss.map(_.size) == List(0)
                  if result.untouchedMemberPreserved then evidence.untouchedMembersPreserved += 1
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

  private final case class RewriteResult(
      root: untpd.TypeDef,
      method: untpd.DefDef,
      untouchedMemberPreserved: Boolean
  )

  private def rewrite(root: untpd.TypeDef, value: String)(using Context): Either[String, RewriteResult] =
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      method <- captured.method(captured.members.head.ref).left.map(_.message)
      resultPlan <- captured.emptyPlan
        .replaceResultType(method.ref, quasiquotes.types.TypeNormalForm.STypeIdent("Int"))
        .left.map(_.message)
      bodyPlan <- resultPlan.replaceBody(method.ref, TermShape.Literal(value)).left.map(_.message)
      rebuilt <- ExistingClassUntypedRewrite(captured, bodyPlan).left.map(_.message)
      template = rebuilt.tree.rhs.asInstanceOf[untpd.Template]
      rewrittenMethod = template.body.head.asInstanceOf[untpd.DefDef]
    yield RewriteResult(
      rebuilt.tree,
      rewrittenMethod,
      rebuilt.directMemberIdentities(1).sameObjectAs(captured.members(1).identity)
    )

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
    """class U053Parameterless:
      |  def value: AnyVal = 41
      |  val untouched: Int = 7
      |
      |class U053EmptyParen:
      |  def value(): AnyVal = 41
      |  val untouched: Int = 9
      |
      |object U053Use:
      |  def parameterlessValue: Int = new U053Parameterless().value
      |  def emptyParenValue: Int = new U053EmptyParen().value()
      |  def value: Int = parameterlessValue + emptyParenValue
      |""".stripMargin
