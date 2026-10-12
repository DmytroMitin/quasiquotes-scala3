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
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.*
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm
import dotty.tools.dotc.parsing.Parser

final class ExistingCurriedMethodCaptureRewriteModelFitTyperRuntimeTest extends munit.FunSuite:
  test("public two-clause rewrite survives ordinary Typer TASTy and distinct curried runtime calling") {
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
      try
        assertEquals(invokeInt(loader, "U052Use$", "value"), 42)
        assertEquals(invokeInt(loader, "U052Use$", "untouched"), 7)
        assertEquals(invokeInt(loader, "U052Use$", "binary"), 42)
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

  private def invokeInt(loader: ClassLoader, moduleName: String, methodName: String): Int =
    val moduleClass = loader.loadClass(moduleName)
    val module = moduleClass.getField("MODULE$").get(null)
    moduleClass.getMethod(methodName).invoke(module).asInstanceOf[Integer].intValue

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
      |class U066BinaryControl:
      |  def combine(a: Int, b: Int): Int = Math.addExact(a, b)
      |
      |object U052Use:
      |  def value: Int = new U052ExistingCurried().combine(20)(22)
      |  def untouched: Int = new U052ExistingCurried().untouched
      |  def binary: Int = new U066BinaryControl().combine(20, 22)
      |""".stripMargin

private object ExistingCurriedMethodCaptureRewriteRuntimeProbe:
  final case class Result(
      root: untpd.TypeDef,
      method: untpd.DefDef,
      untouchedMemberPreserved: Boolean
  )

  def rewrite(root: untpd.TypeDef)(using Context): Either[String, Result] =
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      member <- captured.member(0).left.map(_.message)
      method <- captured.method(member.ref).left.map(_.message)
      first <- method.parameterClauses.lift(0).flatMap(_.parameters.headOption)
        .toRight("the public first unary clause was absent")
      second <- method.parameterClauses.lift(1).flatMap(_.parameters.headOption)
        .toRight("the public second unary clause was absent")
      firstReference <- method.parameterScope.reference(first.ref).left.map(_.message)
      secondReference <- method.parameterScope.reference(second.ref).left.map(_.message)
      replacement = TermShape.Apply(
        TermShape.Select(TermShape.Identifier("Math", false), "addExact"),
        List(firstReference, secondReference)
      )
      p1 <- captured.emptyPlan.replaceParameterType(first.ref, TypeNormalForm.STypeIdent("Int")).left.map(_.message)
      p2 <- p1.replaceParameterType(second.ref, TypeNormalForm.STypeIdent("Int")).left.map(_.message)
      p3 <- p2.replaceResultType(method.ref, TypeNormalForm.STypeIdent("Int")).left.map(_.message)
      p4 <- p3.replaceBody(method.ref, replacement).left.map(_.message)
      rewritten <- ExistingClassUntypedRewrite(captured, p4).left.map(_.message)
      template = rewritten.tree.rhs.asInstanceOf[untpd.Template]
      positioned = template.body.head.asInstanceOf[untpd.DefDef]
    yield Result(
      rewritten.tree,
      positioned,
      template.body(1).eq(root.rhs.asInstanceOf[untpd.Template].body(1))
    )
