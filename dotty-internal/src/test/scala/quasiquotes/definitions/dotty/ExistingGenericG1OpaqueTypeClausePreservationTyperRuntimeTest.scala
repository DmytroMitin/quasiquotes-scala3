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

import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.*
import quasiquotes.types.TypeNormalForm

final class ExistingGenericG1OpaqueTypeClausePreservationTyperRuntimeTest
    extends munit.FunSuite:
  test("U068 public G1 rewrite survives explicit-Type-argument Typer TASTy and runtime") {
    val temporary = Files.createTempDirectory("u068-existing-generic-g1-")
    try
      val source = temporary.resolve("U068ExistingGenericG1Runtime.scala")
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
      assert(driver.opaqueTypeClausePreserved)
      assert(driver.controlsPreserved)
      assert(driver.preTyperClean)
      assert(emitted(output).exists(_.endsWith("GenericIndependent.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try assertEquals(invokeValue(loader, "U068Use$"), 41)
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class RewriteDriver extends Driver:
    @volatile var rewritten = false
    @volatile var opaqueTypeClausePreserved = false
    @volatile var controlsPreserved = false
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(evidence: RewriteDriver) extends Phase:
    def phaseName: String = "u068ExistingGenericG1"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString == "GenericIndependent" =>
              rewrite(root) match
                case Left(problem) =>
                  report.error(problem)
                  root
                case Right(result) =>
                  evidence.rewritten = true
                  evidence.opaqueTypeClausePreserved = result.opaqueTypeClausePreserved
                  evidence.controlsPreserved = result.controlsPreserved
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
      opaqueTypeClausePreserved: Boolean,
      controlsPreserved: Boolean
  )

  private def rewrite(root: untpd.TypeDef)(using Context): Either[String, RewriteResult] =
    val originalTemplate = root.rhs.asInstanceOf[untpd.Template]
    val originalMethod = originalTemplate.body.head.asInstanceOf[untpd.DefDef]
    val originalTypeClause = originalMethod.paramss.head
    val originalTypeParameter = originalTypeClause.head.asInstanceOf[untpd.TypeDef]
    val originalBounds = originalTypeParameter.rhs
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      method <- captured.method(captured.members.head.ref).left.map(_.message)
      parameter <- method.parameterClauses.headOption
        .flatMap(_.parameters.headOption)
        .toRight("public G1 term parameter was missing")
      reference <- method.parameterScope.reference(parameter.ref).left.map(_.message)
      parameterEdited <- captured.emptyPlan
        .replaceParameterType(parameter.ref, TypeNormalForm.STypeIdent("Int"))
        .left.map(_.message)
      resultEdited <- parameterEdited
        .replaceResultType(method.ref, TypeNormalForm.STypeIdent("Int"))
        .left.map(_.message)
      bodyEdited <- resultEdited.replaceBody(method.ref, reference).left.map(_.message)
      applied <- ExistingClassUntypedRewrite(captured, bodyEdited).left.map(_.message)
      rebuiltTemplate = applied.tree.rhs.asInstanceOf[untpd.Template]
      rebuiltMethod <- rebuiltTemplate.body.headOption
        .collect { case value: untpd.DefDef => value }
        .toRight("rewritten first direct member was not a method")
    yield RewriteResult(
      applied.tree,
      rebuiltMethod.paramss.map(_.size) == List(1, 1) &&
        rebuiltMethod.paramss.head.eq(originalTypeClause) &&
        rebuiltMethod.paramss.head.head.eq(originalTypeParameter) &&
        rebuiltMethod.paramss.head.head.asInstanceOf[untpd.TypeDef].rhs.eq(originalBounds),
      applied.directMemberIdentities(1).sameObjectAs(captured.members(1).identity) &&
        applied.directMemberIdentities(2).sameObjectAs(captured.members(2).identity)
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
    """class GenericIndependent:
      |  def keep[A](x: AnyVal): AnyVal = x
      |  def ordinary(x: Int): Int = x
      |  val untouched: Int = 7
      |
      |object U068Use:
      |  def value: Int = new GenericIndependent().keep[String](41)
      |""".stripMargin
