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

final class ExistingGenericG2LocalTypeBinderBodyOnlyRewriteTyperRuntimeTest
    extends munit.FunSuite:
  test("U069 G2 body-only rewrite preserves explicit Type arguments through Typer TASTy and runtime"):
    val temporary = Files.createTempDirectory("u069-existing-generic-g2-")
    try
      val source = temporary.resolve("U069ExistingGenericG2Runtime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(source, Program, StandardCharsets.UTF_8)

      val baseline = new Driver().process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))
      assert(!baseline.hasErrors, clues(baseline.allErrors))
      val baselineLoader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try intercept[java.lang.reflect.InvocationTargetException](invokeValue(baselineLoader))
      finally baselineLoader.close()
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
      assert(driver.dependentIdentityPreserved)
      assert(driver.opaqueTypeClausePreserved)
      assert(driver.siblingPreserved)
      assert(driver.preTyperClean)
      assert(emitted(output).exists(_.endsWith("GenericDependent.tasty")))
      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try assertEquals(invokeValue(loader), "ok")
      finally loader.close()
    finally deleteRecursively(temporary)

  private final class RewriteDriver extends Driver:
    @volatile var rewritten = false
    @volatile var dependentIdentityPreserved = false
    @volatile var opaqueTypeClausePreserved = false
    @volatile var siblingPreserved = false
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(evidence: RewriteDriver) extends Phase:
    def phaseName: String = "u069ExistingGenericG2"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString == "GenericDependent" =>
              rewrite(root) match
                case Left(problem) =>
                  report.error(problem)
                  root
                case Right(result) =>
                  evidence.rewritten = true
                  evidence.dependentIdentityPreserved = result.dependentIdentityPreserved
                  evidence.opaqueTypeClausePreserved = result.opaqueTypeClausePreserved
                  evidence.siblingPreserved = result.siblingPreserved
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
        case other => report.error("expected PackageDef before Typer, found " + other.getClass.getSimpleName)

  private final case class RewriteResult(
      root: untpd.TypeDef,
      dependentIdentityPreserved: Boolean,
      opaqueTypeClausePreserved: Boolean,
      siblingPreserved: Boolean
  )

  private def rewrite(root: untpd.TypeDef)(using Context): Either[String, RewriteResult] =
    val originalTemplate = root.rhs.asInstanceOf[untpd.Template]
    val originalMethod = originalTemplate.body.head.asInstanceOf[untpd.DefDef]
    val originalTypeClause = originalMethod.paramss.head
    val originalTypeParameter = originalTypeClause.head.asInstanceOf[untpd.TypeDef]
    val originalBounds = originalTypeParameter.rhs
    val originalParameter = originalMethod.paramss(1).head.asInstanceOf[untpd.ValDef]
    val originalParameterType = originalParameter.tpt
    val originalResultType = originalMethod.tpt
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      method <- captured.method(captured.members.head.ref).left.map(_.message)
      parameter <- method.parameterClauses.headOption.flatMap(_.parameters.headOption)
        .toRight("public G2 term parameter was missing")
      reference <- method.parameterScope.reference(parameter.ref).left.map(_.message)
      plan <- captured.emptyPlan.replaceBody(method.ref, reference).left.map(_.message)
      applied <- ExistingClassUntypedRewrite(captured, plan).left.map(_.message)
      rewrittenTemplate = applied.tree.rhs.asInstanceOf[untpd.Template]
      rewrittenMethod <- rewrittenTemplate.body.headOption.collect { case value: untpd.DefDef => value }
        .toRight("rewritten first direct member was not a method")
    yield RewriteResult(
      applied.tree,
      rewrittenMethod.paramss(1).head.eq(originalParameter) &&
        rewrittenMethod.paramss(1).head.asInstanceOf[untpd.ValDef].tpt.eq(originalParameterType) &&
        rewrittenMethod.tpt.eq(originalResultType),
      rewrittenMethod.paramss.head.eq(originalTypeClause) &&
        rewrittenMethod.paramss.head.head.eq(originalTypeParameter) &&
        rewrittenMethod.paramss.head.head.asInstanceOf[untpd.TypeDef].rhs.eq(originalBounds),
      applied.directMemberIdentities(1).sameObjectAs(captured.members(1).identity)
    )

  private def emitted(output: Path): Vector[String] =
    val stream = Files.walk(output)
    try stream.filter(Files.isRegularFile(_)).iterator().asScala.map(_.toString).toVector
    finally stream.close()

  private def invokeValue(loader: ClassLoader): String =
    val moduleClass = loader.loadClass("U069Use$")
    val module = moduleClass.getField("MODULE$").get(null)
    moduleClass.getMethod("value").invoke(module).asInstanceOf[String]

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
    """class GenericDependent:
      |  def keep[A](x: A): A = throw new RuntimeException("baseline")
      |  val untouched: Int = 7
      |
      |object U069Use:
      |  def value: String = new GenericDependent().keep[String]("ok")
      |""".stripMargin
