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

import quasiquotes.types.TypeNormalForm

class ExistingUntpdSingleParameterMethodParameterTypeRewriterTyperRuntimeTest
    extends munit.FunSuite:
  test("U031 parameter-type rewrite lets Typer bind the preserved raw RHS to the fresh parameter") {
    val temporary = Files.createTempDirectory("u031-parameter-type-rewrite-")
    try
      val source = temporary.resolve("U031ParameterTypeRewriteRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """class U031ParameterTypeRewriteRuntime:
          |  val before: Int = 1
          |  def bump(x: AnyVal): Int = x + 1
          |  type After = String
          |
          |object U031ParameterTypeRewriteRuntimeUse:
          |  def value: Int = new U031ParameterTypeRewriteRuntime().bump(7)
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val baseline = new Driver().process(
        Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      )
      assert(baseline.hasErrors)
      deleteRecursively(output)
      Files.createDirectories(output)

      val driver = new RewriteDriver
      val reporter = driver.process(
        Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      )

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assert(driver.beforeTyperContractValid)
      assert(emitted(output).exists(_.endsWith("U031ParameterTypeRewriteRuntime.tasty")))
      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U031ParameterTypeRewriteRuntimeUse$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("value").invoke(module), Integer.valueOf(8))
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class RewriteDriver extends Driver:
    @volatile var beforeTyperContractValid: Boolean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(evidence: RewriteDriver) extends Phase:
    def phaseName: String = "u031SingleParameterMethodParameterTypeRewrite"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared =
        for
          originalRoot <- allTrees(unitTree).collectFirst {
            case value: untpd.TypeDef
                if value.name.toString == "U031ParameterTypeRewriteRuntime" => value
          }.toRight("U031 runtime fixture class was not found")
          originalTemplate <- originalRoot.rhs match
            case value: untpd.Template => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          captured <- ExistingUntpdClassMemberFilter.capture(originalRoot).left.map(_.message)
          view <- ExistingUntpdSingleParameterMethodView.capture(captured, 1).left.map(_.message)
          originalState = allTrees(originalRoot).map(tree => (tree, tree.source, tree.span))
          rewritten <- ExistingUntpdSingleParameterMethodParameterTypeRewriter
            .rewrite(view, TypeNormalForm.STypeIdent("Int"))
            .left.map(_.message)
        yield (originalRoot, originalTemplate, view, originalState, rewritten)

      prepared match
        case Left(problem) => report.error(problem)
        case Right((originalRoot, originalTemplate, view, originalState, rewritten)) =>
          val parameter = rewritten.positionedParameter
          val originalUntouched = originalTemplate.body.filterNot(_.eq(view.method))
          val rewrittenUntouched =
            rewritten.positionedTemplate.body.filterNot(_.eq(rewritten.positionedMethod))
          evidence.beforeTyperContractValid =
            !rewritten.loweredParameterType.source.exists &&
              !rewritten.loweredParameterType.span.exists &&
              rewritten.loweredParameterType.symbol == NoSymbol &&
              rewritten.positionedParameterType.source == view.parameterType.source &&
              rewritten.positionedParameterType.span == view.parameterType.span &&
              !rewritten.positionedParameterType.eq(view.parameterType) &&
              !parameter.eq(view.parameter) &&
              parameter.tpt.eq(rewritten.positionedParameterType) &&
              parameter.rhs.isEmpty &&
              rewritten.positionedMethod.tpt.eq(view.resultType) &&
              rewritten.positionedMethod.rhs.eq(view.rhs) &&
              !rewritten.positionedMethod.eq(view.method) &&
              !rewritten.positionedTemplate.eq(originalTemplate) &&
              !rewritten.positionedRoot.eq(originalRoot) &&
              originalUntouched.zip(rewrittenUntouched).forall((left, right) => left.eq(right)) &&
              originalState.forall { case (tree, source, span) =>
                tree.source == source && tree.span == span
              } &&
              allTrees(rewritten.positionedRoot).forall(tree =>
                tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
              )

          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              if tree.eq(originalRoot) then rewritten.positionedRoot
              else super.transform(tree)
          summon[Context].compilationUnit.untpdTree = transformer.transform(unitTree)

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val collector = Vector.newBuilder[untpd.Tree]
    val traverser = new untpd.UntypedTreeTraverser:
      override def traverse(current: untpd.Tree)(using Context): Unit =
        collector += current
        traverseChildren(current)
    traverser.traverse(tree)
    collector.result()

  private def emitted(output: Path): Vector[String] =
    val stream = Files.walk(output)
    try stream.filter(Files.isRegularFile(_)).iterator().asScala.map(_.toString).toVector
    finally stream.close()

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ExistingUntpdSingleParameterMethodParameterTypeRewriter.type],
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
