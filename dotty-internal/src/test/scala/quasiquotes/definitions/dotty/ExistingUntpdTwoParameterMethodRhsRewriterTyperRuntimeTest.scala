package quasiquotes.definitions.dotty

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.util.{NoSource, SourceFile}

class ExistingUntpdTwoParameterMethodRhsRewriterTyperRuntimeTest
    extends munit.FunSuite:
  test("U034 selected Apply binds both preserved parameters before Typer and runs at 42") {
    val temporary = Files.createTempDirectory("u034-rhs-rewrite-")
    try
      val source = temporary.resolve("U034TwoParameterRhsRewriteRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """class U034TwoParameterRhsRewriteRuntime:
          |  val before: Int = 1
          |  def choose(x: Int, y: Int): Int = x - y
          |  type After = String
          |
          |object U034TwoParameterRhsRewriteRuntimeUse:
          |  def value: Int = new U034TwoParameterRhsRewriteRuntime().choose(7, 42)
          |""".stripMargin,
        StandardCharsets.UTF_8
      )
      val driver = new RewriteDriver
      val reporter = driver.process(
        Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      )

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assert(driver.beforeTyperContractValid)
      assert(emitted(output).exists(_.endsWith("U034TwoParameterRhsRewriteRuntime.tasty")))
      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U034TwoParameterRhsRewriteRuntimeUse$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("value").invoke(module).asInstanceOf[Int], 42)
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
    def phaseName: String = "u034TwoParameterMethodRhsRewrite"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared =
        for
          originalRoot <- allTrees(unitTree).collectFirst {
            case value: untpd.TypeDef
                if value.name.toString == "U034TwoParameterRhsRewriteRuntime" => value
          }.toRight("U034 runtime fixture class was not found")
          originalTemplate <- originalRoot.rhs match
            case value: untpd.Template => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          captured <- ExistingUntpdClassMemberFilter.capture(originalRoot).left.map(_.message)
          view <- ExistingUntpdTwoParameterMethodView.capture(captured, 1).left.map(_.message)
          replacement =
            given SourceFile = NoSource
            untpd.Apply(
              untpd.Select(untpd.Ident(termName("Math")), termName("max")),
              untpd.Ident(termName("x")) :: untpd.Ident(termName("y")) :: Nil
            )
          replacementNodes = allTrees(replacement)
          originalState = allTrees(originalRoot).map(tree => (tree, tree.source, tree.span))
          rewritten <- ExistingUntpdTwoParameterMethodRhsRewriter
            .rewrite(view, replacement)
            .left.map(_.message)
        yield (
          originalRoot,
          originalTemplate,
          view,
          replacementNodes,
          originalState,
          rewritten
        )

      prepared match
        case Left(problem) => report.error(problem)
        case Right((originalRoot, originalTemplate, view, replacementNodes, originalState, rewritten)) =>
          val structural = rewritten.structuralResult
          val positioned = rewritten.positionedResult
          val structuralFirst = structural.rebuiltTarget.paramss.head(0).asInstanceOf[untpd.ValDef]
          val structuralSecond = structural.rebuiltTarget.paramss.head(1).asInstanceOf[untpd.ValDef]
          val positionedFirst = positioned.positionedTarget.paramss.head(0).asInstanceOf[untpd.ValDef]
          val positionedSecond = positioned.positionedTarget.paramss.head(1).asInstanceOf[untpd.ValDef]
          val positionedReplacementNodes = allTrees(positioned.positionedReplacement)
          val originalUntouched = originalTemplate.body.filterNot(_.eq(view.method))
          val positionedUntouched =
            positioned.positionedTemplate.body.filterNot(_.eq(positioned.positionedTarget))
          evidence.beforeTyperContractValid =
            Vector[untpd.Tree](
              structural.rebuiltRoot,
              structural.rebuiltTemplate,
              structural.rebuiltTarget
            ).forall(tree =>
              !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol &&
                !tree.isInstanceOf[untpd.TypedSplice]
            ) &&
              replacementNodes.size == 5 &&
              replacementNodes.forall(tree =>
                !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol
              ) &&
              replacementNodes.collect { case value: untpd.Ident => value.name.toString }.toSet ==
                Set("Math", "x", "y") &&
              structuralFirst.eq(view.firstParameter) &&
              structuralFirst.tpt.eq(view.firstParameterType) &&
              structuralSecond.eq(view.secondParameter) &&
              structuralSecond.tpt.eq(view.secondParameterType) &&
              structural.rebuiltTarget.tpt.eq(view.resultType) &&
              positionedFirst.eq(view.firstParameter) &&
              positionedFirst.tpt.eq(view.firstParameterType) &&
              positionedSecond.eq(view.secondParameter) &&
              positionedSecond.tpt.eq(view.secondParameterType) &&
              positioned.positionedTarget.tpt.eq(view.resultType) &&
              positionedReplacementNodes.size == replacementNodes.size &&
              replacementNodes.zip(positionedReplacementNodes).forall((left, right) => !left.eq(right)) &&
              positionedReplacementNodes.forall(tree =>
                tree.source == view.rhs.source && tree.span == view.rhs.span
              ) &&
              originalUntouched.zip(positionedUntouched).forall((left, right) => left.eq(right)) &&
              originalState.forall { case (tree, source, span) =>
                tree.source == source && tree.span == span
              } &&
              allTrees(positioned.positionedRoot).forall(tree =>
                tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
              )

          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              if tree.eq(originalRoot) then positioned.positionedRoot
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
      classOf[ExistingUntpdTwoParameterMethodView.type],
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
