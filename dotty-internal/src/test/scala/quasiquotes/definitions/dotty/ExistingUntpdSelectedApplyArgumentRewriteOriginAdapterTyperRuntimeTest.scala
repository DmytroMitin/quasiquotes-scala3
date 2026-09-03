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

class ExistingUntpdSelectedApplyArgumentRewriteOriginAdapterTyperRuntimeTest
    extends munit.FunSuite:
  private enum Mode:
    case Valid, MissingArgument

  test("granular argument-site origin survives Typer, emits TASTy, and executes") {
    withCompilation(Mode.Valid) { (driver, reporter, output, _) =>
      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assert(driver.beforeTyperContractValid)
      assert(emitted(output).exists(_.endsWith("U014SelectedApplyArgumentOrigin.tasty")))
      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U014SelectedApplyArgumentOriginUse$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("value").invoke(module), Integer.valueOf(22))
      finally loader.close()
    }
  }

  test("invalid replacement is diagnosed at the exact original argument start") {
    withCompilation(Mode.MissingArgument) { (driver, reporter, output, source) =>
      assert(reporter.hasErrors)
      assertEquals(reporter.allErrors.size, 1)
      val problem = reporter.allErrors.head
      assert(problem.message.contains("missingU014Argument"), clues(problem))
      assertEquals(problem.pos.source.path, source.toString)
      assertEquals(problem.pos.start, driver.originalArgumentStart)
      assert(driver.originalArgumentStart > driver.originalApplyStart)
      assert(driver.beforeTyperContractValid)
      assert(!emitted(output).exists(_.endsWith("U014SelectedApplyArgumentOrigin.tasty")))
    }
  }

  private def withCompilation(mode: Mode)(
      check: (OriginDriver, dotty.tools.dotc.reporting.Reporter, Path, Path) => Unit
  ): Unit =
    val temporary = Files.createTempDirectory("u014-selected-apply-argument-origin-")
    try
      val source = temporary.resolve("U014SelectedApplyArgumentOrigin.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """class U014SelectedApplyArgumentOrigin:
          |  object service:
          |    def invoke(a: Int, b: Int): Int = a + b
          |  val oldArg: Int = 1
          |  val keptArg: Int = 20
          |  def keep: Int = 1
          |  def change: Int = service.invoke(oldArg, keptArg)
          |  val opaque: String = "opaque"
          |
          |object U014SelectedApplyArgumentOriginUse:
          |  def value: Int = new U014SelectedApplyArgumentOrigin().change
          |""".stripMargin,
        StandardCharsets.UTF_8
      )
      val driver = new OriginDriver(mode)
      val reporter = driver.process(
        Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      )
      check(driver, reporter, output, source)
    finally deleteRecursively(temporary)

  private final class OriginDriver(mode: Mode) extends Driver:
    @volatile var beforeTyperContractValid: Boolean = false
    @volatile var originalApplyStart: Int = -1
    @volatile var originalArgumentStart: Int = -1

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new RewriteBeforeTyper(mode, OriginDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(mode: Mode, evidence: OriginDriver)
      extends Phase:
    def phaseName: String = "u014SelectedApplyArgumentOriginAdaptation"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared =
        for
          root <- allTrees(unitTree).collectFirst {
            case value: untpd.TypeDef
                if value.name.toString == "U014SelectedApplyArgumentOrigin" => value
          }.toRight("U014 runtime fixture class was not found")
          template <- root.rhs match
            case value: untpd.Template => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          target <- template.body.collectFirst {
            case value: untpd.DefDef if value.name.toString == "change" => value
          }.toRight("U014 runtime target was not found")
          apply <- target.rhs match
            case value: untpd.Apply => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          exactArgument <- apply.args.headOption.toRight("U014 runtime argument missing")
          replacement =
            given SourceFile = NoSource
            mode match
              case Mode.Valid => untpd.Number("2", untpd.NumberKind.Whole(10))
              case Mode.MissingArgument => untpd.Ident(termName("missingU014Argument"))
          originalState = allTrees(root).map(tree => (tree, tree.source, tree.span))
          structural <- ExistingUntpdSelectedApplyArgumentRewriter
            .rewrite(root, target, exactArgument, replacement)
            .left.map(_.message)
          adapted <- ExistingUntpdSelectedApplyArgumentRewriteOriginAdapter
            .adapt(structural)
            .left.map(_.message)
        yield (root, template, target, apply, exactArgument, originalState, structural, adapted)

      prepared match
        case Left(problem) => report.error(problem)
        case Right((root, template, target, apply, exactArgument, originalState, structural, adapted)) =>
          evidence.originalApplyStart = apply.span.start
          evidence.originalArgumentStart = exactArgument.span.start
          val originalUntouched = template.body.filterNot(_.eq(target))
          val positionedUntouched =
            adapted.positionedTemplate.body.filterNot(_.eq(adapted.positionedTarget))
          evidence.beforeTyperContractValid =
            Vector[untpd.Tree](
              structural.rebuiltRoot,
              structural.rebuiltTemplate,
              structural.rebuiltTarget,
              structural.rebuiltApply,
              structural.replacementLeaf
            ).forall(tree =>
              !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol &&
                !tree.isInstanceOf[untpd.TypedSplice]
            ) &&
              originalState.forall { case (tree, source, span) =>
                tree.source == source && tree.span == span
              } &&
              !adapted.positionedRoot.eq(root) &&
              !adapted.positionedRoot.eq(structural.rebuiltRoot) &&
              !adapted.positionedTemplate.eq(template) &&
              !adapted.positionedTemplate.eq(structural.rebuiltTemplate) &&
              !adapted.positionedTarget.eq(target) &&
              !adapted.positionedTarget.eq(structural.rebuiltTarget) &&
              !adapted.positionedApply.eq(apply) &&
              !adapted.positionedApply.eq(structural.rebuiltApply) &&
              !adapted.positionedReplacement.eq(exactArgument) &&
              !adapted.positionedReplacement.eq(structural.replacementLeaf) &&
              adapted.positionedApply.source == apply.source &&
              adapted.positionedApply.span == apply.span &&
              adapted.positionedReplacement.source == exactArgument.source &&
              adapted.positionedReplacement.span == exactArgument.span &&
              adapted.positionedApply.fun.eq(apply.fun) &&
              adapted.positionedApply.args(1).eq(apply.args(1)) &&
              originalUntouched.size == positionedUntouched.size &&
              originalUntouched.zip(positionedUntouched).forall((left, right) => left.eq(right)) &&
              allTrees(adapted.positionedRoot).forall(tree =>
                tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
              )

          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              if tree.eq(root) then adapted.positionedRoot else super.transform(tree)
          summon[Context].compilationUnit.untpdTree = transformer.transform(unitTree)

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val builder = Vector.newBuilder[untpd.Tree]
    val traverser = new untpd.UntypedTreeTraverser:
      override def traverse(current: untpd.Tree)(using Context): Unit =
        builder += current
        traverseChildren(current)
    traverser.traverse(tree)
    builder.result()

  private def emitted(output: Path): Vector[String] =
    val stream = Files.walk(output)
    try stream.filter(Files.isRegularFile(_)).iterator().asScala.map(_.toString).toVector
    finally stream.close()

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ExistingUntpdSelectedApplyArgumentRewriteOriginAdapter.type],
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
