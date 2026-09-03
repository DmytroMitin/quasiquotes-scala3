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

class ExistingUntpdSelectedApplyArgumentApplyRewriteOriginAdapterTyperRuntimeTest
    extends munit.FunSuite:
  private enum Mode:
    case P0, P1, P2, Valid, MissingHelper

  List(Mode.P0, Mode.P1, Mode.P2).foreach { mode =>
    test(s"characterizes $mode before Typer") {
      val failure = intercept[AssertionError] {
        withCompilation(mode) { (_, reporter, _, _) =>
          fail(s"$mode unexpectedly returned a reporter: ${reporter.allErrors}")
        }
      }
      assert(failure.getMessage.contains("position not set"), clues(failure))
    }
  }

  test("P3 child-bearing replacement survives Typer, emits TASTy, and executes") {
    withCompilation(Mode.Valid) { (driver, reporter, output, _) =>
      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assert(driver.beforeTyperContractValid)
      assertEquals(driver.afterCompilerFailures, Vector.empty)
      assert(emitted(output).exists(_.endsWith("U015ChildBearingArgumentOrigin.tasty")))
      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U015ChildBearingArgumentOriginUse$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("value").invoke(module), Integer.valueOf(22))
      finally loader.close()
    }
  }

  test("missing helper is diagnosed at the exact old argument start") {
    withCompilation(Mode.MissingHelper) { (driver, reporter, output, source) =>
      assert(reporter.hasErrors)
      assertEquals(reporter.allErrors.size, 1)
      val problem = reporter.allErrors.head
      assert(problem.message.contains("missingU015Helper"), clues(problem))
      assertEquals(problem.pos.source.path, source.toString)
      assertEquals(problem.pos.start, driver.originalArgumentStart)
      assert(driver.originalArgumentStart > driver.originalApplyStart)
      assert(driver.beforeTyperContractValid)
      assertEquals(driver.afterCompilerFailures, Vector.empty)
      assert(!emitted(output).exists(_.endsWith("U015ChildBearingArgumentOrigin.tasty")))
    }
  }

  private def withCompilation(mode: Mode)(
      check: (OriginDriver, dotty.tools.dotc.reporting.Reporter, Path, Path) => Unit
  ): Unit =
    val temporary = Files.createTempDirectory("u015-child-bearing-argument-origin-")
    try
      val source = temporary.resolve("U015ChildBearingArgumentOrigin.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """class U015ChildBearingArgumentOrigin:
          |  object service:
          |    def invoke(a: Int, b: Int): Int = a + b
          |  def helper(x: Int): Int = x + 1
          |  val oldArg: Int = 1
          |  val keptArg: Int = 1
          |  def keep: Int = 1
          |  def change: Int = service.invoke(oldArg, keptArg)
          |  val opaque: String = "opaque"
          |
          |object U015ChildBearingArgumentOriginUse:
          |  def value: Int = new U015ChildBearingArgumentOrigin().change
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
    @volatile var postCompilerCheck: () => Vector[String] = () => Vector("not-installed")

    def afterCompilerFailures: Vector[String] = postCompilerCheck()

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(mode, OriginDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(mode: Mode, evidence: OriginDriver) extends Phase:
    def phaseName: String = "u015ChildBearingArgumentOriginAdaptation"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared =
        for
          root <- allTrees(unitTree).collectFirst {
            case value: untpd.TypeDef
                if value.name.toString == "U015ChildBearingArgumentOrigin" => value
          }.toRight("U015 runtime fixture class was not found")
          template <- root.rhs match
            case value: untpd.Template => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          target <- template.body.collectFirst {
            case value: untpd.DefDef if value.name.toString == "change" => value
          }.toRight("U015 runtime target was not found")
          outer <- target.rhs match
            case value: untpd.Apply => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          exactArgument <- outer.args.headOption.toRight("U015 runtime argument missing")
          replacement =
            given SourceFile = NoSource
            val name = mode match
              case Mode.Valid | Mode.P0 | Mode.P1 | Mode.P2 => "helper"
              case Mode.MissingHelper => "missingU015Helper"
            untpd.Apply(
              untpd.Ident(termName(name)),
              untpd.Number("20", untpd.NumberKind.Whole(10)) :: Nil
            )
          originalState = allTrees(root).map(tree => (tree, tree.source, tree.span, tree.symbol))
          structural <- ExistingUntpdSelectedApplyArgumentApplyRewriter
            .rewrite(root, target, exactArgument, replacement).left.map(_.message)
          adapted <-
            if mode == Mode.Valid || mode == Mode.MissingHelper then
              ExistingUntpdSelectedApplyArgumentApplyRewriteOriginAdapter
                .adapt(structural).left.map(_.message).map(Some(_))
            else Right(None)
        yield (root, template, target, outer, exactArgument, replacement,
          originalState, structural, adapted)

      prepared match
        case Left(problem) => report.error(problem)
        case Right((root, template, target, outer, exactArgument, replacement,
            originalState, structural, adaptedOption)) =>
          evidence.originalApplyStart = outer.span.start
          evidence.originalArgumentStart = exactArgument.span.start
          val finalRoot = adaptedOption match
            case Some(adapted) =>
              val structuralNodes = Vector[untpd.Tree](
                structural.rebuiltRoot, structural.rebuiltTemplate,
                structural.rebuiltTarget, structural.rebuiltApply,
                replacement, replacement.fun
              ) ++ replacement.args
              val positionedNodes = Vector[untpd.Tree](
                adapted.positionedReplacement, adapted.positionedReplacementFunction
              ) ++ adapted.positionedReplacementArguments
              val originalUntouched = template.body.filterNot(_.eq(target))
              val positionedUntouched = adapted.positionedTemplate.body
                .filterNot(_.eq(adapted.positionedTarget))
              evidence.beforeTyperContractValid =
                structuralNodes.forall(tree =>
                  !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol &&
                    !tree.isInstanceOf[untpd.TypedSplice]
                ) &&
                  positionedNodes.forall(tree =>
                    tree.source == exactArgument.source && tree.span == exactArgument.span &&
                      tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
                  ) &&
                  originalState.forall { case (tree, source, span, symbol) =>
                    tree.source == source && tree.span == span && tree.symbol == symbol
                  } &&
                  adapted.positionedApply.fun.eq(outer.fun) &&
                  adapted.positionedApply.args(1).eq(outer.args(1)) &&
                  !adapted.positionedReplacement.eq(replacement) &&
                  !adapted.positionedReplacementFunction.eq(replacement.fun) &&
                  originalUntouched.size == positionedUntouched.size &&
                  originalUntouched.indices.forall(index =>
                    originalUntouched(index).eq(positionedUntouched(index))
                  )
              val structuralState = structuralNodes.map(tree =>
                (tree, tree.source, tree.span, tree.symbol)
              )
              val originalArguments = outer.args.toVector
              val originalBody = template.body.toVector
              evidence.postCompilerCheck = () =>
                Vector(
                  "original-state" -> originalState.forall {
                    case (tree, source, span, _) =>
                      tree.source == source && tree.span == span
                  },
                  "structural-state" -> structuralState.forall {
                    case (tree, source, span, symbol) =>
                      tree.source == source && tree.span == span && tree.symbol == symbol
                  },
                  "original-links" -> (root.rhs.eq(template) && target.rhs.eq(outer)),
                  "original-arguments" -> (
                    outer.args.size == originalArguments.size &&
                      outer.args.indices.forall(index => outer.args(index).eq(originalArguments(index)))
                  ),
                  "original-body" -> (
                    template.body.size == originalBody.size &&
                      template.body.indices.forall(index => template.body(index).eq(originalBody(index)))
                  ),
                  "structural-links" -> (
                    structural.rebuiltRoot.rhs.eq(structural.rebuiltTemplate) &&
                      structural.rebuiltTarget.rhs.eq(structural.rebuiltApply) &&
                      structural.rebuiltApply.args(structural.argumentIndex)
                        .eq(structural.replacementApply)
                  ),
                  "replacement-links" -> (
                    structural.replacementApply.fun.eq(replacement.fun) &&
                      structural.replacementApply.args.indices.forall(index =>
                        structural.replacementApply.args(index).eq(replacement.args(index))
                      )
                  )
                ).collect { case (name, false) => name }
              adapted.positionedRoot
            case None => strategyRoot(mode, structural)

          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              if tree.eq(root) then finalRoot else super.transform(tree)
          summon[Context].compilationUnit.untpdTree = transformer.transform(unitTree)

    private def strategyRoot(
        mode: Mode,
        structural: ExistingUntpdSelectedApplyArgumentApplyRewriter.Result
    )(using Context): untpd.TypeDef =
      if mode == Mode.P0 then structural.rebuiltRoot
      else
        val replacement =
          if mode == Mode.P1 then structural.replacementApply
          else
            untpd.Apply(structural.replacementApply.fun, structural.replacementApply.args)
              .cloneIn(structural.originalArgument.source)
              .withSpan(structural.originalArgument.span)
        val args = structural.originalApply.args.zipWithIndex.map {
          case (_, index) if index == structural.argumentIndex => replacement
          case (argument, _) => argument
        }
        val outer = untpd.Apply(structural.originalApply.fun, args)
          .cloneIn(structural.originalApply.source).withSpan(structural.originalApply.span)
        val target = untpd.cpy.DefDef(structural.rebuiltTarget)(
          structural.rebuiltTarget.name, structural.rebuiltTarget.paramss,
          structural.rebuiltTarget.tpt, outer
        ).cloneIn(structural.originalTarget.source).withSpan(structural.originalTarget.span)
        val template = untpd.cpy.Template(structural.rebuiltTemplate)(
          structural.rebuiltTemplate.constr,
          structural.rebuiltTemplate.parentsOrDerived,
          structural.rebuiltTemplate.derived,
          structural.rebuiltTemplate.self,
          structural.prefix ::: target :: structural.suffix
        ).cloneIn(structural.originalTemplate.source).withSpan(structural.originalTemplate.span)
        untpd.cpy.TypeDef(structural.rebuiltRoot)(structural.rebuiltRoot.name, template)
          .cloneIn(structural.originalRoot.source).withSpan(structural.originalRoot.span)

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
      classOf[scala.Option[?]], classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ExistingUntpdSelectedApplyArgumentApplyRewriteOriginAdapter.type],
      getClass
    ).flatMap(value =>
      Option(value.getProtectionDomain).flatMap(domain => Option(domain.getCodeSource))
        .map(_.getLocation.toURI)
    ).map(Path.of(_).toString).distinct.mkString(java.io.File.pathSeparator)

  private def deleteRecursively(root: Path): Unit =
    if Files.exists(root) then
      val stream = Files.walk(root)
      try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally stream.close()
