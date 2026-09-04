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

class ExistingUntpdSelectedApplyArgumentWrapRewriteOriginAdapterTyperRuntimeTest
    extends munit.FunSuite:
  private enum Mode:
    case W0, W1, W2, Valid, MissingWrapper

  List(Mode.W0, Mode.W1, Mode.W2).foreach { mode =>
    test(s"characterizes $mode before Typer") {
      val failure = intercept[AssertionError] {
        withCompilation(mode) { (_, reporter, _, _) =>
          fail(s"$mode unexpectedly returned a reporter: ${reporter.allErrors}")
        }
      }
      assert(failure.getMessage.contains("position not set"), clues(failure))
    }
  }

  test("W3 preserved-child wrapper survives Typer, emits TASTy, and executes") {
    withCompilation(Mode.Valid) { (driver, reporter, output, _) =>
      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assert(driver.beforeTyperContractValid)
      assertEquals(driver.afterCompilerFailures, Vector.empty)
      assert(emitted(output).exists(_.endsWith("U018PreservedArgumentWrap.tasty")))
      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U018PreservedArgumentWrapUse$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("value").invoke(module), Integer.valueOf(22))
      finally loader.close()
    }
  }

  test("missing wrapper is diagnosed at the exact original argument start") {
    withCompilation(Mode.MissingWrapper) { (driver, reporter, output, source) =>
      assert(reporter.hasErrors)
      assertEquals(reporter.allErrors.size, 1)
      val problem = reporter.allErrors.head
      assert(problem.message.contains("missingU018Wrapper"), clues(problem))
      assertEquals(problem.pos.source.path, source.toString)
      assertEquals(problem.pos.start, driver.originalArgumentStart)
      assert(driver.originalArgumentStart > driver.originalApplyStart)
      assert(driver.beforeTyperContractValid)
      assertEquals(driver.afterCompilerFailures, Vector.empty)
      assert(!emitted(output).exists(_.endsWith("U018PreservedArgumentWrap.tasty")))
    }
  }

  private def withCompilation(mode: Mode)(
      check: (OriginDriver, dotty.tools.dotc.reporting.Reporter, Path, Path) => Unit
  ): Unit =
    val temporary = Files.createTempDirectory("u018-preserved-argument-wrap-")
    try
      val source = temporary.resolve("U018PreservedArgumentWrap.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """class U018PreservedArgumentWrap:
          |  object service:
          |    def invoke(a: Int, b: Int): Int = a + b
          |  def helper(x: Int): Int = x + 1
          |  val oldArg: Int = 20
          |  val keptArg: Int = 1
          |  def keep: Int = 1
          |  def change: Int = service.invoke(oldArg, keptArg)
          |  val opaque: String = "opaque"
          |
          |object U018PreservedArgumentWrapUse:
          |  def value: Int = new U018PreservedArgumentWrap().change
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
    def phaseName: String = "u018PreservedArgumentWrapOriginAdaptation"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared =
        for
          root <- allTrees(unitTree).collectFirst {
            case value: untpd.TypeDef if value.name.toString == "U018PreservedArgumentWrap" =>
              value
          }.toRight("U018 runtime fixture class was not found")
          template <- root.rhs match
            case value: untpd.Template => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          target <- template.body.collectFirst {
            case value: untpd.DefDef if value.name.toString == "change" => value
          }.toRight("U018 runtime target was not found")
          outer <- target.rhs match
            case value: untpd.Apply => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          exactArgument <- outer.args.headOption.toRight("U018 runtime argument missing")
          wrapperFunction =
            given SourceFile = NoSource
            val name = mode match
              case Mode.MissingWrapper => "missingU018Wrapper"
              case _ => "helper"
            untpd.Ident(termName(name))
          originalState = allTrees(root).map(tree =>
            (tree, tree.source, tree.span, tree.symbol)
          )
          structural <- ExistingUntpdSelectedApplyArgumentWrapRewriter
            .rewrite(root, target, exactArgument, wrapperFunction).left.map(_.message)
          adapted <-
            if mode == Mode.Valid || mode == Mode.MissingWrapper then
              ExistingUntpdSelectedApplyArgumentWrapRewriteOriginAdapter
                .adapt(structural).left.map(_.message).map(Some(_))
            else Right(None)
        yield (
          root,
          template,
          target,
          outer,
          exactArgument,
          wrapperFunction,
          originalState,
          structural,
          adapted
        )

      prepared match
        case Left(problem) => report.error(problem)
        case Right((root, template, target, outer, exactArgument, wrapperFunction,
            originalState, structural, adaptedOption)) =>
          evidence.originalApplyStart = outer.span.start
          evidence.originalArgumentStart = exactArgument.span.start
          val finalRoot = adaptedOption match
            case Some(adapted) =>
              val structuralFreshNodes = Vector[untpd.Tree](
                structural.rebuiltRoot,
                structural.rebuiltTemplate,
                structural.rebuiltTarget,
                structural.rebuiltApply,
                structural.wrapperApply,
                structural.wrapperFunction
              )
              val positionedFreshNodes = Vector[untpd.Tree](
                adapted.positionedWrapperApply,
                adapted.positionedWrapperFunction
              )
              val originalUntouched = template.body.filterNot(_.eq(target))
              val positionedUntouched = adapted.positionedTemplate.body
                .filterNot(_.eq(adapted.positionedTarget))
              evidence.beforeTyperContractValid =
                structuralFreshNodes.forall(tree =>
                  !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol &&
                    !tree.isInstanceOf[untpd.TypedSplice]
                ) &&
                  structural.wrapperApply.args.size == 1 &&
                  structural.wrapperApply.args.head.eq(exactArgument) &&
                  positionedFreshNodes.forall(tree =>
                    tree.source == exactArgument.source && tree.span == exactArgument.span &&
                      tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
                  ) &&
                  adapted.positionedWrapperApply.args.size == 1 &&
                  adapted.positionedWrapperApply.args.head.eq(exactArgument) &&
                  originalState.forall { case (tree, source, span, symbol) =>
                    tree.source == source && tree.span == span && tree.symbol == symbol
                  } &&
                  adapted.positionedApply.fun.eq(outer.fun) &&
                  adapted.positionedApply.args(1).eq(outer.args(1)) &&
                  originalUntouched.size == positionedUntouched.size &&
                  originalUntouched.indices.forall(index =>
                    originalUntouched(index).eq(positionedUntouched(index))
                  )

              val structuralState = structuralFreshNodes.map(tree =>
                (tree, tree.source, tree.span, tree.symbol)
              )
              val originalArguments = outer.args.toVector
              val originalBody = template.body.toVector
              val exactArgumentState =
                (exactArgument.source, exactArgument.span, exactArgument.symbol)
              evidence.postCompilerCheck = () =>
                val originalFailures = originalState.zipWithIndex.flatMap {
                  case ((tree, source, span, _), index) =>
                    Vector(
                      s"original-$index-${tree.getClass.getSimpleName}-source" ->
                        (tree.source == source),
                      s"original-$index-${tree.getClass.getSimpleName}-span" ->
                        (tree.span == span)
                    ).collect { case (name, false) => name }
                }
                originalFailures ++ Vector(
                  "selected-argument-state" -> (
                    exactArgument.source == exactArgumentState._1 &&
                      exactArgument.span == exactArgumentState._2 &&
                      exactArgument.symbol == exactArgumentState._3
                  ),
                  "structural-fresh-state" -> structuralState.forall {
                    case (tree, source, span, symbol) =>
                      tree.source == source && tree.span == span && tree.symbol == symbol
                  },
                  "original-links" -> (root.rhs.eq(template) && target.rhs.eq(outer)),
                  "original-arguments" -> (
                    outer.args.size == originalArguments.size &&
                      outer.args.indices.forall(index =>
                        outer.args(index).eq(originalArguments(index))
                      )
                  ),
                  "original-body" -> (
                    template.body.size == originalBody.size &&
                      template.body.indices.forall(index =>
                        template.body(index).eq(originalBody(index))
                      )
                  ),
                  "structural-links" -> (
                    structural.rebuiltRoot.rhs.eq(structural.rebuiltTemplate) &&
                      structural.rebuiltTarget.rhs.eq(structural.rebuiltApply) &&
                      structural.rebuiltApply.args(structural.argumentIndex)
                        .eq(structural.wrapperApply) &&
                      structural.wrapperApply.fun.eq(wrapperFunction) &&
                      structural.wrapperApply.args.head.eq(exactArgument)
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
        structural: ExistingUntpdSelectedApplyArgumentWrapRewriter.Result
    )(using Context): untpd.TypeDef =
      if mode == Mode.W0 then structural.rebuiltRoot
      else
        val wrapper = mode match
          case Mode.W1 => structural.wrapperApply
          case Mode.W2 =>
            untpd.Apply(structural.wrapperFunction, structural.originalArgument :: Nil)
              .cloneIn(structural.originalArgument.source)
              .withSpan(structural.originalArgument.span)
          case _ => structural.wrapperApply
        positionOuter(structural, wrapper)

    private def positionOuter(
        structural: ExistingUntpdSelectedApplyArgumentWrapRewriter.Result,
        wrapper: untpd.Apply
    )(using Context): untpd.TypeDef =
      val arguments = structural.originalApply.args.zipWithIndex.map {
        case (_, index) if index == structural.argumentIndex => wrapper
        case (argument, _) => argument
      }
      val outer = untpd.Apply(structural.originalApply.fun, arguments)
        .cloneIn(structural.originalApply.source).withSpan(structural.originalApply.span)
      val target = untpd.cpy.DefDef(structural.rebuiltTarget)(
        structural.rebuiltTarget.name,
        structural.rebuiltTarget.paramss,
        structural.rebuiltTarget.tpt,
        outer
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
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ExistingUntpdSelectedApplyArgumentWrapRewriteOriginAdapter.type],
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
