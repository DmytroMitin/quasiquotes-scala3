package quasiquotes.definitions.dotty

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.util.{NoSource, SourceFile}

class ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginAdapterTyperRuntimeTest
    extends munit.FunSuite:
  private enum LeafKind:
    case Ident, Number, Literal

  private enum Mode:
    case S0, S1, S2
    case S3(kind: LeafKind)
    case S4(kind: LeafKind)
    case MissingWrapper, MissingSibling

  List(Mode.S0, Mode.S1, Mode.S2).foreach { mode =>
    test(s"characterizes $mode before Typer") {
      val failure = intercept[AssertionError] {
        withCompilation(mode) { (_, reporter, _, _) =>
          fail(s"$mode unexpectedly returned a reporter: ${reporter.allErrors}")
        }
      }
      assert(failure.getMessage.contains("position not set"), clues(failure))
    }
  }

  LeafKind.values.foreach { kind =>
    test(s"characterizes S3 source-free $kind sibling tolerance") {
      withCompilation(Mode.S3(kind)) { (driver, reporter, output, _) =>
        assert(!reporter.hasErrors, clues(reporter.allErrors))
        assert(driver.beforeTyperContractValid)
        assertEquals(driver.afterCompilerFailures, Vector.empty)
        assert(emitted(output).exists(_.endsWith("U019WrapSibling.tasty")))
        assertEquals(runtimeValue(output), Integer.valueOf(22))
      }
    }

    test(s"S4 $kind sibling survives Typer, emits TASTy, and executes") {
      withCompilation(Mode.S4(kind)) { (driver, reporter, output, _) =>
        assert(!reporter.hasErrors, clues(reporter.allErrors))
        assert(driver.beforeTyperContractValid)
        assertEquals(driver.afterCompilerFailures, Vector.empty)
        assert(emitted(output).exists(_.endsWith("U019WrapSibling.tasty")))
        assertEquals(runtimeValue(output), Integer.valueOf(22))
      }
    }
  }

  List(
    Mode.MissingWrapper -> "missingU019Wrapper",
    Mode.MissingSibling -> "missingU019Sibling"
  ).foreach { case (mode, expectedName) =>
    test(s"$expectedName is diagnosed at the exact original argument start") {
      withCompilation(mode) { (driver, reporter, output, source) =>
        assert(reporter.hasErrors)
        assertEquals(reporter.allErrors.size, 1)
        val problem = reporter.allErrors.head
        assert(problem.message.contains(expectedName), clues(problem))
        assertEquals(problem.pos.source.path, source.toString)
        assertEquals(problem.pos.start, driver.originalArgumentStart)
        assert(driver.originalArgumentStart > driver.originalApplyStart)
        assert(driver.beforeTyperContractValid)
        assertEquals(driver.afterCompilerFailures, Vector.empty)
        assert(!emitted(output).exists(_.endsWith("U019WrapSibling.tasty")))
      }
    }
  }

  private def runtimeValue(output: Path): Object =
    val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
    try
      val moduleClass = loader.loadClass("U019WrapSiblingUse$")
      val module = moduleClass.getField("MODULE$").get(null)
      moduleClass.getMethod("value").invoke(module)
    finally loader.close()

  private def withCompilation(mode: Mode)(
      check: (OriginDriver, dotty.tools.dotc.reporting.Reporter, Path, Path) => Unit
  ): Unit =
    val temporary = Files.createTempDirectory("u019-wrap-sibling-")
    try
      val source = temporary.resolve("U019WrapSibling.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """class U019WrapSibling:
          |  object service:
          |    def invoke(a: Int, b: Int): Int = a + b
          |  def helper(a: Int, b: Int): Int = a + b
          |  val oldArg: Int = 1
          |  val keptArg: Int = 1
          |  val freshValue: Int = 20
          |  def change: Int = service.invoke(oldArg, keptArg)
          |  val opaque: String = "opaque"
          |
          |object U019WrapSiblingUse:
          |  def value: Int = new U019WrapSibling().change
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
    def phaseName: String = "u019WrapSiblingOriginAdaptation"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared =
        for
          root <- allTrees(unitTree).collectFirst {
            case value: untpd.TypeDef if value.name.toString == "U019WrapSibling" => value
          }.toRight("U019 fixture class was not found")
          template <- root.rhs match
            case value: untpd.Template => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          target <- template.body.collectFirst {
            case value: untpd.DefDef if value.name.toString == "change" => value
          }.toRight("U019 target was not found")
          outer <- target.rhs match
            case value: untpd.Apply => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          exactArgument <- outer.args.headOption.toRight("U019 target argument missing")
          wrapperFunction =
            given SourceFile = NoSource
            val name = if mode == Mode.MissingWrapper then "missingU019Wrapper" else "helper"
            untpd.Ident(termName(name))
          sibling =
            given SourceFile = NoSource
            mode match
              case Mode.S3(LeafKind.Ident) | Mode.S4(LeafKind.Ident) =>
                untpd.Ident(termName("freshValue"))
              case Mode.S3(LeafKind.Literal) | Mode.S4(LeafKind.Literal) =>
                untpd.Literal(Constant(20))
              case Mode.MissingSibling => untpd.Ident(termName("missingU019Sibling"))
              case _ => untpd.Number("20", untpd.NumberKind.Whole(10))
          originalState = allTrees(root).map(tree =>
            (tree, tree.source, tree.span, tree.symbol)
          )
          structural <- ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter
            .rewrite(root, target, exactArgument, wrapperFunction, sibling)
            .left.map(_.message)
          adapted <-
            mode match
              case Mode.S4(_) | Mode.MissingWrapper | Mode.MissingSibling =>
                ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginAdapter
                  .adapt(structural).left.map(_.message).map(Some(_))
              case _ => Right(None)
        yield (root, template, target, outer, exactArgument, originalState, structural, adapted)

      prepared match
        case Left(problem) => report.error(problem)
        case Right((root, template, target, outer, exactArgument, originalState,
            structural, adaptedOption)) =>
          evidence.originalApplyStart = outer.span.start
          evidence.originalArgumentStart = exactArgument.span.start
          val finalRoot = adaptedOption match
            case Some(adapted) =>
              installEvidence(
                evidence,
                root,
                template,
                target,
                outer,
                exactArgument,
                originalState,
                structural,
                adapted.positionedWrapperApply,
                adapted.positionedWrapperFunction,
                adapted.positionedFreshSibling
              )
              adapted.positionedRoot
            case None =>
              val strategy = strategyRoot(mode, structural)
              mode match
                case Mode.S3(_) =>
                  val wrapper = strategy.rhs.asInstanceOf[untpd.Template].body
                    .collectFirst { case value: untpd.DefDef if value.name == target.name => value }
                    .get.rhs.asInstanceOf[untpd.Apply].args(structural.argumentIndex)
                    .asInstanceOf[untpd.Apply]
                  installEvidence(
                    evidence,
                    root,
                    template,
                    target,
                    outer,
                    exactArgument,
                    originalState,
                    structural,
                    wrapper,
                    wrapper.fun.asInstanceOf[untpd.Ident],
                    wrapper.args(1),
                    siblingExpectedPositioned = false
                  )
                case _ => ()
              strategy

          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              if tree.eq(root) then finalRoot else super.transform(tree)
          summon[Context].compilationUnit.untpdTree = transformer.transform(unitTree)

    private def installEvidence(
        evidence: OriginDriver,
        root: untpd.TypeDef,
        template: untpd.Template,
        target: untpd.DefDef,
        outer: untpd.Apply,
        exactArgument: untpd.Tree,
        originalState: Vector[(untpd.Tree, SourceFile, dotty.tools.dotc.util.Spans.Span,
          dotty.tools.dotc.core.Symbols.Symbol)],
        structural: ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter.Result,
        wrapper: untpd.Apply,
        function: untpd.Ident,
        sibling: untpd.Tree,
        siblingExpectedPositioned: Boolean = true
    )(using Context): Unit =
      val structuralFresh = Vector[untpd.Tree](
        structural.rebuiltRoot,
        structural.rebuiltTemplate,
        structural.rebuiltTarget,
        structural.rebuiltApply,
        structural.wrapperApply,
        structural.wrapperFunction,
        structural.freshSibling
      )
      val siblingSiteValid =
        if siblingExpectedPositioned then
          sibling.source == exactArgument.source && sibling.span == exactArgument.span
        else !sibling.source.exists && !sibling.span.exists
      evidence.beforeTyperContractValid =
        structuralFresh.forall(tree =>
          !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol &&
            !tree.isInstanceOf[untpd.TypedSplice]
        ) &&
          structural.wrapperApply.args.size == 2 &&
          structural.wrapperApply.args(0).eq(exactArgument) &&
          structural.wrapperApply.args(1).eq(structural.freshSibling) &&
          wrapper.source == exactArgument.source && wrapper.span == exactArgument.span &&
          function.source == exactArgument.source && function.span == exactArgument.span &&
          siblingSiteValid && wrapper.args(0).eq(exactArgument) &&
          wrapper.args(1).eq(sibling) && wrapper.fun.eq(function) &&
          wrapper.symbol == NoSymbol && function.symbol == NoSymbol &&
          sibling.symbol == NoSymbol && originalState.forall {
            case (tree, source, span, symbol) =>
              tree.source == source && tree.span == span && tree.symbol == symbol
          }

      val structuralState = structuralFresh.map(tree =>
        (tree, tree.source, tree.span, tree.symbol)
      )
      val originalArguments = outer.args.toVector
      val originalBody = template.body.toVector
      val argumentState = (exactArgument.source, exactArgument.span, exactArgument.symbol)
      evidence.postCompilerCheck = () =>
        val originalFailures = originalState.zipWithIndex.flatMap {
          case ((tree, source, span, _), index) =>
            Vector(
              s"original-$index-source" -> (tree.source == source),
              s"original-$index-span" -> (tree.span == span)
            ).collect { case (name, false) => name }
        }
        originalFailures ++ Vector(
          "selected-argument-state" -> (
            exactArgument.source == argumentState._1 &&
              exactArgument.span == argumentState._2 &&
              exactArgument.symbol == argumentState._3
          ),
          "structural-state" -> structuralState.forall {
            case (tree, source, span, symbol) =>
              tree.source == source && tree.span == span && tree.symbol == symbol
          },
          "original-links" -> (root.rhs.eq(template) && target.rhs.eq(outer)),
          "original-arguments" -> (
            outer.args.size == originalArguments.size && outer.args.indices.forall(index =>
              outer.args(index).eq(originalArguments(index))
            )
          ),
          "original-body" -> (
            template.body.size == originalBody.size && template.body.indices.forall(index =>
              template.body(index).eq(originalBody(index))
            )
          ),
          "structural-links" -> (
            structural.rebuiltRoot.rhs.eq(structural.rebuiltTemplate) &&
              structural.rebuiltTarget.rhs.eq(structural.rebuiltApply) &&
              structural.rebuiltApply.args(structural.argumentIndex).eq(structural.wrapperApply) &&
              structural.wrapperApply.args(0).eq(exactArgument) &&
              structural.wrapperApply.args(1).eq(structural.freshSibling)
          )
        ).collect { case (name, false) => name }

    private def strategyRoot(
        mode: Mode,
        structural: ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter.Result
    )(using Context): untpd.TypeDef =
      if mode == Mode.S0 then structural.rebuiltRoot
      else
        val site = structural.originalArgument
        val wrapper = mode match
          case Mode.S1 => structural.wrapperApply
          case Mode.S2 =>
            untpd.Apply(
              structural.wrapperFunction,
              site :: structural.freshSibling :: Nil
            ).cloneIn(site.source).withSpan(site.span)
          case Mode.S3(_) =>
            val function = structural.wrapperFunction.cloneIn(site.source)
              .withSpan(site.span).asInstanceOf[untpd.Ident]
            untpd.Apply(function, site :: structural.freshSibling :: Nil)
              .cloneIn(site.source).withSpan(site.span)
          case _ => structural.wrapperApply
        positionOuter(structural, wrapper)

    private def positionOuter(
        structural: ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter.Result,
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
      classOf[ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginAdapter.type],
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
