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

class ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteOriginAdapterTyperRuntimeTest
    extends munit.FunSuite:
  private enum SiblingShape:
    case OneIdent, OneNumber, OneLiteral, ThreeMixed

  private enum Mode:
    case S0, S1, S2
    case S3(shape: SiblingShape)
    case S4(shape: SiblingShape)
    case S5(shape: SiblingShape)
    case S6(shape: SiblingShape)
    case MissingWrapper, MissingSiblingFunction, MissingSiblingArgument

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

  SiblingShape.values.foreach { shape =>
    List(Mode.S3(shape), Mode.S4(shape)).foreach { mode =>
      test(s"characterizes $mode as rejected before Typer") {
        val failure = intercept[AssertionError] {
          withCompilation(mode) { (_, reporter, _, _) =>
            fail(s"$mode unexpectedly returned a reporter: ${reporter.allErrors}")
          }
        }
        assert(failure.getMessage.contains("position not set"), clues(failure))
      }
    }

    List(Mode.S5(shape), Mode.S6(shape)).foreach { mode =>
      test(s"$mode survives Typer, emits TASTy, and executes") {
        withCompilation(mode) { (driver, reporter, output, _) =>
          assert(!reporter.hasErrors, clues(reporter.allErrors))
          assert(driver.beforeTyperContractValid)
          assertEquals(driver.afterCompilerFailures, Vector.empty)
          assert(emitted(output).exists(_.endsWith("U020WrapApplySibling.tasty")))
          assertEquals(runtimeValue(output), Integer.valueOf(22))
        }
      }
    }
  }

  List(
    Mode.MissingWrapper -> "missingU020Wrapper",
    Mode.MissingSiblingFunction -> "missingU020SiblingFunction",
    Mode.MissingSiblingArgument -> "missingU020SiblingArgument"
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
        assert(!emitted(output).exists(_.endsWith("U020WrapApplySibling.tasty")))
      }
    }
  }

  private def runtimeValue(output: Path): Object =
    val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
    try
      val moduleClass = loader.loadClass("U020WrapApplySiblingUse$")
      val module = moduleClass.getField("MODULE$").get(null)
      moduleClass.getMethod("value").invoke(module)
    finally loader.close()

  private def withCompilation(mode: Mode)(
      check: (OriginDriver, dotty.tools.dotc.reporting.Reporter, Path, Path) => Unit
  ): Unit =
    val temporary = Files.createTempDirectory("u020-wrap-apply-sibling-")
    try
      val source = temporary.resolve("U020WrapApplySibling.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """class U020WrapApplySibling:
          |  object service:
          |    def invoke(a: Int, b: Int): Int = a + b
          |  def helper(a: Int, b: Int): Int = a + b
          |  def product(a: Int): Int = a
          |  def product(a: Int, b: Int, c: Int): Int = a * b * c
          |  val oldArg: Int = 1
          |  val keptArg: Int = 1
          |  val freshValue: Int = 20
          |  val freshOne: Int = 1
          |  def change: Int = service.invoke(oldArg, keptArg)
          |  val opaque: String = "opaque"
          |
          |object U020WrapApplySiblingUse:
          |  def value: Int = new U020WrapApplySibling().change
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
    def phaseName: String = "u020WrapApplySiblingOriginAdaptation"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared =
        for
          root <- allTrees(unitTree).collectFirst {
            case value: untpd.TypeDef if value.name.toString == "U020WrapApplySibling" => value
          }.toRight("U020 fixture class was not found")
          template <- root.rhs match
            case value: untpd.Template => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          target <- template.body.collectFirst {
            case value: untpd.DefDef if value.name.toString == "change" => value
          }.toRight("U020 target was not found")
          outer <- target.rhs match
            case value: untpd.Apply => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          exactArgument <- outer.args.headOption.toRight("U020 target argument missing")
          wrapperFunction =
            given SourceFile = NoSource
            val name = if mode == Mode.MissingWrapper then "missingU020Wrapper" else "helper"
            untpd.Ident(termName(name))
          siblingFunction =
            given SourceFile = NoSource
            val name = if mode == Mode.MissingSiblingFunction then
              "missingU020SiblingFunction"
            else "product"
            untpd.Ident(termName(name))
          siblingArguments =
            given SourceFile = NoSource
            mode match
              case Mode.S3(SiblingShape.OneIdent) | Mode.S4(SiblingShape.OneIdent) |
                  Mode.S5(SiblingShape.OneIdent) | Mode.S6(SiblingShape.OneIdent) =>
                List[untpd.Tree](untpd.Ident(termName("freshValue")))
              case Mode.S3(SiblingShape.OneLiteral) | Mode.S4(SiblingShape.OneLiteral) |
                  Mode.S5(SiblingShape.OneLiteral) | Mode.S6(SiblingShape.OneLiteral) =>
                List[untpd.Tree](untpd.Literal(Constant(20)))
              case Mode.S3(SiblingShape.ThreeMixed) | Mode.S4(SiblingShape.ThreeMixed) |
                  Mode.S5(SiblingShape.ThreeMixed) | Mode.S6(SiblingShape.ThreeMixed) =>
                List[untpd.Tree](
                  untpd.Ident(termName("freshOne")),
                  untpd.Number("4", untpd.NumberKind.Whole(10)),
                  untpd.Literal(Constant(5))
                )
              case Mode.MissingSiblingArgument =>
                List[untpd.Tree](untpd.Ident(termName("missingU020SiblingArgument")))
              case _ => List[untpd.Tree](untpd.Number("20", untpd.NumberKind.Whole(10)))
          sibling =
            given SourceFile = NoSource
            untpd.Apply(siblingFunction, siblingArguments)
          originalState = allTrees(root).map(tree =>
            (tree, tree.source, tree.span, tree.symbol)
          )
          structural <- ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriter
            .rewrite(root, target, exactArgument, wrapperFunction, sibling)
            .left.map(_.message)
          adapted <-
            mode match
              case Mode.S6(_) | Mode.MissingWrapper | Mode.MissingSiblingFunction |
                  Mode.MissingSiblingArgument =>
                ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteOriginAdapter
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
                adapted.positionedFreshSiblingApply,
                adapted.positionedFreshSiblingFunction,
                adapted.positionedFreshSiblingArguments,
                siblingArgumentsPositioned = true
              )
              adapted.positionedRoot
            case None =>
              val strategy = strategyRoot(mode, structural)
              mode match
                case Mode.S5(_) =>
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
                    wrapper.args(1).asInstanceOf[untpd.Apply],
                    wrapper.args(1).asInstanceOf[untpd.Apply].fun.asInstanceOf[untpd.Ident],
                    wrapper.args(1).asInstanceOf[untpd.Apply].args,
                    siblingArgumentsPositioned = false
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
        structural: ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriter.Result,
        wrapper: untpd.Apply,
        function: untpd.Ident,
        sibling: untpd.Apply,
        siblingFunction: untpd.Ident,
        siblingArguments: List[untpd.Tree],
        siblingArgumentsPositioned: Boolean
    )(using Context): Unit =
      val structuralFresh = Vector[untpd.Tree](
        structural.rebuiltRoot,
        structural.rebuiltTemplate,
        structural.rebuiltTarget,
        structural.rebuiltApply,
        structural.wrapperApply,
        structural.wrapperFunction,
        structural.freshSiblingApply,
        structural.freshSiblingFunction
      ) ++ structural.freshSiblingArguments
      val siblingArgumentsSiteValid = siblingArguments.forall { argument =>
        if siblingArgumentsPositioned then
          argument.source == exactArgument.source && argument.span == exactArgument.span
        else !argument.source.exists && !argument.span.exists
      }
      evidence.beforeTyperContractValid =
        structuralFresh.forall(tree =>
          !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol &&
            !tree.isInstanceOf[untpd.TypedSplice]
        ) &&
          structural.wrapperApply.args.size == 2 &&
          structural.wrapperApply.args(0).eq(exactArgument) &&
          structural.wrapperApply.args(1).eq(structural.freshSiblingApply) &&
          wrapper.source == exactArgument.source && wrapper.span == exactArgument.span &&
          function.source == exactArgument.source && function.span == exactArgument.span &&
          sibling.source == exactArgument.source && sibling.span == exactArgument.span &&
          siblingFunction.source == exactArgument.source &&
          siblingFunction.span == exactArgument.span && siblingArgumentsSiteValid &&
          wrapper.args(0).eq(exactArgument) &&
          wrapper.args(1).eq(sibling) && wrapper.fun.eq(function) &&
          sibling.fun.eq(siblingFunction) && sibling.args.size == siblingArguments.size &&
          sibling.args.indices.forall(index => sibling.args(index).eq(siblingArguments(index))) &&
          wrapper.symbol == NoSymbol && function.symbol == NoSymbol &&
          sibling.symbol == NoSymbol && siblingFunction.symbol == NoSymbol &&
          siblingArguments.forall(_.symbol == NoSymbol) && originalState.forall {
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
              structural.wrapperApply.args(1).eq(structural.freshSiblingApply) &&
              structural.freshSiblingApply.fun.eq(structural.freshSiblingFunction) &&
              structural.freshSiblingApply.args.indices.forall(index =>
                structural.freshSiblingApply.args(index)
                  .eq(structural.freshSiblingArguments(index))
              )
          )
        ).collect { case (name, false) => name }

    private def strategyRoot(
        mode: Mode,
        structural: ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriter.Result
    )(using Context): untpd.TypeDef =
      if mode == Mode.S0 then structural.rebuiltRoot
      else
        val site = structural.originalArgument
        val wrapper = mode match
          case Mode.S1 => structural.wrapperApply
          case Mode.S2 =>
            untpd.Apply(
              structural.wrapperFunction,
              site :: structural.freshSiblingApply :: Nil
            ).cloneIn(site.source).withSpan(site.span)
          case Mode.S3(_) =>
            val function = structural.wrapperFunction.cloneIn(site.source)
              .withSpan(site.span).asInstanceOf[untpd.Ident]
            untpd.Apply(function, site :: structural.freshSiblingApply :: Nil)
              .cloneIn(site.source).withSpan(site.span)
          case Mode.S4(_) =>
            val function = structural.wrapperFunction.cloneIn(site.source)
              .withSpan(site.span).asInstanceOf[untpd.Ident]
            val sibling = untpd.Apply(
              structural.freshSiblingFunction,
              structural.freshSiblingArguments
            ).cloneIn(site.source).withSpan(site.span)
            untpd.Apply(function, site :: sibling :: Nil)
              .cloneIn(site.source).withSpan(site.span)
          case Mode.S5(_) =>
            val function = structural.wrapperFunction.cloneIn(site.source)
              .withSpan(site.span).asInstanceOf[untpd.Ident]
            val siblingFunction = structural.freshSiblingFunction.cloneIn(site.source)
              .withSpan(site.span).asInstanceOf[untpd.Ident]
            val sibling = untpd.Apply(siblingFunction, structural.freshSiblingArguments)
              .cloneIn(site.source).withSpan(site.span)
            untpd.Apply(function, site :: sibling :: Nil)
              .cloneIn(site.source).withSpan(site.span)
          case _ => structural.wrapperApply
        positionOuter(structural, wrapper)

    private def positionOuter(
        structural: ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriter.Result,
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
      classOf[ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteOriginAdapter.type],
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
