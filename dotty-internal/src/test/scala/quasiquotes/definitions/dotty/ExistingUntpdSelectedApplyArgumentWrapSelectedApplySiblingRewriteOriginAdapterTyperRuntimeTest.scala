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

class ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginAdapterTyperRuntimeTest
    extends munit.FunSuite:
  private enum Shape:
    case OneIdent, OneNumber, OneLiteral, ThreeMixed

  private enum Mode:
    case T0, T1, T2
    case T3(shape: Shape)
    case T4(shape: Shape)
    case T5(shape: Shape)
    case T6(shape: Shape)
    case T7(shape: Shape)
    case MissingWrapper, MissingQualifier, MissingMember, MissingArgument

  List(Mode.T0, Mode.T1, Mode.T2).foreach { mode =>
    test(s"characterizes $mode as rejected before Typer") {
      val failure = intercept[AssertionError] {
        withCompilation(mode) { (_, reporter, _, _) =>
          fail(s"$mode unexpectedly returned a reporter: ${reporter.allErrors}")
        }
      }
      assert(failure.getMessage.contains("position not set"), clues(failure))
    }
  }

  Shape.values.foreach { shape =>
    List(Mode.T3(shape), Mode.T4(shape), Mode.T5(shape)).foreach { mode =>
      test(s"characterizes $mode as rejected before Typer") {
        val failure = intercept[AssertionError] {
          withCompilation(mode) { (_, reporter, _, _) =>
            fail(s"$mode unexpectedly returned a reporter: ${reporter.allErrors}")
          }
        }
        assert(failure.getMessage.contains("position not set"), clues(failure))
      }
    }
  }

  List(Shape.OneIdent, Shape.OneNumber, Shape.OneLiteral).foreach { shape =>
    test(s"T6($shape) is rejected when a detached position-sensitive leaf reaches Typer") {
      val failure = intercept[AssertionError] {
        withCompilation(Mode.T6(shape)) { (_, reporter, _, _) =>
          fail(s"T6($shape) unexpectedly returned a reporter: ${reporter.allErrors}")
        }
      }
      assert(failure.getMessage.contains("position not set"), clues(failure))
    }
  }

  (Mode.T6(Shape.ThreeMixed) :: Shape.values.toList.map(Mode.T7.apply)).foreach { mode =>
    test(s"$mode survives Typer, emits TASTy, and executes") {
      withCompilation(mode) { (driver, reporter, output, _) =>
        assert(!reporter.hasErrors, clues(reporter.allErrors))
        assert(driver.beforeTyperContractValid)
        assert(emitted(output).exists(_.endsWith("U021WrapSelectedApplySibling.tasty")))
        assertEquals(runtimeValue(output), Integer.valueOf(22))
      }
    }
  }

  List(
    Mode.MissingWrapper -> "missingU021Wrapper",
    Mode.MissingQualifier -> "missingU021Catalog",
    Mode.MissingMember -> "missingU021Member",
    Mode.MissingArgument -> "missingU021Argument"
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
        assert(!emitted(output).exists(_.endsWith("U021WrapSelectedApplySibling.tasty")))
      }
    }
  }

  private def runtimeValue(output: Path): Object =
    val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
    try
      val moduleClass = loader.loadClass("U021WrapSelectedApplySiblingUse$")
      val module = moduleClass.getField("MODULE$").get(null)
      moduleClass.getMethod("value").invoke(module)
    finally loader.close()

  private def withCompilation(mode: Mode)(
      check: (OriginDriver, dotty.tools.dotc.reporting.Reporter, Path, Path) => Unit
  ): Unit =
    val temporary = Files.createTempDirectory("u021-wrap-selected-apply-sibling-")
    try
      val source = temporary.resolve("U021WrapSelectedApplySibling.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(source,
        """class U021WrapSelectedApplySibling:
          |  object service:
          |    def invoke(a: Int, b: Int): Int = a + b
          |  def helper(a: Int, b: Int): Int = a + b
          |  object catalog:
          |    def product(a: Int): Int = a
          |    def product(a: Boolean): Int = if a then 20 else 0
          |    def product(a: Int, b: Int, c: Boolean): Int = if c then 20 else 0
          |  val oldArg: Int = 1
          |  val keptArg: Int = 1
          |  val freshValue: Int = 20
          |  def change: Int = service.invoke(oldArg, keptArg)
          |  val opaque: String = "opaque"
          |
          |object U021WrapSelectedApplySiblingUse:
          |  def value: Int = new U021WrapSelectedApplySibling().change
          |""".stripMargin, StandardCharsets.UTF_8)
      val driver = new OriginDriver(mode)
      val reporter = driver.process(Array("-classpath", compilationClasspath,
        "-d", output.toString, source.toString))
      check(driver, reporter, output, source)
    finally deleteRecursively(temporary)

  private final class OriginDriver(mode: Mode) extends Driver:
    @volatile var beforeTyperContractValid = false
    @volatile var originalApplyStart = -1
    @volatile var originalArgumentStart = -1

    override protected def newCompiler(using Context): Compiler = new Compiler:
      override protected def frontendPhases: List[List[Phase]] =
        List(new Parser) :: List(new RewriteBeforeTyper(mode, OriginDriver.this)) ::
          super.frontendPhases.tail

  private final class RewriteBeforeTyper(mode: Mode, evidence: OriginDriver) extends Phase:
    def phaseName: String = "u021WrapSelectedApplySiblingOriginAdaptation"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared = for
        root <- allTrees(unitTree).collectFirst {
          case value: untpd.TypeDef if value.name.toString == "U021WrapSelectedApplySibling" => value
        }.toRight("U021 fixture class was not found")
        template <- root.rhs match
          case value: untpd.Template => Right(value)
          case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
        target <- template.body.collectFirst {
          case value: untpd.DefDef if value.name.toString == "change" => value
        }.toRight("U021 target was not found")
        outer <- target.rhs match
          case value: untpd.Apply => Right(value)
          case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
        exact <- outer.args.headOption.toRight("U021 target argument missing")
        wrapper = detachedIdent(if mode == Mode.MissingWrapper then "missingU021Wrapper" else "helper")
        qualifier = detachedIdent(if mode == Mode.MissingQualifier then "missingU021Catalog" else "catalog")
        member = termName(if mode == Mode.MissingMember then "missingU021Member" else "product")
        leaves = detachedLeaves(mode)
        selection = detachedSelect(qualifier, member)
        sibling = detachedApply(selection, leaves)
        structural <- ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriter
          .rewrite(root, target, exact, wrapper, sibling).left.map(_.message)
        adapted <- mode match
          case Mode.T7(_) | Mode.MissingWrapper | Mode.MissingQualifier |
              Mode.MissingMember | Mode.MissingArgument =>
            ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginAdapter
              .adapt(structural).left.map(_.message).map(Some(_))
          case _ => Right(None)
      yield (root, template, target, outer, exact, structural, adapted)

      prepared match
        case Left(problem) => report.error(problem)
        case Right((root, template, target, outer, exact, structural, adapted)) =>
          evidence.originalApplyStart = outer.span.start
          evidence.originalArgumentStart = exact.span.start
          val finalRoot = adapted match
            case Some(value) =>
              evidence.beforeTyperContractValid = contractValid(structural,
                value.positionedWrapperApply, value.positionedWrapperFunction,
                value.positionedFreshSiblingApply, value.positionedFreshSiblingSelection,
                value.positionedFreshSiblingQualifier, value.positionedFreshSiblingArguments,
                leavesPositioned = true)
              value.positionedRoot
            case None =>
              val result = strategyRoot(mode, structural)
              mode match
                case Mode.T6(_) =>
                  val wrapper = selectedWrapper(result, target.name, structural.argumentIndex)
                  val sibling = wrapper.args(1).asInstanceOf[untpd.Apply]
                  val selection = sibling.fun.asInstanceOf[untpd.Select]
                  evidence.beforeTyperContractValid = contractValid(structural, wrapper,
                    wrapper.fun.asInstanceOf[untpd.Ident], sibling, selection,
                    selection.qualifier.asInstanceOf[untpd.Ident], sibling.args,
                    leavesPositioned = false)
                case _ => ()
              result
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              if tree.eq(root) then finalRoot else super.transform(tree)
          summon[Context].compilationUnit.untpdTree = transformer.transform(unitTree)

    private def strategyRoot(mode: Mode,
        structural: ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriter.Result
    )(using Context): untpd.TypeDef =
      if mode == Mode.T0 then structural.rebuiltRoot
      else
        val site = structural.originalArgument
        val wrapper = mode match
          case Mode.T1 => structural.wrapperApply
          case Mode.T2 => atSite(untpd.Apply(structural.wrapperFunction,
            site :: structural.freshSiblingApply :: Nil), site)
          case Mode.T3(_) =>
            val function = atSite(structural.wrapperFunction, site)
            atSite(untpd.Apply(function, site :: structural.freshSiblingApply :: Nil), site)
          case Mode.T4(_) =>
            val function = atSite(structural.wrapperFunction, site)
            val sibling = atSite(untpd.Apply(structural.freshSiblingSelection,
              structural.freshSiblingArguments), site)
            atSite(untpd.Apply(function, site :: sibling :: Nil), site)
          case Mode.T5(_) =>
            val function = atSite(structural.wrapperFunction, site)
            val selection = atSite(untpd.Select(structural.freshSiblingQualifier,
              structural.freshSiblingMemberName), site)
            val sibling = atSite(untpd.Apply(selection, structural.freshSiblingArguments), site)
            atSite(untpd.Apply(function, site :: sibling :: Nil), site)
          case Mode.T6(_) =>
            val function = atSite(structural.wrapperFunction, site)
            val qualifier = atSite(structural.freshSiblingQualifier, site)
            val selection = atSite(untpd.Select(qualifier, structural.freshSiblingMemberName), site)
            val sibling = atSite(untpd.Apply(selection, structural.freshSiblingArguments), site)
            atSite(untpd.Apply(function, site :: sibling :: Nil), site)
          case _ => structural.wrapperApply
        positionOuter(structural, wrapper)

    private def atSite[A <: untpd.Tree](tree: A, site: untpd.Tree)(using Context): A =
      tree.cloneIn(site.source).withSpan(site.span).asInstanceOf[A]

    private def positionOuter(
        structural: ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriter.Result,
        wrapper: untpd.Apply
    )(using Context): untpd.TypeDef =
      val arguments = structural.originalApply.args.zipWithIndex.map {
        case (_, index) if index == structural.argumentIndex => wrapper
        case (argument, _) => argument
      }
      val outer = untpd.Apply(structural.originalApply.fun, arguments)
        .cloneIn(structural.originalApply.source).withSpan(structural.originalApply.span)
      val target = untpd.cpy.DefDef(structural.rebuiltTarget)(structural.rebuiltTarget.name,
        structural.rebuiltTarget.paramss, structural.rebuiltTarget.tpt, outer)
        .cloneIn(structural.originalTarget.source).withSpan(structural.originalTarget.span)
      val template = untpd.cpy.Template(structural.rebuiltTemplate)(
        structural.rebuiltTemplate.constr, structural.rebuiltTemplate.parentsOrDerived,
        structural.rebuiltTemplate.derived, structural.rebuiltTemplate.self,
        structural.prefix ::: target :: structural.suffix)
        .cloneIn(structural.originalTemplate.source).withSpan(structural.originalTemplate.span)
      untpd.cpy.TypeDef(structural.rebuiltRoot)(structural.rebuiltRoot.name, template)
        .cloneIn(structural.originalRoot.source).withSpan(structural.originalRoot.span)

  private def detachedIdent(name: String)(using Context): untpd.Ident =
    given SourceFile = NoSource
    untpd.Ident(termName(name))

  private def detachedSelect(qualifier: untpd.Ident, member: dotty.tools.dotc.core.Names.TermName)
      (using Context): untpd.Select =
    given SourceFile = NoSource
    untpd.Select(qualifier, member)

  private def detachedApply(function: untpd.Tree, arguments: List[untpd.Tree])
      (using Context): untpd.Apply =
    given SourceFile = NoSource
    untpd.Apply(function, arguments)

  private def detachedLeaves(mode: Mode)(using Context): List[untpd.Tree] =
    given SourceFile = NoSource
    mode match
      case Mode.T3(Shape.OneIdent) | Mode.T4(Shape.OneIdent) | Mode.T5(Shape.OneIdent) |
          Mode.T6(Shape.OneIdent) | Mode.T7(Shape.OneIdent) =>
        List(untpd.Ident(termName("freshValue")))
      case Mode.T3(Shape.OneLiteral) | Mode.T4(Shape.OneLiteral) |
          Mode.T5(Shape.OneLiteral) | Mode.T6(Shape.OneLiteral) |
          Mode.T7(Shape.OneLiteral) =>
        List(untpd.Literal(Constant(true)))
      case Mode.T3(Shape.ThreeMixed) | Mode.T4(Shape.ThreeMixed) |
          Mode.T5(Shape.ThreeMixed) | Mode.T6(Shape.ThreeMixed) |
          Mode.T7(Shape.ThreeMixed) =>
        List(untpd.Ident(termName("freshValue")),
          untpd.Number("4", untpd.NumberKind.Whole(10)), untpd.Literal(Constant(true)))
      case Mode.MissingArgument => List(untpd.Ident(termName("missingU021Argument")))
      case _ => List(untpd.Number("20", untpd.NumberKind.Whole(10)))

  private def contractValid(
      structural: ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriter.Result,
      wrapper: untpd.Apply,
      wrapperFunction: untpd.Ident,
      sibling: untpd.Apply,
      selection: untpd.Select,
      qualifier: untpd.Ident,
      leaves: List[untpd.Tree],
      leavesPositioned: Boolean
  )(using Context): Boolean =
    val site = structural.originalArgument
    val fixed = Vector[untpd.Tree](wrapper, wrapperFunction, sibling, selection, qualifier)
    val fixedSites = fixed.forall(tree => tree.source == site.source && tree.span == site.span)
    val leafSites = leaves.forall(tree =>
      if leavesPositioned then tree.source == site.source && tree.span == site.span
      else !tree.source.exists && !tree.span.exists)
    fixedSites && leafSites && (fixed ++ leaves).forall(_.symbol == NoSymbol) &&
      wrapper.args(0).eq(site) && wrapper.args(1).eq(sibling) &&
      sibling.fun.eq(selection) && selection.qualifier.eq(qualifier) &&
      sibling.args.size == leaves.size && sibling.args.indices.forall(index =>
        sibling.args(index).eq(leaves(index)))

  private def selectedWrapper(root: untpd.TypeDef, targetName: dotty.tools.dotc.core.Names.Name,
      argumentIndex: Int)(using Context): untpd.Apply =
    root.rhs.asInstanceOf[untpd.Template].body.collectFirst {
      case value: untpd.DefDef if value.name == targetName => value
    }.get.rhs.asInstanceOf[untpd.Apply].args(argumentIndex).asInstanceOf[untpd.Apply]

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
    Vector(classOf[scala.Option[?]], classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginAdapter.type],
      getClass).flatMap(value => Option(value.getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource)).map(_.getLocation.toURI))
      .map(Path.of(_).toString).distinct.mkString(java.io.File.pathSeparator)

  private def deleteRecursively(root: Path): Unit =
    if Files.exists(root) then
      val stream = Files.walk(root)
      try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally stream.close()
