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
import dotty.tools.dotc.util.{NoSource, SourceFile}

class ExistingUntpdSelectedApplyArgumentRewriteOriginStrategyProbeTest
    extends munit.FunSuite:
  private enum Strategy:
    case O0StructuralOnly, O1ApplyOnly, O2ArgumentSite

  private final case class Outcome(
      failure: Option[AssertionError],
      errors: Vector[String],
      emittedTasty: Boolean,
      runtimeValue: Option[Int],
      structuralIntermediateIntact: Boolean,
      originalTreeIntact: Boolean,
      identityContractValid: Boolean
  )

  test("characterizes O0, O1, and O2 on a selected Apply argument rewrite") {
    val o0 = compile(Strategy.O0StructuralOnly)
    assert(o0.failure.exists(_.getMessage.contains("position not set")), clues(o0))
    assert(!o0.emittedTasty)
    assert(o0.structuralIntermediateIntact)
    assert(o0.originalTreeIntact)

    val o1 = compile(Strategy.O1ApplyOnly)
    assertEquals(o1.failure, None, clues(o1))
    assertEquals(o1.errors, Vector.empty)
    assert(o1.emittedTasty)
    assertEquals(o1.runtimeValue, Some(22))
    assert(o1.structuralIntermediateIntact)
    assert(o1.originalTreeIntact)
    assert(o1.identityContractValid)

    val o2 = compile(Strategy.O2ArgumentSite)
    assertEquals(o2.failure, None, clues(o2))
    assertEquals(o2.errors, Vector.empty)
    assert(o2.emittedTasty)
    assertEquals(o2.runtimeValue, Some(22))
    assert(o2.structuralIntermediateIntact)
    assert(o2.originalTreeIntact)
    assert(o2.identityContractValid)
  }

  private def compile(strategy: Strategy): Outcome =
    val temporary = Files.createTempDirectory(s"u014-${strategy.toString.toLowerCase}-")
    try
      val source = temporary.resolve("U014OriginStrategy.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """class U014OriginStrategy:
          |  object service:
          |    def invoke(a: Int, b: Int): Int = a + b
          |  val oldArg: Int = 1
          |  val keptArg: Int = 20
          |  def keep: Int = 1
          |  def change: Int = service.invoke(oldArg, keptArg)
          |  val opaque: String = "opaque"
          |
          |object U014OriginStrategyUse:
          |  def value: Int = new U014OriginStrategy().change
          |""".stripMargin,
        StandardCharsets.UTF_8
      )
      val driver = new StrategyDriver(strategy)
      var errors = Vector.empty[String]
      val failure =
        try
          val reporter = driver.process(
            Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
          )
          errors = reporter.allErrors.toVector.map(_.message)
          None
        catch case problem: AssertionError => Some(problem)
      val files = emitted(output)
      val runtimeValue =
        if failure.isEmpty && errors.isEmpty then
          val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
          try
            val moduleClass = loader.loadClass("U014OriginStrategyUse$")
            val module = moduleClass.getField("MODULE$").get(null)
            Some(moduleClass.getMethod("value").invoke(module).asInstanceOf[Int])
          finally loader.close()
        else None
      Outcome(
        failure,
        errors,
        files.exists(_.endsWith("U014OriginStrategy.tasty")),
        runtimeValue,
        driver.structuralIntermediateIntact,
        driver.originalTreeIntact,
        driver.identityContractValid
      )
    finally deleteRecursively(temporary)

  private final class StrategyDriver(strategy: Strategy) extends Driver:
    @volatile var structuralIntermediateIntact: Boolean = false
    @volatile var originalTreeIntact: Boolean = false
    @volatile var identityContractValid: Boolean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new RewriteBeforeTyper(strategy, StrategyDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(
      strategy: Strategy,
      evidence: StrategyDriver
  ) extends Phase:
    def phaseName: String = s"u014OriginStrategy${strategy.toString}"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared =
        for
          root <- allTrees(unitTree).collectFirst {
            case value: untpd.TypeDef if value.name.toString == "U014OriginStrategy" => value
          }.toRight("U014 strategy fixture class was not found")
          template <- root.rhs match
            case value: untpd.Template => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          target <- template.body.collectFirst {
            case value: untpd.DefDef if value.name.toString == "change" => value
          }.toRight("U014 strategy target was not found")
          apply <- target.rhs match
            case value: untpd.Apply => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          exactArgument <- apply.args.headOption.toRight("U014 strategy argument missing")
          replacement =
            given SourceFile = NoSource
            untpd.Number("2", untpd.NumberKind.Whole(10))
          originalState = allTrees(root).map(tree => (tree, tree.source, tree.span))
          structural <- ExistingUntpdSelectedApplyArgumentRewriter
            .rewrite(root, target, exactArgument, replacement)
            .left.map(_.message)
        yield (root, template, target, apply, exactArgument, originalState, structural)

      prepared match
        case Left(problem) => report.error(problem)
        case Right((root, template, target, apply, exactArgument, originalState, structural)) =>
          evidence.structuralIntermediateIntact = sourceFreeStructuralObjects(structural)
          val positionedRoot = strategy match
            case Strategy.O0StructuralOnly => structural.rebuiltRoot
            case Strategy.O1ApplyOnly =>
              positionContainersAndApply(structural, structural.replacementLeaf)
            case Strategy.O2ArgumentSite =>
              val positionedReplacement = structural.replacementLeaf
                .cloneIn(exactArgument.source)
                .withSpan(exactArgument.span)
              positionContainersAndApply(structural, positionedReplacement)

          val positionedTemplate = positionedRoot.rhs.asInstanceOf[untpd.Template]
          val positionedTarget = positionedTemplate.body.collectFirst {
            case value: untpd.DefDef if value.name.toString == "change" => value
          }.getOrElse(throw AssertionError("positioned target missing"))
          val positionedApply = positionedTarget.rhs.asInstanceOf[untpd.Apply]
          evidence.originalTreeIntact = originalState.forall { case (tree, source, span) =>
            tree.source == source && tree.span == span
          }
          evidence.identityContractValid =
            positionedApply.fun.eq(apply.fun) &&
              positionedApply.args(1).eq(apply.args(1)) &&
              !positionedApply.eq(apply) &&
              template.body.filterNot(_.eq(target)).zip(
                positionedTemplate.body.filterNot(_.eq(positionedTarget))
              ).forall((left, right) => left.eq(right)) &&
              allTrees(positionedRoot).forall(tree =>
                tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
              )

          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              if tree.eq(root) then positionedRoot else super.transform(tree)
          summon[Context].compilationUnit.untpdTree = transformer.transform(unitTree)

  private def positionContainersAndApply(
      structural: ExistingUntpdSelectedApplyArgumentRewriter.Result,
      positionedReplacement: untpd.Tree
  )(using Context): untpd.TypeDef =
    val positionedArguments = structural.originalApply.args.zipWithIndex.map {
      case (_, index) if index == structural.argumentIndex => positionedReplacement
      case (argument, _) => argument
    }
    val positionedApply = untpd
      .Apply(structural.originalApply.fun, positionedArguments)
      .cloneIn(structural.originalApply.source)
      .withSpan(structural.originalApply.span)
    val positionedTarget = untpd
      .cpy
      .DefDef(structural.rebuiltTarget)(
        structural.rebuiltTarget.name,
        structural.rebuiltTarget.paramss,
        structural.rebuiltTarget.tpt,
        positionedApply
      )
      .cloneIn(structural.originalTarget.source)
      .withSpan(structural.originalTarget.span)
    val positionedTemplate = untpd
      .cpy
      .Template(structural.rebuiltTemplate)(
        structural.rebuiltTemplate.constr,
        structural.rebuiltTemplate.parentsOrDerived,
        structural.rebuiltTemplate.derived,
        structural.rebuiltTemplate.self,
        structural.prefix ::: positionedTarget :: structural.suffix
      )
      .cloneIn(structural.originalTemplate.source)
      .withSpan(structural.originalTemplate.span)
    untpd
      .cpy
      .TypeDef(structural.rebuiltRoot)(structural.rebuiltRoot.name, positionedTemplate)
      .cloneIn(structural.originalRoot.source)
      .withSpan(structural.originalRoot.span)

  private def sourceFreeStructuralObjects(
      structural: ExistingUntpdSelectedApplyArgumentRewriter.Result
  )(using Context): Boolean =
    Vector[untpd.Tree](
      structural.rebuiltRoot,
      structural.rebuiltTemplate,
      structural.rebuiltTarget,
      structural.rebuiltApply,
      structural.replacementLeaf
    ).forall(tree =>
      !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol &&
        !tree.isInstanceOf[untpd.TypedSplice]
    )

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
      classOf[ExistingUntpdSelectedApplyArgumentRewriter.type],
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
