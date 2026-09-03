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

class ExistingUntpdMethodBodyRewriteSelectedApplyOriginStrategyProbeTest
    extends munit.FunSuite:
  private enum Strategy:
    case S1RootOnly, S2ApplySelectOnly, S3Uniform

  private enum Mode:
    case Valid, MissingService, MissingMember

  private final case class Diagnostic(message: String, sourcePath: String, start: Int)

  private final case class Outcome(
      failure: Option[AssertionError],
      diagnostics: Vector[Diagnostic],
      emittedTasty: Boolean,
      runtimeValue: Option[Int],
      originalRhsStart: Int,
      sourceFreeIntermediateIntact: Boolean,
      originalTreeIntact: Boolean,
      adaptedContractValid: Boolean
  )

  test("S1 root-only and S2 Apply-Select-only attribution remain insufficient") {
    List(Strategy.S1RootOnly, Strategy.S2ApplySelectOnly).foreach { strategy =>
      val outcome = compile(strategy, Mode.Valid)
      assert(
        outcome.failure.exists(_.getMessage.contains("position not set")),
        clues(strategy, outcome)
      )
      assert(!outcome.emittedTasty, clues(strategy, outcome))
      assert(outcome.sourceFreeIntermediateIntact, clues(strategy, outcome))
      assert(outcome.originalTreeIntact, clues(strategy, outcome))
    }
  }

  test("S3 uniform transformation-site attribution survives Typer and executes") {
    val outcome = compile(Strategy.S3Uniform, Mode.Valid)
    assertEquals(outcome.failure, None, clues(outcome))
    assertEquals(outcome.diagnostics, Vector.empty, clues(outcome))
    assert(outcome.emittedTasty, clues(outcome))
    assertEquals(outcome.runtimeValue, Some(21), clues(outcome))
    assert(outcome.sourceFreeIntermediateIntact, clues(outcome))
    assert(outcome.originalTreeIntact, clues(outcome))
    assert(outcome.adaptedContractValid, clues(outcome))
  }

  test("S3 invalid qualifier and member diagnostics use the old RHS site") {
    List(
      Mode.MissingService -> "missingU013Service",
      Mode.MissingMember -> "missingU013Member"
    ).foreach { case (mode, expectedName) =>
      val outcome = compile(Strategy.S3Uniform, mode)
      assertEquals(outcome.failure, None, clues(mode, outcome))
      assertEquals(outcome.diagnostics.size, 1, clues(mode, outcome))
      val diagnostic = outcome.diagnostics.head
      assert(diagnostic.message.contains(expectedName), clues(diagnostic))
      assert(diagnostic.sourcePath.endsWith("U013OriginStrategy.scala"))
      assertEquals(diagnostic.start, outcome.originalRhsStart)
      assert(diagnostic.start > 0)
      assert(!outcome.emittedTasty)
      assert(outcome.adaptedContractValid)
    }
  }

  private def compile(strategy: Strategy, mode: Mode): Outcome =
    val temporary = Files.createTempDirectory("u013-selected-origin-strategy-")
    try
      val source = temporary.resolve("U013OriginStrategy.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """class U013OriginStrategy:
          |  object service:
          |    def invoke(x: Int): Int = x + 1
          |  def keep: Int = 1
          |  def change: Int = 0
          |  val opaque: String = "opaque"
          |
          |object U013OriginStrategyUse:
          |  def value: Int = new U013OriginStrategy().change
          |""".stripMargin,
        StandardCharsets.UTF_8
      )
      val driver = new StrategyDriver(strategy, mode)
      var diagnostics = Vector.empty[Diagnostic]
      val failure =
        try
          val reporter = driver.process(
            Array(
              "-classpath",
              compilationClasspath,
              "-d",
              output.toString,
              source.toString
            )
          )
          diagnostics = reporter.allErrors.toVector.map(problem =>
            Diagnostic(problem.message, problem.pos.source.path, problem.pos.start)
          )
          None
        catch case problem: AssertionError => Some(problem)
      val files = emitted(output)
      val runtimeValue =
        if failure.isEmpty && diagnostics.isEmpty then
          val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
          try
            val moduleClass = loader.loadClass("U013OriginStrategyUse$")
            val module = moduleClass.getField("MODULE$").get(null)
            Some(moduleClass.getMethod("value").invoke(module).asInstanceOf[Int])
          finally loader.close()
        else None
      Outcome(
        failure,
        diagnostics,
        files.exists(_.endsWith("U013OriginStrategy.tasty")),
        runtimeValue,
        driver.originalRhsStart,
        driver.sourceFreeIntermediateIntact,
        driver.originalTreeIntact,
        driver.adaptedContractValid
      )
    finally deleteRecursively(temporary)

  private final class StrategyDriver(strategy: Strategy, mode: Mode) extends Driver:
    @volatile var originalRhsStart: Int = -1
    @volatile var sourceFreeIntermediateIntact: Boolean = false
    @volatile var originalTreeIntact: Boolean = false
    @volatile var adaptedContractValid: Boolean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new AdaptBeforeTyper(strategy, mode, StrategyDriver.this)) ::
            super.frontendPhases.tail

  private final class AdaptBeforeTyper(
      strategy: Strategy,
      mode: Mode,
      evidence: StrategyDriver
  ) extends Phase:
    def phaseName: String = s"u013SelectedApply${strategy.toString}${mode.toString}"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared =
        for
          originalRoot <- allTrees(unitTree).collectFirst {
            case value: untpd.TypeDef if value.name.toString == "U013OriginStrategy" =>
              value
          }.toRight("U013 strategy fixture class was not found")
          originalTemplate <- originalRoot.rhs match
            case value: untpd.Template => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          originalTarget <- originalTemplate.body.collectFirst {
            case value: untpd.DefDef if value.name.toString == "change" => value
          }.toRight("U013 strategy target was not found")
          replacement =
            given SourceFile = NoSource
            val qualifier = mode match
              case Mode.MissingService => "missingU013Service"
              case _ => "service"
            val member = mode match
              case Mode.MissingMember => "missingU013Member"
              case _ => "invoke"
            untpd.Apply(
              untpd.Select(untpd.Ident(termName(qualifier)), termName(member)),
              untpd.Number("20", untpd.NumberKind.Whole(10)) :: Nil
            )
          originalState = allTrees(originalRoot).map(tree => (tree, tree.source, tree.span))
          structural <- ExistingUntpdMethodBodyRewriter
            .rewrite(originalRoot, originalTarget, replacement)
            .left
            .map(_.message)
        yield (originalRoot, originalTemplate, originalTarget, originalState, structural)

      prepared match
        case Left(problem) => report.error(problem)
        case Right((originalRoot, originalTemplate, originalTarget, originalState, structural)) =>
          evidence.originalRhsStart = originalTarget.rhs.span.start
          val replacement = structural.replacementBody.asInstanceOf[untpd.Apply]
          val selection = replacement.fun.asInstanceOf[untpd.Select]
          val source = originalTarget.rhs.source
          val span = originalTarget.rhs.span
          evidence.sourceFreeIntermediateIntact =
            (Vector[untpd.Tree](
              structural.rebuiltRoot,
              structural.rebuiltTemplate,
              structural.rebuiltTarget
            ) ++ allTrees(structural.replacementBody)).forall(tree =>
              !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol &&
                !tree.isInstanceOf[untpd.TypedSplice]
            )

          val positionedReplacement = strategy match
            case Strategy.S1RootOnly =>
              replacement.cloneIn(source).withSpan(span)
            case Strategy.S2ApplySelectOnly =>
              val positionedSelection = untpd
                .Select(selection.qualifier, selection.name)
                .cloneIn(source)
                .withSpan(span)
              untpd.Apply(positionedSelection, replacement.args).cloneIn(source).withSpan(span)
            case Strategy.S3Uniform =>
              val positionedQualifier = selection.qualifier.cloneIn(source).withSpan(span)
              val positionedSelection = untpd
                .Select(positionedQualifier, selection.name)
                .cloneIn(source)
                .withSpan(span)
              val positionedArguments = replacement.args.map(_.cloneIn(source).withSpan(span))
              untpd
                .Apply(positionedSelection, positionedArguments)
                .cloneIn(source)
                .withSpan(span)

          val positionedRoot = positionContainers(structural, positionedReplacement)
          val positionedTemplate = positionedRoot.rhs.asInstanceOf[untpd.Template]
          val positionedTarget = positionedTemplate.body.collectFirst {
            case value: untpd.DefDef if value.name.toString == "change" => value
          }.getOrElse(throw AssertionError("positioned target missing"))
          val originalUntouched = originalTemplate.body.filterNot(_.eq(originalTarget))
          val positionedUntouched = positionedTemplate.body.filterNot(_.eq(positionedTarget))
          evidence.originalTreeIntact = originalState.forall { case (tree, oldSource, oldSpan) =>
            tree.source == oldSource && tree.span == oldSpan
          }
          evidence.adaptedContractValid =
            !positionedRoot.eq(originalRoot) &&
              !positionedRoot.eq(structural.rebuiltRoot) &&
              originalUntouched.size == positionedUntouched.size &&
              originalUntouched.zip(positionedUntouched).forall((left, right) => left.eq(right)) &&
              allTrees(positionedRoot).forall(tree =>
                tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
              )

          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              if tree.eq(originalRoot) then positionedRoot else super.transform(tree)
          summon[Context].compilationUnit.untpdTree = transformer.transform(unitTree)

  private def positionContainers(
      structural: ExistingUntpdMethodBodyRewriter.Result,
      positionedReplacement: untpd.Tree
  )(using Context): untpd.TypeDef =
    val positionedTarget = untpd
      .cpy
      .DefDef(structural.rebuiltTarget)(
        structural.rebuiltTarget.name,
        structural.rebuiltTarget.paramss,
        structural.rebuiltTarget.tpt,
        positionedReplacement
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
      classOf[ExistingUntpdMethodBodyRewriter.type],
      getClass
    )
      .flatMap(value =>
        Option(value.getProtectionDomain)
          .flatMap(domain => Option(domain.getCodeSource))
          .map(_.getLocation.toURI)
      )
      .map(Path.of(_).toString)
      .distinct
      .mkString(java.io.File.pathSeparator)

  private def deleteRecursively(root: Path): Unit =
    if Files.exists(root) then
      val stream = Files.walk(root)
      try
        stream
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.deleteIfExists(_))
      finally stream.close()
