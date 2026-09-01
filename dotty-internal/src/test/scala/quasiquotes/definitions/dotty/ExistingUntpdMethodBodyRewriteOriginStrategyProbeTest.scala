package quasiquotes.definitions.dotty

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
import dotty.tools.dotc.util.Spans.Span

class ExistingUntpdMethodBodyRewriteOriginStrategyProbeTest
    extends munit.FunSuite:
  private enum Strategy:
    case S1RootOnly
    case S2ReconstructedContainers
    case S3OriginalReplacementSite
    case S3RecursiveOriginalReplacementSite
    case S4GeneratedReplacementOrigin

  private final case class Outcome(
      failure: Option[AssertionError],
      hasErrors: Boolean,
      errors: Vector[DiagnosticEvidence],
      emittedTasty: Boolean,
      sourceFreeIntermediateIntact: Boolean,
      originalTreeIntact: Boolean,
      adaptedContractValid: Boolean
  )

  private final case class DiagnosticEvidence(
      message: String,
      sourcePath: String,
      start: Int
  )

  test("characterizes S1-S4 pre-Typer origin strategies without mutating U002 objects") {
    val s1 = compile(Strategy.S1RootOnly)
    assert(s1.failure.exists(_.getMessage.contains("position not set")), clues(s1))
    assert(s1.failure.exists(_.getMessage.contains("DefDef")), clues(s1))
    assert(!s1.emittedTasty)

    val s2 = compile(Strategy.S2ReconstructedContainers)
    assert(s2.failure.exists(_.getMessage.contains("position not set")), clues(s2))
    assert(s2.failure.exists(_.getMessage.contains("Number")), clues(s2))
    assert(!s2.emittedTasty)

    Vector(Strategy.S3OriginalReplacementSite, Strategy.S4GeneratedReplacementOrigin)
      .foreach { strategy =>
        val outcome = compile(strategy)
        assertEquals(outcome.failure, None, clues(strategy, outcome))
        assert(!outcome.hasErrors, clues(strategy, outcome))
        assert(outcome.emittedTasty, clues(strategy, outcome))
        assert(outcome.sourceFreeIntermediateIntact, clues(strategy, outcome))
        assert(outcome.originalTreeIntact, clues(strategy, outcome))
        assert(outcome.adaptedContractValid, clues(strategy, outcome))
      }
  }

  test("attributes invalid replacement diagnostics to the selected replacement origin") {
    val originalSite = compile(Strategy.S3OriginalReplacementSite, invalid = true)
    assertEquals(originalSite.failure, None, clues(originalSite))
    assert(originalSite.hasErrors, clues(originalSite))
    assertEquals(originalSite.errors.size, 1, clues(originalSite.errors))
    assert(
      originalSite.errors.head.message.contains("missingU003Replacement"),
      clues(originalSite.errors)
    )
    assert(
      originalSite.errors.head.sourcePath.endsWith("U003OriginStrategy.scala"),
      clues(originalSite.errors)
    )
    assert(originalSite.errors.head.start > 0, clues(originalSite.errors))

    val generated = compile(Strategy.S4GeneratedReplacementOrigin, invalid = true)
    assertEquals(generated.failure, None, clues(generated))
    assert(generated.hasErrors, clues(generated))
    assertEquals(generated.errors.size, 1, clues(generated.errors))
    assert(generated.errors.head.message.contains("missingU003Replacement"))
    assertEquals(
      generated.errors.head.sourcePath,
      "<quasiquotes-generated:u003-replacement>"
    )
    assertEquals(generated.errors.head.start, 0)
  }

  test("characterizes the child-position requirement for a multi-node replacement") {
    val rootOnly = compile(
      Strategy.S3OriginalReplacementSite,
      multiNode = true
    )
    assert(
      rootOnly.failure.exists(_.getMessage.contains("position not set")),
      clues(rootOnly)
    )
    assert(!rootOnly.emittedTasty)

    val recursive = compile(
      Strategy.S3RecursiveOriginalReplacementSite,
      multiNode = true
    )
    assertEquals(recursive.failure, None, clues(recursive))
    assert(!recursive.hasErrors, clues(recursive))
    assert(recursive.emittedTasty, clues(recursive))
    assert(recursive.sourceFreeIntermediateIntact, clues(recursive))
    assert(recursive.originalTreeIntact, clues(recursive))
  }

  private def compile(
      strategy: Strategy,
      invalid: Boolean = false,
      multiNode: Boolean = false
  ): Outcome =
    val temporary = Files.createTempDirectory(s"u003-${strategy.toString.toLowerCase}-")
    try
      val source = temporary.resolve("U003OriginStrategy.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """@deprecated("fixture", "1")
          |class U003OriginStrategy:
          |  def keep: Int = 1
          |  def change: Int = 2
          |  val opaque: String = "opaque"
          |  def call: Int = change
          |
          |object U003OriginStrategyUse:
          |  def value: Int = new U003OriginStrategy().change
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new StrategyDriver(strategy, invalid, multiNode)
      var hasErrors = false
      var errors = Vector.empty[DiagnosticEvidence]
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
          hasErrors = reporter.hasErrors
          errors = reporter.allErrors.toVector.map(problem =>
            DiagnosticEvidence(
              problem.message,
              problem.pos.source.path,
              problem.pos.start
            )
          )
          None
        catch case problem: AssertionError => Some(problem)
      val emitted =
        val stream = Files.walk(output)
        try
          stream
            .filter(Files.isRegularFile(_))
            .iterator()
            .asScala
            .exists(_.toString.endsWith("U003OriginStrategy.tasty"))
        finally stream.close()
      Outcome(
        failure,
        hasErrors,
        errors,
        emitted,
        driver.sourceFreeIntermediateIntact,
        driver.originalTreeIntact,
        driver.adaptedContractValid
      )
    finally deleteRecursively(temporary)

  private final class StrategyDriver(
      strategy: Strategy,
      invalid: Boolean,
      multiNode: Boolean
  )
      extends Driver:
    @volatile var sourceFreeIntermediateIntact: Boolean = false
    @volatile var originalTreeIntact: Boolean = false
    @volatile var adaptedContractValid: Boolean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(
              new AdaptBeforeTyper(
                strategy,
                invalid,
                multiNode,
                StrategyDriver.this
              )
            ) ::
            super.frontendPhases.tail

  private final class AdaptBeforeTyper(
      strategy: Strategy,
      invalid: Boolean,
      multiNode: Boolean,
      evidence: StrategyDriver
  ) extends Phase:
    def phaseName: String = s"u003OriginStrategy${strategy.toString}"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared =
        for
          originalRoot <- collectTrees(unitTree).collectFirst {
            case value: untpd.TypeDef
                if value.name.toString == "U003OriginStrategy" => value
          }.toRight("U003 runtime fixture class was not found")
          originalTemplate <- originalRoot.rhs match
            case value: untpd.Template => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          originalTarget <- originalTemplate.body.collectFirst {
            case value: untpd.DefDef if value.name.toString == "change" => value
          }.toRight("U003 runtime fixture target was not found")
          replacement =
            given SourceFile = NoSource
            if invalid then untpd.Ident(termName("missingU003Replacement"))
            else if multiNode then
              untpd.Apply(
                untpd.Ident(termName("identity")),
                untpd.Number("20", untpd.NumberKind.Whole(10)) :: Nil
              )
            else untpd.Number("20", untpd.NumberKind.Whole(10))
          result <- ExistingUntpdMethodBodyRewriter
            .rewrite(originalRoot, originalTarget, replacement)
            .left
            .map(_.message)
        yield (originalRoot, originalTemplate, originalTarget, replacement, result)

      prepared match
        case Left(problem) => report.error(problem)
        case Right((originalRoot, originalTemplate, originalTarget, replacement, result)) =>
          val originalTreeState = collectTrees(originalRoot).map(tree =>
            (tree, tree.source, tree.span)
          )
          evidence.sourceFreeIntermediateIntact =
            Vector[untpd.Tree](
              result.rebuiltRoot,
              result.rebuiltTemplate,
              result.rebuiltTarget,
              replacement
            ).forall(tree =>
              !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol
            )

          val adapted = strategy match
            case Strategy.S1RootOnly =>
              result.rebuiltRoot
                .cloneIn(originalRoot.source)
                .withSpan(originalRoot.span)
            case Strategy.S2ReconstructedContainers =>
              positionContainers(result, replacement)
            case Strategy.S3OriginalReplacementSite =>
              val positionedReplacement = replacement
                .cloneIn(originalTarget.rhs.source)
                .withSpan(originalTarget.rhs.span)
              positionContainers(result, positionedReplacement)
            case Strategy.S3RecursiveOriginalReplacementSite =>
              val positionedReplacement = positionEveryReplacementNodeAtSite(
                replacement,
                originalTarget.rhs.source,
                originalTarget.rhs.span
              )
              positionContainers(result, positionedReplacement)
            case Strategy.S4GeneratedReplacementOrigin =>
              val generatedText =
                if invalid then "missingU003Replacement" else "20"
              val generatedSource =
                SourceFile.virtual(
                  "<quasiquotes-generated:u003-replacement>",
                  generatedText
                )
              val positionedReplacement = replacement
                .cloneIn(generatedSource)
                .withSpan(Span(0, generatedText.length, 0))
              positionContainers(result, positionedReplacement)

          val adaptedTemplate = adapted.rhs.asInstanceOf[untpd.Template]
          val adaptedTarget = adaptedTemplate.body.collectFirst {
            case value: untpd.DefDef if value.name.toString == "change" => value
          }.getOrElse(throw AssertionError("adapted target missing"))
          val originalUntouched = originalTemplate.body.filterNot(_.eq(originalTarget))
          val adaptedUntouched = adaptedTemplate.body.filterNot(_.eq(adaptedTarget))
          evidence.originalTreeIntact = originalTreeState.forall {
            case (tree, source, span) =>
              tree.source == source && tree.span == span
          }
          evidence.adaptedContractValid =
            !adapted.eq(originalRoot) &&
              !adapted.eq(result.rebuiltRoot) &&
              originalUntouched.size == adaptedUntouched.size &&
              originalUntouched.zip(adaptedUntouched).forall((left, right) =>
                left.eq(right)
              ) &&
              collectTrees(adapted).forall(tree =>
                tree.symbol == NoSymbol &&
                  !tree.isInstanceOf[untpd.TypedSplice]
              )

          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              if tree.eq(originalRoot) then adapted else super.transform(tree)
          summon[Context].compilationUnit.untpdTree = transformer.transform(unitTree)

  private def positionContainers(
      result: ExistingUntpdMethodBodyRewriter.Result,
      positionedReplacement: untpd.Tree
  )(using Context): untpd.TypeDef =
    val positionedTarget =
      untpd
        .cpy
        .DefDef(result.rebuiltTarget)(
          result.rebuiltTarget.name,
          result.rebuiltTarget.paramss,
          result.rebuiltTarget.tpt,
          positionedReplacement
        )
        .cloneIn(result.originalTarget.source)
        .withSpan(result.originalTarget.span)
    val positionedTemplate =
      untpd
        .cpy
        .Template(result.rebuiltTemplate)(
          result.rebuiltTemplate.constr,
          result.rebuiltTemplate.parentsOrDerived,
          result.rebuiltTemplate.derived,
          result.rebuiltTemplate.self,
          result.prefix ::: positionedTarget :: result.suffix
        )
        .cloneIn(result.originalTemplate.source)
        .withSpan(result.originalTemplate.span)
    untpd
      .cpy
      .TypeDef(result.rebuiltRoot)(result.rebuiltRoot.name, positionedTemplate)
      .cloneIn(result.originalRoot.source)
      .withSpan(result.originalRoot.span)

  private def positionEveryReplacementNodeAtSite(
      replacement: untpd.Tree,
      source: SourceFile,
      span: Span
  )(using Context): untpd.Tree =
    val transformer = new untpd.UntypedTreeMap:
      override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
        super.transform(tree).cloneIn(source).withSpan(span)
    transformer.transform(replacement)

  private def collectTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val collector = Vector.newBuilder[untpd.Tree]
    val traverser = new untpd.UntypedTreeTraverser:
      override def traverse(current: untpd.Tree)(using Context): Unit =
        collector += current
        traverseChildren(current)
    traverser.traverse(tree)
    collector.result()

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
