package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.SemanticDefinition

import scala.util.control.NonFatal

/** One validated direct-member plan followed by at most one owner reconstruction. */
private[quasiquotes] object ExistingUntpdClassOwnerTransaction:
  import ExistingUntpdClassMemberFilter.Capture
  import ExistingUntpdOrdinaryMethodReconstruction.ReconstructedMethod

  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  private[dotty] final class GeneratedNodeEvidence private[ExistingUntpdClassOwnerTransaction] (
      val tree: untpd.Tree,
      val sourceFile: SourceFile,
      val span: Span,
      val symbol: Symbol,
      val modifiers: Option[untpd.Modifiers]
  )

  private[dotty] final class GeneratedPreparationEvidence private[ExistingUntpdClassOwnerTransaction] (
      val definition: SemanticDefinition,
      val requestedVirtualSourceName: String,
      val lowered: DefinitionGeneratedOriginLowering.Lowered,
      val member: untpd.MemberDef,
      val generatedSource: String,
      val sourceFile: SourceFile,
      val graph: Vector[GeneratedNodeEvidence]
  )

  final case class PreparedGeneratedMember private[dotty] (
      definition: SemanticDefinition,
      requestedVirtualSourceName: String,
      lowered: DefinitionGeneratedOriginLowering.Lowered,
      member: untpd.MemberDef,
      generatedSource: String,
      sourceFile: SourceFile,
      private[dotty] val evidence: GeneratedPreparationEvidence
  )

  final case class Result private[dotty] (
      captured: Capture,
      changed: Boolean,
      omittedIndices: Vector[Int],
      methodEdits: Vector[ReconstructedMethod],
      generatedAppends: Vector[PreparedGeneratedMember],
      finalMembers: Vector[untpd.Tree],
      root: untpd.TypeDef,
      template: untpd.Template
  )

  private final case class ValidatedOperations(
      omitted: Set[Int],
      methodEdits: Map[Int, ReconstructedMethod],
      generatedAppends: Vector[PreparedGeneratedMember]
  ):
    def changed: Boolean =
      omitted.nonEmpty || methodEdits.nonEmpty || generatedAppends.nonEmpty

  def prepareGenerated(
      definition: SemanticDefinition,
      virtualSourceName: String
  )(using Context): Either[Error, PreparedGeneratedMember] =
    for
      semantic <- Option(definition).toRight(
        error("DEFINITION_REQUIRED", "the semantic Definition must be present.")
      )
      requestedPath <- Option(virtualSourceName).toRight(
        error("VIRTUAL_SOURCE_NAME_REQUIRED", "the requested virtual source name must be present.")
      )
      lowered <- DefinitionGeneratedOriginLowering
        .lower(semantic, requestedPath)
        .left
        .map(problem => error(
          "GENERATED_MEMBER_PREPARATION_FAILED",
          s"C037 ${problem.code}: ${problem.detail}"
        ))
      member <- Option(lowered.tree).toRight(
        error("INVALID_GENERATED_PREPARATION", "C037 returned no generated member.")
      )
      graph = rawTrees(member).map(tree =>
        new GeneratedNodeEvidence(
          tree,
          tree.source,
          tree.span,
          tree.symbol,
          modifiersOf(tree)
        )
      )
      evidence = new GeneratedPreparationEvidence(
        semantic,
        requestedPath,
        lowered,
        member,
        lowered.generatedSource,
        lowered.sourceFile,
        graph
      )
      prepared = PreparedGeneratedMember(
        semantic,
        requestedPath,
        lowered,
        member,
        lowered.generatedSource,
        lowered.sourceFile,
        evidence
      )
      _ <- validatePreparedGeneratedMember(prepared)
    yield prepared

  def apply(
      captured: Capture,
      omittedIndices: Vector[Int],
      methodEdits: Vector[ReconstructedMethod],
      generatedAppends: Vector[PreparedGeneratedMember]
  )(using Context): Either[Error, Result] =
    for
      original <- Option(captured).toRight(
        error("CAPTURE_REQUIRED", "the original U023 capture must be present.")
      )
      _ <- validateCapture(original)
      omissions <- Option(omittedIndices).toRight(
        error("OMISSIONS_REQUIRED", "the omission collection must be present.")
      )
      edits <- Option(methodEdits).toRight(
        error("METHOD_EDITS_REQUIRED", "the method-edit collection must be present.")
      )
      generated <- Option(generatedAppends).toRight(
        error("GENERATED_APPENDS_REQUIRED", "the generated-append collection must be present.")
      )
      operations <- validateOperations(original, omissions, edits, generated)
      members = buildFinalMembers(original, operations)
      result <-
        if operations.changed then reconstructChanged(original, operations, members)
        else Right(Result(
          original,
          changed = false,
          Vector.empty,
          Vector.empty,
          Vector.empty,
          original.members.map(_.tree),
          original.originalRoot,
          original.originalTemplate
        ))
      _ <- validateResult(result, original)
    yield result

  private[dotty] def validateResult(
      result: Result,
      expectedCapture: Capture
  )(using Context): Either[Error, Unit] =
    val failure = error(
      "FINAL_TRANSACTION_INVARIANT_FAILED",
      "the owner transaction result contradicted its original capture, operations, final member identities, shell provenance, or pre-Typer graph."
    )
    try
      for
        value <- Option(result).toRight(failure)
        expected <- Option(expectedCapture).toRight(failure)
        _ <- validateCapture(expected).left.map(_ => failure)
        _ <- Either.cond(value.captured.eq(expected), (), failure)
        omissions <- Option(value.omittedIndices).toRight(failure)
        edits <- Option(value.methodEdits).toRight(failure)
        generated <- Option(value.generatedAppends).toRight(failure)
        operations <- validateOperations(expected, omissions, edits, generated)
          .left.map(_ => failure)
        finalMembers <- Option(value.finalMembers).toRight(failure)
        expectedMembers = buildFinalMembers(expected, operations)
        _ <- Either.cond(
          identitiesEqual(finalMembers, expectedMembers),
          (),
          failure
        )
        _ <- Either.cond(value.changed == operations.changed, (), failure)
        _ <-
          if operations.changed then validateChangedResult(value, expected, operations, failure)
          else validateNoopResult(value, expected, failure)
      yield ()
    catch
      case NonFatal(_) => Left(failure)

  private def reconstructChanged(
      captured: Capture,
      operations: ValidatedOperations,
      finalMembers: Vector[untpd.Tree]
  )(using Context): Either[Error, Result] =
    ExistingUntpdClassMemberFilter
      .reconstruct(captured, finalMembers)
      .left
      .map(problem => error(
        "OWNER_RECONSTRUCTION_FAILED",
        s"U023 ${problem.code}: ${problem.detail}"
      ))
      .map(reconstructed => Result(
        captured,
        changed = true,
        operations.omitted.toVector.sorted,
        operations.methodEdits.toVector.sortBy(_._1).map(_._2),
        operations.generatedAppends,
        finalMembers,
        reconstructed.root,
        reconstructed.template
      ))

  private def validateOperations(
      captured: Capture,
      omittedIndices: Vector[Int],
      methodEdits: Vector[ReconstructedMethod],
      generatedAppends: Vector[PreparedGeneratedMember]
  )(using Context): Either[Error, ValidatedOperations] =
    for
      omitted <- validateOmissions(captured, omittedIndices)
      edits <- validateMethodEdits(captured, methodEdits)
      _ <- edits.keys.find(omitted.contains) match
        case Some(index) => Left(error(
          "OMISSION_METHOD_EDIT_CONFLICT",
          s"captured direct-member index $index cannot be omitted and method-edited in one transaction."
        ))
        case None => Right(())
      generated <- validateGeneratedAppends(captured, generatedAppends)
      finalCount = captured.members.size - omitted.size + generated.size
      _ <- Either.cond(
        finalCount <= ExistingUntpdClassMemberFilter.MaxDirectMembers,
        (),
        error(
          "DIRECT_MEMBER_LIMIT_OVERFLOW",
          s"the final body would contain $finalCount direct members; the bounded limit is ${ExistingUntpdClassMemberFilter.MaxDirectMembers}."
        )
      )
    yield ValidatedOperations(omitted, edits, generated)

  private def validateOmissions(
      captured: Capture,
      indices: Vector[Int]
  ): Either[Error, Set[Int]] =
    firstDuplicate(indices) match
      case Some(index) => Left(error(
        "DUPLICATE_OMITTED_INDEX",
        s"captured direct-member index $index was omitted more than once."
      ))
      case None =>
        indices.find(index => index < 0 || index >= captured.members.size) match
          case Some(index) => Left(error(
            "OMITTED_INDEX_NOT_CAPTURED",
            s"direct-member index $index was not captured from the original Template body."
          ))
          case None => Right(indices.toSet)

  private def validateMethodEdits(
      captured: Capture,
      edits: Vector[ReconstructedMethod]
  )(using Context): Either[Error, Map[Int, ReconstructedMethod]] =
    edits.foldLeft[Either[Error, Map[Int, ReconstructedMethod]]](Right(Map.empty)) {
      case (collected, edit) =>
        for
          accepted <- collected
          value <- Option(edit).toRight(
            error("METHOD_EDIT_REQUIRED", "a U042 reconstructed method was null.")
          )
          descriptor <- Option(value.descriptor).toRight(
            error("INVALID_METHOD_EDIT", "the U042 reconstructed method had no descriptor.")
          )
          _ <- Either.cond(
            descriptor.captured.eq(captured),
            (),
            error("FOREIGN_METHOD_EDIT", "the U042 method edit belongs to a different original capture.")
          )
          original <- captured.members.lift(descriptor.memberIndex).toRight(
            error("INVALID_METHOD_EDIT", "the U042 method edit targets an uncaptured direct-member index.")
          )
          _ <- Either.cond(
            descriptor.method != null && descriptor.method.eq(original.tree),
            (),
            error("INVALID_METHOD_EDIT", "the U042 descriptor method is not the exact captured member at its retained index.")
          )
          _ <- Either.cond(
            value.positionedMethod != null &&
              !rawGraphHasUnavailableField(value.positionedMethod),
            (),
            error("INVALID_METHOD_EDIT", "the U042 positioned method has unavailable raw fields.")
          )
          _ <- ExistingUntpdOrdinaryMethodReconstruction
            .validateReconstructedMethod(value, descriptor)
            .left
            .map(problem => error("INVALID_METHOD_EDIT", problem.message))
          _ <- Either.cond(
            !accepted.contains(descriptor.memberIndex),
            (),
            error(
              "DUPLICATE_METHOD_EDIT",
              s"captured direct-member index ${descriptor.memberIndex} was method-edited more than once."
            )
          )
        yield accepted.updated(descriptor.memberIndex, value)
    }

  private def validateGeneratedAppends(
      captured: Capture,
      generated: Vector[PreparedGeneratedMember]
  )(using Context): Either[Error, Vector[PreparedGeneratedMember]] =
    generated.foldLeft[Either[Error, Vector[PreparedGeneratedMember]]](Right(Vector.empty)) {
      case (collected, preparation) =>
        for
          accepted <- collected
          value <- Option(preparation).toRight(
            error("GENERATED_PREPARATION_REQUIRED", "a prepared generated member was null.")
          )
          _ <- validatePreparedGeneratedMember(value)
          _ <- Either.cond(
            !accepted.exists(_.member.eq(value.member)),
            (),
            error("DUPLICATE_GENERATED_APPEND", "the exact prepared generated member was requested more than once.")
          )
          _ <- Either.cond(
            !captured.members.exists(_.tree.eq(value.member)),
            (),
            error("GENERATED_MEMBER_ALREADY_PRESENT", "the exact prepared generated member is already present in the original capture.")
          )
        yield accepted :+ value
    }

  private[dotty] def validatePreparedGeneratedMember(
      prepared: PreparedGeneratedMember
  )(using Context): Either[Error, Unit] =
    val failure = error(
      "INVALID_GENERATED_PREPARATION",
      "the prepared generated member no longer matches its exact C037 result, generated source, SourceFile, or pre-Typer provenance graph."
    )
    try
      for
        value <- Option(prepared).toRight(failure)
        _ <- Option(value.definition).toRight(failure)
        requested <- Option(value.requestedVirtualSourceName).toRight(failure)
        lowered <- Option(value.lowered).toRight(failure)
        member <- Option(value.member).toRight(failure)
        text <- Option(value.generatedSource).toRight(failure)
        source <- Option(value.sourceFile).toRight(failure)
        evidence <- Option(value.evidence).toRight(failure)
        _ <- Either.cond(
          value.definition.eq(evidence.definition) &&
            requested == evidence.requestedVirtualSourceName &&
            lowered.eq(evidence.lowered) &&
            member.eq(evidence.member) &&
            text == evidence.generatedSource &&
            source.eq(evidence.sourceFile) &&
          lowered.tree != null && lowered.tree.eq(member) &&
            lowered.generatedSource == text &&
            lowered.sourceFile != null && lowered.sourceFile.eq(source) &&
            lowered.virtualSourceName == requested &&
            source.path == requested &&
            source.content.mkString == text &&
            member.source != null && member.source.eq(source) &&
            member.span.exists && member.span.start == 0 && member.span.end == text.length,
          (),
          failure
        )
        _ <- Either.cond(!rawGraphHasUnavailableField(member), (), failure)
        currentGraph = rawTrees(member)
        _ <- Either.cond(generatedGraphMatchesEvidence(currentGraph, evidence.graph), (), failure)
        graph = currentGraph.filterNot(_.isEmpty)
        _ <- Either.cond(
          graph.nonEmpty && graph.forall(tree =>
            tree != null &&
              tree.source != null && tree.source.eq(source) &&
              tree.span.exists &&
              tree.span.start >= 0 &&
              tree.span.start <= tree.span.point &&
              tree.span.point <= tree.span.end &&
              tree.span.end <= text.length &&
              tree.symbol == NoSymbol &&
              !tree.isInstanceOf[untpd.TypedSplice]
          ),
          (),
          failure
        )
      yield ()
    catch
      case NonFatal(_) => Left(failure)

  private def buildFinalMembers(
      captured: Capture,
      operations: ValidatedOperations
  ): Vector[untpd.Tree] =
    val retained = captured.members.flatMap { original =>
      if operations.omitted.contains(original.index) then None
      else Some(operations.methodEdits.get(original.index)
        .fold(original.tree)(_.positionedMethod))
    }
    retained ++ operations.generatedAppends.map(_.member)

  private def validateNoopResult(
      result: Result,
      captured: Capture,
      failure: Error
  )(using Context): Either[Error, Unit] =
    Either.cond(
      !result.changed &&
        result.root != null && result.root.eq(captured.originalRoot) &&
        result.template != null && result.template.eq(captured.originalTemplate) &&
        result.root.rhs.eq(result.template) &&
        identitiesEqual(result.finalMembers, captured.members.map(_.tree)) &&
        identitiesEqual(result.template.body.toVector, result.finalMembers),
      (),
      failure
    )

  private def validateChangedResult(
      result: Result,
      captured: Capture,
      operations: ValidatedOperations,
      failure: Error
  )(using Context): Either[Error, Unit] =
    val root = result.root
    val template = result.template
    val finalMembers = result.finalMembers
    val omittedAbsent = operations.omitted.forall(index =>
      !finalMembers.exists(_.eq(captured.members(index).tree))
    )
    val generatedExactOnce = operations.generatedAppends.forall(value =>
      finalMembers.count(_.eq(value.member)) == 1
    )
    val cleanGraph = !rawGraphHasUnavailableField(root) && rawTrees(root).forall(tree =>
      tree != null && tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
    )
    Either.cond(
      result.changed &&
        root != null && template != null &&
        !root.eq(captured.originalRoot) &&
        !template.eq(captured.originalTemplate) &&
        root.rhs.eq(template) &&
        root.name == captured.originalRoot.name &&
        root.mods.eq(captured.originalRoot.mods) &&
        template.constr.eq(captured.originalTemplate.constr) &&
        template.parentsOrDerived.eq(captured.originalTemplate.parentsOrDerived) &&
        template.derived.eq(captured.originalTemplate.derived) &&
        template.self.eq(captured.originalTemplate.self) &&
        root.source == captured.originalRoot.source &&
        root.span == captured.originalRoot.span &&
        template.source == captured.originalTemplate.source &&
        template.span == captured.originalTemplate.span &&
        identitiesEqual(template.body.toVector, finalMembers) &&
        omittedAbsent && generatedExactOnce && cleanGraph,
      (),
      failure
    )

  private def validateCapture(
      captured: Capture
  )(using Context): Either[Error, Unit] =
    val failure = error(
      "INVALID_CAPTURE",
      "the original U023 capture no longer provides a complete clean pre-Typer graph."
    )
    try
      for
        _ <- Either.cond(!rawGraphHasUnavailableField(captured.originalRoot), (), failure)
        _ <- ExistingUntpdClassMemberFilter
          .validateCaptured(captured)
          .left
          .map(problem => error("INVALID_CAPTURE", "U023 " + problem.code + ": " + problem.detail))
        root = captured.originalRoot
        template = captured.originalTemplate
        body = template.body
        _ <- Either.cond(
          !root.mods.is(Flags.Trait) &&
            root.source != null && root.source.exists && root.span.exists &&
            template.source != null && template.source.exists && template.span.exists &&
            body.size <= ExistingUntpdClassMemberFilter.MaxDirectMembers &&
            body.forall(tree => tree != null && !tree.isEmpty),
          (),
          failure
        )
        graph = rawTrees(captured.originalRoot)
        _ <- Either.cond(
          graph.nonEmpty && graph.forall(tree =>
            tree != null &&
              tree.symbol == NoSymbol &&
              !tree.isInstanceOf[untpd.TypedSplice]
          ),
          (),
          failure
        )
      yield ()
    catch
      case NonFatal(_) => Left(failure)

  private def rawGraphHasUnavailableField(value: Any): Boolean = value match
    case null => true
    case _: dotty.tools.dotc.ast.Trees.Lazy[?] => true
    case tree: untpd.Tree =>
      !tree.isEmpty && rawChildren(tree).exists(rawGraphHasUnavailableField)
    case modifiers: untpd.Modifiers =>
      modifiers.productIterator.exists(rawGraphHasUnavailableField)
    case values: Iterable[?] =>
      values.iterator.exists(rawGraphHasUnavailableField)
    case _ => false

  private def rawTrees(root: untpd.Tree): Vector[untpd.Tree] =
    val builder = Vector.newBuilder[untpd.Tree]
    def visit(value: Any): Unit = value match
      case tree: untpd.Tree =>
        builder += tree
        rawChildren(tree).foreach(visit)
      case modifiers: untpd.Modifiers =>
        modifiers.productIterator.foreach(visit)
      case values: Iterable[?] =>
        values.foreach(visit)
      case _ => ()
    visit(root)
    builder.result()

  // Compiler tree products omit definition/function modifiers.
  private def rawChildren(tree: untpd.Tree): Iterator[Any] =
    val extra = tree match
      case function: untpd.FunctionWithMods => Iterator(function.mods, function.erasedParams)
      case definition: untpd.DefTree => Iterator(definition.mods)
      case _ => Iterator.empty
    tree.productIterator ++ extra

  private def modifiersOf(tree: untpd.Tree): Option[untpd.Modifiers] = tree match
    case function: untpd.FunctionWithMods => Some(function.mods)
    case definition: untpd.DefTree => Some(definition.mods)
    case _ => None

  private def generatedGraphMatchesEvidence(
      graph: Vector[untpd.Tree],
      evidence: Vector[GeneratedNodeEvidence]
  )(using Context): Boolean =
    graph.size == evidence.size && graph.zip(evidence).forall { (tree, snapshot) =>
      val sourceMatches =
        (tree.source == null && snapshot.sourceFile == null) ||
          (tree.source != null && tree.source.eq(snapshot.sourceFile))
      val modifiersMatch = (modifiersOf(tree), snapshot.modifiers) match
        case (Some(current), Some(original)) => current.eq(original)
        case (None, None) => true
        case _ => false
      tree.eq(snapshot.tree) &&
        sourceMatches &&
        tree.span == snapshot.span &&
        tree.symbol == snapshot.symbol &&
        modifiersMatch
    }

  private def identitiesEqual(
      left: Vector[? <: untpd.Tree],
      right: Vector[? <: untpd.Tree]
  ): Boolean =
    left.size == right.size && left.zip(right).forall((first, second) =>
      first != null && second != null && first.eq(second)
    )

  private def firstDuplicate(values: Vector[Int]): Option[Int] =
    val seen = scala.collection.mutable.HashSet.empty[Int]
    values.find(value => !seen.add(value))

  private def error(code: String, detail: String): Error = Error(code, detail)
