package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

/** Bounded exact-identity filtering of one existing ordinary class Template body. */
private[quasiquotes] object ExistingUntpdClassMemberFilter:
  private[dotty] val MaxDirectMembers = 64

  enum ProvenanceKind:
    case PreservedOriginalObject
    case ReconstructedAtOriginalSite

  final case class Member private[dotty] (
      index: Int,
      tree: untpd.Tree
  )

  final case class Capture private[dotty] (
      originalRoot: untpd.TypeDef,
      originalTemplate: untpd.Template,
      members: Vector[Member]
  )

  private[dotty] final case class Reconstructed(
      root: untpd.TypeDef,
      template: untpd.Template
  )

  final case class Result private[dotty] (
      captured: Capture,
      retained: Vector[Member],
      rebuiltRoot: untpd.TypeDef,
      rebuiltTemplate: untpd.Template
  ):
    val provenanceKinds: Vector[ProvenanceKind] =
      Vector(
        ProvenanceKind.PreservedOriginalObject,
        ProvenanceKind.ReconstructedAtOriginalSite
      )

  def capture(
      container: untpd.Tree
  )(using Context): Either[ExistingUntpdClassMemberFilterError, Capture] =
    for
      present <- Option(container).toRight(
        error("CONTAINER_REQUIRED", "the existing class container was null.")
      )
      root <- present match
        case value: untpd.TypeDef => Right(value)
        case other =>
          Left(
            error(
              "UNSUPPORTED_OUTER_TOPOLOGY",
              s"the existing container was ${nodeKind(other)}, not an ordinary class TypeDef."
            )
          )
      template <- root.rhs match
        case value: untpd.Template => Right(value)
        case other =>
          Left(
            error(
              "TEMPLATE_REQUIRED",
              s"the existing TypeDef rhs was ${nodeKind(other)}, not Template."
            )
          )
      _ <- Either.cond(
        !root.mods.is(Flags.Trait),
        (),
        error(
          "UNSUPPORTED_OUTER_TOPOLOGY",
          "trait Templates are outside the admitted ordinary-class envelope."
        )
      )
      _ <- Option(template.constr).toRight(
        error(
          "TEMPLATE_CONSTRUCTOR_REQUIRED",
          "the admitted Template constructor was null."
        )
      )
      _ <- Option(template.parentsOrDerived).toRight(
        error(
          "UNSUPPORTED_TEMPLATE_TOPOLOGY",
          "the admitted Template parent sequence was null."
        )
      )
      _ <- Option(template.derived).toRight(
        error(
          "UNSUPPORTED_TEMPLATE_TOPOLOGY",
          "the admitted Template derives sequence was null."
        )
      )
      _ <- Option(template.self).toRight(
        error(
          "UNSUPPORTED_TEMPLATE_TOPOLOGY",
          "the admitted Template self tree was null."
        )
      )
      body <- Option(template.body).toRight(
        error(
          "DIRECT_MEMBER_SEQUENCE_REQUIRED",
          "the admitted Template direct-member sequence was null."
        )
      )
      _ <- Either.cond(
        body.size <= MaxDirectMembers,
        (),
        error(
          "DIRECT_MEMBER_LIMIT_EXCEEDED",
          s"the admitted Template has ${body.size} direct members; the bounded limit is $MaxDirectMembers."
        )
      )
      _ <- body.iterator.zipWithIndex.collectFirst {
        case (member, index) if member == null || member.isEmpty => index
      } match
        case Some(index) =>
          Left(
            error(
              "MALFORMED_DIRECT_MEMBER",
              s"direct member $index was null or EmptyTree."
            )
          )
        case None => Right(())
      _ <- Either.cond(
        root.source.exists && root.span.exists && template.source.exists && template.span.exists,
        (),
        error(
          "CHANGED_SHELL_PROVENANCE_REQUIRED",
          "the original class and Template must each provide a source and span for truthful original-site reconstruction."
        )
      )
      _ <- validatePreTyper(root)
    yield Capture(
      root,
      template,
      body.iterator.zipWithIndex.map((tree, index) => Member(index, tree)).toVector
    )

  def retain(
      captured: Capture,
      retainedIndices: Vector[Int]
  )(using Context): Either[ExistingUntpdClassMemberFilterError, Result] =
    for
      presentCapture <- Option(captured).toRight(
        error("CAPTURE_REQUIRED", "the U023 captured class was null.")
      )
      _ <- validateCaptured(presentCapture)
      indices <- Option(retainedIndices).toRight(
        error("SELECTION_REQUIRED", "the retained-index selection was null.")
      )
      _ <- firstDuplicate(indices) match
        case Some(index) =>
          Left(
            error(
              "DUPLICATE_RETAINED_INDEX",
              s"captured direct-member index $index was requested more than once."
            )
          )
        case None => Right(())
      _ <- Either.cond(
        indices.zip(indices.drop(1)).forall((left, right) => left < right),
        (),
        error(
          "RETAINED_INDEX_ORDER",
          "retained indices must be strictly increasing in original body order."
        )
      )
      _ <- indices.find(index => index < 0 || index >= presentCapture.members.size) match
        case Some(index) =>
          Left(
            error(
              "RETAINED_INDEX_NOT_CAPTURED",
              s"direct-member index $index was not captured from the original Template body."
            )
          )
        case None => Right(())
      retained = indices.map(presentCapture.members)
      result <- rebuild(presentCapture, retained)
    yield result

  private def rebuild(
      captured: Capture,
      retained: Vector[Member]
  )(using Context): Either[ExistingUntpdClassMemberFilterError, Result] =
    val retainedTrees = retained.map(_.tree).toList
    for
      reconstructed <- reconstruct(captured, retainedTrees.toVector)
      result = Result(
        captured,
        retained,
        reconstructed.root,
        reconstructed.template
      )
      _ <- verify(result)
    yield result

  private[dotty] def reconstruct(
      captured: Capture,
      bodyTrees: Vector[untpd.Tree]
  )(using Context): Either[ExistingUntpdClassMemberFilterError, Reconstructed] =
    for
      _ <- validateCaptured(captured)
      body <- Option(bodyTrees).toRight(
        error("CAPTURE_INVARIANT_FAILED", "the replacement body vector was null.")
      )
      _ <- Either.cond(
        body.size <= MaxDirectMembers,
        (),
        error(
          "DIRECT_MEMBER_LIMIT_EXCEEDED",
          s"the reconstructed Template would have ${body.size} direct members; the bounded limit is $MaxDirectMembers."
        )
      )
      _ <- body.iterator.zipWithIndex.collectFirst {
        case (member, index) if member == null || member.isEmpty => index
      } match
        case Some(index) =>
          Left(
            error(
              "MALFORMED_DIRECT_MEMBER",
              s"replacement direct member $index was null or EmptyTree."
            )
          )
        case None => Right(())
      reconstructed <- reconstructUnchecked(captured, body)
    yield reconstructed

  private def reconstructUnchecked(
      captured: Capture,
      bodyTrees: Vector[untpd.Tree]
  )(using Context): Either[ExistingUntpdClassMemberFilterError, Reconstructed] =
    val originalRoot = captured.originalRoot
    val originalTemplate = captured.originalTemplate
    val body = bodyTrees.toList
    given SourceFile = NoSource
    val sourceFreeTemplate = untpd.Template(
      originalTemplate.constr,
      originalTemplate.parentsOrDerived,
      originalTemplate.derived,
      originalTemplate.self,
      body
    )
    val sourceFreeRoot =
      untpd.TypeDef(originalRoot.name, sourceFreeTemplate).withMods(originalRoot.mods)
    val rebuiltTemplate = untpd.cpy.Template(sourceFreeTemplate)(
      sourceFreeTemplate.constr,
      sourceFreeTemplate.parentsOrDerived,
      sourceFreeTemplate.derived,
      sourceFreeTemplate.self,
      body
    ).cloneIn(originalTemplate.source).withSpan(originalTemplate.span)
    val rebuiltRoot = untpd.cpy.TypeDef(sourceFreeRoot)(
      sourceFreeRoot.name,
      rebuiltTemplate
    ).cloneIn(originalRoot.source).withSpan(originalRoot.span)
    val valid =
      !rebuiltRoot.eq(originalRoot) &&
        !rebuiltTemplate.eq(originalTemplate) &&
        rebuiltRoot.mods.eq(originalRoot.mods) &&
        rebuiltTemplate.constr.eq(originalTemplate.constr) &&
        rebuiltTemplate.parentsOrDerived.eq(originalTemplate.parentsOrDerived) &&
        rebuiltTemplate.derived.eq(originalTemplate.derived) &&
        rebuiltTemplate.self.eq(originalTemplate.self) &&
        rebuiltRoot.source == originalRoot.source &&
        rebuiltRoot.span == originalRoot.span &&
        rebuiltTemplate.source == originalTemplate.source &&
        rebuiltTemplate.span == originalTemplate.span &&
        rebuiltTemplate.body.size == body.size &&
        rebuiltTemplate.body.zip(body).forall((rebuilt, supplied) => rebuilt.eq(supplied))
    Either.cond(
      valid,
      Reconstructed(rebuiltRoot, rebuiltTemplate),
      error(
        "INSERTION_READY_RECONSTRUCTION_FAILED",
        "the reconstructed class/Template violated the bounded identity or original-site shell provenance invariant."
      )
    )

  private def verify(
      result: Result
  )(using Context): Either[ExistingUntpdClassMemberFilterError, Unit] =
    val captured = result.captured
    val originalRoot = captured.originalRoot
    val originalTemplate = captured.originalTemplate
    val retainedTrees = result.retained.map(_.tree)
    val rebuiltTrees = result.rebuiltTemplate.body.toVector
    val omitted = captured.members.filterNot(member =>
      result.retained.exists(_.index == member.index)
    )
    val valid =
      !result.rebuiltRoot.eq(originalRoot) &&
        !result.rebuiltTemplate.eq(originalTemplate) &&
        result.rebuiltRoot.mods.eq(originalRoot.mods) &&
        result.rebuiltTemplate.constr.eq(originalTemplate.constr) &&
        result.rebuiltTemplate.parentsOrDerived.eq(originalTemplate.parentsOrDerived) &&
        result.rebuiltTemplate.derived.eq(originalTemplate.derived) &&
        result.rebuiltTemplate.self.eq(originalTemplate.self) &&
        result.rebuiltRoot.source == originalRoot.source &&
        result.rebuiltRoot.span == originalRoot.span &&
        result.rebuiltTemplate.source == originalTemplate.source &&
        result.rebuiltTemplate.span == originalTemplate.span &&
        rebuiltTrees.size == retainedTrees.size &&
        rebuiltTrees.zip(retainedTrees).forall((rebuilt, original) => rebuilt.eq(original)) &&
        omitted.forall(member => !rebuiltTrees.exists(_.eq(member.tree))) &&
        rebuiltTrees.forall(tree => tree != null && !tree.isEmpty) &&
        allTrees(result.rebuiltRoot).forall(tree =>
          tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
        )
    Either.cond(
      valid,
      (),
      error(
        "INSERTION_READY_RECONSTRUCTION_FAILED",
        "the reconstructed class/Template violated the bounded identity, omission, provenance, or pre-Typer invariant."
      )
    )

  private def validatePreTyper(
      root: untpd.TypeDef
  )(using Context): Either[ExistingUntpdClassMemberFilterError, Unit] =
    allTrees(root).find(tree =>
      tree.symbol != NoSymbol || tree.isInstanceOf[untpd.TypedSplice]
    ) match
      case Some(tree) =>
        Left(
          error(
            "PRE_TYPER_CONTAINER_REQUIRED",
            s"the admitted existing graph contains symbol-bearing or typed ${nodeKind(tree)}."
          )
        )
      case None => Right(())

  private[dotty] def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val builder = Vector.newBuilder[untpd.Tree]
    val traverser = new untpd.UntypedTreeTraverser:
      override def traverse(current: untpd.Tree)(using Context): Unit =
        builder += current
        traverseChildren(current)
    traverser.traverse(tree)
    builder.result()

  private[dotty] def validateCaptured(
      captured: Capture
  )(using Context): Either[ExistingUntpdClassMemberFilterError, Unit] =
    val valid = Option(captured).exists { value =>
      Option(value.originalRoot).exists { root =>
        Option(value.originalTemplate).exists { template =>
          Option(value.members).exists { members =>
            Option(template.body).exists { body =>
              root.rhs.eq(template) &&
              members.size == body.size &&
              members.zipWithIndex.forall { case (member, index) =>
                member != null &&
                member.index == index &&
                member.tree != null &&
                member.tree.eq(body(index))
              }
            }
          }
        }
      }
    }
    Either.cond(
      valid,
      (),
      error(
        "CAPTURE_INVARIANT_FAILED",
        "the captured class no longer matches its original TypeDef, Template, ordered indices, and direct-member identities."
      )
    )

  private def firstDuplicate(indices: Vector[Int]): Option[Int] =
    val seen = scala.collection.mutable.HashSet.empty[Int]
    indices.find(index => !seen.add(index))

  private def nodeKind(tree: untpd.Tree): String =
    Option(tree).map(_.getClass.getSimpleName).getOrElse("null")

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdClassMemberFilterError =
    ExistingUntpdClassMemberFilterError(code, detail)
