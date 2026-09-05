package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol

/** Appends one exact caller-positioned member to an admitted existing class. */
private[quasiquotes] object ExistingUntpdClassMemberAppender:
  final case class Result private[dotty] (
      captured: ExistingUntpdClassMemberFilter.Capture,
      appendedMember: untpd.MemberDef,
      rebuiltRoot: untpd.TypeDef,
      rebuiltTemplate: untpd.Template
  )

  def append(
      container: untpd.Tree,
      appended: untpd.Tree
  )(using Context): Either[ExistingUntpdClassMemberAppendError, Result] =
    for
      captured <- ExistingUntpdClassMemberFilter
        .capture(container)
        .left
        .map(classifyCaptureFailure)
      result <- appendCapturedMember(captured, appended)
    yield result

  private[dotty] def appendCapturedMember(
      captured: ExistingUntpdClassMemberFilter.Capture,
      appended: untpd.Tree
  )(using Context): Either[ExistingUntpdClassMemberAppendError, Result] =
    for
      presentCapture <- Option(captured).toRight(
        error(
          "MISSING_CAPTURE_OR_CONTAINER",
          "the captured existing class must be present."
        )
      )
      _ <- ExistingUntpdClassMemberFilter
        .validateCaptured(presentCapture)
        .left
        .map(_ =>
          error(
            "RECONSTRUCTION_INVARIANT_FAILURE",
            "the captured existing class no longer matches the U023 capture invariant."
          )
        )
      present <- Option(appended)
        .filterNot(_.isEmpty)
        .toRight(
          error(
            "MISSING_APPENDED_MEMBER",
            "the caller-supplied appended member was null or EmptyTree."
          )
        )
      member <- present match
        case value: untpd.DefDef => Right(value)
        case value: untpd.ValDef => Right(value)
        case other =>
          Left(
            error(
              "UNSUPPORTED_APPENDED_MEMBER_ROLE",
              s"the caller-supplied tree was ${treeKind(other)}, not untpd.DefDef or untpd.ValDef."
            )
          )
      _ <- Either.cond(
        presentCapture.members.size < ExistingUntpdClassMemberFilter.MaxDirectMembers,
        (),
        error(
          "DIRECT_MEMBER_LIMIT_OVERFLOW",
          s"append-one would exceed the bounded ${ExistingUntpdClassMemberFilter.MaxDirectMembers}-member Template body."
        )
      )
      _ <- Either.cond(
        !presentCapture.members.exists(_.tree.eq(member)),
        (),
        error(
          "APPENDED_MEMBER_ALREADY_PRESENT",
          "the exact caller-supplied member is already present in the captured body."
        )
      )
      _ <- validateGeneratedMember(member)
      body = presentCapture.members.map(_.tree) :+ member
      reconstructed <- ExistingUntpdClassMemberFilter
        .reconstruct(presentCapture, body)
        .left
        .map(problem =>
          error(
            "RECONSTRUCTION_INVARIANT_FAILURE",
            problem.message
          )
        )
      result = Result(
        presentCapture,
        member,
        reconstructed.root,
        reconstructed.template
      )
      _ <- verify(result)
    yield result

  private def validateGeneratedMember(
      member: untpd.MemberDef
  )(using Context): Either[ExistingUntpdClassMemberAppendError, Unit] =
    val graph = ExistingUntpdClassMemberFilter.allTrees(member)
    graph.find(_.isInstanceOf[untpd.TypedSplice]) match
      case Some(_) =>
        Left(
          error(
            "APPENDED_MEMBER_TYPED_SPLICE",
            "the caller-supplied member graph contains TypedSplice and is not a pure pre-Typer graph."
          )
        )
      case None =>
        graph.find(_.symbol != NoSymbol) match
          case Some(tree) =>
            Left(
              error(
                "APPENDED_MEMBER_SYMBOL_BEARING",
                s"the caller-supplied member contains symbol-bearing ${treeKind(tree)} and would require owner/symbol or post-Typer repair."
              )
            )
          case None => validateGeneratedProvenance(member, graph)

  private def validateGeneratedProvenance(
      member: untpd.MemberDef,
      graph: Vector[untpd.Tree]
  ): Either[ExistingUntpdClassMemberAppendError, Unit] =
    val materialGraph = graph.filterNot(_.isEmpty)
    val valid =
      member.source.exists &&
        member.source.path.trim.nonEmpty &&
        member.source.content.nonEmpty &&
        member.span.exists &&
        member.span.start == 0 &&
        member.span.end == member.source.content.length &&
        materialGraph.forall { tree =>
          tree.source.exists &&
          tree.source == member.source &&
          tree.span.exists &&
          tree.span.start >= 0 &&
          tree.span.start <= tree.span.point &&
          tree.span.point <= tree.span.end &&
          tree.span.end <= member.source.content.length
        }
    Either.cond(
      valid,
      (),
      error(
        "APPENDED_MEMBER_PROVENANCE_FAILURE",
        "the caller-supplied member must have one nonempty generated SourceFile and a recursively contained, whole-root span graph."
      )
    )

  private def verify(
      result: Result
  )(using Context): Either[ExistingUntpdClassMemberAppendError, Unit] =
    val oldMembers = result.captured.members.map(_.tree)
    val rebuiltMembers = result.rebuiltTemplate.body.toVector
    val valid =
      rebuiltMembers.size == oldMembers.size + 1 &&
        rebuiltMembers.take(oldMembers.size).zip(oldMembers).forall {
          case (rebuilt, original) => rebuilt.eq(original)
        } &&
        rebuiltMembers.lastOption.exists(_.eq(result.appendedMember)) &&
        rebuiltMembers.count(_.eq(result.appendedMember)) == 1 &&
        !result.rebuiltRoot.eq(result.captured.originalRoot) &&
        !result.rebuiltTemplate.eq(result.captured.originalTemplate)
    Either.cond(
      valid,
      (),
      error(
        "RECONSTRUCTION_INVARIANT_FAILURE",
        "append-one violated exact old-member order/identity, exact appended-member identity, or fresh-shell identity."
      )
    )

  private def classifyCaptureFailure(
      problem: ExistingUntpdClassMemberFilterError
  ): ExistingUntpdClassMemberAppendError =
    problem.code match
      case "CONTAINER_REQUIRED" =>
        error("MISSING_CAPTURE_OR_CONTAINER", problem.detail)
      case "UNSUPPORTED_OUTER_TOPOLOGY" =>
        error("UNSUPPORTED_OUTER_TOPOLOGY", problem.detail)
      case "TEMPLATE_REQUIRED" | "TEMPLATE_CONSTRUCTOR_REQUIRED" |
          "UNSUPPORTED_TEMPLATE_TOPOLOGY" | "CHANGED_SHELL_PROVENANCE_REQUIRED" =>
        error("UNSUPPORTED_TEMPLATE_TOPOLOGY", problem.message)
      case "DIRECT_MEMBER_SEQUENCE_REQUIRED" | "MALFORMED_DIRECT_MEMBER" =>
        error("MALFORMED_EXISTING_BODY", problem.message)
      case "DIRECT_MEMBER_LIMIT_EXCEEDED" =>
        error("DIRECT_MEMBER_LIMIT_OVERFLOW", problem.detail)
      case "PRE_TYPER_CONTAINER_REQUIRED" =>
        error("OPERATION_REQUIRES_OWNER_OR_POST_TYPER_REPAIR", problem.message)
      case _ =>
        error("RECONSTRUCTION_INVARIANT_FAILURE", problem.message)

  private def treeKind(tree: untpd.Tree): String =
    Option(tree).fold("null")(_.getClass.getSimpleName)

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdClassMemberAppendError =
    ExistingUntpdClassMemberAppendError(code, detail)
