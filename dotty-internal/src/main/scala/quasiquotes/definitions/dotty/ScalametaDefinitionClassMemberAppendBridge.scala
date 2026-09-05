package quasiquotes.definitions.dotty

import scala.meta.Defn

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.util.SourceFile

/**
 * Exact-version facade that authors one generated definition and appends that
 * exact member to one admitted existing pre-Typer ordinary class.
 */
object ScalametaDefinitionClassMemberAppendBridge:
  /** Stable stage-oriented diagnostic boundary for hybrid composition. */
  final case class Failure(code: String, detail: String) derives CanEqual

  /** Rebuilt class plus the exact generated member and its generated origin. */
  final class Lowered private[dotty] (
      val tree: untpd.TypeDef,
      val appendedMember: untpd.MemberDef,
      val generatedSource: String,
      val generatedSourceFile: SourceFile
  ):
    def virtualSourceName: String = generatedSourceFile.path

    override def toString: String =
      s"Lowered(source=$virtualSourceName, length=${generatedSource.length})"

  /**
   * Authors one supported Scalameta definition through C020, then delegates
   * exact append and enclosing-shell reconstruction to U025.
   */
  def append(
      existingClass: untpd.Tree,
      definition: Defn,
      virtualSourceName: String
  )(using Context): Either[Failure, Lowered] =
    for
      generated <- ScalametaDefinitionGeneratedOriginBridge
        .lower(definition, virtualSourceName)
        .left
        .map(problem =>
          Failure(
            "GENERATED_DEFINITION_FAILED",
            s"${problem.code}: ${problem.detail}"
          )
        )
      appended <- ExistingUntpdClassMemberAppender
        .append(existingClass, generated.tree)
        .left
        .map(problem =>
          Failure("EXISTING_CLASS_APPEND_FAILED", problem.message)
        )
      _ <- Either.cond(
        appended.appendedMember.eq(generated.tree),
        (),
        Failure(
          "EXISTING_CLASS_APPEND_FAILED",
          "FACADE_INVARIANT_FAILED: U025 did not return the exact C020 generated member."
        )
      )
    yield new Lowered(
      appended.rebuiltRoot,
      appended.appendedMember,
      generated.generatedSource,
      generated.sourceFile
    )
