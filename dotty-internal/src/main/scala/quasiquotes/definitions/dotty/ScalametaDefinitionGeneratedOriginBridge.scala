package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.util.SourceFile

import quasiquotes.definitions.{ConstructedDefinition, DefinitionShape}
import quasiquotes.neutral.ScalametaDefinitionProjection

import scala.meta.Defn

/**
 * Exact-version generated-origin bridge for the four bounded concrete val/def
 * families.
 */
object ScalametaDefinitionGeneratedOriginBridge:
  /** Stable diagnostic boundary for projection, completion, and origin work. */
  final case class Failure(code: String, detail: String) derives CanEqual

  /** Positioned insertion-ready member and its deterministic generated origin. */
  final class Lowered private[dotty] (
      val tree: untpd.MemberDef,
      val generatedSource: String,
      val sourceFile: SourceFile
  ):
    def virtualSourceName: String = sourceFile.path

    override def toString: String =
      s"Lowered(source=$virtualSourceName, length=${generatedSource.length})"

  /**
   * Projects and lowers one admitted concrete Scalameta definition. Target
   * admission, placement, rollback, and ordinary typing remain caller-owned.
   */
  def lower(
      definition: Defn,
      virtualSourceName: String
  )(using Context): Either[Failure, Lowered] =
    for
      input <- Option(definition).toRight(
        Failure("MISSING_INPUT", "the Scalameta Defn must be present.")
      )
      projected <- ScalametaDefinitionProjection
        .projectShape(input)
        .left
        .map(problem =>
          Failure(
            "NEUTRAL_PROJECTION_FAILED",
            s"${problem.code}: ${problem.detail}"
          )
        )
      ordinary <- projected.shape match
        case _: DefinitionShape.SimpleTypeAlias =>
          Left(
            Failure(
              "GENERATED_ORIGIN_FAMILY_UNSUPPORTED",
              "DefinitionShape.SimpleTypeAlias has no accepted generic generated-origin authority."
            )
          )
        case value => Right(value)
      completed <- ConstructedDefinition
        .fromShape(ordinary)
        .left
        .map(problem =>
          Failure("DEFINITION_COMPLETION_FAILED", problem.message)
        )
      positioned <- ConstructedDefinitionGeneratedOriginAdapter
        .lower(completed, virtualSourceName)
        .left
        .map(classifyGeneratedOriginFailure)
      result <- positioned.tree match
        case method: untpd.DefDef =>
          Right(
            new Lowered(
              method,
              positioned.generatedSource,
              positioned.sourceFile
            )
          )
        case value: untpd.ValDef =>
          Right(
            new Lowered(
              value,
              positioned.generatedSource,
              positioned.sourceFile
            )
          )
        case other =>
          Left(
            Failure(
              "GENERATED_ORIGIN_FAILED",
              s"generated-origin Definition lowering returned ${treeKind(other)}, not untpd.DefDef or untpd.ValDef."
            )
          )
    yield result

  private def classifyGeneratedOriginFailure(
      problem: ConstructedDefinitionGeneratedOriginError
  ): Failure =
    problem match
      case ConstructedDefinitionGeneratedOriginError.InvalidVirtualSourceName(
            detail
          ) =>
        Failure("INVALID_VIRTUAL_SOURCE", detail)
      case other => Failure("GENERATED_ORIGIN_FAILED", other.message)

  private def treeKind(tree: untpd.Tree | Null): String =
    Option(tree).fold("null")(_.getClass.getName)
