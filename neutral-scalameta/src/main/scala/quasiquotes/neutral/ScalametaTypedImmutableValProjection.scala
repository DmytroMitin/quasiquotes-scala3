package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionShape

import scala.annotation.nowarn
import scala.meta.*

private[quasiquotes] final case class ProjectedDefinitionShape(
    shape: DefinitionShape,
    sourceSpan: Option[NeutralSourceSpan]
) derives CanEqual

/** Compiler-free projection for one reusable explicitly typed immutable value. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaTypedImmutableValProjection:
  def project(
      definition: Defn.Val
  ): Either[NeutralProjectionError, ProjectedDefinitionShape] =
    Option(definition)
      .toRight(
        error(
          "NEUTRAL_DEFINITION_MISSING",
          "the Scalameta Defn.Val must be present."
        )
      )
      .flatMap(projectPresent)

  private def projectPresent(
      definition: Defn.Val
  ): Either[NeutralProjectionError, ProjectedDefinitionShape] =
    for
      _ <- require(
        definition.mods.isEmpty,
        "the typed immutable val must not have modifiers."
      )
      sourceName <- definition.pats match
        case Pat.Var(name) :: Nil => Right(name)
        case _ => topologyFailure("the typed immutable val requires exactly one Pat.Var name.")
      declaredType <- Option(definition.decltpe).flatten
        .toRight(
          error(
            "NEUTRAL_TYPED_VAL_TOPOLOGY_UNSUPPORTED",
            "the typed immutable val requires one explicit declared Type."
          )
        )
      name <- ScalametaDefinitionNameProjection.project(sourceName)
      typeShape <- ScalametaTypeNormalFormProjection
        .projectValidatedShape(declaredType)
        .left
        .map(_ =>
          error(
            "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED",
            "the declared Type is outside the existing N002/Core normal-form family."
          )
        )
      rhs <- Option(definition.rhs)
        .toRight(
          error(
            "NEUTRAL_DEFINITION_BODY_UNSUPPORTED",
            "the immutable value right-hand side must be present."
          )
        )
      rhsShape <- ScalametaTermProjection
        .project(rhs)
        .map(_.shape)
        .left
        .map(_ =>
          error(
            "NEUTRAL_DEFINITION_BODY_UNSUPPORTED",
            "the right-hand side is outside the current Scalameta Term projection family."
          )
        )
      shape <- DefinitionShape
        .immutableVal(name, typeShape, rhsShape)
        .left
        .map(_ =>
          error(
            "NEUTRAL_DEFINITION_CORE_REJECTED",
            "Core DefinitionShape rejected the projected immutable value."
          )
        )
    yield ProjectedDefinitionShape(shape, truthfulSpan(definition))

  private def truthfulSpan(tree: Tree): Option[NeutralSourceSpan] =
    tree.pos match
      case Position.None => None
      case position => Some(NeutralSourceSpan(position.start, position.end))

  private def require(
      condition: Boolean,
      detail: String
  ): Either[NeutralProjectionError, Unit] =
    Either.cond(condition, (), topologyError(detail))

  private def topologyFailure[A](
      detail: String
  ): Either[NeutralProjectionError, A] =
    Left(topologyError(detail))

  private def topologyError(detail: String): NeutralProjectionError =
    error("NEUTRAL_TYPED_VAL_TOPOLOGY_UNSUPPORTED", detail)

  private def error(code: String, detail: String): NeutralProjectionError =
    NeutralProjectionError(code, detail)
