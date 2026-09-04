package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionShape

import scala.annotation.nowarn
import scala.meta.*

/** Compiler-free projection for one reusable non-generic simple type alias. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaSimpleTypeAliasProjection:
  def project(
      definition: Defn.Type
  ): Either[NeutralProjectionError, ProjectedDefinitionShape] =
    Option(definition)
      .toRight(
        error(
          "NEUTRAL_DEFINITION_MISSING",
          "the Scalameta Defn.Type must be present."
        )
      )
      .flatMap(projectPresent)

  private def projectPresent(
      definition: Defn.Type
  ): Either[NeutralProjectionError, ProjectedDefinitionShape] =
    for
      rhs <- projectTopology(definition)
      name <- ScalametaDefinitionNameProjection
        .project(definition.name)
        .left
        .map(_ =>
          error(
            "NEUTRAL_DEFINITION_NAME_UNSUPPORTED",
            "the alias name must satisfy the existing shared Core source-spelling policy."
          )
        )
      rhsShape <- ScalametaTypeNormalFormProjection
        .projectValidatedShape(rhs)
        .left
        .map(_ =>
          error(
            "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED",
            "the alias right-hand side is outside the existing N002/Core normal-form family."
          )
        )
      shape <- DefinitionShape
        .simpleTypeAlias(name, rhsShape)
        .left
        .map(_ =>
          error(
            "NEUTRAL_DEFINITION_CORE_REJECTED",
            "Core DefinitionShape rejected the projected simple type alias."
          )
        )
    yield ProjectedDefinitionShape(shape, truthfulSpan(definition))

  private def projectTopology(
      definition: Defn.Type
  ): Either[NeutralProjectionError, Type] =
    for
      _ <- requireTopology(
        definition.mods.isEmpty,
        "the simple type alias must not have modifiers or opaque semantics."
      )
      _ <- requireTopology(
        definition.tparamClause.values.isEmpty,
        "the simple type alias must not have Type parameters."
      )
      _ <- requireTopology(
        definition.bounds.lo.isEmpty &&
          definition.bounds.hi.isEmpty &&
          definition.bounds.context.isEmpty &&
          definition.bounds.view.isEmpty,
        "the simple type alias must not have lower, upper, context, or view bounds."
      )
      rhs <- Option(definition.body)
        .toRight(topologyError("the simple type alias requires one right-hand side Type."))
    yield rhs

  private def requireTopology(
      condition: Boolean,
      detail: String
  ): Either[NeutralProjectionError, Unit] =
    Either.cond(condition, (), topologyError(detail))

  private def truthfulSpan(tree: Tree): Option[NeutralSourceSpan] =
    tree.pos match
      case Position.None => None
      case position => Some(NeutralSourceSpan(position.start, position.end))

  private def topologyError(detail: String): NeutralProjectionError =
    error("NEUTRAL_SIMPLE_TYPE_ALIAS_TOPOLOGY_UNSUPPORTED", detail)

  private def error(code: String, detail: String): NeutralProjectionError =
    NeutralProjectionError(code, detail)
