package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionShape
import _root_.quasiquotes.terms.TermShapeTraversal

import scala.annotation.nowarn
import scala.meta.*

/** Compiler-free projection for one reusable explicitly typed parameterless method. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaTypedParameterlessDefProjection:
  def project(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedDefinitionShape] =
    Option(definition)
      .toRight(
        error(
          "NEUTRAL_DEFINITION_MISSING",
          "the Scalameta Defn.Def must be present."
        )
      )
      .flatMap(projectPresent)

  private def projectPresent(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedDefinitionShape] =
    for
      _ <- require(
        definition.mods.isEmpty,
        "the typed parameterless def must not have modifiers."
      )
      _ <- require(
        definition.paramClauseGroups.isEmpty,
        "the typed parameterless def must use true parameterless source syntax."
      )
      resultType <- Option(definition.decltpe).flatten
        .toRight(
          topologyError(
            "the typed parameterless def requires one explicit result Type."
          )
        )
      name <- ScalametaDefinitionNameProjection.project(definition.name)
      resultTypeShape <- ScalametaTypeNormalFormProjection
        .projectValidatedShape(resultType)
        .left
        .map(_ =>
          error(
            "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED",
            "the result Type is outside the existing N002/Core normal-form family."
          )
        )
      body <- Option(definition.body)
        .toRight(
          error(
            "NEUTRAL_DEFINITION_BODY_UNSUPPORTED",
            "the parameterless method body must be present."
          )
        )
      bodyShape <- ScalametaTermProjection
        .project(body)
        .map(_.shape)
        .left
        .map(_ =>
          error(
            "NEUTRAL_DEFINITION_BODY_UNSUPPORTED",
            "the body is outside the current Scalameta Term projection family."
          )
        )
      shape <- DefinitionShape
        .parameterlessDef(name, resultTypeShape, bodyShape)
        .left
        .map(_ =>
          error(
            "NEUTRAL_DEFINITION_CORE_REJECTED",
            "Core DefinitionShape rejected the projected parameterless method."
          )
        )
      _ <- Either.cond(
        !TermShapeTraversal
          .identifierEntries(shape.body)
          .exists(entry => !entry.isPlaceholder && entry.name == name.decoded),
        (),
        error(
          "NEUTRAL_DEFINITION_RECURSION_UNSUPPORTED",
          "the parameterless method body must not contain an unqualified self-reference."
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

  private def topologyError(detail: String): NeutralProjectionError =
    error("NEUTRAL_PARAMETERLESS_DEF_TOPOLOGY_UNSUPPORTED", detail)

  private def error(code: String, detail: String): NeutralProjectionError =
    NeutralProjectionError(code, detail)
