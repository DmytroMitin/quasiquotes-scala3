package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.Defn

/** Internal dispatch boundary over the accepted reusable Definition families. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaDefinitionProjection:
  def project(
      definition: Defn
  ): Either[NeutralProjectionError, ProjectedDefinitionShape] =
    Option(definition)
      .toRight(
        NeutralProjectionError(
          "NEUTRAL_DEFINITION_MISSING",
          "the Scalameta Defn must be present."
        )
      )
      .flatMap {
        case value: Defn.Val =>
          ScalametaTypedImmutableValProjection.project(value)
        case method: Defn.Def =>
          projectMethod(method)
        case alias: Defn.Type =>
          ScalametaSimpleTypeAliasProjection.project(alias)
        case _ => Left(familyUnsupported)
      }

  private def projectMethod(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedDefinitionShape] =
    definition.paramClauseGroups match
      case Nil =>
        ScalametaTypedParameterlessDefProjection.project(definition)
      case group :: Nil =>
        group.paramClauses match
          case clause :: Nil =>
            clause.values.size match
              case 1 => ScalametaTypedSingleParameterDefProjection.project(definition)
              case 2 => ScalametaTypedTwoParameterDefProjection.project(definition)
              case _ => Left(familyUnsupported)
          case _ => Left(familyUnsupported)
      case _ => Left(familyUnsupported)

  private def familyUnsupported: NeutralProjectionError =
    NeutralProjectionError(
      "NEUTRAL_DEFINITION_FAMILY_UNSUPPORTED",
      "the Scalameta definition is outside the accepted reusable Definition projection families."
    )
