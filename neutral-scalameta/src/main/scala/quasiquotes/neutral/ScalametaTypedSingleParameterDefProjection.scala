package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionShape
import _root_.quasiquotes.parser.{BinderId, TermShape}
import _root_.quasiquotes.terms.TermShapeTraversal

import scala.annotation.nowarn
import scala.meta.*

/** Compiler-free projection for one reusable explicitly typed single-parameter method. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaTypedSingleParameterDefProjection:
  private val ParameterBinderId = BinderId(0)

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
      topology <- projectTopology(definition)
      methodName <- ScalametaDefinitionNameProjection
        .project(definition.name)
        .left
        .map(_ => nameError("method name"))
      parameterSourceName <- topology.parameter.name match
        case name: Term.Name => Right(name)
        case _ => Left(nameError("parameter name"))
      parameterName <- ScalametaDefinitionNameProjection
        .project(parameterSourceName)
        .left
        .map(_ => nameError("parameter name"))
      parameterTypeShape <- ScalametaTypeNormalFormProjection
        .projectValidatedShape(topology.parameterType)
        .left
        .map(_ => typeError("parameter Type"))
      resultTypeShape <- ScalametaTypeNormalFormProjection
        .projectValidatedShape(topology.resultType)
        .left
        .map(_ => typeError("result Type"))
      body <- Option(definition.body)
        .toRight(bodyError)
      bodyShape <- ScalametaTermProjection
        .projectWithDefinitionBinders(
          body,
          Vector(
            ScalametaTermProjection.DefinitionBinder(
              parameterName.decoded,
              ParameterBinderId
            )
          )
        )
        .map(_.shape)
        .left
        .map(_ => bodyError)
      shape <- DefinitionShape
        .singleParameterDef(
          methodName,
          ParameterBinderId,
          parameterName,
          parameterTypeShape,
          resultTypeShape,
          bodyShape
        )
        .left
        .map(_ =>
          error(
            "NEUTRAL_DEFINITION_CORE_REJECTED",
            "Core DefinitionShape rejected the projected single-parameter method."
          )
        )
      _ <- Either.cond(
        !TermShapeTraversal
          .identifierEntries(shape.body)
          .exists(entry => !entry.isPlaceholder && entry.name == methodName.decoded),
        (),
        error(
          "NEUTRAL_DEFINITION_RECURSION_UNSUPPORTED",
          "the single-parameter method body must not contain an unqualified self-reference."
        )
      )
    yield ProjectedDefinitionShape(shape, truthfulSpan(definition))

  private final case class Topology(
      parameter: Term.Param,
      parameterType: Type,
      resultType: Type
  )

  private def projectTopology(
      definition: Defn.Def
  ): Either[NeutralProjectionError, Topology] =
    for
      _ <- requireTopology(
        definition.mods.isEmpty,
        "the typed single-parameter def must not have modifiers."
      )
      group <- exactlyOne(
        definition.paramClauseGroups,
        "the typed single-parameter def requires one parameter-clause group."
      )
      _ <- requireTopology(
        group.tparamClause.values.isEmpty,
        "the typed single-parameter def must not have Type parameters."
      )
      clause <- exactlyOne(
        group.paramClauses,
        "the typed single-parameter def requires exactly one value-parameter clause."
      )
      _ <- requireTopology(
        clause.mod.isEmpty,
        "the typed single-parameter def requires an ordinary non-contextual parameter clause."
      )
      parameter <- exactlyOne(
        clause.values,
        "the typed single-parameter def requires exactly one ordinary parameter."
      )
      _ <- requireTopology(
        parameter.mods.isEmpty && parameter.default.isEmpty,
        "the ordinary parameter must not have modifiers or a default value."
      )
      parameterType <- Option(parameter.decltpe).flatten
        .toRight(
          topologyError("the ordinary parameter requires one explicit Type.")
        )
      _ <- requireTopology(
        !parameterType.isInstanceOf[Type.ByName] &&
          !parameterType.isInstanceOf[Type.Repeated],
        "by-name and repeated parameter Types are outside the ordinary parameter topology."
      )
      resultType <- Option(definition.decltpe).flatten
        .toRight(
          topologyError("the typed single-parameter def requires one explicit result Type.")
        )
    yield Topology(parameter, parameterType, resultType)

  private def exactlyOne[A](
      values: List[A],
      detail: String
  ): Either[NeutralProjectionError, A] =
    values match
      case value :: Nil => Right(value)
      case _ => Left(topologyError(detail))

  private def requireTopology(
      condition: Boolean,
      detail: String
  ): Either[NeutralProjectionError, Unit] =
    Either.cond(condition, (), topologyError(detail))

  private def nameError(role: String): NeutralProjectionError =
    error(
      "NEUTRAL_DEFINITION_NAME_UNSUPPORTED",
      s"the $role must satisfy the existing shared Core source-spelling policy."
    )

  private def typeError(role: String): NeutralProjectionError =
    error(
      "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED",
      s"the $role is outside the existing N002/Core normal-form family."
    )

  private def bodyError: NeutralProjectionError =
    error(
      "NEUTRAL_DEFINITION_BODY_UNSUPPORTED",
      "the body is outside the current binder-aware Scalameta Term projection family."
    )

  private def truthfulSpan(tree: Tree): Option[NeutralSourceSpan] =
    tree.pos match
      case Position.None => None
      case position => Some(NeutralSourceSpan(position.start, position.end))

  private def topologyError(detail: String): NeutralProjectionError =
    error("NEUTRAL_SINGLE_PARAMETER_DEF_TOPOLOGY_UNSUPPORTED", detail)

  private def error(code: String, detail: String): NeutralProjectionError =
    NeutralProjectionError(code, detail)
