package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionShape
import _root_.quasiquotes.parser.{BinderId, TermShape}
import _root_.quasiquotes.terms.TermShapeTraversal

import scala.annotation.nowarn
import scala.meta.*

/** Compiler-free projection for one reusable explicitly typed two-parameter method. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaTypedTwoParameterDefProjection:
  private val FirstParameterBinderId = BinderId(0)
  private val SecondParameterBinderId = BinderId(1)

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
      firstParameterSourceName <- topology.firstParameter.name match
        case name: Term.Name => Right(name)
        case _ => Left(nameError("first parameter name"))
      firstParameterName <- ScalametaDefinitionNameProjection
        .project(firstParameterSourceName)
        .left
        .map(_ => nameError("first parameter name"))
      secondParameterSourceName <- topology.secondParameter.name match
        case name: Term.Name => Right(name)
        case _ => Left(nameError("second parameter name"))
      secondParameterName <- ScalametaDefinitionNameProjection
        .project(secondParameterSourceName)
        .left
        .map(_ => nameError("second parameter name"))
      _ <- Either.cond(
        firstParameterName != secondParameterName,
        (),
        nameError("first and second parameter names must be distinct")
      )
      firstParameterTypeShape <- ScalametaTypeNormalFormProjection
        .projectValidatedShape(topology.firstParameterType)
        .left
        .map(_ => typeError("first parameter Type"))
      secondParameterTypeShape <- ScalametaTypeNormalFormProjection
        .projectValidatedShape(topology.secondParameterType)
        .left
        .map(_ => typeError("second parameter Type"))
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
              firstParameterName.decoded,
              FirstParameterBinderId
            ),
            ScalametaTermProjection.DefinitionBinder(
              secondParameterName.decoded,
              SecondParameterBinderId
            )
          )
        )
        .map(_.shape)
        .left
        .map(_ => bodyError)
      shape <- DefinitionShape
        .twoParameterDef(
          methodName,
          FirstParameterBinderId,
          firstParameterName,
          firstParameterTypeShape,
          SecondParameterBinderId,
          secondParameterName,
          secondParameterTypeShape,
          resultTypeShape,
          bodyShape
        )
        .left
        .map(_ =>
          error(
            "NEUTRAL_DEFINITION_CORE_REJECTED",
            "Core DefinitionShape rejected the projected two-parameter method."
          )
        )
      _ <- Either.cond(
        !TermShapeTraversal
          .identifierEntries(shape.body)
          .exists(entry => !entry.isPlaceholder && entry.name == methodName.decoded),
        (),
        error(
          "NEUTRAL_DEFINITION_RECURSION_UNSUPPORTED",
          "the two-parameter method body must not contain an unqualified self-reference."
        )
      )
    yield ProjectedDefinitionShape(shape, truthfulSpan(definition))

  private final case class Topology(
      firstParameter: Term.Param,
      firstParameterType: Type,
      secondParameter: Term.Param,
      secondParameterType: Type,
      resultType: Type
  )

  private def projectTopology(
      definition: Defn.Def
  ): Either[NeutralProjectionError, Topology] =
    for
      _ <- requireTopology(
        definition.mods.isEmpty,
        "the typed two-parameter def must not have modifiers."
      )
      group <- exactlyOne(
        definition.paramClauseGroups,
        "the typed two-parameter def requires one parameter-clause group."
      )
      _ <- requireTopology(
        group.tparamClause.values.isEmpty,
        "the typed two-parameter def must not have Type parameters."
      )
      clause <- exactlyOne(
        group.paramClauses,
        "the typed two-parameter def requires exactly one value-parameter clause."
      )
      _ <- requireTopology(
        clause.mod.isEmpty,
        "the typed two-parameter def requires an ordinary non-contextual parameter clause."
      )
      parameters <- exactlyTwo(
        clause.values,
        "the typed two-parameter def requires exactly two ordinary parameters."
      )
      (firstParameter, secondParameter) = parameters
      _ <- requireTopology(
        firstParameter.mods.isEmpty && firstParameter.default.isEmpty,
        "the first ordinary parameter must not have modifiers or a default value."
      )
      _ <- requireTopology(
        secondParameter.mods.isEmpty && secondParameter.default.isEmpty,
        "the second ordinary parameter must not have modifiers or a default value."
      )
      firstParameterType <- Option(firstParameter.decltpe).flatten
        .toRight(
          topologyError("the first ordinary parameter requires one explicit Type.")
        )
      _ <- requireOrdinaryType(firstParameterType, "first")
      secondParameterType <- Option(secondParameter.decltpe).flatten
        .toRight(
          topologyError("the second ordinary parameter requires one explicit Type.")
        )
      _ <- requireOrdinaryType(secondParameterType, "second")
      resultType <- Option(definition.decltpe).flatten
        .toRight(
          topologyError("the typed two-parameter def requires one explicit result Type.")
        )
    yield Topology(
      firstParameter,
      firstParameterType,
      secondParameter,
      secondParameterType,
      resultType
    )

  private def exactlyOne[A](
      values: List[A],
      detail: String
  ): Either[NeutralProjectionError, A] =
    values match
      case value :: Nil => Right(value)
      case _ => Left(topologyError(detail))

  private def exactlyTwo[A](
      values: List[A],
      detail: String
  ): Either[NeutralProjectionError, (A, A)] =
    values match
      case first :: second :: Nil => Right((first, second))
      case _ => Left(topologyError(detail))

  private def requireOrdinaryType(
      parameterType: Type,
      role: String
  ): Either[NeutralProjectionError, Unit] =
    requireTopology(
      !parameterType.isInstanceOf[Type.ByName] &&
        !parameterType.isInstanceOf[Type.Repeated],
      s"by-name and repeated $role parameter Types are outside the ordinary parameter topology."
    )

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
    error("NEUTRAL_TWO_PARAMETER_DEF_TOPOLOGY_UNSUPPORTED", detail)

  private def error(code: String, detail: String): NeutralProjectionError =
    NeutralProjectionError(code, detail)
