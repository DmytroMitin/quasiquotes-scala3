package quasiquotes.neutral

import _root_.quasiquotes.definitions.*
import _root_.quasiquotes.parser.{BinderId, TermShape, TypeShape}
import _root_.quasiquotes.types.TypeNormalForm

import scala.annotation.nowarn
import scala.meta.{Defn, Position, Tree}
import scala.util.control.NonFatal

/** Public authoring facade over the accepted reusable Definition families. */
@nowarn("cat=deprecation")
object ScalametaDefinitionAuthoring:
  /** Stable bounded failure for public semantic Definition authoring. */
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def author(
      definition: SemanticDefinition
  ): Either[Error, Defn] =
    if definition == null then Left(missing)
    else
      for
        _ <- guard(semanticUnsupported)(requireEmptyModifiers(definition))
        expectedSurface <- guard(semanticUnsupported)(declarationSurface(definition))
        input <- guard(semanticUnsupported)(classify(definition))
        shape <- adapt(input)
        authored <- guard(privateAdapterFailed)(
          authorShape(shape).left.map(_ => privateAdapterFailed)
        )
        _ <- guard(roundTripFailed)(
          validatePublicRoundTrip(definition, authored, expectedSurface)
        )
      yield authored

  /** Private N032 compatibility dispatcher retained for five-family consumers. */
  private[quasiquotes] sealed trait ShapeError derives CanEqual

  private[quasiquotes] object ShapeError:
    case object Missing extends ShapeError
    final case class SimpleTypeAlias(
        problem: ScalametaSimpleTypeAliasAuthoring.Error
    ) extends ShapeError
    final case class ImmutableVal(
        problem: ScalametaTypedImmutableValAuthoring.Error
    ) extends ShapeError
    final case class ParameterlessDef(
        problem: ScalametaTypedParameterlessDefAuthoring.Error
    ) extends ShapeError
    final case class SingleParameterDef(
        problem: ScalametaTypedSingleParameterDefAuthoring.Error
    ) extends ShapeError
    final case class TwoParameterDef(
        problem: ScalametaTypedTwoParameterDefAuthoring.Error
    ) extends ShapeError

  private[quasiquotes] def authorShape(
      shape: DefinitionShape
  ): Either[ShapeError, Defn] =
    Option(shape)
      .toRight(ShapeError.Missing)
      .flatMap {
        case alias: DefinitionShape.SimpleTypeAlias =>
          ScalametaSimpleTypeAliasAuthoring
            .author(alias)
            .left
            .map(ShapeError.SimpleTypeAlias.apply)
        case value: DefinitionShape.ImmutableVal =>
          ScalametaTypedImmutableValAuthoring
            .author(value)
            .left
            .map(ShapeError.ImmutableVal.apply)
        case method: DefinitionShape.ParameterlessDef =>
          ScalametaTypedParameterlessDefAuthoring
            .author(method)
            .left
            .map(ShapeError.ParameterlessDef.apply)
        case method: DefinitionShape.SingleParameterDef =>
          ScalametaTypedSingleParameterDefAuthoring
            .author(method)
            .left
            .map(ShapeError.SingleParameterDef.apply)
        case method: DefinitionShape.TwoParameterDef =>
          ScalametaTypedTwoParameterDefAuthoring
            .author(method)
            .left
            .map(ShapeError.TwoParameterDef.apply)
      }

  private sealed trait SemanticInput
  private final case class ValueInput(
      name: DefinitionName,
      declaredType: TypeNormalForm,
      body: TermShape
  ) extends SemanticInput
  private final case class ParameterlessInput(
      name: DefinitionName,
      resultType: TypeNormalForm,
      body: TermShape
  ) extends SemanticInput
  private final case class SingleParameterInput(
      name: DefinitionName,
      method: MethodDefinitionView,
      parameter: DefinitionParameter,
      resultType: TypeNormalForm,
      body: TermShape
  ) extends SemanticInput
  private final case class TwoParameterInput(
      name: DefinitionName,
      method: MethodDefinitionView,
      first: DefinitionParameter,
      second: DefinitionParameter,
      resultType: TypeNormalForm,
      body: TermShape
  ) extends SemanticInput
  private final case class TypeAliasInput(
      name: DefinitionName,
      aliasedType: TypeNormalForm
  ) extends SemanticInput

  private def classify(
      definition: SemanticDefinition
  ): Either[Error, SemanticInput] =
    for
      name <- Option(definition.name).toRight(semanticUnsupported)
      input <- definition.kind match
        case DefinitionKind.Value =>
          definition.asValue match
            case Some(value) if definition.asMethod.isEmpty && definition.asType.isEmpty =>
              for
                declaredType <- Option(value.declaredType).toRight(semanticUnsupported)
                body <- value.body.toRight(semanticUnsupported)
              yield ValueInput(name, declaredType, body)
            case _ => Left(semanticUnsupported)
        case DefinitionKind.Method =>
          definition.asMethod match
            case Some(method) if definition.asValue.isEmpty && definition.asType.isEmpty =>
              classifyMethod(name, method)
            case _ => Left(semanticUnsupported)
        case DefinitionKind.TypeMember =>
          definition.asType match
            case Some(alias) if definition.asValue.isEmpty && definition.asMethod.isEmpty =>
              alias.aliasedType
                .flatMap(Option(_))
                .map(TypeAliasInput(name, _))
                .toRight(semanticUnsupported)
            case _ => Left(semanticUnsupported)
        case _ => Left(semanticUnsupported)
    yield input

  private def classifyMethod(
      name: DefinitionName,
      method: MethodDefinitionView
  ): Either[Error, SemanticInput] =
    for
      clauses <- Option(method.parameterClauses).toRight(semanticUnsupported)
      resultType <- Option(method.resultType).toRight(semanticUnsupported)
      body <- method.body.toRight(semanticUnsupported)
      input <- clauses match
        case Vector() => Right(ParameterlessInput(name, resultType, body))
        case Vector(clause)
            if clause != null &&
              clause.kind == DefinitionParameterClauseKind.Ordinary &&
              clause.parameters != null &&
              clause.parameters.size == 1 &&
              clause.parameters.head != null =>
          Right(
            SingleParameterInput(
              name,
              method,
              clause.parameters.head,
              resultType,
              body
            )
          )
        case Vector(clause)
            if clause != null &&
              clause.kind == DefinitionParameterClauseKind.Ordinary &&
              clause.parameters != null &&
              clause.parameters.size == 2 &&
              clause.parameters.forall(_ != null) =>
          Right(
            TwoParameterInput(
              name,
              method,
              clause.parameters.head,
              clause.parameters(1),
              resultType,
              body
            )
          )
        case _ => Left(semanticUnsupported)
    yield input

  private def adapt(input: SemanticInput): Either[Error, DefinitionShape] =
    input match
      case ValueInput(name, declaredType, body) =>
        for
          privateType <- privateTypeShape(declaredType)
          shape <- guard(privateAdapterFailed)(
            DefinitionShape
              .immutableVal(name, privateType, body)
              .left
              .map(_ => privateAdapterFailed)
          )
        yield shape
      case ParameterlessInput(name, resultType, body) =>
        for
          privateResult <- privateTypeShape(resultType)
          shape <- guard(privateAdapterFailed)(
            DefinitionShape
              .parameterlessDef(name, privateResult, body)
              .left
              .map(_ => privateAdapterFailed)
          )
        yield shape
      case SingleParameterInput(name, method, parameter, resultType, body) =>
        for
          binderId <- parameterBinderId(method, 0)
          parameterType <- privateTypeShape(parameter.declaredType)
          privateResult <- privateTypeShape(resultType)
          shape <- guard(privateAdapterFailed)(
            DefinitionShape
              .singleParameterDef(
                name,
                binderId,
                parameter.name,
                parameterType,
                privateResult,
                body
              )
              .left
              .map(_ => privateAdapterFailed)
          )
        yield shape
      case TwoParameterInput(name, method, first, second, resultType, body) =>
        for
          firstBinderId <- parameterBinderId(method, 0)
          secondBinderId <- parameterBinderId(method, 1)
          _ <- Either.cond(firstBinderId != secondBinderId, (), privateAdapterFailed)
          firstType <- privateTypeShape(first.declaredType)
          secondType <- privateTypeShape(second.declaredType)
          privateResult <- privateTypeShape(resultType)
          shape <- guard(privateAdapterFailed)(
            DefinitionShape
              .twoParameterDef(
                name,
                firstBinderId,
                first.name,
                firstType,
                secondBinderId,
                second.name,
                secondType,
                privateResult,
                body
              )
              .left
              .map(_ => privateAdapterFailed)
          )
        yield shape
      case TypeAliasInput(name, aliasedType) =>
        for
          rhs <- privateTypeShape(aliasedType)
          shape <- guard(privateAdapterFailed)(
            DefinitionShape
              .simpleTypeAlias(name, rhs)
              .left
              .map(_ => privateAdapterFailed)
          )
        yield shape

  private def parameterBinderId(
      method: MethodDefinitionView,
      parameterIndex: Int
  ): Either[Error, BinderId] =
    guard(privateAdapterFailed) {
      method.parameterScope.reference(0, parameterIndex)
        .left
        .map(_ => privateAdapterFailed)
        .flatMap {
          case TermShape.BoundReference(id, _) => Right(id)
          case _ => Left(privateAdapterFailed)
        }
    }

  private def privateTypeShape(
      normalForm: TypeNormalForm
  ): Either[Error, TypeShape] =
    guard(typeAdapterFailed) {
      ScalametaTypeNormalFormAuthoring
        .author(normalForm)
        .left
        .map(_ => typeAdapterFailed)
        .flatMap { authored =>
          if allTrees(authored).forall(_.pos == Position.None) then
            ScalametaTypeNormalFormProjection
              .projectValidatedShape(authored)
              .left
              .map(_ => typeAdapterFailed)
          else Left(typeAdapterFailed)
        }
    }

  private[neutral] def validatePublicRoundTrip(
      expected: SemanticDefinition,
      authored: Defn
  ): Either[Error, Unit] =
    guard(roundTripFailed) {
      declarationSurface(expected)
        .left
        .map(_ => roundTripFailed)
        .flatMap { expectedSurface =>
          validatePublicRoundTrip(expected, authored, expectedSurface)
        }
    }

  private def validatePublicRoundTrip(
      expected: SemanticDefinition,
      authored: Defn,
      expectedSurface: DeclarationSurface
  ): Either[Error, Unit] =
    if authored == null || !allTrees(authored).forall(_.pos == Position.None) then
      Left(roundTripFailed)
    else
      ScalametaDefinitionProjection.project(authored) match
        case Right(ProjectedDefinition(projected, None)) =>
          declarationSurface(projected).flatMap { projectedSurface =>
            Either.cond(
              projected == expected && projectedSurface == expectedSurface,
              (),
              roundTripFailed
            )
          }.left.map(_ => roundTripFailed)
        case _ => Left(roundTripFailed)

  private sealed trait FamilySurface derives CanEqual
  private final case class ValueSurface(declaredType: TypeNormalForm)
      extends FamilySurface
  private final case class MethodSurface(
      clauses: Vector[ClauseSurface],
      resultType: TypeNormalForm
  ) extends FamilySurface
  private final case class TypeSurface(aliasedType: TypeNormalForm)
      extends FamilySurface
  private final case class ClauseSurface(
      kind: String,
      parameters: Vector[ParameterSurface]
  ) derives CanEqual
  private final case class ParameterSurface(
      sourceName: String,
      declaredType: TypeNormalForm
  ) derives CanEqual
  private final case class DeclarationSurface(
      kind: String,
      sourceName: String,
      modifiers: DefinitionModifiers,
      family: FamilySurface
  ) derives CanEqual

  private def declarationSurface(
      definition: SemanticDefinition
  ): Either[Error, DeclarationSurface] =
    if definition == null then Left(missing)
    else
      for
        kind <- Option(definition.kind).toRight(semanticUnsupported)
        name <- Option(definition.name).toRight(semanticUnsupported)
        modifiers <- Option(definition.modifiers).toRight(semanticUnsupported)
        family <- kind match
          case DefinitionKind.Value =>
            definition.asValue match
              case Some(value) if definition.asMethod.isEmpty && definition.asType.isEmpty =>
                Option(value.declaredType)
                  .map(ValueSurface.apply)
                  .toRight(semanticUnsupported)
              case _ => Left(semanticUnsupported)
          case DefinitionKind.Method =>
            definition.asMethod match
              case Some(method) if definition.asValue.isEmpty && definition.asType.isEmpty =>
                for
                  clauses <- clauseSurfaces(method.parameterClauses)
                  resultType <- Option(method.resultType).toRight(semanticUnsupported)
                yield MethodSurface(clauses, resultType)
              case _ => Left(semanticUnsupported)
          case DefinitionKind.TypeMember =>
            definition.asType match
              case Some(alias) if definition.asValue.isEmpty && definition.asMethod.isEmpty =>
                alias.aliasedType
                  .flatMap(Option(_))
                  .map(TypeSurface.apply)
                  .toRight(semanticUnsupported)
              case _ => Left(semanticUnsupported)
          case _ => Left(semanticUnsupported)
      yield DeclarationSurface(kind.code, name.source, modifiers, family)

  private def clauseSurfaces(
      clauses: Vector[DefinitionParameterClause]
  ): Either[Error, Vector[ClauseSurface]] =
    Option(clauses)
      .toRight(semanticUnsupported)
      .flatMap(
        _.foldLeft(Right(Vector.empty): Either[Error, Vector[ClauseSurface]]) {
          (collected, clause) =>
            for
              previous <- collected
              presentClause <- Option(clause).toRight(semanticUnsupported)
              kind <- Option(presentClause.kind).toRight(semanticUnsupported)
              parameters <- Option(presentClause.parameters).toRight(semanticUnsupported)
              current <- parameters.foldLeft(
                Right(Vector.empty): Either[Error, Vector[ParameterSurface]]
              ) { (parameterResult, parameter) =>
                for
                  previousParameters <- parameterResult
                  presentParameter <- Option(parameter).toRight(semanticUnsupported)
                  name <- Option(presentParameter.name).toRight(semanticUnsupported)
                  declaredType <- Option(presentParameter.declaredType)
                    .toRight(semanticUnsupported)
                yield previousParameters :+ ParameterSurface(name.source, declaredType)
              }
            yield previous :+ ClauseSurface(kind.code, current)
        }
      )

  private def requireEmptyModifiers(
      definition: SemanticDefinition
  ): Either[Error, Unit] =
    Either.cond(
      definition.modifiers == DefinitionModifiers.empty,
      (),
      semanticUnsupported
    )

  private def guard[A](
      failure: => Error
  )(operation: => Either[Error, A]): Either[Error, A] =
    try operation
    catch case NonFatal(_) => Left(failure)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)

  private def missing: Error =
    Error(
      "NEUTRAL_DEFINITION_AUTHORING_MISSING",
      "the public SemanticDefinition must be present."
    )

  private def semanticUnsupported: Error =
    Error(
      "NEUTRAL_DEFINITION_AUTHORING_SEMANTIC_UNSUPPORTED",
      "the public SemanticDefinition is outside the supported five-family authoring surface."
    )

  private def typeAdapterFailed: Error =
    Error(
      "NEUTRAL_DEFINITION_AUTHORING_TYPE_ADAPTER_FAILED",
      "a public Type normal form could not cross the accepted neutral Type adapter."
    )

  private def privateAdapterFailed: Error =
    Error(
      "NEUTRAL_DEFINITION_AUTHORING_PRIVATE_ADAPTER_FAILED",
      "the public SemanticDefinition could not cross the accepted private Definition authoring boundary."
    )

  private def roundTripFailed: Error =
    Error(
      "NEUTRAL_DEFINITION_AUTHORING_ROUNDTRIP_FAILED",
      "the authored Definition did not preserve the public semantic and declaration surfaces."
    )
