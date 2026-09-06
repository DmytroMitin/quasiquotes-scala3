package quasiquotes.neutral

import _root_.quasiquotes.definitions.*
import _root_.quasiquotes.parser.{BinderId, BlockStatement, TermShape}
import _root_.quasiquotes.types.TypeNormalForm

import scala.annotation.nowarn
import scala.meta.Defn
import scala.util.control.NonFatal

/** Public projection facade over the accepted reusable Definition families. */
@nowarn("cat=deprecation")
object ScalametaDefinitionProjection:
  def project(
      definition: Defn
  ): Either[NeutralProjectionError, ProjectedDefinition] =
    projectShape(definition).flatMap(adapt)

  /** Compatibility boundary retained for private five-family consumers. */
  private[quasiquotes] def projectShape(
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

  private def adapt(
      projected: ProjectedDefinitionShape
  ): Either[NeutralProjectionError, ProjectedDefinition] =
    try
      adaptShape(projected.shape)
        .map(ProjectedDefinition(_, projected.sourceSpan))
        .left
        .map(_ => semanticAdapterFailure)
    catch case NonFatal(_) => Left(semanticAdapterFailure)

  private def adaptShape(
      shape: DefinitionShape
  ): Either[DefinitionSemanticError, SemanticDefinition] =
    shape match
      case value: DefinitionShape.ImmutableVal =>
        for
          declaredType <- normalForm(value.declaredType)
          definition <- SemanticDefinition.immutableValue(
            value.name,
            declaredType,
            value.rhs,
            DefinitionModifiers.empty
          )
        yield definition
      case method: DefinitionShape.ParameterlessDef =>
        for
          resultType <- normalForm(method.resultType)
          definition <- SemanticDefinition.concreteMethod(
            method.name,
            Vector.empty,
            resultType,
            DefinitionModifiers.empty
          )(_ => Right(method.body))
        yield definition
      case method: DefinitionShape.SingleParameterDef =>
        for
          parameterType <- normalForm(method.parameterType)
          resultType <- normalForm(method.resultType)
          clause <- ordinaryClause(
            Vector(DefinitionParameter(method.parameterName, parameterType))
          )
          definition <- SemanticDefinition.concreteMethod(
            method.name,
            Vector(clause),
            resultType,
            DefinitionModifiers.empty
          ) { scope =>
            scope.reference(0, 0).map { reference =>
              substituteDefinitionParameters(
                method.body,
                Map(method.parameterBinderId -> reference)
              )
            }
          }
        yield definition
      case method: DefinitionShape.TwoParameterDef =>
        for
          firstParameterType <- normalForm(method.firstParameterType)
          secondParameterType <- normalForm(method.secondParameterType)
          resultType <- normalForm(method.resultType)
          clause <- ordinaryClause(
            Vector(
              DefinitionParameter(method.firstParameterName, firstParameterType),
              DefinitionParameter(method.secondParameterName, secondParameterType)
            )
          )
          definition <- SemanticDefinition.concreteMethod(
            method.name,
            Vector(clause),
            resultType,
            DefinitionModifiers.empty
          ) { scope =>
            for
              first <- scope.reference(0, 0)
              second <- scope.reference(0, 1)
            yield substituteDefinitionParameters(
              method.body,
              Map(
                method.firstParameterBinderId -> first,
                method.secondParameterBinderId -> second
              )
            )
          }
        yield definition
      case alias: DefinitionShape.SimpleTypeAlias =>
        normalForm(alias.rhs).flatMap { rhs =>
          SemanticDefinition.typeAlias(
            alias.name,
            rhs,
            DefinitionModifiers.empty
          )
        }

  private def normalForm(
      shape: _root_.quasiquotes.parser.TypeShape
  ): Either[DefinitionSemanticError, TypeNormalForm] =
    TypeNormalForm.fromShape(shape).left.map(_ => semanticFailure)

  private def ordinaryClause(
      parameters: Vector[DefinitionParameter]
  ): Either[DefinitionSemanticError, DefinitionParameterClause] =
    DefinitionParameterClause.ordinary(parameters)

  private[neutral] def substituteDefinitionParameters(
      shape: TermShape,
      replacements: Map[BinderId, TermShape]
  ): TermShape =
    val reserved = binderIds(shape) ++ replacements.values.flatMap(binderIds)
    substituteDefinitionParameters(shape, replacements, FreshBinderIds(reserved))

  private def substituteDefinitionParameters(
      shape: TermShape,
      replacements: Map[BinderId, TermShape],
      freshIds: FreshBinderIds
  ): TermShape =
    shape match
      case bound @ TermShape.BoundReference(binderId, _) =>
        replacements.getOrElse(binderId, bound)
      case TermShape.Lambda1(binderId, displayName, parameterType, body) =>
        val freshBinderId = freshIds.allocate()
        TermShape.Lambda1(
          freshBinderId,
          displayName,
          parameterType,
          substituteDefinitionParameters(
            body,
            replacements.updated(
              binderId,
              TermShape.BoundReference(freshBinderId, displayName)
            ),
            freshIds
          )
        )
      case identifier: TermShape.Identifier => identifier
      case literal: TermShape.Literal => literal
      case TermShape.Select(qualifier, name) =>
        TermShape.Select(
          substituteDefinitionParameters(qualifier, replacements, freshIds),
          name
        )
      case TermShape.Apply(function, arguments) =>
        TermShape.Apply(
          substituteDefinitionParameters(function, replacements, freshIds),
          arguments.map(substituteDefinitionParameters(_, replacements, freshIds))
        )
      case TermShape.New(constructor, arguments) =>
        TermShape.New(
          constructor,
          arguments.map(substituteDefinitionParameters(_, replacements, freshIds))
        )
      case TermShape.Infix(left, operator, right) =>
        TermShape.Infix(
          substituteDefinitionParameters(left, replacements, freshIds),
          operator,
          substituteDefinitionParameters(right, replacements, freshIds)
        )
      case TermShape.Unary(operator, operand) =>
        TermShape.Unary(
          operator,
          substituteDefinitionParameters(operand, replacements, freshIds)
        )
      case TermShape.InterpolatedString(prefix, parts, arguments) =>
        TermShape.InterpolatedString(
          prefix,
          parts,
          arguments.map(substituteDefinitionParameters(_, replacements, freshIds))
        )
      case TermShape.Typed(expression, typeName) =>
        TermShape.Typed(
          substituteDefinitionParameters(expression, replacements, freshIds),
          typeName
        )
      case TermShape.Tuple(elements) =>
        TermShape.Tuple(
          elements.map(substituteDefinitionParameters(_, replacements, freshIds))
        )
      case TermShape.If(condition, thenBranch, elseBranch) =>
        TermShape.If(
          substituteDefinitionParameters(condition, replacements, freshIds),
          substituteDefinitionParameters(thenBranch, replacements, freshIds),
          substituteDefinitionParameters(elseBranch, replacements, freshIds)
        )
      case TermShape.Block(statements, result) =>
        val (adaptedStatements, resultReplacements) = statements.foldLeft(
          (List.empty[BlockStatement], replacements)
        ) { case ((collected, current), statement) =>
          statement match
            case local: BlockStatement.LocalVal =>
              val freshBinderId = freshIds.allocate()
              (
                collected :+ BlockStatement.LocalVal(
                  freshBinderId,
                  local.displayName,
                  local.declaredType,
                  substituteDefinitionParameters(
                    local.initializer,
                    current,
                    freshIds
                  )
                ),
                current.updated(
                  local.binderId,
                  TermShape.BoundReference(freshBinderId, local.displayName)
                )
              )
            case local: BlockStatement.LocalDef =>
              val freshMethodBinderId = freshIds.allocate()
              val freshParameterBinderId = freshIds.allocate()
              (
                collected :+ BlockStatement.LocalDef(
                  freshMethodBinderId,
                  local.methodDisplayName,
                  freshParameterBinderId,
                  local.parameterDisplayName,
                  local.parameterType,
                  local.resultType,
                  substituteDefinitionParameters(
                    local.body,
                    (current - local.methodBinderId).updated(
                      local.parameterBinderId,
                      TermShape.BoundReference(
                        freshParameterBinderId,
                        local.parameterDisplayName
                      )
                    ),
                    freshIds
                  )
                ),
                current.updated(
                  local.methodBinderId,
                  TermShape.BoundReference(
                    freshMethodBinderId,
                    local.methodDisplayName
                  )
                )
              )
            case term: TermShape =>
              (
                collected :+ substituteDefinitionParameters(
                  term,
                  current,
                  freshIds
                ),
                current
              )
        }
        TermShape.Block(
          adaptedStatements,
          substituteDefinitionParameters(result, resultReplacements, freshIds)
        )
      case TermShape.Parenthesized(expression) =>
        TermShape.Parenthesized(
          substituteDefinitionParameters(expression, replacements, freshIds)
        )
      case unsupported: TermShape.Unsupported => unsupported

  private def binderIds(shape: TermShape): Set[BinderId] =
    shape match
      case TermShape.BoundReference(binderId, _) => Set(binderId)
      case TermShape.Lambda1(binderId, _, _, body) => binderIds(body) + binderId
      case TermShape.Select(qualifier, _) => binderIds(qualifier)
      case TermShape.Apply(function, arguments) =>
        binderIds(function) ++ arguments.flatMap(binderIds)
      case TermShape.New(_, arguments) => arguments.flatMap(binderIds).toSet
      case TermShape.Infix(left, _, right) => binderIds(left) ++ binderIds(right)
      case TermShape.Unary(_, operand) => binderIds(operand)
      case TermShape.InterpolatedString(_, _, arguments) =>
        arguments.flatMap(binderIds).toSet
      case TermShape.Typed(expression, _) => binderIds(expression)
      case TermShape.Tuple(elements) => elements.flatMap(binderIds).toSet
      case TermShape.If(condition, thenBranch, elseBranch) =>
        binderIds(condition) ++ binderIds(thenBranch) ++ binderIds(elseBranch)
      case TermShape.Block(statements, result) =>
        statements.foldLeft(binderIds(result)) {
          case (ids, local: BlockStatement.LocalVal) =>
            ids ++ binderIds(local.initializer) + local.binderId
          case (ids, local: BlockStatement.LocalDef) =>
            ids ++ binderIds(local.body) + local.methodBinderId + local.parameterBinderId
          case (ids, term: TermShape) => ids ++ binderIds(term)
        }
      case TermShape.Parenthesized(expression) => binderIds(expression)
      case _: TermShape.Identifier | _: TermShape.Literal | _: TermShape.Unsupported =>
        Set.empty

  private final class FreshBinderIds private (initial: Set[BinderId]):
    private var reserved = initial.map(_.value)
    private var candidate = 0

    def allocate(): BinderId =
      while reserved.contains(candidate) do candidate += 1
      val result = BinderId(candidate)
      reserved += candidate
      candidate += 1
      result

  private object FreshBinderIds:
    def apply(initial: Set[BinderId]): FreshBinderIds = new FreshBinderIds(initial)

  private def semanticFailure: DefinitionSemanticError =
    DefinitionSemanticError(
      "DEFINITION_SEMANTIC_INVALID_BODY",
      "the private Definition shape is outside the accepted public semantic model."
    )

  private def semanticAdapterFailure: NeutralProjectionError =
    NeutralProjectionError(
      "NEUTRAL_DEFINITION_SEMANTIC_ADAPTER_FAILED",
      "the accepted private Definition projection could not be represented by the public semantic model."
    )

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
