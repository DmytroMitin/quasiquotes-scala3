package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionName
import _root_.quasiquotes.parser.{BinderId, BlockStatement, ConstructorNamePolicy, TermShape, TypeShape}
import _root_.quasiquotes.terms.TermShapeTraversal

import scala.annotation.nowarn
import scala.meta.*
import scala.util.control.NonFatal

/** Direct structural authoring for the bounded generic TermShape family, with an externally seeded Definition-binder seam. */
@nowarn("cat=deprecation")
object ScalametaTermShapeAuthoring:
  /** Stable bounded failure for TermShape-to-Scalameta Term authoring. */
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  /** A definition binder available while authoring one definition body. */
  private[quasiquotes] final case class DefinitionBinder(
      binderId: BinderId,
      name: DefinitionName
  )

  private final case class AuthoredDefinitionBinder(
      binderId: BinderId,
      sourceName: String
  )

  private final case class AuthoredLambdaBinder(
      binderId: BinderId,
      sourceName: String
  )

  private final case class AuthoredP2LocalValBinder(
      binderId: BinderId,
      sourceName: String
  )

  private final case class AuthoringScope(
      definitionBinders: Vector[AuthoredDefinitionBinder],
      definitionBinderAware: Boolean,
      lambdaBinder: Option[AuthoredLambdaBinder],
      p2LocalValBinder: Option[AuthoredP2LocalValBinder]
  ):
    def sourceNameFor(binderId: BinderId): Option[String] =
      p2LocalValBinder
        .filter(_.binderId == binderId)
        .map(_.sourceName)
        .orElse(
          lambdaBinder
            .filter(_.binderId == binderId)
            .map(_.sourceName)
        )
        .orElse(definitionBinders.find(_.binderId == binderId).map(_.sourceName))

  private val SupportedUnaryOperators = Set("+", "-", "!", "~")
  private val BinderFreeScope =
    AuthoringScope(
      Vector.empty,
      definitionBinderAware = false,
      lambdaBinder = None,
      p2LocalValBinder = None
    )

  def author(shape: TermShape): Either[Error, Term] =
    Option(shape)
      .toRight(error("NEUTRAL_TERM_AUTHORING_MISSING", "the TermShape must be present."))
      .flatMap(authorPresent(_, BinderFreeScope))
      .flatMap(candidate => validateRoundTrip(shape, candidate))

  private[quasiquotes] def authorWithDefinitionBinders(
      shape: TermShape,
      binders: Vector[DefinitionBinder]
  ): Either[Error, Term] =
    Option(shape)
      .toRight(error("NEUTRAL_TERM_AUTHORING_MISSING", "the TermShape must be present."))
      .flatMap(present =>
        validateDefinitionBinders(binders).flatMap { authoredBinders =>
          val scope = AuthoringScope(
            authoredBinders,
            definitionBinderAware = true,
            lambdaBinder = None,
            p2LocalValBinder = None
          )
          authorPresent(present, scope)
            .flatMap(candidate => validateDefinitionRoundTrip(present, candidate, authoredBinders))
        }
      )

  private def authorPresent(shape: TermShape, scope: AuthoringScope): Either[Error, Term] =
    Option(shape)
      .toRight(structureError("the admitted TermShape contains a missing recursive child."))
      .flatMap {
        case TermShape.Literal(value) =>
          authorLiteral(value)
        case TermShape.Identifier(_, true) =>
          Left(structureError("placeholder identifiers are outside binder-free N013 authoring."))
        case TermShape.Identifier(name, false) =>
          requirePresent(name, "identifier names must be present.")
            .flatMap(value => construct("identifier")(Term.Name(value)))
        case TermShape.BoundReference(binderId, _) if scope.p2LocalValBinder.nonEmpty =>
          Option(binderId)
            .flatMap(scope.sourceNameFor)
            .toRight(
              p2ScopeUnsupported(
                "bound references inside P2 must resolve to an active local, Lambda, or Definition binder."
              )
            )
            .flatMap(value => construct("P2-scoped bound reference")(Term.Name(value)))
        case TermShape.BoundReference(binderId, _) if scope.lambdaBinder.nonEmpty =>
          Option(binderId)
            .flatMap(scope.sourceNameFor)
            .toRight(
              lambdaScopeUnsupported(
                "bound references inside Lambda1 must resolve to an active Lambda or Definition binder."
              )
            )
            .flatMap(value => construct("lambda-scoped bound reference")(Term.Name(value)))
        case TermShape.BoundReference(binderId, _) if scope.definitionBinderAware =>
          Option(binderId)
            .flatMap(scope.sourceNameFor)
            .toRight(
              structureError(
                "bound references must resolve to a supplied definition binder."
              )
            )
            .flatMap(value => construct("definition-bound reference")(Term.Name(value)))
        case TermShape.Select(qualifier, name) =>
          for
            selectedName <- requirePresent(name, "selected names must be present.")
            authoredQualifier <- authorPresent(qualifier, scope)
            authored <- construct("selection")(
              Term.Select(authoredQualifier, Term.Name(selectedName))
            )
          yield authored
        case TermShape.Apply(function: TermShape.Apply, _) =>
          Left(
            structureError(
              "an Apply directly in function position would advertise multiple argument lists."
            )
          )
        case TermShape.Apply(function, arguments) =>
          for
            authoredFunction <- authorPresent(function, scope)
            authoredArguments <- traverse(arguments)(authorPresent(_, scope))
            authored <- construct("ordinary Apply")(
              Term.Apply(authoredFunction, Term.ArgClause(authoredArguments))
            )
          yield authored
        case TermShape.New(constructor, arguments) =>
          for
            validatedConstructor <- ConstructorNamePolicy
              .validate(constructor)
              .left
              .map(structureError)
            authoredArguments <- traverse(arguments)(authorPresent(_, scope))
            authored <- construct("constructor-new term") {
              val segments = validatedConstructor.split("\\.", -1).toList
              val qualifier = segments.init.tail.foldLeft[Term.Ref](Term.Name(segments.head)) {
                case (current, segment) => Term.Select(current, Term.Name(segment))
              }
              val constructorType = Type.Select(qualifier, Type.Name(segments.last))
              Term.New(
                Init(
                  constructorType,
                  Name.Anonymous(),
                  List(Term.ArgClause(authoredArguments))
                )
              )
            }
          yield authored
        case TermShape.Infix(left, operator, right) =>
          for
            authoredOperator <- requirePresent(operator, "infix operators must be present.")
            authoredLeft <- authorPresent(left, scope)
            authoredRight <- authorPresent(right, scope)
            authored <- construct("binary infix term")(
              Term.ApplyInfix(
                authoredLeft,
                Term.Name(authoredOperator),
                Type.ArgClause(Nil),
                Term.ArgClause(List(authoredRight))
              )
            )
          yield authored
        case TermShape.Unary(operator, operand) =>
          for
            _ <- require(
              Option(operator).exists(SupportedUnaryOperators),
              "unary terms support exactly +, -, !, and ~."
            )
            authoredOperand <- authorPresent(operand, scope)
            authored <- construct("unary term")(
              Term.ApplyUnary(Term.Name(operator), authoredOperand)
            )
          yield authored
        case TermShape.Tuple(elements) =>
          for
            presentElements <- Option(elements)
              .toRight(structureError("tuple element lists must be present."))
            _ <- require(
              presentElements.size >= 2 && presentElements.size <= 22,
              s"tuple terms require arity 2 through 22, found ${presentElements.size}."
            )
            authoredElements <- traverse(presentElements)(authorPresent(_, scope))
            authored <- construct("tuple term")(Term.Tuple(authoredElements))
          yield authored
        case TermShape.If(condition, thenBranch, elseBranch) =>
          for
            authoredCondition <- authorPresent(condition, scope)
            authoredThen <- authorPresent(thenBranch, scope)
            authoredElse <- authorPresent(elseBranch, scope)
            authored <- construct("explicit three-branch if term")(
              Term.If(authoredCondition, authoredThen, authoredElse)
            )
          yield authored
        case TermShape.InterpolatedString(prefix, parts, arguments) =>
          for
            _ <- require(
              Option(prefix).contains("s"),
              "standard interpolation authoring admits exactly the s prefix."
            )
            presentParts <- Option(parts)
              .toRight(structureError("interpolation part lists must be present."))
            presentArguments <- Option(arguments)
              .toRight(structureError("interpolation argument lists must be present."))
            _ <- require(
              presentParts.size == presentArguments.size + 1,
              "interpolation authoring requires one more part than argument."
            )
            encodedParts <- traverse(presentParts)(encodeStandardInterpolationPart)
            authoredArguments <- traverse(presentArguments)(authorInterpolationArgument(_, scope))
            authored <- construct("standard s interpolation")(
              Term.Interpolate(
                Term.Name("s"),
                encodedParts.map(Lit.String(_)),
                authoredArguments
              )
            )
          yield authored
        case TermShape.Typed(expression, typeName) =>
          for
            authoredExpression <- authorPresent(expression, scope)
            authoredType <- authorPrimitiveAscriptionType(typeName)
            authored <- construct("typed/ascribed term")(
              Term.Ascribe(authoredExpression, authoredType)
            )
          yield authored
        case TermShape.Lambda1(binderId, displayName, parameterType, body) =>
          authorLambda1(binderId, displayName, parameterType, body, scope)
        case TermShape.Block(
              (local: BlockStatement.LocalDef) :: Nil,
              result
            ) =>
          authorP3LocalIdentityDef(local, result, scope)
        case TermShape.Block(
              (local: BlockStatement.LocalVal) :: Nil,
              result
            ) =>
          authorP2LocalVal(local, result, scope)
        case TermShape.Block(statements, result) =>
          for
            authoredStatements <- traverse(statements)(authorBlockStatement(_, scope))
            authoredResult <- authorPresent(result, scope)
            authored <- construct("binder-free P1 block")(
              Term.Block(authoredStatements :+ authoredResult)
            )
          yield authored
        case _ =>
          Left(
            error(
              "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED",
              "this TermShape family is outside binder-free N013-N015/N019 authoring."
            )
          )
      }

  private def authorLambda1(
      binderId: BinderId,
      displayName: String,
      parameterType: String,
      body: TermShape,
      scope: AuthoringScope
  ): Either[Error, Term.Function] =
    if scope.lambdaBinder.nonEmpty then
      Left(
        error(
          "NEUTRAL_TERM_AUTHORING_LAMBDA_NESTED_UNSUPPORTED",
          "nested Lambda1 terms are outside bounded Scalameta authoring."
        )
      )
    else
      for
        presentBinderId <- Option(binderId).toRight(
          lambdaScopeUnsupported("Lambda1 binder ids must be present.")
        )
        _ <- Either.cond(
          !scope.definitionBinders.exists(_.binderId == presentBinderId),
          (),
          lambdaScopeUnsupported(
            "a Lambda1 binder id must not collide with an active Definition binder id."
          )
        )
        presentName <- requirePresent(displayName, "Lambda1 parameter names must be present.")
        authoredType <- authorLambdaParameterType(parameterType)
        parameterName <- construct("Lambda1 parameter name")(Term.Name(presentName))
        authoredBody <- authorPresent(
          body,
          scope.copy(lambdaBinder = Some(AuthoredLambdaBinder(presentBinderId, presentName)))
        )
        authored <- construct("ordinary one-parameter Lambda1")(
          Term.Function(
            Term.ParamClause(
              List(Term.Param(Nil, parameterName, Some(authoredType), None))
            ),
            authoredBody
          )
        )
      yield authored

  private def authorP3LocalIdentityDef(
      local: BlockStatement.LocalDef,
      result: TermShape,
      scope: AuthoringScope
  ): Either[Error, Term.Block] =
    for
      methodBinderId <- Option(local.methodBinderId).toRight(
        p3ScopeUnsupported("P3 local method binder ids must be present.")
      )
      parameterBinderId <- Option(local.parameterBinderId).toRight(
        p3ScopeUnsupported("P3 local parameter binder ids must be present.")
      )
      _ <- Either.cond(
        methodBinderId != parameterBinderId,
        (),
        p3ScopeUnsupported("P3 method and parameter binder ids must be distinct.")
      )
      _ <- Either.cond(
        !scope.definitionBinders.exists(binder =>
          binder.binderId == methodBinderId || binder.binderId == parameterBinderId
        ) &&
          !scope.lambdaBinder.exists(binder =>
            binder.binderId == methodBinderId || binder.binderId == parameterBinderId
          ) &&
          !scope.p2LocalValBinder.exists(binder =>
            binder.binderId == methodBinderId || binder.binderId == parameterBinderId
          ),
        (),
        p3ScopeUnsupported(
          "P3 method and parameter binder ids must not collide with active Definition, Lambda, or P2 binders."
        )
      )
      _ <- Either.cond(
        scope.lambdaBinder.isEmpty && scope.p2LocalValBinder.isEmpty,
        (),
        p3NestedUnsupported
      )
      _ <- Either.cond(
        scope.definitionBinders.forall(_.binderId.value <= Int.MaxValue - 2),
        (),
        p3ScopeUnsupported(
          "P3 authoring requires capacity for two fresh projector binder ids after the active Definition scope."
        )
      )
      methodName <- requirePresent(
        local.methodDisplayName,
        "P3 local method names must be present."
      )
      parameterName <- requirePresent(
        local.parameterDisplayName,
        "P3 local parameter names must be present."
      )
      typeName <- authorP3IdentityType(local.parameterType, local.resultType)
      _ <- local.body match
        case TermShape.BoundReference(binderId, _) if binderId == parameterBinderId => Right(())
        case _ => Left(p3BodyUnsupported)
      _ <- result match
        case TermShape.BoundReference(binderId, _) if binderId == methodBinderId => Right(())
        case _ => Left(p3ResultUnsupported)
      authoredMethodName <- construct("P3 local method declaration name")(Term.Name(methodName))
      authoredParameterName <- construct("P3 local parameter declaration name")(Term.Name(parameterName))
      authoredParameterType <- construct("P3 local parameter type")(Type.Name(typeName))
      authoredResultType <- construct("P3 local result type")(Type.Name(typeName))
      authoredBody <- construct("P3 local method body reference")(Term.Name(parameterName))
      authoredResult <- construct("P3 local method result reference")(Term.Name(methodName))
      authoredDefinition <- construct("P3 local identity method")(
        Defn.Def(
          Nil,
          authoredMethodName,
          List(
            Member.ParamClauseGroup(
              Type.ParamClause(Nil),
              List(
                Term.ParamClause(
                  List(
                    Term.Param(
                      Nil,
                      authoredParameterName,
                      Some(authoredParameterType),
                      None
                    )
                  )
                )
              )
            )
          ),
          Some(authoredResultType),
          authoredBody
        )
      )
      authored <- construct("P3 local identity-method block")(
        Term.Block(List(authoredDefinition, authoredResult))
      )
    yield authored

  private def authorP2LocalVal(
      local: BlockStatement.LocalVal,
      result: TermShape,
      scope: AuthoringScope
  ): Either[Error, Term.Block] =
    if scope.p2LocalValBinder.nonEmpty then
      Left(
        error(
          "NEUTRAL_TERM_AUTHORING_P2_NESTED_UNSUPPORTED",
          "a nested P2 local immutable val is outside bounded Scalameta authoring."
        )
      )
    else
      for
        binderId <- Option(local.binderId).toRight(
          p2ScopeUnsupported("P2 local binder ids must be present.")
        )
        _ <- Either.cond(
          !scope.lambdaBinder.exists(_.binderId == binderId) &&
            !scope.definitionBinders.exists(_.binderId == binderId),
          (),
          p2ScopeUnsupported(
            "a P2 local binder id must not collide with an active Lambda or Definition binder id."
          )
        )
        displayName <- requirePresent(
          local.displayName,
          "P2 local binder names must be present."
        )
        declaredType <- authorP2DeclaredType(local.declaredType)
        authoredName <- construct("P2 local binder name")(Term.Name(displayName))
        authoredInitializer <- authorPresent(local.initializer, scope)
        authoredResult <- authorPresent(
          result,
          scope.copy(
            p2LocalValBinder = Some(AuthoredP2LocalValBinder(binderId, displayName))
          )
        )
        authoredDefinition <- construct("P2 local immutable val")(
          Defn.Val(
            Nil,
            List(Pat.Var(authoredName)),
            Some(declaredType),
            authoredInitializer
          )
        )
        authored <- construct("P2 local immutable-val block")(
          Term.Block(List(authoredDefinition, authoredResult))
        )
      yield authored

  private def authorP2DeclaredType(typeName: String): Either[Error, Type.Name] =
    typeName match
      case "Int" | "String" | "Boolean" | "AnyVal" => Right(Type.Name(typeName))
      case _ => Left(p2DeclaredTypeUnsupported)

  private def authorP3IdentityType(
      parameterType: TypeShape,
      resultType: TypeShape
  ): Either[Error, String] =
    (parameterType, resultType) match
      case (TypeShape.Identifier(parameter), TypeShape.Identifier(result))
          if parameter == result &&
            (parameter == "Int" || parameter == "String" || parameter == "Boolean") =>
        Right(parameter)
      case _ => Left(p3TypeUnsupported)

  private def authorLambdaParameterType(typeName: String): Either[Error, Type.Name] =
    typeName match
      case "Int" | "String" | "Boolean" => Right(Type.Name(typeName))
      case _ => Left(lambdaParameterTypeUnsupported)

  private def authorPrimitiveAscriptionType(typeName: String): Either[Error, Type.Name] =
    typeName match
      case "Int" | "String" | "Boolean" => Right(Type.Name(typeName))
      case _ => Left(typedTypeUnsupported)

  private def authorBlockStatement(
      statement: BlockStatement,
      scope: AuthoringScope
  ): Either[Error, Term] =
    Option(statement)
      .toRight(structureError("binder-free P1 block prefixes must be present."))
      .flatMap {
        case term: TermShape => authorPresent(term, scope)
        case _: BlockStatement.LocalVal | _: BlockStatement.LocalDef =>
          Left(
            error(
              "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED",
              "binder-free P1 authoring does not admit local definitions or binders."
            )
          )
      }

  private def authorInterpolationArgument(
      shape: TermShape,
      scope: AuthoringScope
  ): Either[Error, Term] =
    authorPresent(shape, scope).flatMap {
      case block: Term.Block =>
        construct("standard s interpolation block argument wrapper")(
          Term.Block(List(block))
        )
      case other => Right(other)
    }

  private def authorLiteral(value: String): Either[Error, Term] =
    Option(value)
      .toRight(
        error(
          "NEUTRAL_TERM_AUTHORING_LITERAL_UNSUPPORTED",
          "literal semantic values must be present."
        )
      )
      .flatMap { present =>
        if present == "true" then Right(Lit.Boolean(true))
        else if present == "false" then Right(Lit.Boolean(false))
        else if present.length >= 2 && present.head == '"' && present.last == '"' then
          Right(Lit.String(present.substring(1, present.length - 1)))
        else if isCanonicalDecimal(present) then
          present.toIntOption match
            case Some(integer) => Right(Lit.Int(integer))
            case None =>
              Left(
                error(
                  "NEUTRAL_TERM_AUTHORING_LITERAL_UNSUPPORTED",
                  "literals require a canonical Int decimal, true, false, or an outer-quoted semantic String value."
                )
              )
        else
          Left(
            error(
              "NEUTRAL_TERM_AUTHORING_LITERAL_UNSUPPORTED",
              "literals require a canonical Int decimal, true, false, or an outer-quoted semantic String value."
            )
          )
      }

  private def encodeStandardInterpolationPart(value: String): Either[Error, String] =
    Option(value)
      .toRight(structureError("interpolation semantic parts must be present."))
      .map(
        _.flatMap {
          case '\\' => "\\\\"
          case '"' => "\\u0022"
          case '\n' => "\\n"
          case '\r' => "\\r"
          case '\t' => "\\t"
          case '\b' => "\\b"
          case '\f' => "\\f"
          case character if character < ' ' || character == '\u007f' =>
            f"\\u${character.toInt}%04x"
          case character => character.toString
        }
      )

  private def isCanonicalDecimal(value: String): Boolean =
    val firstDigit = if value.startsWith("-") then 1 else 0
    firstDigit < value.length &&
    value.charAt(firstDigit) >= '1' &&
    value.charAt(firstDigit) <= '9' &&
    value.substring(firstDigit + 1).forall(character => character >= '0' && character <= '9') ||
    value == "0"

  private def validateDefinitionBinders(
      binders: Vector[DefinitionBinder]
  ): Either[Error, Vector[AuthoredDefinitionBinder]] =
    val failure = structureError(
      "definition binders require present distinct BinderIds, distinct authorable names, and non-overflowing BinderIds."
    )

    Option(binders)
      .toRight(failure)
      .flatMap(
        _.foldLeft(Right(Vector.empty): Either[Error, Vector[AuthoredDefinitionBinder]]) {
          (accumulated, binder) =>
            for
              authored <- accumulated
              presentBinder <- Option(binder).toRight(failure)
              binderId <- Option(presentBinder.binderId).toRight(failure)
              _ <- Either.cond(binderId.value < Int.MaxValue, (), failure)
              definitionName <- Option(presentBinder.name).toRight(failure)
              authoredName <- ScalametaTermDefinitionNameAuthoring
                .author(definitionName)
                .toRight(failure)
            yield authored :+ AuthoredDefinitionBinder(binderId, authoredName.value)
        }
      )
      .flatMap { authored =>
        val binderIds = authored.map(_.binderId)
        val sourceNames = authored.map(_.sourceName)
        Either.cond(
          binderIds.distinct.size == binderIds.size &&
            sourceNames.distinct.size == sourceNames.size,
          authored,
          failure
        )
      }

  private def validateDefinitionRoundTrip(
      expected: TermShape,
      candidate: Term,
      binders: Vector[AuthoredDefinitionBinder]
  ): Either[Error, Term] =
    val rejected = error(
      "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED",
      "the authored definition body did not preserve its scoped neutral meaning."
    )
    val projectionBinders = binders.map(binder =>
      ScalametaTermProjection.DefinitionBinder(binder.sourceName, binder.binderId)
    )
    val binderIds = binders.map(_.binderId)

    ScalametaTermProjection
      .projectWithDefinitionBinders(candidate, projectionBinders)
      .left
      .map(_ => rejected)
      .flatMap(projected =>
        for
          _ <- Either.cond(
            TermShapeTraversal.alphaNormalizeInScope(projected.shape, binderIds) ==
              TermShapeTraversal.alphaNormalizeInScope(expected, binderIds),
            (),
            rejected
          )
          _ <- Either.cond(projected.sourceSpan.isEmpty, (), rejected)
        yield candidate
      )

  private def validateRoundTrip(
      expected: TermShape,
      candidate: Term
  ): Either[Error, Term] =
    val normalizedExpected = TermShapeTraversal.alphaNormalize(expected)

    ScalametaTermProjection
      .project(candidate)
      .left
      .map(problem =>
        error(
          "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED",
          s"the existing neutral projector rejected the authored term with ${problem.code}."
        )
      )
      .flatMap(projected =>
        val preservesMeaning =
          if normalizedExpected == expected then projected.shape == expected
          else TermShapeTraversal.alphaNormalize(projected.shape) == normalizedExpected

        for
          _ <- Either.cond(
            preservesMeaning,
            (),
            error(
              "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED",
              "the authored term did not project to the exact input TermShape."
            )
          )
          _ <- Either.cond(
            projected.sourceSpan.isEmpty,
            (),
            error(
              "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED",
              "a fresh authored term unexpectedly carried source provenance."
            )
          )
        yield candidate
      )

  private def traverse[A, B](
      values: List[A]
  )(transform: A => Either[Error, B]): Either[Error, List[B]] =
    Option(values)
      .toRight(structureError("recursive TermShape lists must be present."))
      .flatMap(
        _.foldRight(Right(Nil): Either[Error, List[B]]) { (value, accumulated) =>
          for
            head <- transform(value)
            tail <- accumulated
          yield head :: tail
        }
      )

  private def construct[A <: Tree](role: String)(candidate: => A): Either[Error, A] =
    try Right(candidate)
    catch
      case NonFatal(_) =>
        Left(structureError(s"the $role could not be constructed from the supplied structure."))

  private def requirePresent(value: String, detail: String): Either[Error, String] =
    Option(value).toRight(structureError(detail))

  private def require(condition: Boolean, detail: String): Either[Error, Unit] =
    Either.cond(condition, (), structureError(detail))

  private def structureError(detail: String): Error =
    error("NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED", detail)

  private def typedTypeUnsupported: Error =
    error(
      "NEUTRAL_TERM_AUTHORING_TYPED_TYPE_UNSUPPORTED",
      "typed/ascribed authoring admits only canonical Int, String, and Boolean."
    )

  private def lambdaParameterTypeUnsupported: Error =
    error(
      "NEUTRAL_TERM_AUTHORING_LAMBDA_PARAMETER_TYPE_UNSUPPORTED",
      "Lambda1 parameter authoring admits only canonical Int, String, and Boolean."
    )

  private def lambdaScopeUnsupported(detail: String): Error =
    error("NEUTRAL_TERM_AUTHORING_LAMBDA_SCOPE_UNSUPPORTED", detail)

  private def p2DeclaredTypeUnsupported: Error =
    error(
      "NEUTRAL_TERM_AUTHORING_P2_DECLARED_TYPE_UNSUPPORTED",
      "P2 declared-Type authoring admits only canonical Int, String, Boolean, and AnyVal."
    )

  private def p2ScopeUnsupported(detail: String): Error =
    error("NEUTRAL_TERM_AUTHORING_P2_SCOPE_UNSUPPORTED", detail)

  private def p3ScopeUnsupported(detail: String): Error =
    error("NEUTRAL_TERM_AUTHORING_P3_SCOPE_UNSUPPORTED", detail)

  private def p3TypeUnsupported: Error =
    error(
      "NEUTRAL_TERM_AUTHORING_P3_TYPE_UNSUPPORTED",
      "P3 identity method authoring requires equal canonical Int, String, or Boolean parameter and result Types."
    )

  private def p3BodyUnsupported: Error =
    error(
      "NEUTRAL_TERM_AUTHORING_P3_BODY_UNSUPPORTED",
      "P3 identity method bodies must be exactly the local parameter BoundReference."
    )

  private def p3ResultUnsupported: Error =
    error(
      "NEUTRAL_TERM_AUTHORING_P3_RESULT_UNSUPPORTED",
      "P3 block results must be exactly the local method BoundReference."
    )

  private def p3NestedUnsupported: Error =
    error(
      "NEUTRAL_TERM_AUTHORING_P3_NESTED_UNSUPPORTED",
      "a P3 local identity method nested under Lambda1 or P2 is outside bounded Scalameta authoring."
    )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
