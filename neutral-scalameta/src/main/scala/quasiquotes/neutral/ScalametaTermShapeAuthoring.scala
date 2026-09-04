package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionName
import _root_.quasiquotes.parser.{BinderId, BlockStatement, ConstructorNamePolicy, TermShape}
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

  private final case class AuthoringScope(
      definitionBinders: Vector[AuthoredDefinitionBinder],
      definitionBinderAware: Boolean
  ):
    def sourceNameFor(binderId: BinderId): Option[String] =
      definitionBinders.find(_.binderId == binderId).map(_.sourceName)

  private val SupportedUnaryOperators = Set("+", "-", "!", "~")
  private val BinderFreeScope = AuthoringScope(Vector.empty, definitionBinderAware = false)

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
          val scope = AuthoringScope(authoredBinders, definitionBinderAware = true)
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
        for
          _ <- Either.cond(
            projected.shape == expected,
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

  private def construct[A <: Term](role: String)(candidate: => A): Either[Error, A] =
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

  private def error(code: String, detail: String): Error =
    Error(code, detail)
