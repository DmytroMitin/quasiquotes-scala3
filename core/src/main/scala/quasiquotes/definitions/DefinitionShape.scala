package quasiquotes.definitions

import quasiquotes.parser.{TermShape, TypeShape}
import quasiquotes.types.TypeNormalForm

private[quasiquotes] sealed trait DefinitionShape derives CanEqual:
  def name: DefinitionName
  def render: String

private[quasiquotes] object DefinitionShape:
  private val SupportedUnaryOperators = Set("+", "-", "!", "~")
  private val SupportedAscriptionTypes = Set("Int", "String", "Boolean")

  final class ParameterlessDef private[DefinitionShape] (
      val name: DefinitionName,
      val resultType: TypeShape,
      val body: TermShape
  ) extends DefinitionShape:
    def render: String =
      s"ParameterlessDef(name=${name.render}, resultType=${resultType.render}, body=${body.render})"

    override def equals(other: Any): Boolean =
      other match
        case that: ParameterlessDef =>
          name == that.name && resultType == that.resultType && body == that.body
        case _ => false

    override def hashCode: Int = (name, resultType, body).hashCode
    override def toString: String = render

  final class ImmutableVal private[DefinitionShape] (
      val name: DefinitionName,
      val declaredType: TypeShape,
      val rhs: TermShape
  ) extends DefinitionShape:
    def render: String =
      s"ImmutableVal(name=${name.render}, declaredType=${declaredType.render}, rhs=${rhs.render})"

    override def equals(other: Any): Boolean =
      other match
        case that: ImmutableVal =>
          name == that.name && declaredType == that.declaredType && rhs == that.rhs
        case _ => false

    override def hashCode: Int = (name, declaredType, rhs).hashCode
    override def toString: String = render

  def parameterlessDef(
      name: DefinitionName,
      resultType: TypeShape,
      body: TermShape
  ): Either[DefinitionError, ParameterlessDef] =
    for
      _ <- validateType(resultType, "method result type")
      _ <- validateTerm(body, "method body")
    yield new ParameterlessDef(name, resultType, body)

  def immutableVal(
      name: DefinitionName,
      declaredType: TypeShape,
      rhs: TermShape
  ): Either[DefinitionError, ImmutableVal] =
    for
      _ <- validateType(declaredType, "value declared type")
      _ <- validateTerm(rhs, "value right-hand side")
    yield new ImmutableVal(name, declaredType, rhs)

  private def validateType(
      shape: TypeShape,
      component: String
  ): Either[DefinitionError, Unit] =
    TypeNormalForm
      .fromShape(shape)
      .map(_ => ())
      .left
      .map(_ => DefinitionError.UnsupportedDefinitionType(component))

  private def validateTerm(
      shape: TermShape,
      component: String
  ): Either[DefinitionError, Unit] =
    firstUnsupportedTerm(shape)
      .toLeft(())
      .left
      .map(reason => DefinitionError.UnsupportedDefinitionBody(component, reason))

  private def firstUnsupportedTerm(shape: TermShape): Option[String] =
    shape match
      case TermShape.Identifier(_, true) =>
        Some("placeholder identifiers require authoritative template metadata and are not representation-core bodies")
      case TermShape.Identifier(_, false) | TermShape.Literal(_) =>
        None
      case TermShape.Select(qualifier, _) =>
        firstUnsupportedTerm(qualifier)
      case TermShape.Apply(function, arguments) =>
        firstUnsupportedTerm(function).orElse(firstUnsupported(arguments))
      case TermShape.Infix(left, _, right) =>
        firstUnsupportedTerm(left).orElse(firstUnsupportedTerm(right))
      case TermShape.Unary(operator, operand) =>
        if !SupportedUnaryOperators(operator) then
          Some("unary bodies support only +, -, !, and ~")
        else firstUnsupportedTerm(operand)
      case TermShape.Typed(expression, typeName) =>
        if !SupportedAscriptionTypes(typeName) then
          Some("typed bodies support only Int, String, and Boolean ascriptions")
        else firstUnsupportedTerm(expression)
      case TermShape.Tuple(elements) =>
        if elements.size < 2 || elements.size > 22 then
          Some("tuple bodies must contain between 2 and 22 elements")
        else firstUnsupported(elements)
      case TermShape.If(condition, thenBranch, elseBranch) =>
        firstUnsupportedTerm(condition)
          .orElse(firstUnsupportedTerm(thenBranch))
          .orElse(firstUnsupportedTerm(elseBranch))
      case TermShape.Parenthesized(expression) =>
        firstUnsupportedTerm(expression)
      case TermShape.Unsupported(_, _) =>
        Some("the body contains a term shape outside the currently supported structural subset")

  private def firstUnsupported(shapes: List[TermShape]): Option[String] =
    shapes.iterator.map(firstUnsupportedTerm).collectFirst { case Some(reason) => reason }
