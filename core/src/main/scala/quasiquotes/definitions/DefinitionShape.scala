package quasiquotes.definitions

import quasiquotes.parser.{BinderId, TermShape, TypeShape}
import quasiquotes.terms.TermShapeTraversal
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

  final class SingleParameterDef private[DefinitionShape] (
      val name: DefinitionName,
      val parameterBinderId: BinderId,
      val parameterName: DefinitionName,
      val parameterType: TypeShape,
      val resultType: TypeShape,
      val body: TermShape
  ) extends DefinitionShape:
    private lazy val semanticBody: TermShape =
      TermShapeTraversal.alphaNormalizeInScope(body, parameterBinderId)

    def render: String =
      s"SingleParameterDef(name=${name.render}, parameter=${parameterName.render}: ${parameterType.render}, resultType=${resultType.render}, body=${body.render})"

    override def equals(other: Any): Boolean =
      other match
        case that: SingleParameterDef =>
          name == that.name &&
            parameterType == that.parameterType &&
            resultType == that.resultType &&
            semanticBody == that.semanticBody
        case _ => false

    override def hashCode: Int =
      ("SingleParameterDef", name, parameterType, resultType, semanticBody).hashCode

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

  def singleParameterDef(
      name: DefinitionName,
      parameterBinderId: BinderId,
      parameterName: DefinitionName,
      parameterType: TypeShape,
      resultType: TypeShape,
      body: TermShape
  ): Either[DefinitionError, SingleParameterDef] =
    for
      _ <- validateType(parameterType, "method parameter type")
      _ <- validateType(resultType, "method result type")
      _ <- validateTerm(
        body,
        "method body",
        Some(parameterBinderId)
      )
    yield
      new SingleParameterDef(
        name,
        parameterBinderId,
        parameterName,
        parameterType,
        resultType,
        body
      )

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
      component: String,
      allowedDefinitionBinder: Option[BinderId] = None
  ): Either[DefinitionError, Unit] =
    firstUnsupportedTerm(shape, allowedDefinitionBinder)
      .toLeft(())
      .left
      .map(reason => DefinitionError.UnsupportedDefinitionBody(component, reason))

  private def firstUnsupportedTerm(
      shape: TermShape,
      allowedDefinitionBinder: Option[BinderId]
  ): Option[String] =
    shape match
      case TermShape.Identifier(_, true) =>
        Some("placeholder identifiers require authoritative template metadata and are not representation-core bodies")
      case TermShape.Identifier(_, false) | TermShape.Literal(_) =>
        None
      case TermShape.BoundReference(binderId, _) =>
        allowedDefinitionBinder match
          case Some(expected) if binderId == expected => None
          case Some(_) =>
            Some("bound references must resolve to the single ordinary method parameter")
          case None =>
            Some("lambda-bound references are only valid inside the bounded Lambda1 term tranche")
      case TermShape.Lambda1(_, _, _, _) =>
        Some("Lambda1 definition bodies require a later exact-backend tranche")
      case TermShape.Select(qualifier, _) =>
        firstUnsupportedTerm(qualifier, allowedDefinitionBinder)
      case TermShape.Apply(function, arguments) =>
        firstUnsupportedTerm(function, allowedDefinitionBinder)
          .orElse(firstUnsupported(arguments, allowedDefinitionBinder))
      case TermShape.New(_, arguments) =>
        Some("constructor new expressions are not part of the bounded definition-body backend")
      case TermShape.Infix(left, _, right) =>
        firstUnsupportedTerm(left, allowedDefinitionBinder)
          .orElse(firstUnsupportedTerm(right, allowedDefinitionBinder))
      case TermShape.Unary(operator, operand) =>
        if !SupportedUnaryOperators(operator) then
          Some("unary bodies support only +, -, !, and ~")
        else firstUnsupportedTerm(operand, allowedDefinitionBinder)
      case TermShape.InterpolatedString("s", parts, arguments) =>
        if parts.size != arguments.size + 1 then
          Some("interpolated string parts/arguments are inconsistent")
        else firstUnsupported(arguments, allowedDefinitionBinder)
      case TermShape.InterpolatedString(_, _, _) =>
        Some("definition bodies support only the standard s interpolator")
      case TermShape.Typed(expression, typeName) =>
        if !SupportedAscriptionTypes(typeName) then
          Some("typed bodies support only Int, String, and Boolean ascriptions")
        else firstUnsupportedTerm(expression, allowedDefinitionBinder)
      case TermShape.Tuple(elements) =>
        if elements.size < 2 || elements.size > 22 then
          Some("tuple bodies must contain between 2 and 22 elements")
        else firstUnsupported(elements, allowedDefinitionBinder)
      case TermShape.If(condition, thenBranch, elseBranch) =>
        firstUnsupportedTerm(condition, allowedDefinitionBinder)
          .orElse(firstUnsupportedTerm(thenBranch, allowedDefinitionBinder))
          .orElse(firstUnsupportedTerm(elseBranch, allowedDefinitionBinder))
      case TermShape.Parenthesized(expression) =>
        firstUnsupportedTerm(expression, allowedDefinitionBinder)
      case TermShape.Unsupported(_, _) =>
        Some("the body contains a term shape outside the currently supported structural subset")

  private def firstUnsupported(
      shapes: List[TermShape],
      allowedDefinitionBinder: Option[BinderId]
  ): Option[String] =
    shapes.iterator
      .map(firstUnsupportedTerm(_, allowedDefinitionBinder))
      .collectFirst { case Some(reason) => reason }
