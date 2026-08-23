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

  final class TwoParameterDef private[DefinitionShape] (
      val name: DefinitionName,
      val firstParameterBinderId: BinderId,
      val firstParameterName: DefinitionName,
      val firstParameterType: TypeShape,
      val secondParameterBinderId: BinderId,
      val secondParameterName: DefinitionName,
      val secondParameterType: TypeShape,
      val resultType: TypeShape,
      val body: TermShape
  ) extends DefinitionShape:
    private lazy val semanticBody: TermShape =
      TermShapeTraversal.alphaNormalizeInScope(
        body,
        Vector(firstParameterBinderId, secondParameterBinderId)
      )

    def render: String =
      s"TwoParameterDef(name=${name.render}, firstParameter=${firstParameterName.render}: ${firstParameterType.render}, secondParameter=${secondParameterName.render}: ${secondParameterType.render}, resultType=${resultType.render}, body=${body.render})"

    override def equals(other: Any): Boolean =
      other match
        case that: TwoParameterDef =>
          name == that.name &&
            firstParameterType == that.firstParameterType &&
            secondParameterType == that.secondParameterType &&
            resultType == that.resultType &&
            semanticBody == that.semanticBody
        case _ => false

    override def hashCode: Int =
      (
        "TwoParameterDef",
        name,
        firstParameterType,
        secondParameterType,
        resultType,
        semanticBody
      ).hashCode

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
        Vector(parameterBinderId)
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

  def twoParameterDef(
      name: DefinitionName,
      firstParameterBinderId: BinderId,
      firstParameterName: DefinitionName,
      firstParameterType: TypeShape,
      secondParameterBinderId: BinderId,
      secondParameterName: DefinitionName,
      secondParameterType: TypeShape,
      resultType: TypeShape,
      body: TermShape
  ): Either[DefinitionError, TwoParameterDef] =
    for
      _ <- validateTwoParameterList(
        firstParameterBinderId,
        firstParameterName,
        secondParameterBinderId,
        secondParameterName
      )
      _ <- validateType(firstParameterType, "first method parameter type")
      _ <- validateType(secondParameterType, "second method parameter type")
      _ <- validateType(resultType, "method result type")
      _ <- validateTerm(
        body,
        "method body",
        Vector(firstParameterBinderId, secondParameterBinderId)
      )
    yield
      new TwoParameterDef(
        name,
        firstParameterBinderId,
        firstParameterName,
        firstParameterType,
        secondParameterBinderId,
        secondParameterName,
        secondParameterType,
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
      allowedDefinitionBinders: Vector[BinderId] = Vector.empty
  ): Either[DefinitionError, Unit] =
    firstUnsupportedTerm(shape, allowedDefinitionBinders)
      .toLeft(())
      .left
      .map(reason => DefinitionError.UnsupportedDefinitionBody(component, reason))

  private def firstUnsupportedTerm(
      shape: TermShape,
      allowedDefinitionBinders: Vector[BinderId]
  ): Option[String] =
    shape match
      case TermShape.Identifier(_, true) =>
        Some("placeholder identifiers require authoritative template metadata and are not representation-core bodies")
      case TermShape.Identifier(_, false) | TermShape.Literal(_) =>
        None
      case TermShape.BoundReference(binderId, _) =>
        if allowedDefinitionBinders.contains(binderId) then None
        else if allowedDefinitionBinders.size == 1 then
          Some("bound references must resolve to the single ordinary method parameter")
        else if allowedDefinitionBinders.size == 2 then
          Some("bound references must resolve to one of the two ordinary method parameters")
        else
          Some("lambda-bound references are only valid inside the bounded Lambda1 term tranche")
      case TermShape.Lambda1(_, _, _, _) =>
        Some("Lambda1 definition bodies require a later exact-backend tranche")
      case TermShape.Select(qualifier, _) =>
        firstUnsupportedTerm(qualifier, allowedDefinitionBinders)
      case TermShape.Apply(function, arguments) =>
        firstUnsupportedTerm(function, allowedDefinitionBinders)
          .orElse(firstUnsupported(arguments, allowedDefinitionBinders))
      case TermShape.New(_, arguments) =>
        Some("constructor new expressions are not part of the bounded definition-body backend")
      case TermShape.Infix(left, _, right) =>
        firstUnsupportedTerm(left, allowedDefinitionBinders)
          .orElse(firstUnsupportedTerm(right, allowedDefinitionBinders))
      case TermShape.Unary(operator, operand) =>
        if !SupportedUnaryOperators(operator) then
          Some("unary bodies support only +, -, !, and ~")
        else firstUnsupportedTerm(operand, allowedDefinitionBinders)
      case TermShape.InterpolatedString("s", parts, arguments) =>
        if parts.size != arguments.size + 1 then
          Some("interpolated string parts/arguments are inconsistent")
        else firstUnsupported(arguments, allowedDefinitionBinders)
      case TermShape.InterpolatedString(_, _, _) =>
        Some("definition bodies support only the standard s interpolator")
      case TermShape.Typed(expression, typeName) =>
        if !SupportedAscriptionTypes(typeName) then
          Some("typed bodies support only Int, String, and Boolean ascriptions")
        else firstUnsupportedTerm(expression, allowedDefinitionBinders)
      case TermShape.Tuple(elements) =>
        if elements.size < 2 || elements.size > 22 then
          Some("tuple bodies must contain between 2 and 22 elements")
        else firstUnsupported(elements, allowedDefinitionBinders)
      case TermShape.If(condition, thenBranch, elseBranch) =>
        firstUnsupportedTerm(condition, allowedDefinitionBinders)
          .orElse(firstUnsupportedTerm(thenBranch, allowedDefinitionBinders))
          .orElse(firstUnsupportedTerm(elseBranch, allowedDefinitionBinders))
      case TermShape.Block(_, _) =>
        Some("P1 blocks are not part of the bounded Definition quasiquote body tranche")
      case TermShape.Parenthesized(expression) =>
        firstUnsupportedTerm(expression, allowedDefinitionBinders)
      case TermShape.Unsupported(_, _) =>
        Some("the body contains a term shape outside the currently supported structural subset")

  private def firstUnsupported(
      shapes: List[TermShape],
      allowedDefinitionBinders: Vector[BinderId]
  ): Option[String] =
    shapes.iterator
      .map(firstUnsupportedTerm(_, allowedDefinitionBinders))
      .collectFirst { case Some(reason) => reason }

  private def validateTwoParameterList(
      firstBinderId: BinderId,
      firstName: DefinitionName,
      secondBinderId: BinderId,
      secondName: DefinitionName
  ): Either[DefinitionError, Unit] =
    if firstBinderId == secondBinderId then
      Left(
        DefinitionError.InvalidTwoParameterList(
          "parameter binder identities must be distinct"
        )
      )
    else if firstName == secondName then
      Left(
        DefinitionError.InvalidTwoParameterList(
          "declared parameter names must be distinct"
        )
      )
    else Right(())
