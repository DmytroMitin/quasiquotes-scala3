package quasiquotes.publicapi

/**
 * A compiler-free completed type in the deliberately bounded public slice.
 * Its representation is hidden; projections and equality are structural.
 */
final class CompletedType private (
    private val representation: CompletedType.Representation
) derives CanEqual:
  def kindCode: String =
    representation match
      case CompletedType.Representation.Named(_) => "named"
      case CompletedType.Representation.TypeParameter(_) => "type-parameter"
      case CompletedType.Representation.Applied(_, _) => "applied"

  def name: Option[String] =
    representation match
      case CompletedType.Representation.Named(value) => Some(value)
      case CompletedType.Representation.TypeParameter(value) => Some(value)
      case CompletedType.Representation.Applied(_, _) => None

  def constructor: Option[CompletedType] =
    representation match
      case CompletedType.Representation.Applied(value, _) => Some(value)
      case _ => None

  def arguments: Vector[CompletedType] =
    representation match
      case CompletedType.Representation.Applied(_, values) => values
      case _ => Vector.empty

  def source: String =
    representation match
      case CompletedType.Representation.Named(value) => value
      case CompletedType.Representation.TypeParameter(value) => value
      case CompletedType.Representation.Applied(value, values) =>
        s"${value.source}[${values.map(_.source).mkString(", ")}]"

  override def equals(other: Any): Boolean =
    other match
      case that: CompletedType => representation == that.representation
      case _ => false

  override def hashCode: Int = representation.hashCode

  override def toString: String = source

object CompletedType:
  private enum Representation derives CanEqual:
    case Named(name: String)
    case TypeParameter(name: String)
    case Applied(constructor: CompletedType, arguments: Vector[CompletedType])

  def named(name: String): Either[PublicFailure, CompletedType] =
    validatedName(name, FailureAnchor.TypeName)
      .map(value => new CompletedType(Representation.Named(value)))

  def typeParameter(name: String): Either[PublicFailure, CompletedType] =
    validatedName(name, FailureAnchor.TypeParameter)
      .map(value => new CompletedType(Representation.TypeParameter(value)))

  def applied(
      constructor: CompletedType,
      arguments: Vector[CompletedType]
  ): Either[PublicFailure, CompletedType] =
    if constructor == null then
      Left(PublicFailure.invalidTypeApplication("The type constructor must be present."))
    else if arguments == null || arguments.isEmpty then
      Left(PublicFailure.invalidTypeApplication("A type application requires at least one argument."))
    else if arguments.exists(_ == null) then
      Left(PublicFailure.invalidTypeApplication("Type application arguments must be present."))
    else if constructor.kindCode != "named" then
      Left(PublicFailure.invalidTypeApplication("Only a named type may be applied in this bounded slice."))
    else
      Right(new CompletedType(Representation.Applied(constructor, arguments)))

  private[publicapi] def firstUndeclared(
      value: CompletedType,
      declared: String
  ): Option[String] =
    value.representation match
      case Representation.Named(_) => None
      case Representation.TypeParameter(name) =>
        Option.when(name != declared)(name)
      case Representation.Applied(constructor, arguments) =>
        firstUndeclared(constructor, declared)
          .orElse(arguments.iterator.flatMap(firstUndeclared(_, declared)).nextOption())

  private def validatedName(
      value: String,
      anchor: FailureAnchor
  ): Either[PublicFailure, String] =
    Either.cond(
      value != null && PublicIdentifier.isValid(value),
      value,
      PublicFailure.invalidName(String.valueOf(value), anchor)
    )
