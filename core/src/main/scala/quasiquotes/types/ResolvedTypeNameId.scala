package quasiquotes.types

/** Compiler-free owner categories admitted by the bounded global selected-Type
  * resolution surface.
  */
enum ResolvedTypeOwnerKind derives CanEqual:
  case Package
  case Type
  case Module

/** One ordered declaration-owner segment in a resolved global Type identity. */
final case class ResolvedTypeOwnerSegment(
    kind: ResolvedTypeOwnerKind,
    name: String
) derives CanEqual:
  require(name.nonEmpty, "Resolved Type owner names must be nonempty.")
  require(!name.contains('$'), "Resolved Type owner names must not contain compiler module encodings.")

  def render: String = s"$kind($name)"

/** Declaration identity for the bounded globally addressable selected-Type
  * surface. Source spelling is deliberately not part of equality.
  */
final case class ResolvedTypeNameId(
    owners: Vector[ResolvedTypeOwnerSegment],
    terminalName: String
) derives CanEqual:
  require(owners.nonEmpty, "A resolved selected Type identity requires at least one global owner.")
  require(terminalName.nonEmpty, "A resolved selected Type terminal name must be nonempty.")
  require(!terminalName.contains('$'), "Resolved Type terminal names must not contain compiler module encodings.")

  def canonicalSource: String =
    (owners.map(_.name) :+ terminalName).mkString(".")

  def render: String =
    s"ResolvedTypeNameId(${owners.map(_.render).mkString("/")}::$terminalName)"

private[quasiquotes] object StandardResolvedTypeNames:
  private def pkg(name: String): ResolvedTypeOwnerSegment =
    ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, name)

  val ListId: ResolvedTypeNameId =
    ResolvedTypeNameId(Vector(pkg("scala"), pkg("collection"), pkg("immutable")), "List")

  val OptionId: ResolvedTypeNameId =
    ResolvedTypeNameId(Vector(pkg("scala")), "Option")

  val EitherId: ResolvedTypeNameId =
    ResolvedTypeNameId(Vector(pkg("scala"), pkg("util")), "Either")

  val IntId: ResolvedTypeNameId =
    ResolvedTypeNameId(Vector(pkg("scala")), "Int")

  val BooleanId: ResolvedTypeNameId =
    ResolvedTypeNameId(Vector(pkg("scala")), "Boolean")

  val AnyValId: ResolvedTypeNameId =
    ResolvedTypeNameId(Vector(pkg("scala")), "AnyVal")

  def fixedSourceName(id: ResolvedTypeNameId): Option[String] =
    if id == ListId then Some("List")
    else if id == OptionId then Some("Option")
    else if id == EitherId then Some("Either")
    else if id == IntId then Some("Int")
    else if id == BooleanId then Some("Boolean")
    else if id == AnyValId then Some("AnyVal")
    else None
