package quasiquotes.types

import quasiquotes.parser.TypeShape

/** Deterministic compiler-free projection of a frontend-validated global Type
  * environment. Entries are keyed only by the canonical path derived from the
  * resolved identity, so arbitrary source labels cannot be introduced.
  */
private[quasiquotes] final class ResolvedTypeEnvironment private (
    private val entries: Map[Vector[String], ResolvedTypeNameId]
):
  def resolveSelected(shape: TypeShape): Either[TypeQuasiquoteError, ResolvedTypeNameId] =
    selectedPath(shape).flatMap { path =>
      entries.get(path).toRight(
        TypeQuasiquoteError(
          TypeNameResolutionDiagnostics.unresolved(path.mkString("."))
        )
      )
    }

  def contains(id: ResolvedTypeNameId): Boolean =
    entries.get(pathOf(id)).contains(id)

  private def selectedPath(shape: TypeShape): Either[TypeQuasiquoteError, Vector[String]] =
    def loop(current: TypeShape): Option[Vector[String]] =
      current match
        case TypeShape.Identifier(name) => Some(Vector(name))
        case TypeShape.Select(qualifier, name) => loop(qualifier).map(_ :+ name)
        case _ => None

    shape match
      case TypeShape.Select(_, _) =>
        loop(shape).toRight(
          TypeQuasiquoteError(TypeNameResolutionDiagnostics.unsupportedQualifier(shape.render))
        )
      case _ =>
        Left(TypeQuasiquoteError(TypeNameResolutionDiagnostics.unsupportedQualifier(shape.render)))

  private def pathOf(id: ResolvedTypeNameId): Vector[String] =
    id.owners.map(_.name) :+ id.terminalName

private[quasiquotes] object ResolvedTypeEnvironment:
  def fromIds(ids: Iterable[ResolvedTypeNameId]): Either[TypeQuasiquoteError, ResolvedTypeEnvironment] =
    val grouped = ids.toVector.groupBy(id => id.owners.map(_.name) :+ id.terminalName)
    grouped.collectFirst { case (path, values) if values.sizeCompare(1) > 0 => path } match
      case Some(path) =>
        Left(
          TypeQuasiquoteError(
            TypeNameResolutionDiagnostics.ambiguous(path.mkString("."))
          )
        )
      case None =>
        Right(new ResolvedTypeEnvironment(grouped.view.mapValues(_.head).toMap))

private[quasiquotes] object TypeNameResolutionDiagnostics:
  val Unresolved = "TYPE_NAME_RESOLUTION_UNRESOLVED"
  val Ambiguous = "TYPE_NAME_RESOLUTION_AMBIGUOUS"
  val UnsupportedQualifier = "TYPE_NAME_RESOLUTION_UNSUPPORTED_QUALIFIER"
  val UnstableTermPrefix = "TYPE_NAME_RESOLUTION_UNSTABLE_TERM_PREFIX"
  val ConstructorPolicyMismatch = "TYPE_NAME_RESOLUTION_CONSTRUCTOR_POLICY_MISMATCH"
  val CompilerShapeUnsupported = "TYPE_NAME_RESOLUTION_COMPILER_SHAPE_UNSUPPORTED"
  val ResolvedFamilyUnsupported = "TYPE_NAME_RESOLVED_FAMILY_UNSUPPORTED"

  def unresolved(source: String): String =
    s"$Unresolved: no canonical global Type binding exists for `$source`."

  def ambiguous(source: String): String =
    s"$Ambiguous: more than one binding was supplied for canonical global Type `$source`."

  def unsupportedQualifier(source: String): String =
    s"$UnsupportedQualifier: `$source` is not a canonical package/type/module selected path."

  def unstableTermPrefix(source: String): String =
    s"$UnstableTermPrefix: `$source` has a stable-term path prefix; prefix identity is deferred."

  def constructorPolicyMismatch(id: ResolvedTypeNameId, arity: Int): String =
    s"$ConstructorPolicyMismatch: `${id.canonicalSource}`/$arity is not one of the exact admitted List/1, Option/1, Either/2 declarations."

  def compilerShapeUnsupported(detail: String): String =
    s"$CompilerShapeUnsupported: $detail"

  def resolvedFamilyUnsupported(source: String): String =
    s"$ResolvedFamilyUnsupported: resolved Type `$source` is outside the bounded selected-Type family."
