package quasiquotes.types

import scala.quoted.*

/** Explicit compiler-side witnesses for the bounded canonical global selected
  * Type surface. The type parameter keeps Quotes-owned values out of core.
  */
final class GlobalSelectedTypeEnvironment[T] private[types] (
    private[quasiquotes] val semanticEnvironment: ResolvedTypeEnvironment,
    private val bindings: Map[ResolvedTypeNameId, GlobalSelectedTypeEnvironment.Binding[T]]
):
  private[quasiquotes] def binding(
      id: ResolvedTypeNameId
  ): Option[GlobalSelectedTypeEnvironment.Binding[T]] =
    bindings.get(id)

object GlobalSelectedTypeEnvironment:
  private[quasiquotes] enum WitnessRole:
    case Terminal
    case Constructor(arity: Int)

  private[quasiquotes] final case class Binding[T](
      id: ResolvedTypeNameId,
      witness: T,
      role: WitnessRole
  )

  /** Build an environment from exact typed witnesses. Canonical source paths
    * are derived from each witness; callers cannot attach arbitrary labels.
    * An applied witness contributes its constructor declaration.
    */
  def fromWitnesses(using q: Quotes)(
      witnesses: q.reflect.TypeRepr*
  ): Either[TypeQuasiquoteError, GlobalSelectedTypeEnvironment[q.reflect.TypeRepr]] =
    val derived = witnesses.toVector.map(ResolvedTypeReflection.bindingFromWitness(_))
    collect(derived).flatMap { values =>
      ResolvedTypeEnvironment.fromIds(values.map(_.id)).map { semantic =>
        new GlobalSelectedTypeEnvironment(
          semantic,
          values.map(value => value.id -> value).toMap
        )
      }
    }

  private def collect[A](
      values: Vector[Either[TypeQuasiquoteError, A]]
  ): Either[TypeQuasiquoteError, Vector[A]] =
    values.foldLeft[Either[TypeQuasiquoteError, Vector[A]]](Right(Vector.empty)) {
      (accumulated, value) =>
        for
          current <- accumulated
          next <- value
        yield current :+ next
    }
