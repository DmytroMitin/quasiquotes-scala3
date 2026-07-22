package quasiquotes.construct

import quasiquotes.types.ConstructedType

final case class PlaceholderSource[+T](source: String, holes: Vector[T])

private[construct] sealed trait QuasiquoteHole[+T]

private[construct] object QuasiquoteHole:
  final case class Term[+T](term: T) extends QuasiquoteHole[T]
  final case class ConstructedTypeSplice(constructedType: ConstructedType) extends QuasiquoteHole[Nothing]

private[construct] final case class PlaceholderBinding[+T](name: String, hole: QuasiquoteHole[T])

private[construct] final case class CategorizedPlaceholderSource[+T](
    source: String,
    bindings: Vector[PlaceholderBinding[T]]
)

object PlaceholderSource:
  def synthesize[T](parts: Seq[String], holes: Seq[T]): Either[QuasiquoteError, PlaceholderSource[T]] =
    if parts.length != holes.length + 1 then
      Left(
        QuasiquoteError.HoleCountMismatch(
          expected = parts.length - 1,
          actual = holes.length
        )
      )
    else
      val builder = new StringBuilder(parts.head)
      holes.zipWithIndex.foreach { (hole, index) =>
        builder.append(s"__hole$index")
        builder.append(parts(index + 1))
      }
      Right(PlaceholderSource[T](builder.result(), holes.toVector))

  private[construct] def synthesizeCategorized[T](
      parts: Seq[String],
      holes: Seq[QuasiquoteHole[T]]
  ): Either[QuasiquoteError, CategorizedPlaceholderSource[T]] =
    if parts.length != holes.length + 1 then
      Left(
        QuasiquoteError.HoleCountMismatch(
          expected = parts.length - 1,
          actual = holes.length
        )
      )
    else
      val builder = new StringBuilder(parts.head)
      val bindings = holes.zipWithIndex.map { (hole, index) =>
        val name = hole match
          case _: QuasiquoteHole.Term[?] => s"__qq_term_hole_$index"
          case _: QuasiquoteHole.ConstructedTypeSplice => s"__qq_type_hole_$index"
        builder.append(name)
        builder.append(parts(index + 1))
        PlaceholderBinding(name, hole)
      }
      Right(CategorizedPlaceholderSource(builder.result(), bindings.toVector))
