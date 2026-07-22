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
    bindings: Vector[PlaceholderBinding[T]],
    literalCategorizedNames: Set[String]
)

object PlaceholderSource:
  private val CategorizedNamePattern = "__qq_(?:term|type)_hole_[0-9]+(?:_[0-9]+)*".r

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
      val literalSource = parts.mkString
      val literalCategorizedNames = CategorizedNamePattern.findAllIn(literalSource).toSet
      var generatedNames = Set.empty[String]
      val bindings = holes.zipWithIndex.map { (hole, index) =>
        val baseName = hole match
          case _: QuasiquoteHole.Term[?] => s"__qq_term_hole_$index"
          case _: QuasiquoteHole.ConstructedTypeSplice => s"__qq_type_hole_$index"
        val name = freshCategorizedName(baseName, literalSource, generatedNames)
        generatedNames += name
        builder.append(name)
        builder.append(parts(index + 1))
        PlaceholderBinding(name, hole)
      }
      Right(CategorizedPlaceholderSource(builder.result(), bindings.toVector, literalCategorizedNames))

  private[construct] def isCategorizedName(name: String): Boolean =
    CategorizedNamePattern.matches(name)

  private def freshCategorizedName(baseName: String, literalSource: String, generatedNames: Set[String]): String =
    Iterator.from(0)
      .map(attempt => if attempt == 0 then baseName else s"${baseName}_$attempt")
      .find(candidate => !literalSource.contains(candidate) && !generatedNames.contains(candidate))
      .get
