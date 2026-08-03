package quasiquotes.construct

import dotty.tools.dotc.ast.untpd

import quasiquotes.parser.DottySourceSpanAdapter
import quasiquotes.source.SourceSpan

private[construct] final case class PlaceholderOccurrence[+T](
    binding: PlaceholderBinding[T],
    generatedSpan: Option[SourceSpan]
)

private[construct] final case class UnknownPlaceholderOccurrence(
    name: String,
    generatedSpan: Option[SourceSpan]
)

private[construct] final case class UntypedIdentifierOccurrence(
    name: String,
    generatedSpan: Option[SourceSpan]
)

private[construct] final class CategorizedPlaceholderIndex[T](
    val bindings: Vector[PlaceholderBinding[T]],
    literalCategorizedNames: Set[String] = Set.empty
):
  private val bindingsByName = bindings.map(binding => binding.name -> binding).toMap

  require(bindingsByName.size == bindings.size, "Categorized placeholder binding names must be unique")

  def lookup(name: String): Option[PlaceholderBinding[T]] =
    bindingsByName.get(name)

  def resolve(
      name: String,
      expected: PlaceholderCategory,
      position: PlaceholderPosition
  ): Either[QuasiquoteError, Option[PlaceholderBinding[T]]] =
    lookup(name) match
      case Some(binding) if categoryOf(binding.hole) == expected => Right(Some(binding))
      case Some(binding) =>
        Left(
          QuasiquoteError.PlaceholderCategoryMismatch(
            name = name,
            actual = categoryOf(binding.hole),
            position = position
          )
        )
      case None if isUnknownCategorizedName(name) => Left(QuasiquoteError.UnknownPlaceholder(name))
      case None => Right(None)

  def findIn(tree: untpd.Tree): List[PlaceholderBinding[T]] =
    findOccurrences(tree).map(_.binding)

  def findOccurrences(tree: untpd.Tree): List[PlaceholderOccurrence[T]] =
    UntypedPlaceholderTraversal.identifierOccurrences(tree).flatMap { occurrence =>
      bindingsByName.get(occurrence.name).map(PlaceholderOccurrence(_, occurrence.generatedSpan))
    }

  def firstUnknownIn(tree: untpd.Tree): Option[String] =
    firstUnknownOccurrence(tree).map(_.name)

  def firstUnknownOccurrence(tree: untpd.Tree): Option[UnknownPlaceholderOccurrence] =
    UntypedPlaceholderTraversal.identifierOccurrences(tree).collectFirst {
      case occurrence if isUnknownCategorizedName(occurrence.name) =>
        UnknownPlaceholderOccurrence(occurrence.name, occurrence.generatedSpan)
    }

  def categoryOf(hole: QuasiquoteHole[T]): PlaceholderCategory =
    hole match
      case _: QuasiquoteHole.Term[?] => PlaceholderCategory.TermSplice
      case _: QuasiquoteHole.ConstructedTypeSplice => PlaceholderCategory.ConstructedTypeSplice

  private def isUnknownCategorizedName(name: String): Boolean =
    PlaceholderSource.isCategorizedName(name) &&
      !bindingsByName.contains(name) &&
      !literalCategorizedNames.contains(name)

private[construct] object UntypedPlaceholderTraversal:
  def identifierNames(tree: untpd.Tree): List[String] =
    identifierOccurrences(tree).map(_.name)

  def identifierOccurrences(tree: untpd.Tree): List[UntypedIdentifierOccurrence] =
    tree match
      case ident @ untpd.Ident(name) => UntypedIdentifierOccurrence(name.toString, DottySourceSpanAdapter.fromTree(ident)) :: Nil
      case untpd.Select(qualifier, _) => identifierOccurrences(qualifier)
      case untpd.TypeApply(function, arguments) =>
        identifierOccurrences(function) ++ arguments.flatMap(identifierOccurrences)
      case untpd.Apply(function, arguments) =>
        identifierOccurrences(function) ++ arguments.flatMap(identifierOccurrences)
      case untpd.Typed(expression, typeTree) =>
        identifierOccurrences(expression) ++ identifierOccurrences(typeTree)
      case untpd.InfixOp(left, operator, right) =>
        identifierOccurrences(left) ++ identifierOccurrences(operator) ++ identifierOccurrences(right)
      case untpd.Tuple(elements) => elements.flatMap(identifierOccurrences)
      case untpd.Parens(inner) => identifierOccurrences(inner)
      case untpd.TypedSplice(inner) => identifierOccurrences(inner)
      case untpd.New(typeTree) => identifierOccurrences(typeTree)
      case untpd.AppliedTypeTree(constructor, arguments) =>
        identifierOccurrences(constructor) ++ arguments.flatMap(identifierOccurrences)
      case untpd.SingletonTypeTree(reference) => identifierOccurrences(reference)
      case untpd.ByNameTypeTree(result) => identifierOccurrences(result)
      case _ => Nil
