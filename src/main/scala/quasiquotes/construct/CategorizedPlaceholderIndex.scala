package quasiquotes.construct

import dotty.tools.dotc.ast.untpd

private[construct] enum PlaceholderCategory derives CanEqual:
  case TermSplice
  case ConstructedTypeSplice

  def label: String =
    this match
      case TermSplice => "Term splice"
      case ConstructedTypeSplice => "Constructed-type splice"

private[construct] enum PlaceholderPosition derives CanEqual:
  case Term
  case ExpressionAscriptionType
  case UnsupportedType(context: String)
  case UnsupportedTerm(context: String)

  def invalidPhrase: String =
    this match
      case Term => "in term position"
      case ExpressionAscriptionType => "as the complete type of an expression ascription"
      case UnsupportedType(context) => s"inside $context"
      case UnsupportedTerm(context) => s"inside $context"

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
    UntypedPlaceholderTraversal.identifierNames(tree).flatMap(bindingsByName.get)

  def firstUnknownIn(tree: untpd.Tree): Option[String] =
    UntypedPlaceholderTraversal.identifierNames(tree).find(isUnknownCategorizedName)

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
    tree match
      case untpd.Ident(name) => name.toString :: Nil
      case untpd.Select(qualifier, _) => identifierNames(qualifier)
      case untpd.TypeApply(function, arguments) =>
        identifierNames(function) ++ arguments.flatMap(identifierNames)
      case untpd.Apply(function, arguments) =>
        identifierNames(function) ++ arguments.flatMap(identifierNames)
      case untpd.Typed(expression, typeTree) =>
        identifierNames(expression) ++ identifierNames(typeTree)
      case untpd.InfixOp(left, operator, right) =>
        identifierNames(left) ++ identifierNames(operator) ++ identifierNames(right)
      case untpd.Tuple(elements) => elements.flatMap(identifierNames)
      case untpd.Parens(inner) => identifierNames(inner)
      case untpd.TypedSplice(inner) => identifierNames(inner)
      case untpd.New(typeTree) => identifierNames(typeTree)
      case untpd.AppliedTypeTree(constructor, arguments) =>
        identifierNames(constructor) ++ arguments.flatMap(identifierNames)
      case untpd.SingletonTypeTree(reference) => identifierNames(reference)
      case untpd.ByNameTypeTree(result) => identifierNames(result)
      case _ => Nil
