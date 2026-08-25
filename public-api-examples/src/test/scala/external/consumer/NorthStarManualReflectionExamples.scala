package external.consumer

import scala.quoted.*

final case class NorthStarProduct(left: Int, right: Int)

object NorthStarManualReflectionExamples:
  inline def dynamicAppliedTypeArity: Int =
    ${ dynamicAppliedTypeArityImpl }

  inline def refinementAliasIsString: Boolean =
    ${ refinementAliasIsStringImpl }

  inline def constructExistingProduct[T](left: Int, right: Int): T =
    ${ constructExistingProductImpl[T]('left, 'right) }

  private def dynamicAppliedTypeArityImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*

    val constructor = TypeRepr.of[Either[Any, Any]] match
      case AppliedType(found, _) => found
      case other =>
        report.errorAndAbort(
          s"expected an applied Either type, obtained ${other.show}"
        )
    val arguments: List[TypeRepr] = List(TypeRepr.of[Int], TypeRepr.of[String])
    val applied = AppliedType(constructor, arguments)
    if !(applied =:= TypeRepr.of[Either[Int, String]]) then
      report.errorAndAbort(s"unexpected dynamic AppliedType result: ${applied.show}")
    Expr(arguments.length)

  private def refinementAliasIsStringImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    val members: List[(String, TypeRepr)] = List("Out" -> TypeRepr.of[String])
    val refined = members.foldLeft(TypeRepr.of[AnyRef]) {
      case (parent, (name, alias)) =>
        Refinement(parent, name, TypeBounds(alias, alias))
    }
    val expected = TypeRepr.of[AnyRef { type Out = String }]
    Expr((refined =:= expected) && (refined match
      case Refinement(parent, "Out", TypeBounds(lower, upper)) =>
        parent =:= TypeRepr.of[AnyRef] &&
          lower =:= TypeRepr.of[String] &&
          upper =:= TypeRepr.of[String]
      case _ => false
    ))

  private def constructExistingProductImpl[T: Type](
      left: Expr[Int],
      right: Expr[Int]
  )(using q: Quotes): Expr[T] =
    import q.reflect.*

    val existing = TypeRepr.of[T]
    val arguments: List[Term] = List(left.asTerm, right.asTerm)
    existing.asType match
      case '[t] =>
        Apply(
          Select(New(TypeTree.of[t]), existing.typeSymbol.primaryConstructor),
          arguments
        ).asExprOf[T]
