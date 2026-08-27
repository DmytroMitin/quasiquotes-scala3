package external.consumer

// snippet:frontend-first-use:start
import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*
import quasiquotes.matching.QuasiPattern
import quasiquotes.types.{
  ConstructedType,
  QuasiTypeConstruct,
  TypeNormalForm,
  TypeNormalFormSource,
  TypePatternSource,
  TypeQuasiquoteError,
  toTypeRepr
}

object FrontendFirstUseSnippet:
  val parsed = TypeNormalFormSource.fromSource(
    "Either[List[Int], Option[String]]"
  )
  val pattern = TypePatternSource.fromSource(
    "Either[List[$left], Option[$right]]"
  )
  val constructed = QuasiTypeConstruct.fromTemplate(
    "Either[List[$left], Option[$right]]",
    "left" -> TypeNormalForm.STypeIdent("Int"),
    "right" -> TypeNormalForm.STypeIdent("String")
  )
  val termPattern = QuasiPattern.term("$left + $right")
  val constructorPattern = QuasiPattern.term(
    "new java.lang.StringBuilder($capacity)"
  )

  inline def add(left: Int, right: Int): Int =
    ${ addImpl('left, 'right) }

  inline def capacity(value: Int): Int =
    ${ capacityImpl('value) }

  private def addImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using Quotes): Expr[Int] =
    import quotes.reflect.*

    qr"${left.asTerm} + ${right.asTerm}".asExprOf[Int]

  private def capacityImpl(value: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*

    qr"new java.lang.StringBuilder(${value.asTerm}).capacity()".asExprOf[Int]

  private def lowerInsideMacro(
      value: ConstructedType
  )(using q: Quotes): Either[TypeQuasiquoteError, q.reflect.TypeRepr] =
    value.toTypeRepr
// snippet:frontend-first-use:end
