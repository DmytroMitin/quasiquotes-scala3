package quasiquotes.types

import scala.quoted.*

/** Test-only evidence for the reflected-type transport decision. */
object ReflectedTypeTransportProbeMacros:
  inline def constructorFromTqr(value: String): Int =
    ${ constructorFromTqrImpl('value) }

  inline def typeTreeAndTypeEvidenceAgree[T]: Boolean =
    ${ typeTreeAndTypeEvidenceAgreeImpl[T] }

  private def constructorFromTqrImpl(value: Expr[String])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import QuasiTypequotes.tqr

    val reflected: q.reflect.TypeRepr = tqr"String"
    val transported: q.reflect.TypeRepr = directTransport(reflected)
    val constructor = Select.overloaded(
      New(Inferred(transported)),
      "<init>",
      Nil,
      value.asTerm :: Nil
    )
    Select.unique(constructor, "length").appliedToNone.asExprOf[Int]

  private def typeTreeAndTypeEvidenceAgreeImpl[T: Type](using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    val fromTree: q.reflect.TypeRepr = TypeTree.of[T].tpe
    val fromEvidence: q.reflect.TypeRepr = TypeRepr.of[T]
    Expr(directTransport(fromTree) =:= fromEvidence)

  private def directTransport(using q: Quotes)(
      reflected: q.reflect.TypeRepr
  ): q.reflect.TypeRepr = reflected
