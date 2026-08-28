package external.consumer

import scala.quoted.*

import quasiquotes.types.QuasiTypequotes.tqr

object ScalametaReflectedConstructorTypeMacros:
  inline def fromTypeRepr(capacity: Int): Int =
    ${ fromTypeReprImpl('capacity) }

  inline def fromTypeTreeTpe(capacity: Int): Int =
    ${ fromTypeTreeTpeImpl('capacity) }

  inline def fromTqr(capacity: Int): Int =
    ${ fromTqrImpl('capacity) }

  private def fromTypeReprImpl(capacity: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    buildCapacity(capacity, TypeRepr.of[java.lang.StringBuilder])

  private def fromTypeTreeTpeImpl(capacity: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    buildCapacity(capacity, TypeTree.of[java.lang.StringBuilder].tpe)

  private def fromTqrImpl(capacity: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    val constructorType: q.reflect.TypeRepr = tqr"java.lang.StringBuilder"
    buildCapacity(capacity, constructorType)

  private def buildCapacity(using q: Quotes)(
      capacity: Expr[Int],
      constructorType: q.reflect.TypeRepr
  ): Expr[Int] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiquotes.qr
    qr"new $constructorType(${capacity.asTerm}).capacity()".asExprOf[Int]
