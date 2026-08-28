package quasiquotes.construct

import scala.quoted.*

import quasiquotes.types.QuasiTypequotes.*

object ReflectedConstructorTypeMacros:
  inline def fromTypeRepr(capacity: Int): Int =
    ${ fromTypeReprImpl('capacity) }

  inline def fromTypeTreeTpe(capacity: Int): Int =
    ${ fromTypeTreeTpeImpl('capacity) }

  inline def fromTqr(capacity: Int): Int =
    ${ fromTqrImpl('capacity) }

  private def fromTypeReprImpl(capacity: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    val constructorType: q.reflect.TypeRepr =
      q.reflect.TypeRepr.of[java.lang.StringBuilder]
    buildCapacity(capacity, constructorType)

  private def fromTypeTreeTpeImpl(capacity: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    val constructorType: q.reflect.TypeRepr =
      q.reflect.TypeTree.of[java.lang.StringBuilder].tpe
    buildCapacity(capacity, constructorType)

  private def fromTqrImpl(capacity: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    val constructorType: q.reflect.TypeRepr =
      tqr"java.lang.StringBuilder"
    buildCapacity(capacity, constructorType)

  private def buildCapacity(using q: Quotes)(
      capacity: Expr[Int],
      constructorType: q.reflect.TypeRepr
  ): Expr[Int] =
    import q.reflect.*
    import Quasiquotes.*
    qr"new $constructorType(${capacity.asTerm}).capacity()".asExprOf[Int]
