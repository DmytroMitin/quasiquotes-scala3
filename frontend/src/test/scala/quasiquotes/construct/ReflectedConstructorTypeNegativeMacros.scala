package quasiquotes.construct

import scala.quoted.*

import quasiquotes.types.{ConstructedType, TypeNormalForm}

object ReflectedConstructorTypeNegativeMacros:
  inline def reflectedInTermPosition: Int =
    ${ reflectedInTermPositionImpl }

  inline def reflectedInAscriptionPosition: Int =
    ${ reflectedInAscriptionPositionImpl }

  inline def reflectedInMethodTypePosition: Int =
    ${ reflectedInMethodTypePositionImpl }

  inline def reflectedInPartialConstructorPath: Int =
    ${ reflectedInPartialConstructorPathImpl }

  inline def reflectedInAppliedConstructorType: Int =
    ${ reflectedInAppliedConstructorTypeImpl }

  inline def termInConstructorTypePosition: Int =
    ${ termInConstructorTypePositionImpl }

  inline def constructedTypeInConstructorTypePosition: Int =
    ${ constructedTypeInConstructorTypePositionImpl }

  inline def nonInstantiableReflectedType: Runnable =
    ${ nonInstantiableReflectedTypeImpl }

  private def reflectedInTermPositionImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import Quasiquotes.*
    val constructorType = TypeRepr.of[java.lang.StringBuilder]
    qr"$constructorType"
    Expr(0)

  private def reflectedInAscriptionPositionImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import Quasiquotes.*
    val constructorType = TypeRepr.of[java.lang.StringBuilder]
    qr"(${Expr(0).asTerm}: $constructorType)"
    Expr(0)

  private def reflectedInMethodTypePositionImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import Quasiquotes.*
    val methodType = TypeRepr.of[Int]
    qr"identity[$methodType](${Expr(0).asTerm})"
    Expr(0)

  private def reflectedInPartialConstructorPathImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import Quasiquotes.*
    val constructorType = TypeRepr.of[java.lang.StringBuilder]
    qr"new java.lang.$constructorType(${Expr(1).asTerm})"
    Expr(0)

  private def reflectedInAppliedConstructorTypeImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import Quasiquotes.*
    val constructorType = TypeRepr.of[java.lang.StringBuilder]
    qr"new $constructorType[Int](${Expr(1).asTerm})"
    Expr(0)

  private def termInConstructorTypePositionImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import Quasiquotes.*
    val notAType = Expr("java.lang.StringBuilder").asTerm
    qr"new $notAType(${Expr(1).asTerm})"
    Expr(0)

  private def constructedTypeInConstructorTypePositionImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import Quasiquotes.*
    val constructed = ConstructedType(TypeNormalForm.STypeIdent("Int"))
    val compilerFreeType = QuasiTypeSplices.typeSplice(constructed)
    qr"new $compilerFreeType(${Expr(1).asTerm})"
    Expr(0)

  private def nonInstantiableReflectedTypeImpl(using q: Quotes): Expr[Runnable] =
    import q.reflect.*
    import Quasiquotes.*
    val interfaceType = TypeRepr.of[Runnable]
    qr"new $interfaceType()".asExprOf[Runnable]
