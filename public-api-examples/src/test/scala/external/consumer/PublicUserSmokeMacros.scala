package external.consumer

import scala.quoted.*

object PublicUserSmokeMacros:
  inline def add(left: Int, right: Int): Int =
    ${ addImpl('left, 'right) }

  inline def greeting(name: String): String =
    ${ greetingImpl('name) }

  inline def lambdaIdentity(value: Int): Int => Any =
    ${ lambdaIdentityImpl('value) }

  inline def lambdaPreservesOuter(outer: Int): Int => Any =
    ${ lambdaPreservesOuterImpl('outer) }

  private def addImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*

    qr"${left.asTerm} + ${right.asTerm}".asExprOf[Int]

  private def greetingImpl(name: Expr[String])(using Quotes): Expr[String] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*

    val nameTerm = name.asTerm
    qr"""s"hello $nameTerm"""".asExprOf[String]

  private def lambdaIdentityImpl(value: Expr[Int])(using Quotes): Expr[Int => Any] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*

    qr"(x: Int) => x".asExprOf[Int => Any]

  private def lambdaPreservesOuterImpl(outer: Expr[Int])(using Quotes): Expr[Int => Any] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*

    val outerTerm = outer.asTerm
    qr"(x: Int) => $outerTerm".asExprOf[Int => Any]
