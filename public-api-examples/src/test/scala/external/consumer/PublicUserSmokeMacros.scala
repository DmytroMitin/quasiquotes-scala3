package external.consumer

import scala.quoted.*

object PublicUserSmokeMacros:
  inline def add(left: Int, right: Int): Int =
    ${ addImpl('left, 'right) }

  inline def greeting(name: String): String =
    ${ greetingImpl('name) }

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
