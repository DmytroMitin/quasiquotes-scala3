package external.consumer

// snippet:lambda1-first-use:start
import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*
import quasiquotes.matching.QuasiPattern

object Lambda1FirstUseSnippet:
  inline def increment(value: Int): Int =
    ${ incrementImpl('value) }

  inline def alphaEquivalent: Boolean =
    ${ alphaEquivalentImpl }

  inline def freeReferenceDoesNotMatchBound(free: Int): Boolean =
    ${ freeReferenceDoesNotMatchBoundImpl('free) }

  inline def completeBodyHoleMatches: Boolean =
    ${ completeBodyHoleMatchesImpl }

  inline def preserveX(x: Int): Int => Int =
    ${ preserveXImpl('x) }

  private def incrementImpl(value: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*

    val fn = qr"(x: Int) => x + 1".asExprOf[Int => Int]
    '{ $fn($value) }

  private def alphaEquivalentImpl(using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    val target = '{ (renamed: Int) => renamed + 1 }
    Expr(QuasiPattern.termOrThrow("(x: Int) => x + 1").matchTerm(target.asTerm).isRight)

  private def freeReferenceDoesNotMatchBoundImpl(
      free: Expr[Int]
  )(using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    val target = '{ (_: Int) => $free }
    Expr(QuasiPattern.termOrThrow("(x: Int) => x").matchTerm(target.asTerm).isLeft)

  private def completeBodyHoleMatchesImpl(using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    val target = '{ (renamed: Int) => renamed + 1 }
    val matched = QuasiPattern.termOrThrow("(x: Int) => $body").matchTerm(target.asTerm)
    Expr(matched.exists(_.binding("body").nonEmpty))

  private def preserveXImpl(x: Expr[Int])(using Quotes): Expr[Int => Int] =
    import quotes.reflect.*

    val externalX = x.asTerm
    qr"(x: Int) => $externalX".asExprOf[Int => Int]
// snippet:lambda1-first-use:end
