package external.consumer

// snippet:p1-block-first-use:start
import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*
import quasiquotes.matching.QuasiPattern.*

object P1BlockFirstUseSnippet:
  inline def ordered(inline first: Unit, inline second: Unit, inline result: Int): Int =
    ${ orderedImpl('first, 'second, 'result) }

  inline def capture(inline expression: Int): (Int, Int, Int) =
    ${ captureImpl('expression) }

  private def orderedImpl(
      first: Expr[Unit],
      second: Expr[Unit],
      result: Expr[Int]
  )(using q: Quotes): Expr[Int] =
    import q.reflect.*

    qr"{ ${first.asTerm}; ${second.asTerm}; ${result.asTerm} }".asExprOf[Int]

  private def captureImpl(
      expression: Expr[Int]
  )(using q: Quotes): Expr[(Int, Int, Int)] =
    import q.reflect.*

    expression.asTerm match
      case qq"{ $first; $second; $result }" =>
        '{ (${first.asExprOf[Int]}, ${second.asExprOf[Int]}, ${result.asExprOf[Int]}) }
      case _ => '{ (-1, -1, -1) }
// snippet:p1-block-first-use:end
