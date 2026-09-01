package external.consumer

import scala.quoted.*

import quasiquotes.matching.QuasiPattern.*

private object Q002RankedQqFirstUseTarget:
  def three(first: Int, second: Int, third: Int): Int = first + second + third

object Q002RankedQqFirstUseSnippet:
  inline def captureThree: List[Int] = ${ captureThreeImpl }

  private def captureThreeImpl(using q: Quotes): Expr[List[Int]] =
    import q.reflect.*

    '{ Q002RankedQqFirstUseTarget.three(1, 2, 3) }.asTerm match
      case qq"$function(..$arguments)" =>
        val _: q.reflect.Term = function
        val _: Seq[q.reflect.Term] = arguments
        Expr.ofList(arguments.toList.map(_.asExprOf[Int]))
      case _ => '{ Nil }
