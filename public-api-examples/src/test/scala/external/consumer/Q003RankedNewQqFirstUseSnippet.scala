package external.consumer

import scala.quoted.*

import quasiquotes.matching.QuasiPattern.*

final class Q003ExternalConstructor():
  def this(first: Int, second: Int, third: Int) = this()

object Q003RankedNewQqFirstUseSnippet:
  inline def captureThree: List[Int] = ${ captureThreeImpl }

  private def captureThreeImpl(using q: Quotes): Expr[List[Int]] =
    import q.reflect.*

    '{ new Q003ExternalConstructor(1, 2, 3) }.asTerm match
      case qq"new external.consumer.Q003ExternalConstructor(..$arguments)" =>
        val _: Seq[q.reflect.Term] = arguments
        Expr.ofList(arguments.toList.map(_.asExprOf[Int]))
      case _ => '{ Nil }
