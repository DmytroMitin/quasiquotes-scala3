package external.consumer

// snippet:readme-quick-start:start
import scala.quoted.*
import quasiquotes.construct.Quasiquotes.*

object ReadmeQuickStart:
  inline def add(left: Int, right: Int): Int =
    ${ addImpl('left, 'right) }

  private def addImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using Quotes): Expr[Int] =
    import quotes.reflect.*

    addImplTerm(left.asTerm, right.asTerm).asExprOf[Int]

  private def addImplTerm(using q: Quotes)(
      left: q.reflect.Term,
      right: q.reflect.Term
  ): q.reflect.Term =
    qr"$left + $right"
// snippet:readme-quick-start:end
