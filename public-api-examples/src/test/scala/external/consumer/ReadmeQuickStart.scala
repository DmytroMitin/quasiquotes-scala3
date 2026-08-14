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

    qr"${left.asTerm} + ${right.asTerm}".asExprOf[Int]
// snippet:readme-quick-start:end
