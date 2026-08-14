package external.consumer

// snippet:qq-extractor-first-use:start
import scala.quoted.*

import quasiquotes.matching.QuasiPattern.*

object QqExtractorFirstUseSnippet:
  inline def splitAddition(expression: Int): (Int, Int) =
    ${ splitAdditionImpl('expression) }

  inline def isAddition(expression: Int): Boolean =
    ${ isAdditionImpl('expression) }

  inline def nestedMiddle(expression: Int): Int =
    ${ nestedMiddleImpl('expression) }

  inline def literalAndCapture(expression: Int): Int =
    ${ literalAndCaptureImpl('expression) }

  inline def malformedTemplate: Unit =
    ${ malformedTemplateImpl }

  private def splitAdditionImpl(
      expression: Expr[Int]
  )(using q: Quotes): Expr[(Int, Int)] =
    import q.reflect.*

    expression.asTerm match
      case qq"$left + $right" =>
        '{ (${ left.asExprOf[Int] }, ${ right.asExprOf[Int] }) }
      case _ => '{ (-1, -1) }

  private def isAdditionImpl(expression: Expr[Int])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    expression.asTerm match
      case qq"$left + $right" => Expr(true)
      case _ => Expr(false)

  private def nestedMiddleImpl(expression: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*

    expression.asTerm match
      case qq"($left + $middle) + $right" => middle.asExprOf[Int]
      case _ => Expr(-1)

  private def literalAndCaptureImpl(expression: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*

    expression.asTerm match
      case qq"qqCapture0 + $value" => value.asExprOf[Int]
      case _ => Expr(-1)

  private def malformedTemplateImpl(using q: Quotes): Expr[Unit] =
    import q.reflect.*

    Expr(1).asTerm match
      case qq"$value +" => '{ () }
      case _ => '{ () }
// snippet:qq-extractor-first-use:end
