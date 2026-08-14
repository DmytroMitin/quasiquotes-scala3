package quasiquotes.matching

import scala.quoted.*

object TermPatternExtractorMacros:
  inline def orderedCapture(expression: Int): (Int, Int) =
    ${ orderedCaptureImpl('expression) }

  inline def matchesPlus(expression: Int): Boolean =
    ${ matchesPlusImpl('expression) }

  inline def nestedCapture(expression: Int): Int =
    ${ nestedCaptureImpl('expression) }

  inline def literalAndCapture(expression: Int): Int =
    ${ literalAndCaptureImpl('expression) }

  inline def malformedTemplate: Unit =
    ${ malformedTemplateImpl }

  private def orderedCaptureImpl(expression: Expr[Int])(using q: Quotes): Expr[(Int, Int)] =
    import q.reflect.*
    import QuasiPattern.*

    expression.asTerm match
      case qq"$left + $right" =>
        '{ (${ left.asExprOf[Int] }, ${ right.asExprOf[Int] }) }
      case _ => '{ (-1, -1) }

  private def matchesPlusImpl(expression: Expr[Int])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import QuasiPattern.*

    expression.asTerm match
      case qq"$left + $right" => Expr(true)
      case _ => Expr(false)

  private def nestedCaptureImpl(expression: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import QuasiPattern.*

    expression.asTerm match
      case qq"f(g($value))" => value.asExprOf[Int]
      case _ => Expr(-1)

  private def literalAndCaptureImpl(expression: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import QuasiPattern.*

    expression.asTerm match
      case qq"qqCapture0 + $value" => value.asExprOf[Int]
      case _ => Expr(-1)

  private def malformedTemplateImpl(using q: Quotes): Expr[Unit] =
    import q.reflect.*
    import QuasiPattern.*

    Expr(1).asTerm match
      case qq"$value +" => '{ () }
      case _ => '{ () }
