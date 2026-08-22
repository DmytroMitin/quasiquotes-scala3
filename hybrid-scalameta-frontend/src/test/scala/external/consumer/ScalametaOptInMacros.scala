package external.consumer

import scala.quoted.*

object ScalametaOptInMacros:
  inline def constructed: (Int, Int) = ${ constructedImpl }
  inline def matched: (Int, Int) = ${ matchedImpl }
  inline def generatedCaptureIsOriginal: Boolean = ${ generatedCaptureIsOriginalImpl }
  inline def currentDefaultControl: (Int, (Int, Int)) = ${ currentDefaultControlImpl }

  private def constructedImpl(using q: Quotes): Expr[(Int, Int)] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiquotes.*

    val supplied = '{ 7 }.asTerm
    val literal = qr"42".asExprOf[Int]
    val original = qr"$supplied".asInstanceOf[AnyRef].eq(supplied.asInstanceOf[AnyRef])
    '{ ($literal, ${ Expr(if original then 1 else 0) }) }

  private def matchedImpl(using q: Quotes): Expr[(Int, Int)] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.*

    '{ 20 + 22 }.asTerm match
      case qq"$left + $right" =>
        '{ (${ left.asExprOf[Int] }, ${ right.asExprOf[Int] }) }
      case _ => '{ (-1, -1) }

  private def generatedCaptureIsOriginalImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.*

    val generated = Literal(IntConstant(42))
    val captured = generated match
      case qq"$whole" => whole
      case _ => report.errorAndAbort("generated Scalameta opt-in match failed")
    Expr(captured.asInstanceOf[AnyRef].eq(generated.asInstanceOf[AnyRef]))

  private def currentDefaultControlImpl(using q: Quotes): Expr[(Int, (Int, Int))] =
    import q.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    import quasiquotes.matching.QuasiPattern.*

    val constructed = qr"42".asExprOf[Int]
    val captures = '{ 20 + 22 }.asTerm match
      case qq"$left + $right" =>
        '{ (${ left.asExprOf[Int] }, ${ right.asExprOf[Int] }) }
      case _ => '{ (-1, -1) }
    '{ ($constructed, $captures) }
