package external.consumer

import scala.quoted.*

object ScalametaOptInMacros:
  inline def constructed: (Int, Int) = ${ constructedImpl }
  inline def matched: (Int, Int) = ${ matchedImpl }
  inline def generatedCaptureIsOriginal: Boolean = ${ generatedCaptureIsOriginalImpl }
  inline def blockConstructionPreservesChildren: Boolean = ${ blockConstructionPreservesChildrenImpl }
  inline def blockCapturesAreOriginal: Boolean = ${ blockCapturesAreOriginalImpl }
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

  private def blockConstructionPreservesChildrenImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiquotes.*

    val prefix = Literal(IntConstant(1))
    val result = Literal(IntConstant(2))
    val constructed = qr"{ $prefix; $result }"
    val preserved = constructed match
      case Block((first: Term) :: Nil, last) =>
        first.asInstanceOf[AnyRef].eq(prefix.asInstanceOf[AnyRef]) &&
          last.asInstanceOf[AnyRef].eq(result.asInstanceOf[AnyRef])
      case _ => false
    Expr(preserved)

  private def blockCapturesAreOriginalImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.*

    val prefix = Literal(IntConstant(1))
    val result = Literal(IntConstant(2))
    val generated = Block(List(prefix), result)
    generated match
      case qq"{ $capturedPrefix; $capturedResult }" =>
        Expr(
          capturedPrefix.asInstanceOf[AnyRef].eq(prefix.asInstanceOf[AnyRef]) &&
            capturedResult.asInstanceOf[AnyRef].eq(result.asInstanceOf[AnyRef])
        )
      case _ => Expr(false)

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
