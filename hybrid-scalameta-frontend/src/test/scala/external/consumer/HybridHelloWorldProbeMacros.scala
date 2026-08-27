package external.consumer

import scala.quoted.*

object HybridHelloWorldProbeMacros:
  inline def currentBuildCurrentMatch(left: Int, right: Int): Int =
    ${ currentBuildCurrentMatchImpl('left, 'right) }

  inline def scalametaBuildScalametaMatch(left: Int, right: Int): Int =
    ${ scalametaBuildScalametaMatchImpl('left, 'right) }

  inline def currentBuildScalametaMatch(left: Int, right: Int): Int =
    ${ currentBuildScalametaMatchImpl('left, 'right) }

  inline def scalametaBuildCurrentMatch(left: Int, right: Int): Int =
    ${ scalametaBuildCurrentMatchImpl('left, 'right) }

  inline def scalametaFacadeBuildMatch(left: Int, right: Int): Int =
    ${ scalametaFacadeBuildMatchImpl('left, 'right) }

  private def currentBuildCurrentMatchImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import quasiquotes.construct.Quasiquotes.qr
    import quasiquotes.matching.QuasiPattern.qq

    val built = qr"${left.asTerm} + ${right.asTerm}"
    built match
      case qq"$capturedLeft + $capturedRight" =>
        '{ ${capturedLeft.asExprOf[Int]} + ${capturedRight.asExprOf[Int]} }
      case _ => report.errorAndAbort("Current qr/qq hello-world probe did not match.")

  private def scalametaBuildScalametaMatchImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq
    import quasiquotes.scalameta.ScalametaQuasiquotes.qr

    val built = qr"${left.asTerm} + ${right.asTerm}"
    built match
      case qq"$capturedLeft + $capturedRight" =>
        '{ ${capturedLeft.asExprOf[Int]} + ${capturedRight.asExprOf[Int]} }
      case _ => report.errorAndAbort("Scalameta qr/qq hello-world probe did not match.")

  private def currentBuildScalametaMatchImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import quasiquotes.construct.Quasiquotes.qr
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq

    val built = qr"${left.asTerm} + ${right.asTerm}"
    built match
      case qq"$capturedLeft + $capturedRight" =>
        '{ ${capturedLeft.asExprOf[Int]} + ${capturedRight.asExprOf[Int]} }
      case _ => report.errorAndAbort("Current-build/Scalameta-match probe did not match.")

  private def scalametaBuildCurrentMatchImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import quasiquotes.matching.QuasiPattern.qq
    import quasiquotes.scalameta.ScalametaQuasiquotes.qr

    val built = qr"${left.asTerm} + ${right.asTerm}"
    built match
      case qq"$capturedLeft + $capturedRight" =>
        '{ ${capturedLeft.asExprOf[Int]} + ${capturedRight.asExprOf[Int]} }
      case _ => report.errorAndAbort("Scalameta-build/current-match probe did not match.")

  private def scalametaFacadeBuildMatchImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import quasiquotes.scalameta.Quasiquotes.*

    val built = qr"${left.asTerm} + ${right.asTerm}"
    built match
      case qq"$capturedLeft + $capturedRight" =>
        val intType = TypeRepr.of[Int]
        val typeRoundTrip = tqr"List[$intType]" match
          case tqq"List[$captured]" => captured =:= intType
          case _ => false
        if !typeRoundTrip then report.errorAndAbort("Scalameta façade Type probe did not match.")
        '{ ${capturedLeft.asExprOf[Int]} + ${capturedRight.asExprOf[Int]} }
      case _ => report.errorAndAbort("Scalameta façade Term probe did not match.")
