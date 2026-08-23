package external.consumer

import scala.quoted.*

object ScalametaTypeOptInMacros:
  inline def constructedTypesAgree: Boolean = ${ constructedTypesAgreeImpl }
  inline def orderedCapturesAreOriginal: Boolean = ${ orderedCapturesAreOriginalImpl }
  inline def zeroCapturePatternMatches: Boolean = ${ zeroCapturePatternMatchesImpl }
  inline def currentDottyDefaultStillWorks: Boolean = ${ currentDottyDefaultStillWorksImpl }

  private def constructedTypesAgreeImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiquotes.*

    val left = TypeRepr.of[Int]
    val right = TypeRepr.of[String]
    val zero = tqr"Either[List[Int], Option[String]]"
    val spliced = tqr"Either[List[$left], Option[$right]]"
    val expected = TypeRepr.of[Either[List[Int], Option[String]]]
    Expr((zero =:= expected) && (spliced =:= expected))

  private def orderedCapturesAreOriginalImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.*

    val target = TypeRepr.of[Either[List[Int], Option[String]]]
    val expected = target match
      case AppliedType(_, left :: right :: Nil) =>
        val leftChild = left match
          case AppliedType(_, child :: Nil) => child
        val rightChild = right match
          case AppliedType(_, child :: Nil) => child
        (leftChild, rightChild)
    val captures = target match
      case tqq"Either[List[$left], Option[$right]]" => (left, right)
      case _ => report.errorAndAbort("Scalameta opt-in tqq did not match the admitted target")
    Expr(
      captures._1.asInstanceOf[AnyRef].eq(expected._1.asInstanceOf[AnyRef]) &&
        captures._2.asInstanceOf[AnyRef].eq(expected._2.asInstanceOf[AnyRef])
    )

  private def zeroCapturePatternMatchesImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.*

    val matched = TypeRepr.of[Either[List[Int], Option[String]]] match
      case tqq"Either[List[Int], Option[String]]" => true
      case _ => false
    Expr(matched)

  private def currentDottyDefaultStillWorksImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.types.QuasiTypequotes.*

    val left = TypeRepr.of[Int]
    val constructed = tqr"Either[List[$left], Option[String]]"
    val expected = TypeRepr.of[Either[List[Int], Option[String]]]
    val matched = expected match
      case tqq"Either[List[$captured], Option[String]]" =>
        captured.asInstanceOf[AnyRef].eq(left.asInstanceOf[AnyRef]) || captured =:= left
      case _ => false
    Expr((constructed =:= expected) && matched)
