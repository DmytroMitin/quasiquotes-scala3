package external.consumer

// snippet:type-interpolator-first-use:start
import scala.quoted.*

import quasiquotes.types.*
import quasiquotes.types.QuasiTypequotes.*

object TypeInterpolatorFirstUseSnippet:
  inline def constructionSummary: String = ${ constructionSummaryImpl }
  inline def captureSummary[T]: String = ${ captureSummaryImpl[T] }
  inline def zeroHoleMatches[T]: Boolean = ${ zeroHoleMatchesImpl[T] }
  inline def unsupportedTargetFallsThrough: Boolean = ${ unsupportedTargetFallsThroughImpl }
  inline def ordinaryApisCoexist: Boolean = ${ ordinaryApisCoexistImpl }

  private def constructionSummaryImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    val element: q.reflect.TypeRepr = TypeRepr.of[String]
    val constructed: q.reflect.TypeRepr = tqr"Either[Int, List[$element]]"
    Expr(TargetTypeReprInspector.inspect(constructed).fold(_.message, _.render))

  private def captureSummaryImpl[T: Type](using q: Quotes): Expr[String] =
    import q.reflect.*

    val target: q.reflect.TypeRepr = TypeRepr.of[T]
    target match
      case tqq"Either[$left, $right]" =>
        Expr(
          List(left, right)
            .map(TargetTypeReprInspector.inspect(_).fold(_.message, _.render))
            .mkString(" then ")
        )
      case _ => Expr("no-match")

  private def zeroHoleMatchesImpl[T: Type](using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    val target: q.reflect.TypeRepr = TypeRepr.of[T]
    Expr(target match
      case tqq"Int" => true
      case _ => false
    )

  private def unsupportedTargetFallsThroughImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    val target: q.reflect.TypeRepr = TypeRepr.of[Map[Int, String]]
    Expr(target match
      case tqq"$captured" => false
      case _ => true
    )

  private def ordinaryApisCoexistImpl(using Quotes): Expr[Boolean] =
    val constructionFunction
        : (String, Seq[(String, TypeNormalForm)]) => Either[TypeQuasiquoteError, ConstructedType] =
      tqr
    val patternFunction
        : String => Either[TypeQuasiquoteError, QuasiTypePattern] =
      tqq
    Expr(
      constructionFunction("Int", Seq.empty).isRight &&
        patternFunction("Int").isRight
    )
// snippet:type-interpolator-first-use:end
