package quasiquotes.types

import scala.quoted.*

object TypeInterpolatorMacros:
  inline def constructionSummary: String = ${ constructionSummaryImpl }
  inline def zeroHoleMatches[T]: Boolean = ${ zeroHoleMatchesImpl[T] }
  inline def oneCaptureSummary[T]: String = ${ oneCaptureSummaryImpl[T] }
  inline def twoCaptureSummary[T]: String = ${ twoCaptureSummaryImpl[T] }
  inline def originalSubtreeProvenance: Boolean = ${ originalSubtreeProvenanceImpl }
  inline def unsupportedTargetFallsThrough: Boolean = ${ unsupportedTargetFallsThroughImpl }
  inline def wildcardImportCompatibility: Boolean = ${ wildcardImportCompatibilityImpl }
  inline def selectiveImportCompatibility: Boolean = ${ selectiveImportCompatibilityImpl }
  inline def programmaticRepeatedHoleCompatibility: Boolean = ${ programmaticRepeatedHoleCompatibilityImpl }

  inline def malformedTqr: Unit = ${ malformedTqrImpl }
  inline def malformedTqq: Unit = ${ malformedTqqImpl }
  inline def unsupportedTqrTemplate: Unit = ${ unsupportedTqrTemplateImpl }
  inline def unsupportedTqrSplice: Unit = ${ unsupportedTqrSpliceImpl }
  inline def unsupportedTqqTemplate: Unit = ${ unsupportedTqqTemplateImpl }
  inline def constructorHoleTqq: Unit = ${ constructorHoleTqqImpl }
  inline def hostileTqrArity: Unit = ${ hostileTqrArityImpl }
  inline def nullTqrContext: Unit = ${ nullTqrContextImpl }
  inline def nullTqqContext: Unit = ${ nullTqqContextImpl }

  private def constructionSummaryImpl(using q: Quotes): Expr[String] =
    import q.reflect.*
    import QuasiTypequotes.*

    val element: q.reflect.TypeRepr = TypeRepr.of[String]
    val left: q.reflect.TypeRepr = TypeRepr.of[Int]
    val right: q.reflect.TypeRepr = TypeRepr.of[Boolean]
    val zero: q.reflect.TypeRepr = tqr"Int"
    val one: q.reflect.TypeRepr = tqr"List[$element]"
    val multiple: q.reflect.TypeRepr = tqr"Either[$left, $right]"
    Expr(List(zero, one, multiple).map(renderNormalForm).mkString(" | "))

  private def zeroHoleMatchesImpl[T: Type](using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import QuasiTypequotes.*

    val target: q.reflect.TypeRepr = TypeRepr.of[T]
    Expr(target match
      case tqq"Int" => true
      case _ => false
    )

  private def oneCaptureSummaryImpl[T: Type](using q: Quotes): Expr[String] =
    import q.reflect.*
    import QuasiTypequotes.*

    val target: q.reflect.TypeRepr = TypeRepr.of[T]
    target match
      case tqq"List[$argument]" => Expr(ownedNormalForm(argument))
      case _ => Expr("no-match")

  private def twoCaptureSummaryImpl[T: Type](using q: Quotes): Expr[String] =
    import q.reflect.*
    import QuasiTypequotes.*

    val target: q.reflect.TypeRepr = TypeRepr.of[T]
    target match
      case tqq"Either[$left, $right]" =>
        Expr(s"${ownedNormalForm(left)} then ${ownedNormalForm(right)}")
      case _ => Expr("no-match")

  private def originalSubtreeProvenanceImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import QuasiTypequotes.*

    val target: q.reflect.TypeRepr = TypeRepr.of[Either[List[Int], Option[String]]]
    val expected = target match
      case AppliedType(_, first :: second :: Nil) => (first, second)
      case _ => report.errorAndAbort("unexpected provenance proof target")
    Expr(target match
      case tqq"Either[$first, $second]" =>
        sameReference(first, expected._1) && sameReference(second, expected._2)
      case _ => false
    )

  private def unsupportedTargetFallsThroughImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import QuasiTypequotes.*

    val target: q.reflect.TypeRepr = TypeRepr.of[Map[Int, String]]
    Expr(target match
      case tqq"$captured" => false
      case _ => true
    )

  private def wildcardImportCompatibilityImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import QuasiTypequotes.*

    val constructionFunction
        : (String, Seq[(String, TypeNormalForm)]) => Either[TypeQuasiquoteError, ConstructedType] =
      tqr
    val patternFunction
        : String => Either[TypeQuasiquoteError, QuasiTypePattern] =
      tqq
    val constructed: q.reflect.TypeRepr = tqr"Int"
    val syntaxMatched = TypeRepr.of[Int] match
      case tqq"Int" => true
      case _ => false
    Expr(
      constructionFunction("Int", Seq.empty).isRight &&
        patternFunction("Int").isRight &&
        renderNormalForm(constructed) == "STypeIdent(Int)" &&
        syntaxMatched
    )

  private def selectiveImportCompatibilityImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import QuasiTypequotes.{tqq, tqr}

    val constructed: q.reflect.TypeRepr = tqr"Int"
    val ordinaryConstruction = tqr("Int")
    val ordinaryPattern = tqq("Int")
    val syntaxMatched = TypeRepr.of[Int] match
      case tqq"Int" => true
      case _ => false
    Expr(
      renderNormalForm(constructed) == "STypeIdent(Int)" &&
        ordinaryConstruction.isRight && ordinaryPattern.isRight && syntaxMatched
    )

  private def programmaticRepeatedHoleCompatibilityImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    val pattern = QuasiTypequotes.tqq("Either[$same, $same]").toOption.get
    val positive = pattern.matchTypeReprResult(TypeRepr.of[Either[Int, Int]]).isDefined
    val negative = pattern.matchTypeReprResult(TypeRepr.of[Either[Int, String]]).isEmpty
    Expr(positive && negative)

  private def malformedTqrImpl(using q: Quotes): Expr[Unit] =
    import q.reflect.*
    import QuasiTypequotes.*
    val _: q.reflect.TypeRepr = tqr"List["
    '{ () }

  private def malformedTqqImpl(using q: Quotes): Expr[Unit] =
    import q.reflect.*
    import QuasiTypequotes.*
    TypeRepr.of[Int] match
      case tqq"List[" => '{ () }
      case _ => '{ () }

  private def unsupportedTqrTemplateImpl(using q: Quotes): Expr[Unit] =
    import q.reflect.*
    import QuasiTypequotes.*
    val _: q.reflect.TypeRepr = tqr"Map[Int, String]"
    '{ () }

  private def unsupportedTqrSpliceImpl(using q: Quotes): Expr[Unit] =
    import q.reflect.*
    import QuasiTypequotes.*
    val unsupported: q.reflect.TypeRepr = TypeRepr.of[Map[Int, String]]
    val _: q.reflect.TypeRepr = tqr"List[$unsupported]"
    '{ () }

  private def unsupportedTqqTemplateImpl(using q: Quotes): Expr[Unit] =
    import q.reflect.*
    import QuasiTypequotes.*
    TypeRepr.of[Int] match
      case tqq"Map[Int, String]" => '{ () }
      case _ => '{ () }

  private def constructorHoleTqqImpl(using q: Quotes): Expr[Unit] =
    import q.reflect.*
    import QuasiTypequotes.*
    TypeRepr.of[List[Int]] match
      case tqq"$constructor[Int]" => '{ () }
      case _ => '{ () }

  private def hostileTqrArityImpl(using q: Quotes): Expr[Unit] =
    import q.reflect.*
    import QuasiTypequotes.*
    val _: q.reflect.TypeRepr = StringContext("List[", "]").tqr()
    '{ () }

  private def nullTqrContextImpl(using q: Quotes): Expr[Unit] =
    import q.reflect.*
    import QuasiTypequotes.*
    val context: StringContext = null
    val _: q.reflect.TypeRepr = context.tqr()
    '{ () }

  private def nullTqqContextImpl(using q: Quotes): Expr[Unit] =
    import q.reflect.*
    import QuasiTypequotes.*
    val context: StringContext = null
    TypeRepr.of[Int] match
      case context.tqq() => '{ () }
      case _ => '{ () }

  private def renderNormalForm(using q: Quotes)(value: q.reflect.TypeRepr): String =
    TargetTypeReprInspector.inspect(value).fold(_.message, _.render)

  private def ownedNormalForm(using q: Quotes)(value: q.reflect.TypeRepr): String =
    renderNormalForm(value)

  private def sameReference(using q: Quotes)(
      left: q.reflect.TypeRepr,
      right: q.reflect.TypeRepr
  ): Boolean =
    left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]
