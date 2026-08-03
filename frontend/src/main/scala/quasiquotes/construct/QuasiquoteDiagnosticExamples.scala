package quasiquotes.construct

import scala.quoted.*

import quasiquotes.parser.{DottySourceSpanAdapter, TinyTermParser}
import quasiquotes.source.*
import quasiquotes.types.{ConstructedType, TypeNormalForm}

object QuasiquoteDiagnosticExamples:
  inline def locatedLoweringSummary: List[String] = ${ locatedLoweringSummaryImpl }

  inline def locatedBuilderSummary: List[String] = ${ locatedBuilderSummaryImpl }

  inline def positionResolverSummary(inline value: Int): String =
    ${ positionResolverSummaryImpl('value) }

  inline def validQuasiquote(inline value: Int): Int =
    ${ validQuasiquoteImpl('value) }

  inline def invalidTermInType(inline value: Int): Any =
    ${ invalidTermInTypeImpl('value) }

  inline def invalidConstructedTypeInTerm: Any =
    ${ invalidConstructedTypeInTermImpl }

  inline def invalidLiteralParse: Any =
    ${ invalidLiteralParseImpl }

  private def locatedLoweringSummaryImpl(using Quotes): Expr[List[String]] =
    import quotes.reflect.*

    val intTerm = '{ 1 }.asTerm
    val constructedInt = ConstructedType(TypeNormalForm.STypeIdent("Int"))
    val constructedAnyVal = ConstructedType(TypeNormalForm.STypeIdent("AnyVal"))

    val termInType = loweringFailure(
      "(__qq_term_hole_0: __qq_term_hole_1)",
      Vector(
        PlaceholderBinding("__qq_term_hole_0", QuasiquoteHole.Term(intTerm)),
        PlaceholderBinding("__qq_term_hole_1", QuasiquoteHole.Term(intTerm))
      )
    )
    val typeInTerm = loweringFailure(
      "__qq_type_hole_0",
      Vector(PlaceholderBinding("__qq_type_hole_0", QuasiquoteHole.ConstructedTypeSplice(constructedInt)))
    )
    val unknown = loweringFailure("__qq_type_hole_99", Vector.empty)
    val unsupportedTypePosition = loweringFailure(
      "identity[__qq_type_hole_0](1)",
      Vector(PlaceholderBinding("__qq_type_hole_0", QuasiquoteHole.ConstructedTypeSplice(constructedInt)))
    )
    val typeLowering = loweringFailure(
      "(1: __qq_type_hole_0)",
      Vector(PlaceholderBinding("__qq_type_hole_0", QuasiquoteHole.ConstructedTypeSplice(constructedAnyVal)))
    )
    val genericSource = "value match { case x => x }"
    val genericParsed = TinyTermParser.parseOrThrow(genericSource)
    val generic = ParsedTermLowerer.lowerLocated(genericParsed.rawTree, Vector.empty).swap.toOption.get
    val genericTreeSpan = DottySourceSpanAdapter.fromTree(genericParsed.rawTree)
    val legacySame =
      ParsedTermLowerer.lower(genericParsed.rawTree, Vector.empty).swap.toOption.contains(generic.error)

    Expr.ofList(
      List(
        Expr(renderFailure("term-in-type", termInType)),
        Expr(renderFailure("type-in-term", typeInTerm)),
        Expr(renderFailure("unknown", unknown)),
        Expr(renderFailure("unsupported-type-position", unsupportedTypePosition)),
        Expr(renderFailure("type-lowering", typeLowering)),
        Expr(s"generic|same-span=${generic.generatedSpan == genericTreeSpan}|has-span=${generic.generatedSpan.nonEmpty}"),
        Expr(s"legacy|same-error=$legacySame")
      )
    )

  private def locatedBuilderSummaryImpl(using Quotes): Expr[List[String]] =
    import quotes.reflect.*

    val firstTerm = '{ 1 }.asTerm
    val secondTerm = '{ 2 }.asTerm
    val constructedInt = ConstructedType(TypeNormalForm.STypeIdent("Int"))

    val termFailure = QuasiquoteBuilder
      .buildLocated(Seq("(", ": ", ")"), Seq(firstTerm, secondTerm))
      .swap.toOption.get
    val constructedFailure = QuasiquoteBuilder
      .buildLocated(Seq("", ""), Seq(QuasiTypeSplices.typeSplice(constructedInt)))
      .swap.toOption.get
    val parseFailure = QuasiquoteBuilder
      .buildLocated(Seq("foo; bar"), Seq.empty)
      .swap.toOption.get
    val legacyTermFailure = QuasiquoteBuilder
      .build(Seq("(", ": ", ")"), Seq(firstTerm, secondTerm))
      .swap.toOption.get
    val locatedSuccess = QuasiquoteBuilder.buildLocated(Seq("", " + 1"), Seq(secondTerm)).toOption.get
    val legacySuccess = QuasiquoteBuilder.build(Seq("", " + 1"), Seq(secondTerm)).toOption.get
    val sameSuccess =
      locatedSuccess.show(using Printer.TreeStructure) == legacySuccess.show(using Printer.TreeStructure)

    Expr.ofList(
      List(
        Expr(s"term|${renderOrigins(termFailure.location)}"),
        Expr(s"constructed|${renderOrigins(constructedFailure.location)}"),
        Expr(s"parse|${renderOrigins(parseFailure.location)}|${parseFailure.error.message}"),
        Expr(s"legacy|same-error=${legacyTermFailure == termFailure.error}"),
        Expr(s"success|same-tree=$sameSuccess")
      )
    )

  private def positionResolverSummaryImpl(value: Expr[Int])(using Quotes): Expr[String] =
    import quotes.reflect.*

    val term = value.asTerm
    val termPosition = term.pos
    val selected = MacroDiagnosticPositionResolver.resolve(
      MacroDiagnosticAnchor.TermInterpolationArgument(0),
      Seq(term)
    )
    val fallback = MacroDiagnosticPositionResolver.resolve(MacroDiagnosticAnchor.MacroExpansion, Seq(term))
    val invalid = MacroDiagnosticPositionResolver.resolve(
      MacroDiagnosticAnchor.TermInterpolationArgument(99),
      Seq(term)
    )
    val negative = MacroDiagnosticPositionResolver.resolve(
      MacroDiagnosticAnchor.TermInterpolationArgument(-1),
      Seq(term)
    )
    val typeFallback = MacroDiagnosticPositionResolver.resolve(
      MacroDiagnosticAnchor.TermInterpolationArgument(0),
      Seq(QuasiTypeSplices.typeSplice(ConstructedType(TypeNormalForm.STypeIdent("Int"))))
    )

    Expr(
      s"term-selected=${samePosition(selected, termPosition)} " +
        s"term-valid=${termPosition.start >= 0 && termPosition.end >= termPosition.start} " +
        s"invalid-fallback=${samePosition(invalid, fallback)} " +
        s"negative-fallback=${samePosition(negative, fallback)} " +
        s"type-fallback=${samePosition(typeFallback, fallback)}"
    )

  private def validQuasiquoteImpl(value: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import Quasiquotes.*

    val term = value.asTerm
    qr"$term + 1".asExprOf[Int]

  private def invalidTermInTypeImpl(value: Expr[Int])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    import Quasiquotes.*

    val expression = '{ 1 }.asTerm
    val typeArgument = value.asTerm
    qr"($expression: $typeArgument)".asExpr

  private def invalidConstructedTypeInTermImpl(using Quotes): Expr[Any] =
    import quotes.reflect.*
    import Quasiquotes.*
    import QuasiTypeSplices.typeSplice

    val constructed = ConstructedType(TypeNormalForm.STypeIdent("Int"))
    qr"${typeSplice(constructed)}".asExpr

  private def invalidLiteralParseImpl(using Quotes): Expr[Any] =
    import quotes.reflect.*
    import Quasiquotes.*

    qr"foo; bar".asExpr

  private def loweringFailure(using Quotes)(
      source: String,
      bindings: Vector[PlaceholderBinding[quotes.reflect.Term]]
  ): QuasiquoteLoweringFailure =
    val parsed = TinyTermParser.parseOrThrow(source)
    ParsedTermLowerer.lowerLocated(parsed.rawTree, bindings).swap.toOption.get

  private def renderFailure(label: String, failure: QuasiquoteLoweringFailure): String =
    s"$label|${failure.error.message}|${failure.generatedSpan.map(span => s"[${span.start},${span.end})").getOrElse("none")}"

  private def renderOrigins(location: Option[DiagnosticLocation]): String =
    location.toVector.flatMap(_.origins).map {
      case SourceOrigin.InterpolationArgument(_, index, category) => s"arg:$index:$category"
      case SourceOrigin.LiteralPart(_, index, span) => s"literal:$index:[${span.start},${span.end})"
      case SourceOrigin.OriginalText(sourceId, span) => s"original:${sourceId.value}:[${span.start},${span.end})"
      case SourceOrigin.RewrittenHole(_, span, name, role) => s"hole:$name:$role:[${span.start},${span.end})"
    }.mkString(",")

  private def samePosition(using q: Quotes)(left: q.reflect.Position, right: q.reflect.Position): Boolean =
    left.start == right.start && left.end == right.end
