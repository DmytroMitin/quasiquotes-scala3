package quasiquotes.types

import quasiquotes.source.*

class LocatedTypeDiagnosticTest extends munit.FunSuite:
  private val intForm = TypeNormalForm.STypeIdent("Int")

  test("located type-pattern success and matching preserve legacy behavior") {
    val source = "List[$t]"
    val located = TypePatternSource.fromSourceLocated(source)

    assertEquals(located.left.map(_.diagnostic), TypePatternSource.fromSource(source))
    assertEquals(
      located.toOption.flatMap(TypePattern.matchNormalForm(_, TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("List"), List(intForm)))),
      Some(TypeMatchResult(Map("t" -> intForm)))
    )
  }

  test("type-pattern parse diagnostics map structured spans through original text") {
    val source = "Int)"
    val located = TypePatternSource.fromSourceLocated(source).swap.toOption.get

    assertEquals(located.diagnostic, TypePatternSource.fromSource(source).swap.toOption.get)
    assertEquals(located.location.map(_.sourceId), Some(SourceId.VirtualTypePatternParserInput))
    assertEquals(located.location.map(_.span.end), Some(source.length))
    assertEquals(located.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))
    assert(located.location.exists(location => location.span.start > 0 && !location.span.isEmpty))
  }

  test("type-pattern shape failures keep exact messages and conservative whole-pattern locations") {
    val selected = "scala.Int"
    val selectedLocated = TypePatternSource.fromSourceLocated(selected).swap.toOption.get
    assertEquals(
      selectedLocated.diagnostic.message,
      "Selected type syntax `scala.Int` is not supported; use unqualified `Int` in the current experimental surface."
    )
    assertEquals(selectedLocated.location.map(_.span), Some(SourceSpan(0, selected.length)))
    assertEquals(selectedLocated.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))

    val repeatedSource = "($t, $t, $t, $t)"
    val mapped = TypePattern.rewriteSourceMapped(repeatedSource)
    val repeatedLocated = TypePatternSource.fromSourceLocated(repeatedSource).swap.toOption.get
    val repeatedOrigins = repeatedLocated.location.toVector.flatMap(_.origins).collect {
      case origin: SourceOrigin.RewrittenHole => origin.originalSpan
    }
    assertEquals(repeatedLocated.location.map(_.span), Some(SourceSpan(0, mapped.generatedSource.length)))
    assertEquals(repeatedLocated.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
    assertEquals(
      repeatedOrigins,
      Vector(SourceSpan(1, 3), SourceSpan(5, 7), SourceSpan(9, 11), SourceSpan(13, 15))
    )
    assertEquals(repeatedLocated.diagnostic.message, "Unsupported tuple arity for type-pattern construction: expected 2 or 3 elements, but found 4.")
  }

  test("type-template located failures use distinct role and source identity") {
    val source = "($t, $t, $t, $t)"
    val mapped = TypeTemplate.rewriteSourceMapped(source)
    val located = TypeTemplateSource.fromSourceLocated(source).swap.toOption.get
    val rewritten = located.location.toVector.flatMap(_.origins).collect {
      case origin: SourceOrigin.RewrittenHole => origin
    }

    assertEquals(located.location.map(_.sourceId), Some(SourceId.VirtualTypeTemplateParserInput))
    assertEquals(located.location.map(_.span), Some(SourceSpan(0, mapped.generatedSource.length)))
    assertEquals(located.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
    assertEquals(rewritten.map(_.role).distinct, Vector(HoleRole.TypeTemplate))
    assertEquals(located.diagnostic.message, "Unsupported tuple arity for type-template construction: expected 2 or 3 elements, but found 4.")
    assertEquals(TypeTemplateSource.fromSource(source).swap.toOption, Some(located.diagnostic))
  }

  test("Tuple3 and Function2 mapped holes retain ordered distinct origins") {
    val tuple = TypePattern.rewriteSourceMapped("($a, $b, $c)")
    val function = TypeTemplate.rewriteSourceMapped("($a, $b) => $r")

    assertEquals(
      tuple.occurrences.map(occurrence => occurrence.name -> occurrence.originalSpan),
      Vector(
        "a" -> SourceSpan(1, 3),
        "b" -> SourceSpan(5, 7),
        "c" -> SourceSpan(9, 11)
      )
    )
    assertEquals(
      function.occurrences.map(occurrence => occurrence.name -> occurrence.originalSpan),
      Vector(
        "a" -> SourceSpan(1, 3),
        "b" -> SourceSpan(5, 7),
        "r" -> SourceSpan(12, 14)
      )
    )
  }

  test("type-template parse diagnostics and selected-shape messages remain compatible") {
    val parseSource = "Int)"
    val parsed = TypeTemplateSource.fromSourceLocated(parseSource).swap.toOption.get
    assertEquals(parsed.location.map(_.sourceId), Some(SourceId.VirtualTypeTemplateParserInput))
    assertEquals(parsed.location.map(_.span.end), Some(parseSource.length))
    assertEquals(parsed.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))
    assertEquals(TypeTemplateSource.fromSourceLocated("Int String").swap.toOption.flatMap(_.location), None)

    val selected = TypeTemplateSource.fromSourceLocated("scala.Int").swap.toOption.get
    assertEquals(
      selected.diagnostic.message,
      "Selected type syntax `scala.Int` is not supported; use unqualified `Int` in the current experimental surface."
    )
    assertEquals(selected.location.map(_.span), Some(SourceSpan(0, "scala.Int".length)))
    assertEquals(selected.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
  }

  test("higher-level located type patterns preserve hole and no-hole construction") {
    assertEquals(
      QuasiTypeExamples.locatedPatternSummary("List[$t]"),
      "success=true legacySuccess=true expected=false holes=true"
    )
    assertEquals(
      QuasiTypeExamples.locatedPatternSummary("List[Int]"),
      "success=true legacySuccess=true expected=true holes=false"
    )
    assertEquals(
      QuasiTypeExamples.locatedPatternSummary("scala.Int"),
      "error=true legacySame=true location=virtual-type-pattern-parser-input:0-9:origins=1 message=Selected type syntax `scala.Int` is not supported; use unqualified `Int` in the current experimental surface."
    )
  }

  test("located construction success is identical to the legacy API") {
    val located = QuasiTypeConstruct.fromTemplateLocated("List[$t]", "t" -> intForm)
    val legacy = QuasiTypeConstruct.fromTemplate("List[$t]", "t" -> intForm)

    assertEquals(located.left.map(_.diagnostic), legacy)
    assertEquals(located.toOption.map(_.source), Some("List[Int]"))
  }

  test("Tuple3 and Function2 located map and varargs construction agree") {
    val bindings = Map(
      "a" -> TypeNormalForm.STypeIdent("Int"),
      "b" -> TypeNormalForm.STypeIdent("String"),
      "c" -> TypeNormalForm.STypeIdent("Boolean")
    )
    val tupleMap = QuasiTypeConstruct.fromTemplateLocated("($a, $b, $c)", bindings)
    val tupleVarargs = QuasiTypeConstruct.fromTemplateLocated(
      "($a, $b, $c)",
      "a" -> bindings("a"),
      "b" -> bindings("b"),
      "c" -> bindings("c")
    )
    val function = QuasiTypeConstruct.fromTemplateLocated(
      "($a, $b) => $c",
      bindings
    )

    assertEquals(tupleMap, tupleVarargs)
    assertEquals(tupleMap.map(_.source), Right("(Int, String, Boolean)"))
    assertEquals(function.map(_.source), Right("(Int, String) => Boolean"))
    assertEquals(tupleMap.left.map(_.diagnostic), QuasiTypeConstruct.fromTemplate("($a, $b, $c)", bindings))
    assertEquals(function.left.map(_.diagnostic), QuasiTypeConstruct.fromTemplate("($a, $b) => $c", bindings))
  }

  test("a unique missing binding points at its exact rewritten-hole occurrence") {
    val source = "List[$t]"
    val located = QuasiTypeConstruct.fromTemplateLocated(source, Map.empty).swap.toOption.get

    assertEquals(located.diagnostic.message, "Missing type-construction binding `$t`.")
    assertEquals(located.location.map(_.sourceId), Some(SourceId.VirtualTypeTemplateParserInput))
    assertEquals(located.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))
    assertEquals(located.location.map(_.origins), Some(Vector(
      SourceOrigin.RewrittenHole(SourceId.TypeTemplate, SourceSpan(5, 7), "t", HoleRole.TypeTemplate)
    )))
    assertEquals(QuasiTypeConstruct.fromTemplate(source).swap.toOption, Some(located.diagnostic))
  }

  test("a repeated missing binding uses the whole template instead of an arbitrary occurrence") {
    val source = "($t, $t)"
    val mapped = TypeTemplate.rewriteSourceMapped(source)
    val located = QuasiTypeConstruct.fromTemplateLocated(source, Map.empty).swap.toOption.get
    val occurrences = located.location.toVector.flatMap(_.origins).collect {
      case origin: SourceOrigin.RewrittenHole => origin.originalSpan
    }

    assertEquals(located.location.map(_.span), Some(SourceSpan(0, mapped.generatedSource.length)))
    assertEquals(located.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
    assertEquals(occurrences, Vector(SourceSpan(1, 3), SourceSpan(5, 7)))
  }

  test("Tuple3 unique and repeated missing bindings retain exact and whole precision") {
    val unique = QuasiTypeConstruct
      .fromTemplateLocated("($a, $b, $c)", "a" -> intForm, "b" -> intForm)
      .swap.toOption.get
    val repeated = QuasiTypeConstruct
      .fromTemplateLocated("($t, $t, $u)", "u" -> intForm)
      .swap.toOption.get

    assertEquals(unique.diagnostic.message, "Missing type-construction binding `$c`.")
    assertEquals(unique.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))
    assertEquals(
      unique.location.toVector.flatMap(_.origins).collect {
        case origin: SourceOrigin.RewrittenHole => origin.originalSpan
      },
      Vector(SourceSpan(9, 11))
    )
    assertEquals(repeated.diagnostic.message, "Missing type-construction binding `$t`.")
    assertEquals(repeated.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
    assertEquals(
      repeated.location.toVector.flatMap(_.origins).collect {
        case origin: SourceOrigin.RewrittenHole => origin.originalSpan
      },
      Vector(SourceSpan(1, 3), SourceSpan(5, 7), SourceSpan(9, 11))
    )
  }

  test("unsupported Tuple4 and Function3 keep whole-source located diagnostics") {
    val tupleSource = "($a, $b, $c, $d)"
    val functionSource = "($a, $b, $c) => $r"
    val tuple = TypePatternSource.fromSourceLocated(tupleSource).swap.toOption.get
    val function = TypeTemplateSource.fromSourceLocated(functionSource).swap.toOption.get

    assert(tuple.diagnostic.message.contains("Unsupported tuple arity for type-pattern construction"))
    assert(!tuple.diagnostic.message.contains("__tqhole_"))
    assertEquals(tuple.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
    assertEquals(tuple.location.map(_.span), Some(SourceSpan(0, TypePattern.rewriteSourceMapped(tupleSource).generatedSource.length)))
    assert(function.diagnostic.message.contains("Unsupported function arity for type-template construction"))
    assert(!function.diagnostic.message.contains("__tqconstructhole_"))
    assertEquals(function.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
    assertEquals(function.location.map(_.span), Some(SourceSpan(0, TypeTemplate.rewriteSourceMapped(functionSource).generatedSource.length)))
    assertEquals(TypePatternSource.fromSource(tupleSource).swap.toOption, Some(tuple.diagnostic))
    assertEquals(TypeTemplateSource.fromSource(functionSource).swap.toOption, Some(function.diagnostic))
  }

  test("extra bindings and validation failures use truthful whole-template locations") {
    val extra = QuasiTypeConstruct.fromTemplateLocated(
      "List[$t]",
      "t" -> intForm,
      "extra" -> TypeNormalForm.STypeIdent("String")
    ).swap.toOption.get
    assertEquals(extra.diagnostic.message, "Unexpected type-construction binding(s): `$extra`. Remove bindings that do not occur in the template.")
    assertEquals(extra.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))

    val source = "List[$t]"
    val mapped = TypeTemplate.rewriteSourceMapped(source)
    val validation = QuasiTypeConstruct.fromTemplateLocated(
      source,
      "t" -> TypeNormalForm.STypeIdent("AnyVal")
    ).swap.toOption.get
    assertEquals(validation.diagnostic.message, "Unsupported type-construction identifier `AnyVal`; supported identifiers are Int, String, Boolean.")
    assertEquals(validation.location.map(_.span), Some(SourceSpan(0, mapped.generatedSource.length)))
    assertEquals(validation.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
  }

  test("prefix-like literals use ordinary identifier validation without spurious locations") {
    val pattern = TypePatternSource.fromSourceLocated("__tqhole_x").swap.toOption.get
    val template = TypeTemplateSource.fromSourceLocated("__tqconstructhole_x").swap.toOption.get

    assertEquals(pattern.diagnostic.message, "Unsupported type identifier `__tqhole_x`; supported identifiers are Int, String, Boolean, AnyVal.")
    assertEquals(template.diagnostic.message, "Unsupported type-construction identifier `__tqconstructhole_x`; supported identifiers are Int, String, Boolean.")
    assert(pattern.location.nonEmpty)
    assert(template.location.nonEmpty)
  }

  test("collision-safe type-pattern restoration preserves prefix-sharing literal identifiers") {
    val sources = List(
      "(__tqhole_t_1, $t, Int, String)",
      "(__tqhole_t, __tqhole_t_1, $t, Int)"
    )

    sources.foreach { source =>
      val mapped = TypePattern.rewriteSourceMapped(source)
      val generated = mapped.occurrences.head.generatedName
      val located = TypePatternSource.fromSourceLocated(source).swap.toOption.get
      val messageIdentifiers =
        HoleSourceRewriter.scan(located.diagnostic.message, allowUnicodeIdentifiers = false).literalIdentifiers

      assertEquals(TypePatternSource.fromSource(source).swap.toOption, Some(located.diagnostic))
      assertEquals(located.diagnostic.message, "Unsupported tuple arity for type-pattern construction: expected 2 or 3 elements, but found 4.")
      assert(!messageIdentifiers.contains(generated))
      assertEquals(located.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
      assertEquals(
        located.location.toVector.flatMap(_.origins).collect {
          case origin: SourceOrigin.RewrittenHole => origin.originalSpan
        },
        Vector(SourceSpan(source.indexOf("$t"), source.indexOf("$t") + 2))
      )
    }
  }

  test("collision-safe type-template restoration preserves prefix-sharing literal identifiers") {
    val sources = List(
      "(__tqconstructhole_t_1, $t, Int, String)",
      "(__tqconstructhole_t, __tqconstructhole_t_1, $t, Int)"
    )

    sources.foreach { source =>
      val mapped = TypeTemplate.rewriteSourceMapped(source)
      val generated = mapped.occurrences.head.generatedName
      val located = TypeTemplateSource.fromSourceLocated(source).swap.toOption.get
      val messageIdentifiers =
        HoleSourceRewriter.scan(located.diagnostic.message, allowUnicodeIdentifiers = false).literalIdentifiers

      assertEquals(TypeTemplateSource.fromSource(source).swap.toOption, Some(located.diagnostic))
      assertEquals(located.diagnostic.message, "Unsupported tuple arity for type-template construction: expected 2 or 3 elements, but found 4.")
      assert(!messageIdentifiers.contains(generated))
      assertEquals(located.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
      assertEquals(
        located.location.toVector.flatMap(_.origins).collect {
          case origin: SourceOrigin.RewrittenHole => origin.originalSpan
        },
        Vector(SourceSpan(source.indexOf("$t"), source.indexOf("$t") + 2))
      )
    }
  }
