package quasiquotes.types

import quasiquotes.source.*

class LocatedTypeDiagnosticTest extends munit.FunSuite:
  private val intForm = TypeNormalForm.STypeIdent("Int")

  test("located type-pattern success and matching preserve legacy behavior") {
    val source = "List[$t]"
    val located = TypePattern.fromSourceLocated(source)

    assertEquals(located.left.map(_.diagnostic), TypePattern.fromSource(source))
    assertEquals(
      located.toOption.flatMap(TypePattern.matchNormalForm(_, TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("List"), List(intForm)))),
      Some(TypeMatchResult(Map("t" -> intForm)))
    )
  }

  test("type-pattern parse diagnostics map structured spans through original text") {
    val source = "Int)"
    val located = TypePattern.fromSourceLocated(source).swap.toOption.get

    assertEquals(located.diagnostic, TypePattern.fromSource(source).swap.toOption.get)
    assertEquals(located.location.map(_.sourceId), Some(SourceId.VirtualTypePatternParserInput))
    assertEquals(located.location.map(_.span.end), Some(source.length))
    assertEquals(located.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))
    assert(located.location.exists(location => location.span.start > 0 && !location.span.isEmpty))
  }

  test("type-pattern shape failures keep exact messages and conservative whole-pattern locations") {
    val selected = "scala.Int"
    val selectedLocated = TypePattern.fromSourceLocated(selected).swap.toOption.get
    assertEquals(
      selectedLocated.diagnostic.message,
      "Selected type syntax is not supported for Phase 18 type-hole matching; `scala.Int` vs `Int` remains an explicit TODO."
    )
    assertEquals(selectedLocated.location.map(_.span), Some(SourceSpan(0, selected.length)))
    assertEquals(selectedLocated.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))

    val repeatedSource = "($t, $t, $t, $t)"
    val mapped = TypePattern.rewriteSourceMapped(repeatedSource)
    val repeatedLocated = TypePattern.fromSourceLocated(repeatedSource).swap.toOption.get
    val repeatedOrigins = repeatedLocated.location.toVector.flatMap(_.origins).collect {
      case origin: SourceOrigin.RewrittenHole => origin.originalSpan
    }
    assertEquals(repeatedLocated.location.map(_.span), Some(SourceSpan(0, mapped.generatedSource.length)))
    assertEquals(repeatedLocated.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
    assertEquals(
      repeatedOrigins,
      Vector(SourceSpan(1, 3), SourceSpan(5, 7), SourceSpan(9, 11), SourceSpan(13, 15))
    )
    assert(repeatedLocated.diagnostic.message.contains("Unsupported tuple type pattern shape"))
  }

  test("type-template located failures use distinct role and source identity") {
    val source = "($t, $t, $t, $t)"
    val mapped = TypeTemplate.rewriteSourceMapped(source)
    val located = TypeTemplate.fromSourceLocated(source).swap.toOption.get
    val rewritten = located.location.toVector.flatMap(_.origins).collect {
      case origin: SourceOrigin.RewrittenHole => origin
    }

    assertEquals(located.location.map(_.sourceId), Some(SourceId.VirtualTypeTemplateParserInput))
    assertEquals(located.location.map(_.span), Some(SourceSpan(0, mapped.generatedSource.length)))
    assertEquals(located.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
    assertEquals(rewritten.map(_.role).distinct, Vector(HoleRole.TypeTemplate))
    assert(located.diagnostic.message.contains("Unsupported tuple type construction template shape"))
    assertEquals(TypeTemplate.fromSource(source).swap.toOption, Some(located.diagnostic))
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
    val parsed = TypeTemplate.fromSourceLocated(parseSource).swap.toOption.get
    assertEquals(parsed.location.map(_.sourceId), Some(SourceId.VirtualTypeTemplateParserInput))
    assertEquals(parsed.location.map(_.span.end), Some(parseSource.length))
    assertEquals(parsed.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))
    assertEquals(TypeTemplate.fromSourceLocated("Int String").swap.toOption.flatMap(_.location), None)

    val selected = TypeTemplate.fromSourceLocated("scala.Int").swap.toOption.get
    assertEquals(
      selected.diagnostic.message,
      "Selected type syntax is not supported for Phase 21 type construction; `scala.Int` vs `Int` remains an explicit TODO."
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
      "error=true legacySame=true location=virtual-type-pattern-parser-input:0-9:origins=1 message=Selected type syntax is not supported for Phase 18 type-hole matching; `scala.Int` vs `Int` remains an explicit TODO."
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

    assertEquals(located.diagnostic.message, "Missing type-construction binding `t`")
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

    assertEquals(unique.diagnostic.message, "Missing type-construction binding `c`")
    assertEquals(unique.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))
    assertEquals(
      unique.location.toVector.flatMap(_.origins).collect {
        case origin: SourceOrigin.RewrittenHole => origin.originalSpan
      },
      Vector(SourceSpan(9, 11))
    )
    assertEquals(repeated.diagnostic.message, "Missing type-construction binding `t`")
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
    val tuple = TypePattern.fromSourceLocated(tupleSource).swap.toOption.get
    val function = TypeTemplate.fromSourceLocated(functionSource).swap.toOption.get

    assert(tuple.diagnostic.message.contains("Unsupported tuple type pattern shape"))
    assert(!tuple.diagnostic.message.contains("__tqhole_"))
    assertEquals(tuple.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
    assertEquals(tuple.location.map(_.span), Some(SourceSpan(0, TypePattern.rewriteSourceMapped(tupleSource).generatedSource.length)))
    assert(function.diagnostic.message.contains("Unsupported function type construction template shape"))
    assert(!function.diagnostic.message.contains("__tqconstructhole_"))
    assertEquals(function.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
    assertEquals(function.location.map(_.span), Some(SourceSpan(0, TypeTemplate.rewriteSourceMapped(functionSource).generatedSource.length)))
    assertEquals(TypePattern.fromSource(tupleSource).swap.toOption, Some(tuple.diagnostic))
    assertEquals(TypeTemplate.fromSource(functionSource).swap.toOption, Some(function.diagnostic))
  }

  test("extra bindings have no location and validation failures use the whole template") {
    val extra = QuasiTypeConstruct.fromTemplateLocated(
      "List[$t]",
      "t" -> intForm,
      "extra" -> TypeNormalForm.STypeIdent("String")
    ).swap.toOption.get
    assertEquals(extra.diagnostic.message, "Extra type-construction binding(s): extra")
    assertEquals(extra.location, None)

    val source = "List[$t]"
    val mapped = TypeTemplate.rewriteSourceMapped(source)
    val validation = QuasiTypeConstruct.fromTemplateLocated(
      source,
      "t" -> TypeNormalForm.STypeIdent("AnyVal")
    ).swap.toOption.get
    assertEquals(validation.diagnostic.message, "Unsupported constructed type identifier for Phase 21: AnyVal")
    assertEquals(validation.location.map(_.span), Some(SourceSpan(0, mapped.generatedSource.length)))
    assertEquals(validation.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
  }

  test("prefix-like literals use ordinary identifier validation without spurious locations") {
    val pattern = TypePattern.fromSourceLocated("__tqhole_x").swap.toOption.get
    val template = TypeTemplate.fromSourceLocated("__tqconstructhole_x").swap.toOption.get

    assertEquals(pattern.diagnostic.message, "Unsupported type identifier for Phase 15 structural normal form: __tqhole_x")
    assertEquals(template.diagnostic.message, "Unsupported type construction template identifier for Phase 21: __tqconstructhole_x")
    assert(pattern.location.nonEmpty)
    assert(template.location.nonEmpty)
  }
