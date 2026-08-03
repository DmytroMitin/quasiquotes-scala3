package quasiquotes.types

import quasiquotes.parser.{TinyTypeParser, TypeShape}
import quasiquotes.source.*

class CollisionSafeTypeHoleTest extends munit.FunSuite:
  private val intForm = TypeNormalForm.STypeIdent("Int")

  test("source type patterns classify literal prefixes normally and generated holes authoritatively") {
    val literal = TypePatternSource.fromSource("__tqhole_t").swap.toOption.get
    val hole = TypePatternSource.fromSource("$t").toOption.get

    assertEquals(literal.message, "Unsupported type identifier for Phase 15 structural normal form: __tqhole_t")
    assertEquals(hole, TypePattern.TPHole("t"))
  }

  test("mixed type-pattern source keeps literal and generated identities distinct") {
    val source = "(__tqhole_t, $t)"
    val mapped = TypePattern.rewriteSourceMapped(source)
    val result = TypePatternSource.fromSource(source).swap.toOption.get

    assertEquals(mapped.generatedSource, "(__tqhole_t, __tqhole_t_1)")
    assertEquals(mapped.generatedHoleIndex.semanticNameFor("__tqhole_t"), None)
    assertEquals(mapped.generatedHoleIndex.semanticNameFor("__tqhole_t_1"), Some("t"))
    assertEquals(result.message, "Unsupported type identifier for Phase 15 structural normal form: __tqhole_t")
  }

  test("type-pattern collisions are deterministic and repeated holes reuse one name") {
    val source = "(__tqhole_t, __tqhole_t_1, $t, $t)"
    val first = TypePattern.rewriteSourceMapped(source)
    val second = TypePattern.rewriteSourceMapped(source)

    assertEquals(first, second)
    assertEquals(first.occurrences.map(_.generatedName), Vector("__tqhole_t_2", "__tqhole_t_2"))
    assertEquals(first.occurrences.map(_.name), Vector("t", "t"))
  }

  test("repeated type-hole matching behavior is unchanged") {
    val pattern = TypePatternSource.fromSource("($t, $t)").toOption.get
    val same = TypeNormalForm.STypeTuple(List(intForm, intForm))
    val different = TypeNormalForm.STypeTuple(List(intForm, TypeNormalForm.STypeIdent("String")))

    assertEquals(TypePattern.matchNormalForm(pattern, same), Some(TypeMatchResult(Map("t" -> intForm))))
    assertEquals(TypePattern.matchNormalForm(pattern, different), None)
  }

  test("type-pattern located and legacy failures retain messages and origins") {
    val source = "(__tqhole_t, $t, $t, $t)"
    val located = TypePatternSource.fromSourceLocated(source).swap.toOption.get
    val rewritten = located.location.toVector.flatMap(_.origins).collect {
      case origin: SourceOrigin.RewrittenHole => origin
    }

    assertEquals(located.diagnostic, TypePatternSource.fromSource(source).swap.toOption.get)
    assert(located.diagnostic.message.contains("Unsupported tuple type pattern shape for Phase 18 type-hole matching"))
    assertEquals(rewritten.map(_.holeName), Vector("t", "t", "t"))
    assertEquals(
      rewritten.map(_.originalSpan),
      Vector(SourceSpan(13, 15), SourceSpan(17, 19), SourceSpan(21, 23))
    )
  }

  test("Int versus scala.Int and unsupported syntax boundaries remain unchanged") {
    assert(TypePatternSource.fromSource("Int").isRight)
    assertEquals(
      TypePatternSource.fromSource("scala.Int").swap.toOption.get.message,
      "Selected type syntax is not supported for Phase 18 type-hole matching; `scala.Int` vs `Int` remains an explicit TODO."
    )
    assert(TypePatternSource.fromSource("List[?]").isLeft)
  }

  test("direct low-level type-pattern API retains documented prefix compatibility") {
    assertEquals(
      TypePattern.fromShape(TypeShape.Identifier("__tqhole_t")),
      Right(TypePattern.TPHole("t"))
    )
    assert(TypePatternSource.fromSource("__tqhole_t").isLeft)
  }

  test("source type templates classify literal prefixes normally and generated holes authoritatively") {
    val literal = TypeTemplateSource.fromSource("__tqconstructhole_t").swap.toOption.get
    val hole = TypeTemplateSource.fromSource("$t").toOption.get

    assertEquals(literal.message, "Unsupported type construction template identifier for Phase 21: __tqconstructhole_t")
    assertEquals(hole, TypeTemplate.TTHole("t"))
  }

  test("mixed type-template source gets a fresh hole name without reclassifying the literal") {
    val source = "(__tqconstructhole_t, $t)"
    val mapped = TypeTemplate.rewriteSourceMapped(source)
    val result = TypeTemplateSource.fromSource(source).swap.toOption.get

    assertEquals(mapped.generatedSource, "(__tqconstructhole_t, __tqconstructhole_t_1)")
    assertEquals(mapped.generatedHoleIndex.semanticNameFor("__tqconstructhole_t"), None)
    assertEquals(mapped.generatedHoleIndex.semanticNameFor("__tqconstructhole_t_1"), Some("t"))
    assertEquals(result.message, "Unsupported type construction template identifier for Phase 21: __tqconstructhole_t")
  }

  test("literal template prefixes are not counted as semantic hole names") {
    val template = TypeTemplate.TTTuple(List(
      TypeTemplate.TTIdent("__tqconstructhole_t"),
      TypeTemplate.TTHole("t")
    ))

    assertEquals(TypeTemplate.holeNames(template), Set("t"))
    assertEquals(
      TypeTemplate.fromShapeWithHoles(TypeShape.Identifier("__tqconstructhole_t"), GeneratedHoleIndex.empty).swap.toOption.get.message,
      "Unsupported type construction template identifier for Phase 21: __tqconstructhole_t"
    )
  }

  test("missing, repeated missing, extra, and validation diagnostics remain semantic") {
    val missing = QuasiTypeConstruct.fromTemplateLocated("List[$t]", Map.empty).swap.toOption.get
    assertEquals(missing.diagnostic.message, "Missing type-construction binding `t`")
    assertEquals(missing.location.toVector.flatMap(_.origins).collect {
      case origin: SourceOrigin.RewrittenHole => origin.originalSpan
    }, Vector(SourceSpan(5, 7)))

    val repeated = QuasiTypeConstruct.fromTemplateLocated("($t, $t)", Map.empty).swap.toOption.get
    assertEquals(repeated.location.toVector.flatMap(_.origins).collect {
      case origin: SourceOrigin.RewrittenHole => origin.originalSpan
    }, Vector(SourceSpan(1, 3), SourceSpan(5, 7)))

    val extra = QuasiTypeConstruct.fromTemplateLocated("List[$t]", "t" -> intForm, "extra" -> intForm).swap.toOption.get
    assertEquals(extra.diagnostic.message, "Extra type-construction binding(s): extra")
    assertEquals(extra.location, None)

    val validation = QuasiTypeConstruct.fromTemplateLocated("List[$t]", "t" -> TypeNormalForm.STypeIdent("AnyVal")).swap.toOption.get
    assertEquals(validation.diagnostic.message, "Unsupported constructed type identifier for Phase 21: AnyVal")
  }

  test("collision-safe generated names never leak into constructed source output") {
    assertEquals(
      QuasiTypeConstruct.fromTemplate("($t, $t)", "t" -> intForm).map(_.source),
      Right("(Int, Int)")
    )
  }

  test("direct low-level template API retains documented prefix compatibility") {
    assertEquals(
      TypeTemplate.fromShape(TypeShape.Identifier("__tqconstructhole_t")),
      Right(TypeTemplate.TTHole("t"))
    )
    assert(TypeTemplateSource.fromSource("__tqconstructhole_t").isLeft)
  }
