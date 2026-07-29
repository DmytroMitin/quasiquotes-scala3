package quasiquotes.terms.parser

import quasiquotes.parser.{
  TermShape,
  TermShapeInspector,
  TinyTermParser
}
import quasiquotes.source.{
  DiagnosticPrecision,
  HoleRole,
  SourceOrigin
}
import quasiquotes.terms.{
  ConstructedTerm,
  TermShapeTraversal
}
import quasiquotes.terms.dotty.ConstructedTermUntypedBackend
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

class TermTemplateSourceAdapterTest extends munit.FunSuite:
  import TermTemplateHoleCategory.*
  import TermTemplateSourceAdapterError.*

  private def term(name: String): CategorizedHoleOccurrence =
    CategorizedHoleOccurrence(name, Term)

  private def tpe(name: String): CategorizedHoleOccurrence =
    CategorizedHoleOccurrence(name, Type)

  private def parsed(
      source: String,
      occurrences: CategorizedHoleOccurrence*
  ) =
    TermTemplateSourceAdapter
      .parseLocated(source, occurrences.toVector)
      .fold(error => fail(error.diagnostic.message), identity)

  private def error(
      source: String,
      occurrences: CategorizedHoleOccurrence*
  ) =
    TermTemplateSourceAdapter
      .parseLocated(source, occurrences.toVector)
      .swap
      .fold(value => fail(s"Expected adapter failure, received $value"), identity)

  private val one =
    ConstructedTerm
      .fromShape(TermShape.Literal("1"))
      .fold(value => fail(value.message), identity)

  test("parses hole-free source to semantic and located templates") {
    val located = parsed("service.answer")
    assertEquals(
      located.template.root,
      TermShape.Select(
        TermShape.Identifier("service", false),
        "answer"
      )
    )
    assertEquals(located.termOccurrences, Vector.empty)
    assertEquals(located.typeOccurrences, Vector.empty)
    assertEquals(
      TermTemplateSourceAdapter.parse("service.answer", Vector.empty),
      Right(located.template)
    )
  }

  test("parses one root term hole with an exact identifier address") {
    val located = parsed("$value", term("value"))
    assertEquals(
      located.template.termHoleOccurrences.map(item =>
        item.name -> item.identifierOrdinal
      ),
      Vector("value" -> 0)
    )
    assertEquals(
      located.termOccurrences.map(_.source.originalSpan.start),
      Vector(0)
    )
  }

  Vector(
    "$function($argument)" ->
      Vector(term("function"), term("argument")),
    "$left + $right" ->
      Vector(term("left"), term("right")),
    "($first, $second)" ->
      Vector(term("first"), term("second")),
    "if $condition then $yes else $no" ->
      Vector(term("condition"), term("yes"), term("no")),
    "-$operand" ->
      Vector(term("operand")),
    "($value)" ->
      Vector(term("value")),
    "($value: Int)" ->
      Vector(term("value"))
  ).foreach { case (source, plan) =>
    test(s"accepts complete term identifier positions: $source") {
      val located = parsed(source, plan*)
      assertEquals(
        located.template.termHoleOccurrences.map(_.name),
        plan.map(_.name)
      )
    }
  }

  test("repeated term occurrences share one generated identifier and binding") {
    val located = parsed("($x, $x)", term("x"), term("x"))
    assertEquals(
      located.termOccurrences.map(_.source.generatedName).distinct.size,
      1
    )
    assertEquals(
      located.template.termHoleOccurrences.map(_.identifierOrdinal),
      Vector(0, 1)
    )
    assertEquals(
      located.template.complete(Map("x" -> one), Map.empty).map(_.root),
      Right(TermShape.Tuple(List(one.root, one.root)))
    )
  }

  Vector(
    "$t" -> TypeTemplate.TTHole("t"),
    "List[$element]" ->
      TypeTemplate.TTApply(
        TypeTemplate.TTIdent("List"),
        List(TypeTemplate.TTHole("element"))
      ),
    "Option[$element]" ->
      TypeTemplate.TTApply(
        TypeTemplate.TTIdent("Option"),
        List(TypeTemplate.TTHole("element"))
      ),
    "($left, $right)" ->
      TypeTemplate.TTTuple(
        List(
          TypeTemplate.TTHole("left"),
          TypeTemplate.TTHole("right")
        )
      ),
    "($a, $b, $c)" ->
      TypeTemplate.TTTuple(
        List(
          TypeTemplate.TTHole("a"),
          TypeTemplate.TTHole("b"),
          TypeTemplate.TTHole("c")
        )
      ),
    "$argument => $result" ->
      TypeTemplate.TTFunction(
        List(TypeTemplate.TTHole("argument")),
        TypeTemplate.TTHole("result")
      ),
    "($a, $b) => $result" ->
      TypeTemplate.TTFunction(
        List(TypeTemplate.TTHole("a"), TypeTemplate.TTHole("b")),
        TypeTemplate.TTHole("result")
      )
  ).foreach { case (typeSource, expected) =>
    val names =
      typeSource
        .split("[^A-Za-z0-9_]+")
        .toVector
        .filter(name =>
          name.nonEmpty &&
            !Set("List", "Option")(name)
        )
    test(s"extracts one typed-node sidecar: $typeSource") {
      val plan = term("value") +: names.map(tpe)
      val located = parsed(s"($$value: $typeSource)", plan*)
      assertEquals(located.template.ascriptionTypes, Vector(expected))
      assertEquals(located.typeOccurrences.map(_.name), names)
    }
  }

  test("several typed nodes retain exact typed-node preorder") {
    val located = parsed(
      "(($left: List[$a]), ($right: ($b, String)))",
      term("left"),
      tpe("a"),
      term("right"),
      tpe("b")
    )
    assertEquals(
      located.template.ascriptionTypes,
      Vector(
        TypeTemplate.TTApply(
          TypeTemplate.TTIdent("List"),
          List(TypeTemplate.TTHole("a"))
        ),
        TypeTemplate.TTTuple(
          List(
            TypeTemplate.TTHole("b"),
            TypeTemplate.TTIdent("String")
          )
        )
      )
    )
    assertEquals(located.typeOccurrences.map(_.name), Vector("a", "b"))
  }

  test("same semantic name is independent in term and type namespaces") {
    val located = parsed(
      "($same: Option[$same])",
      term("same"),
      tpe("same")
    )
    val termGenerated =
      located.template.termHoleIndex.generatedNameFor("same").get
    val typeGenerated =
      located.template.typeHoleIndex.generatedNameFor("same").get
    assertNotEquals(termGenerated, typeGenerated)
    assertEquals(
      located.template.ascriptionTypes,
      Vector(
        TypeTemplate.TTApply(
          TypeTemplate.TTIdent("Option"),
          List(TypeTemplate.TTHole("same"))
        )
      )
    )
    val completed = located.template
      .complete(
        Map("same" -> one),
        Map("same" -> TypeNormalForm.STypeIdent("String"))
      )
      .fold(value => fail(value.message), identity)
    assertEquals(
      completed.root,
      TermShape.Parenthesized(
        TermShape.Typed(one.root, "Option[String]")
      )
    )
  }

  test("repeated names remain independent across both categories") {
    val located = parsed(
      "(($same: $same), ($same: $same))",
      term("same"),
      tpe("same"),
      term("same"),
      tpe("same")
    )
    assertEquals(
      located.termOccurrences.map(_.source.generatedName).distinct.size,
      1
    )
    assertEquals(
      located.typeOccurrences.map(_.generatedName).distinct.size,
      1
    )
    assertNotEquals(
      located.termOccurrences.head.source.generatedName,
      located.typeOccurrences.head.generatedName
    )
  }

  test("ordinary generated-looking identifiers remain ordinary under collision") {
    val located = parsed(
      "(__qq_tt_term_x, $x, __qq_tt_type_t)",
      term("x")
    )
    val generated =
      located.template.termHoleIndex.generatedNameFor("x").get
    assertEquals(generated, "__qq_tt_term_x_1")
    assert(
      TermShapeTraversal
        .identifierEntries(located.template.root)
        .exists(_.name == "__qq_tt_term_x")
    )
    assert(
      TermShapeTraversal
        .identifierEntries(located.template.root)
        .exists(_.name == "__qq_tt_type_t")
    )
  }

  test("scanner ignores dollars in comments strings and backticks") {
    val located = parsed(
      """(
        |  $value, // $comment
        |  "$string",
        |  `$backtick`
        |)""".stripMargin,
      term("value")
    )
    assertEquals(located.termOccurrences.map(_.semantic.name), Vector("value"))
  }

  test("a dollar in a character literal is not categorized before literal rejection") {
    val failure = error("'$'")
    assert(failure.diagnostic.isInstanceOf[UnsupportedTermShape])
  }

  Vector("0", "1_000", "-20", "true", "false", "\"text\"")
    .foreach { source =>
      test(s"preserves admitted parser-origin literal: $source") {
        val located = parsed(source)
        assert(
          located.template.root.isInstanceOf[TermShape.Literal]
        )
      }
    }

  test("source map covers both categories without gaps and exact roles agree") {
    val source = "($value: List[$element])"
    val located = parsed(source, term("value"), tpe("element"))
    val segments = located.sourceMap.segments
    assertEquals(segments.head.generatedSpan.start, 0)
    assertEquals(
      segments.last.generatedSpan.end,
      located.sourceMap.generatedSource.length
    )
    assert(
      segments.zip(segments.drop(1)).forall { case (left, right) =>
        left.generatedSpan.end == right.generatedSpan.start
      }
    )
    assertEquals(
      located.termOccurrences.map(_.source.role),
      Vector(HoleRole.TermTemplate)
    )
    assertEquals(
      located.typeOccurrences.map(_.role),
      Vector(HoleRole.TypeTemplate)
    )
    assertEquals(
      located.termOccurrences.head.source.originalSpan,
      quasiquotes.source.SourceSpan(1, 7)
    )
    assertEquals(
      located.typeOccurrences.head.originalSpan,
      quasiquotes.source.SourceSpan(14, 22)
    )
  }

  test("located completion reports a unique missing binding exactly") {
    val located = parsed("$value", term("value"))
    val failure = located.complete(Map.empty, Map.empty).swap.toOption.get
    assertEquals(failure.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))
    assert(
      failure.location.toVector.flatMap(_.origins).exists {
        case SourceOrigin.RewrittenHole(_, _, "value", HoleRole.TermTemplate) =>
          true
        case _ => false
      }
    )
  }

  test("produced template completes and composes with the Phase 44 backend") {
    val located = parsed(
      "($same: Option[$same])",
      term("same"),
      tpe("same")
    )
    val completed = located.template
      .complete(
        Map("same" -> one),
        Map("same" -> TypeNormalForm.STypeIdent("String"))
      )
      .fold(value => fail(value.message), identity)
    val raw = ConstructedTermUntypedBackend
      .lower(completed)
      .fold(value => fail(value.message), identity)
    assertEquals(
      TermShapeInspector.rawStructure(raw),
      "Parens(Typed(Number(1,Whole(10)),AppliedTypeTree))"
    )
  }

  test("generated source is parsed exactly once") {
    var calls = 0
    val result = TermTemplateSourceAdapter.parseLocatedUsing(
      "($value: List[$element])",
      Vector(term("value"), tpe("element"))
    ) { generated =>
      calls += 1
      TinyTermParser.parse(generated)
    }
    assert(result.isRight)
    assertEquals(calls, 1)
  }

  test("semantic equality ignores category-specific transport spellings") {
    val plan = Vector(term("value"), tpe("element"))
    val first = TermTemplateSourceAdapter
      .parseLocatedUsingPrefixes(
        "($value: List[$element])",
        plan,
        "__qq_tt_term_first_",
        "__qq_tt_type_first_"
      )(TinyTermParser.parse)
      .toOption
      .get
      .template
    val second = TermTemplateSourceAdapter
      .parseLocatedUsingPrefixes(
        "($value: List[$element])",
        plan,
        "__qq_tt_term_second_",
        "__qq_tt_type_second_"
      )(TinyTermParser.parse)
      .toOption
      .get
      .template
    assertEquals(first, second)
    assertEquals(first.hashCode, second.hashCode)
  }

  test("semantic equality distinguishes categories names positions types and structure") {
    val baseline =
      parsed("($value: List[$element])", term("value"), tpe("element")).template
    val differentName =
      parsed("($other: List[$element])", term("other"), tpe("element")).template
    val differentPosition =
      parsed("(fixed: List[$value])", tpe("value")).template
    val differentType =
      parsed("($value: Option[$element])", term("value"), tpe("element")).template
    val differentStructure =
      parsed("f($value: List[$element])", term("value"), tpe("element")).template
    assertNotEquals(baseline, differentName)
    assertNotEquals(baseline, differentPosition)
    assertNotEquals(baseline, differentType)
    assertNotEquals(baseline, differentStructure)
  }

  test("rejects a missing categorized descriptor") {
    assert(error("$a + $b", term("a")).diagnostic.isInstanceOf[OccurrenceCountMismatch])
  }

  test("rejects an extra categorized descriptor") {
    assert(error("$a", term("a"), term("b")).diagnostic.isInstanceOf[OccurrenceCountMismatch])
  }

  test("rejects descriptor name or order mismatch") {
    val failure = error("$a + $b", term("b"), term("a"))
    assertEquals(
      failure.diagnostic,
      OccurrenceNameMismatch(0, "a", "b")
    )
  }

  test("rejects invalid dollar syntax and invalid planned names") {
    assert(error("$ + value").diagnostic.isInstanceOf[InvalidDollarSyntax])
    assert(
      error("$value", CategorizedHoleOccurrence("bad-name", Term))
        .diagnostic
        .isInstanceOf[InvalidHoleName]
    )
  }

  test("rejects a term hole in select-name position") {
    val failure = error("service.$member", term("member"))
    assertEquals(failure.diagnostic, TermMarkerInInvalidPosition("member"))
  }

  test("rejects a type hole as a term identifier") {
    val failure = error("$value", tpe("value"))
    assertEquals(failure.diagnostic, TypeMarkerInTermPosition("value"))
  }

  test("rejects a term hole inside type syntax") {
    val failure = error("(value: List[$element])", term("element"))
    assertEquals(failure.diagnostic, TermMarkerInsideType("element"))
  }

  test("rejects a type hole outside an ascription") {
    val failure = error("service.$member", tpe("member"))
    assertEquals(failure.diagnostic, TypeMarkerOutsideAscription("member"))
  }

  test("rejects unsupported term and type syntax") {
    assert(error("{ value }").diagnostic.isInstanceOf[UnsupportedTermShape])
    assert(
      error("(value: Either[$left, Int])", tpe("left"))
        .diagnostic
        .isInstanceOf[UnsupportedTypeTemplateShape]
    )
  }

  Vector("0x10", "0b10", "10L", "'x'", "1.0", "1.0f", "null")
    .foreach { source =>
      test(s"rejects unsupported parser-origin literal: $source") {
        assert(
          error(source).diagnostic.isInstanceOf[
            UnsupportedTermShape
          ] ||
            error(source).diagnostic.isInstanceOf[ParserFailure]
        )
      }
    }

  test("parse diagnostics restore exact repeated semantic hole spelling") {
    val failure = error("$value; $value", term("value"), term("value"))
    assert(failure.diagnostic.isInstanceOf[ParserFailure])
    assert(failure.diagnostic.message.contains("$value"))
    assert(!failure.diagnostic.message.contains("__qq_tt_term_"))
  }
