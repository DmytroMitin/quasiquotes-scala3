package quasiquotes.definitions

import quasiquotes.source.*

class DefinitionQuasiquoteConstructionTest extends munit.FunSuite:
  import DefinitionQuasiquotes.*
  import DefinitionQuasiquoteTestFixtures.*

  test("hole-free method and value construct completed definitions") {
    val method =
      dqr"def answer: Int = 1".fold(error => fail(error.diagnostic.message), identity)
    val value =
      dqr"""val answer: String = "text"""".fold(
        error => fail(error.diagnostic.message),
        identity
      )

    assertEquals(
      method.constructed.render,
      "ConstructedParameterlessDef(name=PlainName(answer), resultType=STypeIdent(Int), body=ConstructedTerm(root=Literal(1), ascriptions=[]))"
    )
    assertEquals(
      value.constructed.render,
      "ConstructedImmutableVal(name=PlainName(answer), declaredType=STypeIdent(String), rhs=ConstructedTerm(root=Literal(\"text\"), ascriptions=[]))"
    )
    assertEquals(method.sourceEvidence.interpolationOccurrences, Vector.empty)
    assertEquals(value.sourceEvidence.interpolationOccurrences, Vector.empty)
  }

  test("all three roles construct once and retain ordered role-specific origins") {
    val listT = definitionType("List[String]")
    val value = bodyTerm("value")
    val optionT = bodyType("Option[String]")
    val fallback = bodyTerm("fallback")
    val result =
      dqr"def convert: $listT = if ready then ($value: $optionT) else $fallback"
        .fold(error => fail(error.diagnostic.message), identity)

    assertEquals(
      result.sourceEvidence.interpolationOccurrences.map(_.semanticIdentity),
      Vector(
        "definitionArgument0",
        "definitionArgument1",
        "definitionArgument2",
        "definitionArgument3"
      )
    )
    assertEquals(
      result.sourceEvidence.interpolationOccurrences.map(_.category),
      Vector(
        InterpolationCategory.DefinitionTypeSplice,
        InterpolationCategory.DefinitionBodyTermSplice,
        InterpolationCategory.DefinitionBodyTypeSplice,
        InterpolationCategory.DefinitionBodyTermSplice
      )
    )
    assertEquals(
      result.constructed.render,
      "ConstructedParameterlessDef(name=PlainName(convert), resultType=STypeApply(STypeIdent(List), [STypeIdent(String)]), body=ConstructedTerm(root=If(Ident(ready), Parens(Typed(Ident(value), Type(Option[String]))), Ident(fallback)), ascriptions=[STypeApply(STypeIdent(Option), [STypeIdent(String)])]))"
    )
    assertSurfaceEvidence(result.sourceEvidence)
    assertLiteralOrigins(
      result.sourceEvidence,
      Vector(
        "def convert: ",
        " = if ready then (",
        ": ",
        ") else ",
        ""
      )
    )
  }

  test("backticked name and the same type payload in definition and body roles remain distinct") {
    val same = tpe("String")
    val result =
      dqr"def `type`: ${DefinitionArguments.definitionType(same)} = (${bodyTerm("value")}: ${DefinitionArguments.bodyType(same)})"
        .toOption
        .get

    val occurrences = result.sourceEvidence.interpolationOccurrences
    assertEquals(occurrences.map(_.argumentIndex), Vector(0, 1, 2))
    assertEquals(
      occurrences.filter(_.argumentIndex != 1).map(_.category),
      Vector(
        InterpolationCategory.DefinitionTypeSplice,
        InterpolationCategory.DefinitionBodyTypeSplice
      )
    )
    assertEquals(result.constructed.name.source, "`type`")
  }

  test("reused and equal-but-distinct descriptors remain occurrence-distinct") {
    val repeated = bodyTerm("value")
    val repeatedResult =
      dqr"def pair: (Int, Int) = ($repeated, $repeated)".toOption.get
    assertEquals(
      repeatedResult.sourceEvidence.interpolationOccurrences.map(_.semanticIdentity),
      Vector("definitionArgument0", "definitionArgument1")
    )

    val left = bodyTerm("value")
    val right = bodyTerm("value")
    val distinctResult =
      dqr"def pair: (Int, Int) = ($left, $right)".toOption.get
    assertEquals(
      distinctResult.sourceEvidence.interpolationOccurrences.map(_.origin.argumentIndex),
      Vector(0, 1)
    )
  }

  test("literal identifier and generated-prefix lookalikes do not collide with semantic identity") {
    val inserted = bodyTerm("value")
    val result =
      dqr"def definitionArgument0: Int = __qq_dt_body_term_literal + $inserted"
        .toOption
        .get
    assertEquals(result.constructed.name.source, "definitionArgument0")
    assert(result.constructed.render.contains("Ident(__qq_dt_body_term_literal)"))
    assert(result.constructed.render.contains("Ident(value)"))
    assertEquals(
      result.sourceEvidence.interpolationOccurrences.map(_.semanticIdentity),
      Vector("definitionArgument0")
    )
  }

  test("prefix lexical boundary survives interpolated completion") {
    val negative = bodyTerm("-1")
    val result = dqr"def negative: Int = -($negative)".toOption.get
    assert(result.constructed.render.contains("Unary(-, Parens(Literal(-1)))"))
  }

  test("literal scanner preserves dollars in strings and comments while rejecting no owned markers") {
    val source = StringContext(
      """def text: String = "$inside" /* $comment */"""
    )
    val result = source.dqr().toOption.get
    assert(result.constructed.render.contains("Literal(\"$inside\")"))
    assertEquals(result.sourceEvidence.interpolationOccurrences, Vector.empty)
  }

  test("literal source preserves line endings Unicode escaped strings and long decimals") {
    val prefix =
      "def unicode: String = /* BMP λ supplementary 😀\r\n */ "
    val escaped = bodyTerm("\"quote=\\\" slash=\\\\\"")
    val unicode = StringContext(prefix, "").dqr(escaped).toOption.get
    assertEquals(
      unicode.sourceEvidence.interpolationOccurrences.head.assembledMarkerSpan.start,
      prefix.length
    )
    assert(unicode.constructed.render.contains("quote="))
    assert(unicode.constructed.render.contains("slash="))

    val decimal =
      StringContext("val longValue: Int = -214748364900000000000000000000")
        .dqr()
        .toOption
        .get
    assert(decimal.constructed.render.contains("-214748364900000000000000000000"))
  }

  private def assertSurfaceEvidence(
      evidence: DefinitionQuasiquoteSourceEvidence
  ): Unit =
    val spans = evidence.sourceMap.segments.map(_.generatedSpan)
    assertEquals(evidence.sourceId, evidence.sourceMap.generatedSourceId)
    assertEquals(spans.head.start, 0)
    assertEquals(spans.last.end, evidence.sourceMap.generatedSource.length)
    assert(spans.zip(spans.drop(1)).forall { case (left, right) =>
      left.end == right.start
    })
    assert(evidence.components.definition.end <= evidence.sourceMap.generatedSource.length)
    assert(
      evidence.sourceMap.segments.forall(_.origin match
        case _: SourceOrigin.LiteralPart => true
        case _: SourceOrigin.InterpolationArgument => true
        case _ => false)
    )

  private def assertLiteralOrigins(
      evidence: DefinitionQuasiquoteSourceEvidence,
      parts: Vector[String]
  ): Unit =
    evidence.sourceMap.segments.foreach {
      case GeneratedSegment(
            generatedSpan,
            SourceOrigin.LiteralPart(_, partIndex, spanWithinPart)
          ) =>
        assertEquals(
          evidence.sourceMap.generatedSource.slice(
            generatedSpan.start,
            generatedSpan.end
          ),
          parts(partIndex).slice(spanWithinPart.start, spanWithinPart.end)
        )
      case _ => ()
    }
