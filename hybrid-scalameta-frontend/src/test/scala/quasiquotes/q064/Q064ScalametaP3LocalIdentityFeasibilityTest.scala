package quasiquotes.q064

import quasiquotes.matching.{BlockPatternStatement, TermPattern}
import quasiquotes.types.TypeNormalForm

final class Q064ScalametaP3LocalIdentityFeasibilityTest extends munit.FunSuite:
  private val intPattern =
    "{ def id(value: Int): Int = value; id($argument) }"

  test("Scalameta parses the exact Defn.Def plus Apply grammar before bounded semantic admission"):
    val syntax = Q064ScalametaP3PatternProbe.inspectSyntax(intPattern).fold(fail(_), identity)
    assertEquals(syntax.rootKind, "Term.Block")
    assertEquals(syntax.statementKinds, List("Defn.Def", "Term.Apply"))
    assertEquals(syntax.methodName, "id")
    assertEquals(syntax.parameterName, "value")
    assertEquals(syntax.parameterType, "Int")
    assertEquals(syntax.resultType, "Int")
    assertEquals(syntax.bodySyntax, "value")
    assertEquals(syntax.resultSyntax, "id(__qqhole_argument)")
    assert(syntax.exactCompilerAccepted, syntax)
    assertEquals(syntax.p2AdmissionAccepted, true)
    assertEquals(syntax.p2AdmissionDetail, "")

  test("the test-only adapter creates the accepted B1 M1 T1 private Core pattern"):
    val compiled = Q064ScalametaP3PatternProbe.compile(intPattern).fold(fail(_), identity)
    assertEquals(compiled.semanticHoleNames, Vector("argument"))
    assertEquals(compiled.parameterType, TypeNormalForm.STypeIdent("Int"))
    assertEquals(compiled.resultType, TypeNormalForm.STypeIdent("Int"))
    compiled.pattern match
      case TermPattern.Block(
            List(
              BlockPatternStatement.LocalDef(
                methodId,
                "id",
                parameterId,
                "value",
                TypeNormalForm.STypeIdent("Int"),
                TypeNormalForm.STypeIdent("Int"),
                TermPattern.BoundReference(bodyId, "value")
              )
            ),
            TermPattern.Apply(
              TermPattern.BoundReference(resultId, "id"),
              List(TermPattern.Hole("argument"))
            )
          ) =>
        assertNotEquals(methodId, parameterId)
        assertEquals(bodyId, parameterId)
        assertEquals(resultId, methodId)
      case other => fail(s"unexpected Q064 private model: ${other.render}")

  test("Int String and Boolean patterns match alpha-renamed targets with exact Term identity"):
    val observations = Q064ScalametaP3MatchingMacros.positiveObservations
    assertEquals(
      observations,
      List(
        "Int:true:true:true:true",
        "String:true:true:true:true",
        "Boolean:true:true:true:true",
        "q063r:true:true:true:true"
      )
    )

  test("semantic holes come only from GeneratedHoleIndex and same-name repetitions stay one binding"):
    val incidental = Q064ScalametaP3PatternProbe
      .compile("{ def id(value: Int): Int = value; id(($argument, __qqhole_argument)._1) }")
      .fold(fail(_), identity)
    assertEquals(incidental.semanticHoleNames, Vector("argument"))
    assertEquals(incidental.generatedHoleNames.size, 1)
    assertNotEquals(incidental.generatedHoleNames.head, "__qqhole_argument")
    assert(incidental.pattern.render.contains("Hole($argument)"), incidental.pattern.render)
    assert(incidental.pattern.render.contains("Ident(__qqhole_argument)"), incidental.pattern.render)

    val repeated = Q064ScalametaP3PatternProbe
      .compile("{ def id(value: Int): Int = value; id(($same, $same)._1) }")
      .fold(fail(_), identity)
    assertEquals(repeated.semanticHoleNames, Vector("same", "same"))
    assertEquals(repeated.generatedHoleNames.distinct.size, 1)
    assertEquals(Q064ScalametaP3MatchingMacros.repeatedHoleMatches, true)

    assert(Q064ScalametaP3PatternProbe.compile("{ def id(value: Int): Int = value; id(__qqhole_argument) }").isLeft)
    assert(Q064ScalametaP3PatternProbe.compile("{ def id(value: Int): Int = value; id(($first, $second)._1) }").isLeft)

  test("grammar and exact compiler acceptance remain distinct from bounded P3 semantic admission"):
    val semanticNegatives = List(
      "{ def id(value: Int): Int = 1; id($argument) }",
      "{ def id(value: Int): Int = value; other($argument) }",
      "{ def id(value: Int): String = value.toString; id($argument) }",
      "{ def id(value: Double): Double = value; id($argument) }",
      "{ def id(value: Int) = value; id($argument) }",
      "{ def id[A](value: Int): Int = value; id($argument) }",
      "{ inline def id(value: Int): Int = value; id($argument) }",
      "{ infix def id(value: Int): Int = value; id($argument) }",
      "{ implicit def id(value: Int): Int = value; id($argument) }",
      "{ @deprecated(\"q064\", \"\") def id(value: Int): Int = value; id($argument) }",
      "{ def id(@deprecated(\"q064\", \"\") value: Int): Int = value; id($argument) }",
      "{ def id(using value: Int): Int = value; id(using $argument) }",
      "{ def id(value: => Int): Int = value; id($argument) }",
      "{ def id(value: Int*): Int = value.head; id($argument) }",
      "{ def id(value: Int = 1): Int = value; id($argument) }",
      "{ def id(value: Int)(other: Int): Int = value; id($argument)(1) }",
      "{ def first(value: Int): Int = value; def second(value: Int): Int = value; first($argument) }",
      "{ def id(value: Int): Int = { def nested(x: Int): Int = x; nested(value) }; id($argument) }"
    )
    semanticNegatives.foreach { source =>
      val syntax = Q064ScalametaP3PatternProbe.syntaxGates(source).fold(fail(_), identity)
      assert(syntax.scalametaParsed, source)
      assert(syntax.exactCompilerAccepted, source)
      assert(Q064ScalametaP3PatternProbe.compile(source).isLeft, source)
    }

  test("wrong targets and local binder confusion reject atomically"):
    assertEquals(
      Q064ScalametaP3MatchingMacros.negativeObservations,
      List(
        "wrong-body:false",
        "wrong-call:false",
        "wrong-type:false",
        "lambda-body:false",
        "non-block:false"
      )
    )

  test("current typed-Scalameta production fails closed after parse and never falls back"):
    val failure = quasiquotes.scalameta.TermFrontend.compile(intPattern).left.toOption.getOrElse(
      fail("current production unexpectedly compiled P3")
    )
    assertEquals(failure.category, "SCALAMETA_PATTERN_LOWERING_UNSUPPORTED")
    assert(failure.detail.contains("P2 block does not support local def definitions"), failure)
