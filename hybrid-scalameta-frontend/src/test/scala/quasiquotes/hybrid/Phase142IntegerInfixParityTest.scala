package quasiquotes.hybrid

import scala.quoted.staging.{Compiler, withQuotes}

import scala.meta.{Term as MetaTerm, *}
import scala.meta.dialects

import _root_.quasiquotes.construct.hybrid.{HybridTermFrontend, ScalametaTermFrontend}
import _root_.quasiquotes.neutral.ScalametaTermProjection

final class Phase142IntegerInfixParityTest extends munit.FunSuite:
  private val expectedRows = List(
    ("literal", "1", "Int", "1", "Literal(1)"),
    ("addition", "1 + 1", "Int", "2", "Infix(Literal(1), +, Literal(1))"),
    (
      "precedence",
      "1 + 2 * 3",
      "Int",
      "7",
      "Infix(Literal(1), +, Infix(Literal(2), *, Literal(3)))"
    ),
    ("negative-left", "-1 + 2", "Int", "1", "Infix(Literal(-1), +, Literal(2))"),
    ("subtraction", "7 - 3", "Int", "4", "Infix(Literal(7), -, Literal(3))"),
    ("division", "7 / 2", "Int", "3", "Infix(Literal(7), /, Literal(2))"),
    ("remainder", "7 % 4", "Int", "3", "Infix(Literal(7), %, Literal(4))"),
    ("equal", "1 == 1", "Boolean", "true", "Infix(Literal(1), ==, Literal(1))"),
    ("not-equal", "1 != 2", "Boolean", "true", "Infix(Literal(1), !=, Literal(2))"),
    ("less", "1 < 2", "Boolean", "true", "Infix(Literal(1), <, Literal(2))"),
    ("less-equal", "1 <= 1", "Boolean", "true", "Infix(Literal(1), <=, Literal(1))"),
    ("greater", "2 > 1", "Boolean", "true", "Infix(Literal(2), >, Literal(1))"),
    ("greater-equal", "2 >= 2", "Boolean", "true", "Infix(Literal(2), >=, Literal(2))")
  )

  test("fixed integer and infix matrix preserves typed values, precedence, and Scalameta routing"):
    val rows = Phase142IntegerInfixParityMacros.matrixEvidence

    assertEquals(rows.map(row => (row.id, row.source)), expectedRows.map(row => (row._1, row._2)))
    rows.zip(expectedRows).foreach { (row, expected) =>
      val (_, _, expectedType, expectedValue, expectedShape) = expected
      assertEquals(row.expectedType, expectedType, row.id)
      assertEquals(row.currentValue, expectedValue, row.id)
      assertEquals(row.scalametaValue, expectedValue, row.id)
      assertEquals(row.programmaticValue, expectedValue, row.id)
      assert(row.currentHasExpectedType, row.id)
      assert(row.scalametaHasExpectedType, row.id)
      assert(row.programmaticHasExpectedType, row.id)
      assertEquals(row.currentStructure, row.scalametaStructure, row.id)
      assertEquals(row.scalametaStructure, row.programmaticStructure, row.id)
      assertEquals(row.neutralShape, expectedShape, row.id)
      assertEquals(row.currentParserShape, expectedShape, row.id)
      assertEquals(row.engine, "Scalameta", row.id)
      assert(row.primaryFailureEmpty, row.id)
    }

  test("current and Scalameta typed matchers agree and preserve reflected capture identity"):
    val evidence = Phase142IntegerInfixParityMacros.matchingEvidence

    assert(evidence.currentFixedSuccess)
    assert(evidence.scalametaFixedSuccess)
    assert(evidence.currentFixedMismatch)
    assert(evidence.scalametaFixedMismatch)
    assert(evidence.currentPatternMatchesScalametaTerm)
    assert(evidence.scalametaPatternMatchesCurrentTerm)
    assert(evidence.currentQqCapturesCurrentOriginals)
    assert(evidence.currentQqCapturesScalametaOriginals)
    assert(evidence.scalametaQqCapturesCurrentOriginals)
    assert(evidence.scalametaQqCapturesScalametaOriginals)
    assertEquals(evidence.scalametaPatternEngine, "Scalameta")
    assert(evidence.scalametaPatternPrimaryFailureEmpty)

  test("fallback and neutral-overlap boundaries remain explicit"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val parseFallback = HybridTermFrontend.build(using q)(
        Seq("if true then 1 else 2"),
        Nil,
        dialects.Scala213
      )
      val terminalLowering = HybridTermFrontend.build(using q)(
        Seq("{ def boundedIdentity(value: Int): Int = value; boundedIdentity(1) }"),
        Nil
      )
      val hybridString = HybridTermFrontend.build(using q)(Seq("\"value\""), Nil)
      val neutralString = ScalametaTermProjection.project(Lit.String("value"))

      val typedInfix = MetaTerm.ApplyInfix(
        Lit.Int(1),
        MetaTerm.Name("+"),
        scala.meta.Type.ArgClause(List(scala.meta.Type.Name("Int"))),
        MetaTerm.ArgClause(List(Lit.Int(2)))
      )
      val contextualInfix = MetaTerm.ApplyInfix(
        Lit.Int(1),
        MetaTerm.Name("+"),
        scala.meta.Type.ArgClause(Nil),
        MetaTerm.ArgClause(List(Lit.Int(2)), Some(Mod.Using()))
      )
      val multipleRhs = MetaTerm.ApplyInfix(
        Lit.Int(1),
        MetaTerm.Name("+"),
        scala.meta.Type.ArgClause(Nil),
        MetaTerm.ArgClause(List(Lit.Int(2), Lit.Int(3)))
      )
      def hybridTree(tree: MetaTerm) =
        ScalametaTermFrontend.lowerTree(using q)(tree, Map.empty, Map.empty, Map.empty, Map.empty)

      val parsedHoleTopology = dialects.Scala3("termHole + 2").parse[MetaTerm].get
      val callerHole = '{ 40 }.asTerm
      val hybridHole = ScalametaTermFrontend.lower(using q)(Seq("", " + 2"), Seq(callerHole))

      (
        parseFallback.map(result => result.engine.toString -> result.primaryFailure.map(_.category)),
        terminalLowering.left.map(_.category),
        hybridString.map(result => result.engine.toString -> result.primaryFailure.isEmpty),
        neutralString.left.map(_.code),
        ScalametaTermProjection.project(typedInfix).left.map(_.code),
        hybridTree(typedInfix).isRight,
        ScalametaTermProjection.project(contextualInfix).left.map(_.code),
        hybridTree(contextualInfix).isRight,
        ScalametaTermProjection.project(multipleRhs).left.map(_.code),
        hybridTree(multipleRhs).left.map(_.category),
        ScalametaTermProjection.project(parsedHoleTopology).map(_.shape.render),
        hybridHole.map(term => term.tpe.widen =:= TypeRepr.of[Int])
      )

    assertEquals(evidence._1, Right("CurrentDottyFallback" -> Some("SCALAMETA_PARSE_FAILURE")))
    assertEquals(evidence._2, Left("SCALAMETA_LOWERING_UNSUPPORTED"))
    assertEquals(evidence._3, Right("Scalameta" -> true))
    assertEquals(evidence._4, Left("NEUTRAL_TERM_UNSUPPORTED"))
    assertEquals(evidence._5, Left("NEUTRAL_INFIX_TYPE_ARGUMENTS_UNSUPPORTED"))
    assert(evidence._6)
    assertEquals(evidence._7, Left("NEUTRAL_INFIX_ARGUMENT_CLAUSE_UNSUPPORTED"))
    assert(evidence._8)
    assertEquals(evidence._9, Left("NEUTRAL_INFIX_ARGUMENT_CLAUSE_UNSUPPORTED"))
    assertEquals(evidence._10, Left("SCALAMETA_LOWERING_UNSUPPORTED"))
    assertEquals(evidence._11, Right("Infix(Ident(termHole), +, Literal(2))"))
    assertEquals(evidence._12, Right(true))
