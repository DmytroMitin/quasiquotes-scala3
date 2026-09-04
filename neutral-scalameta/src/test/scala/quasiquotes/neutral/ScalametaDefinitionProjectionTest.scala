package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionNameSpelling, DefinitionShape}
import _root_.quasiquotes.parser.{BinderId, TermShape, TypeShape}

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaDefinitionProjectionTest extends munit.FunSuite:
  test("dispatches each canonical family to its accepted projector without changing the result"):
    val value = parsedVal("val answer: Int = 42")
    val parameterless = parsedDef("def answer: Int = 42")
    val single = parsedDef("def id(x: Int): Int = x")
    val pair = parsedDef("def pair(x: Int, y: Int): (Int, Int) = (x, y)")
    val alias = parsedType("type Result = Option[Int]")

    assertEquivalent(value, ScalametaTypedImmutableValProjection.project(value))
    assertEquivalent(
      parameterless,
      ScalametaTypedParameterlessDefProjection.project(parameterless)
    )
    assertEquivalent(single, ScalametaTypedSingleParameterDefProjection.project(single))
    assertEquivalent(pair, ScalametaTypedTwoParameterDefProjection.project(pair))
    assertEquivalent(alias, ScalametaSimpleTypeAliasProjection.project(alias))

  test("preserves renamed and Core-admitted backticked names through direct delegation"):
    val fixtures = List(
      parsedVal("val renamedValue: Int = 42") ->
        ScalametaTypedImmutableValProjection.project(parsedVal("val renamedValue: Int = 42")),
      parsedDef("def `type`: Int = 42") ->
        ScalametaTypedParameterlessDefProjection.project(parsedDef("def `type`: Int = 42")),
      parsedDef("def renamed(`match`: Int): Int = `match`") ->
        ScalametaTypedSingleParameterDefProjection.project(
          parsedDef("def renamed(`match`: Int): Int = `match`")
        ),
      parsedDef("def renamed(left: Int, `then`: Int): Int = left + `then`") ->
        ScalametaTypedTwoParameterDefProjection.project(
          parsedDef("def renamed(left: Int, `then`: Int): Int = left + `then`")
        ),
      parsedType("type `type` = Int") ->
        ScalametaSimpleTypeAliasProjection.project(parsedType("type `type` = Int"))
    )

    fixtures.foreach(assertEquivalent)

    val keyword = dispatched(parsedType("type `type` = Int")).shape
      .asInstanceOf[DefinitionShape.SimpleTypeAlias]
    assertEquals(keyword.name.decoded, "type")
    assertEquals(keyword.name.source, "`type`")
    assertEquals(keyword.name.spelling, DefinitionNameSpelling.BacktickedKeyword)

  test("preserves N022 and N023 binder identities shadowing and source order"):
    val sameSpelling = parsedDef("def answer(answer: Int): Int = answer")
    val firstMatches = parsedDef("def answer(answer: Int, y: Int): (Int, Int) = (answer, y)")
    val secondMatches = parsedDef("def answer(x: Int, answer: Int): (Int, Int) = (x, answer)")
    val forward = parsedDef("def pair(x: Int, y: Int): (Int, Int) = (x, y)")
    val reverse = parsedDef("def pair(x: Int, y: Int): (Int, Int) = (y, x)")

    List(
      sameSpelling -> ScalametaTypedSingleParameterDefProjection.project(sameSpelling),
      firstMatches -> ScalametaTypedTwoParameterDefProjection.project(firstMatches),
      secondMatches -> ScalametaTypedTwoParameterDefProjection.project(secondMatches),
      forward -> ScalametaTypedTwoParameterDefProjection.project(forward),
      reverse -> ScalametaTypedTwoParameterDefProjection.project(reverse)
    ).foreach(assertEquivalent)

    val single = dispatched(sameSpelling).shape
      .asInstanceOf[DefinitionShape.SingleParameterDef]
    assertEquals(single.parameterBinderId, BinderId(0))
    assertEquals(single.parameterName.decoded, "answer")
    assertEquals(single.body, TermShape.BoundReference(BinderId(0), "answer"))

    val first = dispatched(firstMatches).shape.asInstanceOf[DefinitionShape.TwoParameterDef]
    assertEquals(first.firstParameterBinderId, BinderId(0))
    assertEquals(first.secondParameterBinderId, BinderId(1))
    assertEquals(first.firstParameterName.decoded, "answer")

    val second = dispatched(secondMatches).shape.asInstanceOf[DefinitionShape.TwoParameterDef]
    assertEquals(second.firstParameterName.decoded, "x")
    assertEquals(second.secondParameterName.decoded, "answer")

    val forwardBody = dispatched(forward).shape
      .asInstanceOf[DefinitionShape.TwoParameterDef]
      .body
    val reverseBody = dispatched(reverse).shape
      .asInstanceOf[DefinitionShape.TwoParameterDef]
      .body
    assertEquals(
      forwardBody,
      TermShape.Tuple(
        List(
          TermShape.BoundReference(BinderId(0), "x"),
          TermShape.BoundReference(BinderId(1), "y")
        )
      )
    )
    assertEquals(
      reverseBody,
      TermShape.Tuple(
        List(
          TermShape.BoundReference(BinderId(1), "y"),
          TermShape.BoundReference(BinderId(0), "x")
        )
      )
    )

  test("preserves positioned and unpositioned provenance for each major root kind"):
    val value = parsedVal("val answer: Int = 42")
    val method = parsedDef("def answer: Int = 42")
    val alias = parsedType("type Result = Int")

    List[Defn](value, method, alias).foreach { definition =>
      val projected = dispatched(definition)
      assertEquals(
        projected.sourceSpan,
        Some(NeutralSourceSpan(definition.pos.start, definition.pos.end))
      )
    }

    List[Defn](value.copy(), method.copy(), alias.copy()).foreach { definition =>
      assertEquals(definition.pos, Position.None)
      assertEquals(dispatched(definition).sourceSpan, None)
    }

  test("rejects unsupported Definition kinds and neighboring method families at dispatcher level"):
    val definitions = List(
      parsed("var answer: Int = 42"),
      parsed("class Answer"),
      parsed("trait Answer"),
      parsed("object Answer"),
      parsed("enum Answer { case Yes }"),
      parsed("given answer: Ordering[Int] = Ordering.Int"),
      parsed("def answer(): Int = 42"),
      parsed("def answer(x: Int, y: Int, z: Int): Int = x"),
      parsed("def answer(x: Int)(y: Int): Int = x"),
      parsed("def answer[A]: Int = 42")
    )

    definitions.foreach(assertErrorCode(_, "NEUTRAL_DEFINITION_FAMILY_UNSUPPORTED"))

  test("preserves N020 and N021 failures after family selection"):
    val valueFailures = List(
      parsedVal("private val answer: Int = 42"),
      parsedVal("val `answer`: Int = 42"),
      parsedVal("val answer: Double = 42")
    )
    valueFailures.foreach(definition =>
      assertEquivalent(definition, ScalametaTypedImmutableValProjection.project(definition))
    )

    val methodFailures = List(
      parsedDef("inline def answer: Int = 42"),
      parsedDef("def answer = 42"),
      parsedDef("def answer: Int = answer")
    )
    methodFailures.foreach(definition =>
      assertEquivalent(
        definition,
        ScalametaTypedParameterlessDefProjection.project(definition)
      )
    )

  test("preserves N022 and N023 failures selected only by structural arity"):
    val singleFailures = List(
      parsedDef("def id[A](x: Int): Int = x"),
      parsedDef("def id(using x: Int): Int = x"),
      parsedDef("def id(x: Double): Int = x"),
      parsedDef("def id(x: Int): Int = value match { case _ => x }"),
      parsedDef("def id(x: Int): Int = id")
    )
    singleFailures.foreach(definition =>
      assertEquivalent(
        definition,
        ScalametaTypedSingleParameterDefProjection.project(definition)
      )
    )

    val pairFailures = List(
      parsedDef("def pair(x: Int, x: Int): Int = x"),
      parsedDef("def pair(x: Int, y: Int): Double = x"),
      parsedDef("def pair(x: Int, y: Int): Int = value match { case _ => x }"),
      parsedDef("def pair(x: Int, y: Int): Int = pair")
    )
    pairFailures.foreach(definition =>
      assertEquivalent(
        definition,
        ScalametaTypedTwoParameterDefProjection.project(definition)
      )
    )

  test("preserves N024 topology and RHS failures after runtime-kind selection"):
    val failures = List(
      parsedType("opaque type Result = Int"),
      parsedType("type Result[A] = A"),
      parsedType("type Result >: Int <: AnyVal = Int"),
      parsedType("type Result = Double")
    )

    failures.foreach(definition =>
      assertEquivalent(definition, ScalametaSimpleTypeAliasProjection.project(definition))
    )

  test("reports stable missing input before family selection"):
    assertEquals(
      ScalametaDefinitionProjection.project(null),
      Left(
        NeutralProjectionError(
          "NEUTRAL_DEFINITION_MISSING",
          "the Scalameta Defn must be present."
        )
      )
    )

  private def assertEquivalent(
      fixture: (Defn, Either[NeutralProjectionError, ProjectedDefinitionShape])
  ): Unit =
    assertEquivalent(fixture._1, fixture._2)

  private def assertEquivalent(
      definition: Defn,
      direct: Either[NeutralProjectionError, ProjectedDefinitionShape]
  ): Unit =
    val dispatchedResult = ScalametaDefinitionProjection.project(definition)
    assertEquals(dispatchedResult, direct, clues(definition))
    (dispatchedResult, direct) match
      case (Right(dispatchedValue), Right(directValue)) =>
        assertEquals(dispatchedValue.shape.getClass.getName, directValue.shape.getClass.getName)
        assertEquals(dispatchedValue.shape.render, directValue.shape.render)
        assertEquals(dispatchedValue.sourceSpan, directValue.sourceSpan)
      case _ => ()

  private def dispatched(definition: Defn): ProjectedDefinitionShape =
    ScalametaDefinitionProjection.project(definition) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def assertErrorCode(definition: Defn, expected: String): Unit =
    assertEquals(
      ScalametaDefinitionProjection.project(definition).left.toOption.map(_.code),
      Some(expected),
      clues(definition)
    )

  private def parsed(source: String): Defn =
    Scala3(source).parse[Stat].get match
      case definition: Defn => definition
      case other => fail(s"expected Defn, found ${other.productPrefix}")

  private def parsedVal(source: String): Defn.Val =
    parsed(source).asInstanceOf[Defn.Val]

  private def parsedDef(source: String): Defn.Def =
    parsed(source).asInstanceOf[Defn.Def]

  private def parsedType(source: String): Defn.Type =
    parsed(source).asInstanceOf[Defn.Type]
