package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionName
import _root_.quasiquotes.parser.{BinderId, BlockStatement, TermShape, TypeShape}
import _root_.quasiquotes.terms.TermShapeTraversal

import scala.meta.*

final class ScalametaP2LocalValAuthoringTest extends munit.FunSuite:
  private type DefinitionBinder = ScalametaTermShapeAuthoring.DefinitionBinder

  private val id0 = BinderId(0)
  private val id3 = BinderId(3)
  private val id7 = BinderId(7)

  test("authors canonical and noncanonical P2 binders with the four leaf declared Types"):
    val fixtures = List(
      p2(id0, "x", "Int", literal("1"), bound(id0, "x")),
      p2(id7, "text", "String", literal("\"seed\""), bound(id7, "stale")),
      p2(id7, "flag", "Boolean", literal("true"), bound(id7, "ignored")),
      p2(id7, "value", "AnyVal", literal("1"), bound(id7, "old"))
    )

    fixtures.foreach { shape =>
      val authored = author(shape).asInstanceOf[Term.Block]
      val definition = authored.stats.head.asInstanceOf[Defn.Val]

      assertEquals(authored.stats.map(_.productPrefix), List("Defn.Val", "Term.Name"))
      assertEquals(definition.mods, Nil)
      assertEquals(definition.pats.size, 1)
      assert(definition.decltpe.exists(_.isInstanceOf[Type.Name]))
      assert(allTrees(authored).forall(_.pos == Position.None), clues(shape))
      assertPublicAlphaRoundTrip(shape, authored)
    }

  test("authors the initializer outside and the result inside the local scope"):
    val shape = p2(
      id7,
      "x",
      "Int",
      TermShape.Identifier("x", false),
      bound(id7, "stale")
    )
    val authored = author(shape)

    ScalametaTermProjection.project(authored).toOption.get.shape match
      case TermShape.Block(
            List(
              BlockStatement.LocalVal(
                projectedId,
                "x",
                "Int",
                TermShape.Identifier("x", false)
              )
            ),
            TermShape.BoundReference(resultId, "x")
          ) => assertEquals(resultId, projectedId)
      case other => fail(s"unexpected old/new-scope projection: ${other.render}")
    assertPublicAlphaRoundTrip(shape, authored)

  test("reuses every required recursive family in the P2 result"):
    val local = bound(id7, "stale")
    val results = List(
      TermShape.Select(local, "field"),
      TermShape.Apply(TermShape.Identifier("consume", false), List(local)),
      TermShape.Infix(local, "+", literal("1")),
      TermShape.Tuple(List(local, literal("2"))),
      TermShape.If(TermShape.Identifier("flag", false), local, literal("0")),
      TermShape.InterpolatedString("s", List("value=", ""), List(local)),
      TermShape.Typed(local, "Int")
    )

    results.foreach { result =>
      val shape = p2(id7, "x", "Int", literal("1"), result)
      assertPublicAlphaRoundTrip(shape, author(shape))
    }

  test("preserves unrelated free results and selected same-name members"):
    val fixtures = List(
      p2(id7, "x", "Int", literal("1"), TermShape.Identifier("other", false)),
      p2(
        id7,
        "x",
        "Int",
        literal("1"),
        TermShape.Select(TermShape.Identifier("service", false), "x")
      )
    )

    fixtures.foreach(shape => assertPublicAlphaRoundTrip(shape, author(shape)))

  test("threads distinct Lambda and local binders through initializer and result"):
    val shape = TermShape.Lambda1(
      id3,
      "outer",
      "Int",
      p2(
        id7,
        "x",
        "Int",
        bound(id3, "outer-stale"),
        TermShape.Tuple(List(bound(id3, "outer-old"), bound(id7, "local-old")))
      )
    )

    assertPublicAlphaRoundTrip(shape, author(shape))

  test("threads seeded Definition and local binders through initializer and result"):
    val outer = definitionBinder(id3, "outer")
    val shape = p2(
      id7,
      "x",
      "Int",
      bound(id3, "outer-stale"),
      TermShape.Tuple(List(bound(id3, "outer-old"), bound(id7, "local-old")))
    )

    val authored = authorSeeded(shape, Vector(outer))
    assertSeededAlphaRoundTrip(shape, authored, Vector(outer))
    assert(allTrees(authored).forall(_.pos == Position.None))

  test("pins seeded same-name shadowing to existing projection semantics"):
    val outer = definitionBinder(id3, "x")
    val localReference = p2(
      id7,
      "x",
      "Int",
      bound(id3, "outer-initializer"),
      bound(id7, "local-result")
    )
    val capturedOuterReference = p2(
      id7,
      "x",
      "Int",
      literal("1"),
      bound(id3, "outer-result")
    )

    assertSeededAlphaRoundTrip(
      localReference,
      authorSeeded(localReference, Vector(outer)),
      Vector(outer)
    )
    assertSeededErrorCode(
      capturedOuterReference,
      Vector(outer),
      "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED"
    )

  test("rejects every unsupported P2 declared Type with one stable category"):
    val unsupported = List(
      "Long",
      "Option[Int]",
      "Either[Int, String]",
      "(Int, String)",
      "Int => String",
      "scala.Int",
      "java.lang.String",
      "",
      null
    )

    unsupported.foreach { declaredType =>
      assertErrorCode(
        p2(id7, "x", declaredType, literal("1"), bound(id7, "x")),
        "NEUTRAL_TERM_AUTHORING_P2_DECLARED_TYPE_UNSUPPORTED"
      )
    }

  test("fails closed for malformed P2 structure and preserves child categories"):
    val failures = List(
      p2(null, "x", "Int", literal("1"), literal("2")) ->
        "NEUTRAL_TERM_AUTHORING_P2_SCOPE_UNSUPPORTED",
      p2(id7, null, "Int", literal("1"), literal("2")) ->
        "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED",
      p2(id7, "", "Int", literal("1"), literal("2")) ->
        "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED",
      p2(id7, "bad.name", "Int", literal("1"), literal("2")) ->
        "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED",
      p2(id7, "match", "Int", literal("1"), literal("2")) ->
        "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED",
      p2(id7, "x", "Int", null, literal("2")) ->
        "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED",
      p2(id7, "x", "Int", literal("1"), null) ->
        "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED",
      p2(id7, "x", "Int", TermShape.Parenthesized(literal("1")), literal("2")) ->
        "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED",
      p2(id7, "x", "Int", literal("01"), literal("2")) ->
        "NEUTRAL_TERM_AUTHORING_LITERAL_UNSUPPORTED"
    )

    failures.foreach { (shape, expectedCode) => assertErrorCode(shape, expectedCode) }

  test("rejects a local self-reference in the initializer and unknown result references"):
    val initializerSelfReference = p2(
      id7,
      "x",
      "Int",
      bound(id7, "x"),
      bound(id7, "x")
    )
    val unknownResult = p2(
      id7,
      "x",
      "Int",
      literal("1"),
      bound(BinderId(9), "missing")
    )

    assert(ScalametaTermShapeAuthoring.author(initializerSelfReference).isLeft)
    assertErrorCode(unknownResult, "NEUTRAL_TERM_AUTHORING_P2_SCOPE_UNSUPPORTED")

  test("rejects nested and second P2 values without adding a global tracker"):
    val inner = p2(BinderId(8), "y", "Int", literal("2"), bound(BinderId(8), "y"))
    val nestedResult = p2(id7, "x", "Int", literal("1"), inner)
    val nestedInitializer = p2(id7, "x", "Int", inner, bound(id7, "x"))
    val siblings = TermShape.Tuple(
      List(
        p2(id7, "x", "Int", literal("1"), bound(id7, "x")),
        p2(BinderId(8), "y", "Int", literal("2"), bound(BinderId(8), "y"))
      )
    )

    assertErrorCode(nestedResult, "NEUTRAL_TERM_AUTHORING_P2_NESTED_UNSUPPORTED")
    assertErrorCode(nestedInitializer, "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED")
    assertErrorCode(siblings, "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED")

  test("rejects local BinderId collisions with active Lambda and Definition binders"):
    val lambdaCollision = TermShape.Lambda1(
      id7,
      "outer",
      "Int",
      p2(id7, "x", "Int", literal("1"), bound(id7, "x"))
    )
    val outer = definitionBinder(id7, "outer")
    val definitionCollision = p2(id7, "x", "Int", literal("1"), bound(id7, "x"))

    assertErrorCode(lambdaCollision, "NEUTRAL_TERM_AUTHORING_P2_SCOPE_UNSUPPORTED")
    assertSeededErrorCode(
      definitionCollision,
      Vector(outer),
      "NEUTRAL_TERM_AUTHORING_P2_SCOPE_UNSUPPORTED"
    )

  test("rejects result capture and same-name Lambda/P2 source shadowing"):
    val captures = List(
      TermShape.Identifier("x", false),
      TermShape.Tuple(List(literal("1"), TermShape.Identifier("x", false))),
      TermShape.Typed(TermShape.Identifier("x", false), "Int")
    )
    val sameNameUnderLambda = TermShape.Lambda1(
      id3,
      "x",
      "Int",
      p2(id7, "x", "Int", literal("1"), bound(id7, "x"))
    )

    captures.foreach(result =>
      assertErrorCode(
        p2(id7, "x", "Int", literal("1"), result),
        "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED"
      )
    )
    assertErrorCode(sameNameUnderLambda, "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED")

  test("preserves public raw BoundReference Parenthesized and invalid-P3 exclusions"):
    assertEquals(
      ScalametaTermShapeAuthoring.author(bound(id0, "x")),
      Left(
        ScalametaTermShapeAuthoring.Error(
          "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED",
          "this TermShape family is outside binder-free N013-N015/N019 authoring."
        )
      )
    )
    assertErrorCode(
      TermShape.Parenthesized(TermShape.Identifier("x", false)),
      "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED"
    )
    val localDef = BlockStatement.LocalDef(
      BinderId(1),
      "identity",
      BinderId(2),
      "value",
      TypeShape.Identifier("Int"),
      TypeShape.Identifier("Int"),
      bound(BinderId(2), "value")
    )
    assertErrorCode(
      TermShape.Block(List(localDef), TermShape.Identifier("identity", false)),
      "NEUTRAL_TERM_AUTHORING_P3_RESULT_UNSUPPORTED"
    )

  private def p2(
      binderId: BinderId,
      name: String,
      declaredType: String,
      initializer: TermShape,
      result: TermShape
  ): TermShape =
    TermShape.Block(
      List(BlockStatement.LocalVal(binderId, name, declaredType, initializer)),
      result
    )

  private def bound(binderId: BinderId, displayName: String): TermShape =
    TermShape.BoundReference(binderId, displayName)

  private def literal(value: String): TermShape = TermShape.Literal(value)

  private def definitionBinder(id: BinderId, name: String): DefinitionBinder =
    ScalametaTermShapeAuthoring.DefinitionBinder(
      id,
      DefinitionName.plain(name).toOption.get
    )

  private def author(shape: TermShape): Term =
    ScalametaTermShapeAuthoring.author(shape) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def authorSeeded(
      shape: TermShape,
      binders: Vector[DefinitionBinder]
  ): Term =
    ScalametaTermShapeAuthoring.authorWithDefinitionBinders(shape, binders) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def assertPublicAlphaRoundTrip(expected: TermShape, authored: Term): Unit =
    val projected = ScalametaTermProjection.project(authored).toOption.get
    assertEquals(
      TermShapeTraversal.alphaNormalize(projected.shape),
      TermShapeTraversal.alphaNormalize(expected)
    )
    assertEquals(projected.sourceSpan, None)

  private def assertSeededAlphaRoundTrip(
      expected: TermShape,
      authored: Term,
      binders: Vector[DefinitionBinder]
  ): Unit =
    val projectionBinders = binders.map(binder =>
      ScalametaTermProjection.DefinitionBinder(binder.name.decoded, binder.binderId)
    )
    val projected = ScalametaTermProjection
      .projectWithDefinitionBinders(authored, projectionBinders)
      .toOption
      .get
    val binderIds = binders.map(_.binderId)

    assertEquals(
      TermShapeTraversal.alphaNormalizeInScope(projected.shape, binderIds),
      TermShapeTraversal.alphaNormalizeInScope(expected, binderIds)
    )
    assertEquals(projected.sourceSpan, None)

  private def assertErrorCode(shape: TermShape, expectedCode: String): Unit =
    assertEquals(
      ScalametaTermShapeAuthoring.author(shape).left.toOption.map(_.code),
      Some(expectedCode),
      clues(shape)
    )

  private def assertSeededErrorCode(
      shape: TermShape,
      binders: Vector[DefinitionBinder],
      expectedCode: String
  ): Unit =
    assertEquals(
      ScalametaTermShapeAuthoring
        .authorWithDefinitionBinders(shape, binders)
        .left
        .toOption
        .map(_.code),
      Some(expectedCode),
      clues(shape)
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
