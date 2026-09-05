package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionName
import _root_.quasiquotes.parser.{BinderId, BlockStatement, TermShape, TypeShape}
import _root_.quasiquotes.terms.TermShapeTraversal

import scala.meta.*

final class ScalametaP3LocalIdentityAuthoringTest extends munit.FunSuite:
  private type DefinitionBinder = ScalametaTermShapeAuthoring.DefinitionBinder

  private val id0 = BinderId(0)
  private val id1 = BinderId(1)
  private val id3 = BinderId(3)
  private val id7 = BinderId(7)
  private val id8 = BinderId(8)

  test("authors canonical noncanonical and same-spelled identities for all three Types"):
    val fixtures = List(
      p3(id0, "id", id1, "x", identType("Int"), identType("Int"), bound(id1, "x"), bound(id0, "id")),
      p3(id7, "renamed", id8, "argument", identType("Int"), identType("Int"), bound(id8, "stale-parameter"), bound(id7, "stale-method")),
      p3(id7, "keep", id8, "value", identType("String"), identType("String"), bound(id8, "old"), bound(id7, "old")),
      p3(id7, "choose", id8, "flag", identType("Boolean"), identType("Boolean"), bound(id8, "old"), bound(id7, "old")),
      p3(id7, "same", id8, "same", identType("Int"), identType("Int"), bound(id8, "parameter"), bound(id7, "method"))
    )

    fixtures.foreach { shape =>
      val authored = author(shape).asInstanceOf[Term.Block]
      assertEquals(authored.stats.map(_.productPrefix), List("Defn.Def", "Term.Name"))
      assert(allTrees(authored).forall(_.pos == Position.None), clues(shape))
      assertPublicAlphaRoundTrip(shape, authored)
    }

  test("keeps parameter and method binders in disjoint activation sites"):
    val shape = p3(
      id7,
      "identity",
      id8,
      "value",
      identType("Int"),
      identType("Int"),
      bound(id8, "ignored-body-display"),
      bound(id7, "ignored-result-display")
    )

    ScalametaTermProjection.project(author(shape)).toOption.get.shape match
      case TermShape.Block(
            List(BlockStatement.LocalDef(methodId, "identity", parameterId, "value", _, _, body)),
            result
          ) =>
        assertEquals(body, TermShape.BoundReference(parameterId, "value"))
        assertEquals(result, TermShape.BoundReference(methodId, "identity"))
        assertNotEquals(methodId, parameterId)
      case other => fail(s"unexpected P3 projection: ${other.render}")

  test("authors a root P3 under distinct seeded Definition binders"):
    val outer = definitionBinder(id3, "outer")
    val shape = p3(
      id7,
      "identity",
      id8,
      "value",
      identType("Int"),
      identType("Int"),
      bound(id8, "stale"),
      bound(id7, "stale")
    )

    val authored = authorSeeded(shape, Vector(outer))
    assertSeededAlphaRoundTrip(shape, authored, Vector(outer))
    assert(allTrees(authored).forall(_.pos == Position.None))

  test("rejects missing duplicate and outer-colliding local BinderIds"):
    val base = p3(id7, "identity", id8, "value", identType("Int"), identType("Int"), bound(id8, "value"), bound(id7, "identity"))
    val failures = List(
      replaceIds(base, null, id8) -> "NEUTRAL_TERM_AUTHORING_P3_SCOPE_UNSUPPORTED",
      replaceIds(base, id7, null) -> "NEUTRAL_TERM_AUTHORING_P3_SCOPE_UNSUPPORTED",
      replaceIds(base, id7, id7) -> "NEUTRAL_TERM_AUTHORING_P3_SCOPE_UNSUPPORTED"
    )

    failures.foreach { (shape, code) => assertErrorCode(shape, code) }
    assertSeededErrorCode(replaceIds(base, id3, id8), Vector(definitionBinder(id3, "outer")), "NEUTRAL_TERM_AUTHORING_P3_SCOPE_UNSUPPORTED")
    assertSeededErrorCode(replaceIds(base, id7, id3), Vector(definitionBinder(id3, "outer")), "NEUTRAL_TERM_AUTHORING_P3_SCOPE_UNSUPPORTED")
    assertSeededErrorCode(
      base,
      Vector(definitionBinder(BinderId(Int.MaxValue - 1), "outer")),
      "NEUTRAL_TERM_AUTHORING_P3_SCOPE_UNSUPPORTED"
    )

  test("rejects collisions and distinct P3 nesting under active Lambda and P2 scopes"):
    val lambdaCollision = TermShape.Lambda1(
      id3,
      "outer",
      "Int",
      p3(id3, "identity", id8, "value", identType("Int"), identType("Int"), bound(id8, "value"), bound(id3, "identity"))
    )
    val lambdaNested = TermShape.Lambda1(
      id3,
      "outer",
      "Int",
      p3(id7, "identity", id8, "value", identType("Int"), identType("Int"), bound(id8, "value"), bound(id7, "identity"))
    )
    val lambdaParameterCollision = TermShape.Lambda1(
      id3,
      "outer",
      "Int",
      p3(id7, "identity", id3, "value", identType("Int"), identType("Int"), bound(id3, "value"), bound(id7, "identity"))
    )
    val p2Collision = p2(
      id3,
      "outer",
      p3(id3, "identity", id8, "value", identType("Int"), identType("Int"), bound(id8, "value"), bound(id3, "identity"))
    )
    val p2Nested = p2(
      id3,
      "outer",
      p3(id7, "identity", id8, "value", identType("Int"), identType("Int"), bound(id8, "value"), bound(id7, "identity"))
    )
    val p2ParameterCollision = p2(
      id3,
      "outer",
      p3(id7, "identity", id3, "value", identType("Int"), identType("Int"), bound(id3, "value"), bound(id7, "identity"))
    )

    assertErrorCode(lambdaCollision, "NEUTRAL_TERM_AUTHORING_P3_SCOPE_UNSUPPORTED")
    assertErrorCode(lambdaParameterCollision, "NEUTRAL_TERM_AUTHORING_P3_SCOPE_UNSUPPORTED")
    assertErrorCode(p2Collision, "NEUTRAL_TERM_AUTHORING_P3_SCOPE_UNSUPPORTED")
    assertErrorCode(p2ParameterCollision, "NEUTRAL_TERM_AUTHORING_P3_SCOPE_UNSUPPORTED")
    assertErrorCode(lambdaNested, "NEUTRAL_TERM_AUTHORING_P3_NESTED_UNSUPPORTED")
    assertErrorCode(p2Nested, "NEUTRAL_TERM_AUTHORING_P3_NESTED_UNSUPPORTED")

  test("rejects null empty malformed and keyword declaration names through existing boundaries"):
    val failures = List(
      p3(id7, null, id8, "value", identType("Int"), identType("Int"), bound(id8, "value"), bound(id7, "identity")) -> "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED",
      p3(id7, "identity", id8, null, identType("Int"), identType("Int"), bound(id8, "value"), bound(id7, "identity")) -> "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED",
      p3(id7, "", id8, "value", identType("Int"), identType("Int"), bound(id8, "value"), bound(id7, "identity")) -> "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED",
      p3(id7, "identity", id8, "", identType("Int"), identType("Int"), bound(id8, "value"), bound(id7, "identity")) -> "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED",
      p3(id7, "bad.name", id8, "value", identType("Int"), identType("Int"), bound(id8, "value"), bound(id7, "identity")) -> "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED",
      p3(id7, "identity", id8, "bad.name", identType("Int"), identType("Int"), bound(id8, "value"), bound(id7, "identity")) -> "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED",
      p3(id7, "match", id8, "value", identType("Int"), identType("Int"), bound(id8, "value"), bound(id7, "identity")) -> "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED",
      p3(id7, "identity", id8, "match", identType("Int"), identType("Int"), bound(id8, "value"), bound(id7, "identity")) -> "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED"
    )

    failures.foreach { (shape, code) => assertErrorCode(shape, code) }

  test("rejects every non-leaf primitive or mismatched P3 Type pair"):
    val unsupported = List[(TypeShape, TypeShape)](
      (null: TypeShape) -> identType("Int"),
      identType("Int") -> (null: TypeShape),
      identType("AnyVal") -> identType("AnyVal"),
      identType("Long") -> identType("Long"),
      TypeShape.Select(identType("scala"), "Int") -> TypeShape.Select(identType("scala"), "Int"),
      TypeShape.Apply(identType("Option"), List(identType("Int"))) -> TypeShape.Apply(identType("Option"), List(identType("Int"))),
      TypeShape.Tuple(List(identType("Int"), identType("String"))) -> TypeShape.Tuple(List(identType("Int"), identType("String"))),
      TypeShape.Function(List(identType("Int")), identType("String")) -> TypeShape.Function(List(identType("Int")), identType("String")),
      TypeShape.Parenthesized(identType("Int")) -> TypeShape.Parenthesized(identType("Int")),
      identType("Int") -> identType("String")
    )

    unsupported.foreach { (parameterType, resultType) =>
      assertErrorCode(
        p3(id7, "identity", id8, "value", parameterType, resultType, bound(id8, "value"), bound(id7, "identity")),
        "NEUTRAL_TERM_AUTHORING_P3_TYPE_UNSUPPORTED"
      )
    }

  test("requires the body to be exactly the parameter reference"):
    val bodies = List(
      bound(id7, "recursive"),
      TermShape.Identifier("value", false),
      TermShape.Identifier("other", false),
      TermShape.Literal("1"),
      TermShape.Apply(TermShape.Identifier("f", false), List(bound(id8, "value"))),
      p2(BinderId(10), "nested", bound(BinderId(10), "nested")),
      null
    )

    bodies.foreach { body =>
      assertErrorCode(
        p3(id7, "identity", id8, "value", identType("Int"), identType("Int"), body, bound(id7, "identity")),
        "NEUTRAL_TERM_AUTHORING_P3_BODY_UNSUPPORTED"
      )
    }

  test("requires the final result to be exactly the method reference"):
    val results = List(
      bound(id8, "parameter"),
      TermShape.Identifier("identity", false),
      TermShape.Identifier("other", false),
      TermShape.Literal("1"),
      TermShape.Apply(bound(id7, "identity"), List(TermShape.Literal("1"))),
      p2(BinderId(10), "nested", bound(BinderId(10), "nested")),
      null
    )

    results.foreach { result =>
      assertErrorCode(
        p3(id7, "identity", id8, "value", identType("Int"), identType("Int"), bound(id8, "value"), result),
        "NEUTRAL_TERM_AUTHORING_P3_RESULT_UNSUPPORTED"
      )
    }

  test("rejects null carriers second P3 and P2-P3 mixed definitions without widening admission"):
    val first = p3(id7, "identity", id8, "value", identType("Int"), identType("Int"), bound(id8, "value"), bound(id7, "identity"))
    val second = p3(BinderId(10), "keep", BinderId(11), "input", identType("String"), identType("String"), bound(BinderId(11), "input"), bound(BinderId(10), "keep"))
    val nullCarrier = TermShape.Block(List(null), TermShape.Identifier("result", false))
    val siblings = TermShape.Tuple(List(first, second))
    val p2Initializer = TermShape.Block(
      List(BlockStatement.LocalVal(id3, "outer", "Int", first)),
      bound(id3, "outer")
    )

    assertErrorCode(nullCarrier, "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED")
    assertErrorCode(siblings, "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED")
    assertErrorCode(p2Initializer, "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED")

  test("preserves Parenthesized P1 P2 Lambda Typed and public raw BoundReference boundaries"):
    val p1 = TermShape.Block(List(TermShape.Literal("1")), TermShape.Identifier("result", false))
    val p2Shape = p2(id7, "value", bound(id7, "value"))
    val lambda = TermShape.Lambda1(id7, "value", "Int", bound(id7, "value"))
    val typed = TermShape.Typed(TermShape.Literal("1"), "Int")

    List(p1, p2Shape, lambda, typed).foreach(shape => assertPublicAlphaRoundTrip(shape, author(shape)))
    assertErrorCode(TermShape.Parenthesized(TermShape.Literal("1")), "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED")
    assertEquals(
      ScalametaTermShapeAuthoring.author(bound(id0, "x")),
      Left(
        ScalametaTermShapeAuthoring.Error(
          "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED",
          "this TermShape family is outside binder-free N013-N015/N019 authoring."
        )
      )
    )

  private def p3(
      methodId: BinderId,
      methodName: String,
      parameterId: BinderId,
      parameterName: String,
      parameterType: TypeShape,
      resultType: TypeShape,
      body: TermShape,
      result: TermShape
  ): TermShape =
    TermShape.Block(
      List(BlockStatement.LocalDef(methodId, methodName, parameterId, parameterName, parameterType, resultType, body)),
      result
    )

  private def replaceIds(shape: TermShape, methodId: BinderId, parameterId: BinderId): TermShape =
    shape match
      case TermShape.Block(List(local: BlockStatement.LocalDef), _) =>
        p3(
          methodId,
          local.methodDisplayName,
          parameterId,
          local.parameterDisplayName,
          local.parameterType,
          local.resultType,
          bound(parameterId, "value"),
          bound(methodId, "identity")
        )
      case other => fail(other.render)

  private def p2(id: BinderId, name: String, result: TermShape): TermShape =
    TermShape.Block(
      List(BlockStatement.LocalVal(id, name, "Int", TermShape.Literal("1"))),
      result
    )

  private def bound(id: BinderId, display: String): TermShape =
    TermShape.BoundReference(id, display)

  private def identType(name: String): TypeShape = TypeShape.Identifier(name)

  private def definitionBinder(id: BinderId, name: String): DefinitionBinder =
    ScalametaTermShapeAuthoring.DefinitionBinder(id, DefinitionName.plain(name).toOption.get)

  private def author(shape: TermShape): Term =
    ScalametaTermShapeAuthoring.author(shape) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def authorSeeded(shape: TermShape, binders: Vector[DefinitionBinder]): Term =
    ScalametaTermShapeAuthoring.authorWithDefinitionBinders(shape, binders) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def assertPublicAlphaRoundTrip(expected: TermShape, authored: Term): Unit =
    val projected = ScalametaTermProjection.project(authored).toOption.get
    assertEquals(TermShapeTraversal.alphaNormalize(projected.shape), TermShapeTraversal.alphaNormalize(expected))
    assertEquals(projected.sourceSpan, None)

  private def assertSeededAlphaRoundTrip(expected: TermShape, authored: Term, binders: Vector[DefinitionBinder]): Unit =
    val projectionBinders = binders.map(binder => ScalametaTermProjection.DefinitionBinder(binder.name.decoded, binder.binderId))
    val projected = ScalametaTermProjection.projectWithDefinitionBinders(authored, projectionBinders).toOption.get
    val binderIds = binders.map(_.binderId)
    assertEquals(
      TermShapeTraversal.alphaNormalizeInScope(projected.shape, binderIds),
      TermShapeTraversal.alphaNormalizeInScope(expected, binderIds)
    )
    assertEquals(projected.sourceSpan, None)

  private def assertErrorCode(shape: TermShape, expected: String): Unit =
    assertEquals(ScalametaTermShapeAuthoring.author(shape).left.toOption.map(_.code), Some(expected), clues(shape))

  private def assertSeededErrorCode(shape: TermShape, binders: Vector[DefinitionBinder], expected: String): Unit =
    assertEquals(
      ScalametaTermShapeAuthoring.authorWithDefinitionBinders(shape, binders).left.toOption.map(_.code),
      Some(expected),
      clues(shape)
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
