package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionName
import _root_.quasiquotes.parser.{BinderId, TermShape}
import _root_.quasiquotes.terms.TermShapeTraversal

import scala.meta.*

final class ScalametaLambda1AuthoringTest extends munit.FunSuite:
  private type DefinitionBinder = ScalametaTermShapeAuthoring.DefinitionBinder

  private val id0 = BinderId(0)
  private val id7 = BinderId(7)
  private val bound0 = TermShape.BoundReference(id0, "x")

  test("authors canonical and noncanonical Lambda binders with alpha-exact semantics"):
    val fixtures = List(
      lambda(id0, "x", "Int", bound0),
      lambda(id7, "value", "Int", TermShape.BoundReference(id7, "stale")),
      lambda(id7, "text", "String", TermShape.BoundReference(id7, "ignored")),
      lambda(
        id7,
        "flag",
        "Boolean",
        TermShape.If(
          TermShape.BoundReference(id7, "stale"),
          TermShape.Literal("true"),
          TermShape.Literal("false")
        )
      )
    )

    fixtures.foreach { shape =>
      val authored = author(shape).asInstanceOf[Term.Function]

      assertEquals(authored.paramClause.values.size, 1, clues(shape))
      assertEquals(authored.paramClause.mod, None, clues(shape))
      assertEquals(authored.paramClause.values.head.mods, Nil, clues(shape))
      assert(allTrees(authored).forall(_.pos == Position.None), clues(shape))
      assertPublicAlphaRoundTrip(shape, authored)
    }

  test("authors one fresh backticked-keyword Lambda parameter structurally"):
    val shape = lambda(
      id7,
      "match",
      "String",
      TermShape.BoundReference(id7, "stale")
    )
    val authored = author(shape).asInstanceOf[Term.Function]
    val parameter = authored.paramClause.values.head

    assertEquals(parameter.name.value, "match")
    assertEquals(parameter.name.tokens.map(_.text).mkString, "`match`")
    assertEquals(authored.body.asInstanceOf[Term.Name].tokens.map(_.text).mkString, "`match`")
    assertPublicAlphaRoundTrip(shape, authored)

  test("reuses every admitted recursive family inside the Lambda body"):
    val bound = TermShape.BoundReference(id7, "stale")
    val bodies = List(
      TermShape.Select(bound, "field"),
      TermShape.Apply(TermShape.Identifier("f", false), List(bound)),
      TermShape.New("synthetic.unresolved.Widget", List(bound)),
      TermShape.Infix(bound, "+", TermShape.Literal("1")),
      TermShape.Unary("-", bound),
      TermShape.Tuple(List(bound, TermShape.Identifier("free", false))),
      TermShape.If(TermShape.Identifier("flag", false), bound, TermShape.Literal("0")),
      TermShape.InterpolatedString("s", List("value=", ""), List(bound)),
      TermShape.Typed(bound, "Int"),
      TermShape.Block(List(TermShape.Literal("1")), bound)
    )

    bodies.foreach { body =>
      val shape = lambda(id7, "x", "Int", body)
      val authored = author(shape)

      assertPublicAlphaRoundTrip(shape, authored)
      assert(allTrees(authored).forall(_.pos == Position.None), clues(body))
    }

  test("preserves unrelated free names and selected same-name members"):
    val fixtures = List(
      lambda(id7, "x", "Int", TermShape.Identifier("other", false)),
      lambda(
        id7,
        "x",
        "Int",
        TermShape.Select(TermShape.Identifier("service", false), "x")
      )
    )

    fixtures.foreach(shape => assertPublicAlphaRoundTrip(shape, author(shape)))

  test("authors sibling Lambda values with independent lexical scopes"):
    val shape = TermShape.Tuple(
      List(
        lambda(id7, "left", "Int", TermShape.BoundReference(id7, "stale")),
        lambda(BinderId(2), "right", "String", TermShape.BoundReference(BinderId(2), "old"))
      )
    )

    assertPublicAlphaRoundTrip(shape, author(shape))

  test("threads distinct seeded Definition and Lambda binders through one body"):
    val outerId = BinderId(7)
    val lambdaId = BinderId(11)
    val outer = definitionBinder(outerId, "outer")
    val shape = lambda(
      lambdaId,
      "x",
      "Int",
      TermShape.Tuple(
        List(
          TermShape.BoundReference(outerId, "outer-stale"),
          TermShape.BoundReference(lambdaId, "lambda-stale")
        )
      )
    )
    val authored = authorSeeded(shape, Vector(outer))

    assertSeededAlphaRoundTrip(shape, authored, Vector(outer))
    assert(allTrees(authored).forall(_.pos == Position.None))

  test("allows lexical same-name shadowing only for Lambda-bound references"):
    val outerId = BinderId(7)
    val lambdaId = BinderId(11)
    val outer = definitionBinder(outerId, "x")
    val lambdaReference = lambda(
      lambdaId,
      "x",
      "Int",
      TermShape.BoundReference(lambdaId, "stale")
    )
    val outerReference = lambda(
      lambdaId,
      "x",
      "Int",
      TermShape.BoundReference(outerId, "stale")
    )

    assertSeededAlphaRoundTrip(
      lambdaReference,
      authorSeeded(lambdaReference, Vector(outer)),
      Vector(outer)
    )
    assertSeededErrorCode(
      outerReference,
      Vector(outer),
      "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED"
    )

  test("rejects every unsupported Lambda parameter Type with one stable category"):
    val unsupported = List(
      "Long",
      "AnyVal",
      "Option[Int]",
      "scala.Int",
      "java.lang.String",
      "(Int, String)",
      "Int => String",
      "",
      null
    )

    unsupported.foreach { parameterType =>
      assertErrorCode(
        lambda(id7, "x", parameterType, TermShape.BoundReference(id7, "x")),
        "NEUTRAL_TERM_AUTHORING_LAMBDA_PARAMETER_TYPE_UNSUPPORTED"
      )
    }

  test("fails closed for malformed Lambda structure and preserves child categories"):
    val failures = List(
      lambda(id7, "x", "Int", null) -> "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED",
      lambda(null, "x", "Int", TermShape.Literal("1")) ->
        "NEUTRAL_TERM_AUTHORING_LAMBDA_SCOPE_UNSUPPORTED",
      lambda(id7, null, "Int", TermShape.Literal("1")) ->
        "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED",
      lambda(id7, "", "Int", TermShape.Literal("1")) ->
        "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED",
      lambda(id7, "bad.name", "Int", TermShape.Literal("1")) ->
        "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED",
      lambda(id7, "x", "Int", TermShape.Parenthesized(bound0)) ->
        "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED",
      lambda(id7, "x", "Int", TermShape.Literal("01")) ->
        "NEUTRAL_TERM_AUTHORING_LITERAL_UNSUPPORTED",
      lambda(id7, "x", "Int", TermShape.Identifier("bad-name", false)) ->
        "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED"
    )

    failures.foreach { (shape, expectedCode) => assertErrorCode(shape, expectedCode) }

  test("rejects nested Lambda and unresolved lexical BoundReferences"):
    val nested = lambda(
      id7,
      "x",
      "Int",
      lambda(BinderId(8), "y", "Int", TermShape.BoundReference(BinderId(8), "y"))
    )
    val unknown = lambda(
      id7,
      "x",
      "Int",
      TermShape.BoundReference(BinderId(9), "missing")
    )

    assertErrorCode(nested, "NEUTRAL_TERM_AUTHORING_LAMBDA_NESTED_UNSUPPORTED")
    assertErrorCode(unknown, "NEUTRAL_TERM_AUTHORING_LAMBDA_SCOPE_UNSUPPORTED")

  test("rejects direct and compound free-name capture under the Lambda"):
    val captures = List(
      TermShape.Identifier("x", false),
      TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Identifier("x", false))),
      TermShape.Typed(TermShape.Identifier("x", false), "Int")
    )

    captures.foreach(body =>
      assertErrorCode(
        lambda(id7, "x", "Int", body),
        "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED"
      )
    )

  test("rejects a Lambda BinderId colliding with an enclosing Definition binder"):
    val outer = definitionBinder(id7, "outer")
    val shape = lambda(id7, "x", "Int", TermShape.BoundReference(id7, "x"))

    assertSeededErrorCode(
      shape,
      Vector(outer),
      "NEUTRAL_TERM_AUTHORING_LAMBDA_SCOPE_UNSUPPORTED"
    )

  test("preserves the public raw BoundReference failure exactly"):
    assertEquals(
      ScalametaTermShapeAuthoring.author(bound0),
      Left(
        ScalametaTermShapeAuthoring.Error(
          "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED",
          "this TermShape family is outside binder-free N013-N015/N019 authoring."
        )
      )
    )

  private def lambda(
      binderId: BinderId,
      name: String,
      parameterType: String,
      body: TermShape
  ): TermShape =
    TermShape.Lambda1(binderId, name, parameterType, body)

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
