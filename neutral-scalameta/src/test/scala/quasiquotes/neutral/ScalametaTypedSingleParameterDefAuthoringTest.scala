package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionName, DefinitionNameSpelling, DefinitionShape}
import _root_.quasiquotes.parser.{BinderId, TermShape, TypeShape}
import _root_.quasiquotes.terms.TermShapeTraversal

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaTypedSingleParameterDefAuthoringTest extends munit.FunSuite:
  private final case class Fixture(label: String, shape: DefinitionShape.SingleParameterDef)

  private val id0 = BinderId(0)
  private val intType = TypeShape.Identifier("Int")
  private val stringType = TypeShape.Identifier("String")
  private val booleanType = TypeShape.Identifier("Boolean")

  private val fixtures = List(
    Fixture("canonical identity", method("id", id0, "x", intType, intType, bound(id0, "x"))),
    Fixture(
      "nonzero binder and stale display name",
      method("identity", BinderId(7), "value", intType, intType, bound(BinderId(7), "stale"))
    ),
    Fixture(
      "parameter selection qualifier",
      method("field", id0, "x", intType, intType, TermShape.Select(bound(id0, "x"), "value"))
    ),
    Fixture(
      "parameter Apply argument",
      method(
        "computed",
        id0,
        "x",
        intType,
        intType,
        TermShape.Apply(TermShape.Identifier("compute", false), List(bound(id0, "x")))
      )
    ),
    Fixture(
      "parameter infix operand",
      method(
        "increment",
        id0,
        "x",
        intType,
        intType,
        TermShape.Infix(bound(id0, "x"), "+", TermShape.Literal("1"))
      )
    ),
    Fixture(
      "tuple if and interpolation",
      method(
        "message",
        id0,
        "x",
        intType,
        stringType,
        TermShape.InterpolatedString(
          "s",
          List("value=", ""),
          List(
            TermShape.If(
              TermShape.Identifier("enabled", false),
              bound(id0, "x"),
              TermShape.Tuple(List(TermShape.Literal("0"), bound(id0, "x")))
            )
          )
        )
      )
    ),
    Fixture(
      "unrelated free identifier",
      method("copy", id0, "x", intType, intType, TermShape.Identifier("source", false))
    ),
    Fixture(
      "selected field matches parameter",
      method(
        "selected",
        id0,
        "x",
        intType,
        intType,
        TermShape.Select(TermShape.Identifier("service", false), "x")
      )
    ),
    Fixture(
      "selected field matches method",
      method(
        "answer",
        id0,
        "x",
        intType,
        intType,
        TermShape.Select(TermShape.Identifier("service", false), "answer")
      )
    ),
    Fixture(
      "method name equals bound parameter name",
      method("answer", id0, "answer", intType, intType, bound(id0, "answer"))
    ),
    Fixture(
      "recursive parameter and result Types",
      method(
        "nested",
        id0,
        "value",
        TypeShape.Apply(TypeShape.Identifier("List"), List(intType)),
        TypeShape.Apply(
          TypeShape.Identifier("Either"),
          List(
            TypeShape.Apply(TypeShape.Identifier("Option"), List(stringType)),
            TypeShape.Tuple(List(intType, booleanType))
          )
        ),
        TermShape.Identifier("result", false)
      )
    ),
    Fixture(
      "backticked method name",
      method(
        DefinitionName.backticked("`type`").toOption.get,
        id0,
        plainName("x"),
        intType,
        intType,
        bound(id0, "x")
      )
    ),
    Fixture(
      "backticked parameter name",
      method(
        plainName("keyword"),
        id0,
        DefinitionName.backticked("`match`").toOption.get,
        intType,
        intType,
        bound(id0, "stale")
      )
    )
  )

  test("authors the complete honest-intersection matrix to exact fresh ordinary topology"):
    fixtures.foreach { fixture =>
      val authored = author(fixture.shape)
      val group = authored.paramClauseGroups.head
      val clause = group.paramClauses.head
      val parameter = clause.values.head

      assertEquals(authored.productPrefix, "Defn.Def", clues(fixture.label))
      assertEquals(authored.mods, Nil, clues(fixture.label))
      assertEquals(authored.paramClauseGroups.size, 1, clues(fixture.label))
      assertEquals(group.tparamClause.values, Nil, clues(fixture.label))
      assertEquals(group.paramClauses.size, 1, clues(fixture.label))
      assertEquals(clause.mod, None, clues(fixture.label))
      assertEquals(clause.values.size, 1, clues(fixture.label))
      assertEquals(parameter.mods, Nil, clues(fixture.label))
      assertEquals(parameter.default, None, clues(fixture.label))
      assert(parameter.decltpe.nonEmpty, clues(fixture.label))
      assert(authored.decltpe.nonEmpty, clues(fixture.label))
      assert(allTrees(authored).forall(_.pos == Position.None), clues(fixture.label))
    }

  test("round-trips every accepted row semantically through N022 and agrees with N025"):
    fixtures.foreach { fixture =>
      val authored = author(fixture.shape)
      val direct = project(authored)
      val dispatched = ScalametaDefinitionProjection.project(authored)

      assertSemanticRoundTrip(fixture.shape, direct, fixture.label)
      assertEquals(dispatched, Right(direct), clues(fixture.label))
    }

  test("canonicalizes arbitrary BinderIds and stale display names without changing meaning"):
    val original = fixtures.find(_.label == "nonzero binder and stale display name").get.shape
    val projected = single(project(author(original)))

    assertEquals(original.parameterBinderId, BinderId(7))
    assertEquals(original.body, TermShape.BoundReference(BinderId(7), "stale"))
    assertEquals(projected.parameterBinderId, BinderId(0))
    assertEquals(projected.parameterName.decoded, "value")
    assertEquals(projected.body, TermShape.BoundReference(BinderId(0), "value"))
    assertEquals(projected, original)

  test("alpha-equivalent input BinderIds author to equivalent source semantics"):
    val first = method("id", BinderId(2), "x", intType, intType, bound(BinderId(2), "old"))
    val second = method("id", BinderId(9), "x", intType, intType, bound(BinderId(9), "other"))
    val firstProjected = single(project(author(first)))
    val secondProjected = single(project(author(second)))

    assertEquals(first, second)
    assertEquals(firstProjected, secondProjected)
    assertEquals(firstProjected.parameterBinderId, BinderId(0))
    assertEquals(secondProjected.parameterBinderId, BinderId(0))
    assertEquals(firstProjected.body, TermShape.BoundReference(BinderId(0), "x"))
    assertEquals(secondProjected.body, TermShape.BoundReference(BinderId(0), "x"))

  test("preserves exact backticked method and parameter source spellings"):
    val methodKeyword = single(project(author(fixtures.find(_.label == "backticked method name").get.shape)))
    val parameterKeyword = single(
      project(author(fixtures.find(_.label == "backticked parameter name").get.shape))
    )

    assertEquals(methodKeyword.name.source, "`type`")
    assertEquals(methodKeyword.name.spelling, DefinitionNameSpelling.BacktickedKeyword)
    assertEquals(parameterKeyword.parameterName.source, "`match`")
    assertEquals(parameterKeyword.parameterName.spelling, DefinitionNameSpelling.BacktickedKeyword)

  test("rejects missing input and confirms invalid children remain outside the Core carrier"):
    assertEquals(
      ScalametaTypedSingleParameterDefAuthoring.author(null),
      Left(
        ScalametaTypedSingleParameterDefAuthoring.Error(
          "NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_MISSING",
          "the single-parameter def shape must be present."
        )
      )
    )

    val name = plainName("method")
    val parameterName = plainName("x")
    val literal = TermShape.Literal("1")
    val unsupportedType = TypeShape.Unsupported("Type.Match", "outside N011")
    val coreRejected = List(
      DefinitionShape.singleParameterDef(name, id0, parameterName, null, intType, literal),
      DefinitionShape.singleParameterDef(name, id0, parameterName, unsupportedType, intType, literal),
      DefinitionShape.singleParameterDef(name, id0, parameterName, intType, null, literal),
      DefinitionShape.singleParameterDef(name, id0, parameterName, intType, unsupportedType, literal),
      DefinitionShape.singleParameterDef(name, id0, parameterName, intType, intType, null),
      DefinitionShape.singleParameterDef(
        name,
        id0,
        parameterName,
        intType,
        intType,
        TermShape.New("synthetic.unresolved.Widget", Nil)
      )
    )

    assert(coreRejected.forall(_.isLeft))

  test("maps missing or overflowing BinderIds and malformed names without throwing"):
    val missingBinder = method(
      plainName("method"),
      null,
      plainName("x"),
      intType,
      intType,
      TermShape.Literal("1")
    )
    val overflowingBinder = method(
      plainName("method"),
      BinderId(Int.MaxValue),
      plainName("x"),
      intType,
      intType,
      TermShape.Literal("1")
    )
    val missingMethodName = method(
      null,
      id0,
      plainName("x"),
      intType,
      intType,
      TermShape.Literal("1")
    )
    val missingParameterName = method(
      plainName("method"),
      id0,
      null,
      intType,
      intType,
      TermShape.Literal("1")
    )

    assertErrorCode(missingBinder, "NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_TERM_UNSUPPORTED")
    assertErrorCode(overflowingBinder, "NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_TERM_UNSUPPORTED")
    assertErrorCode(missingMethodName, "NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_NAME_UNSUPPORTED")
    assertErrorCode(missingParameterName, "NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_NAME_UNSUPPORTED")

  test("keeps the Core and N029 body-family intersection fail closed in both directions"):
    val coreRejectedNew = DefinitionShape.singleParameterDef(
      plainName("method"),
      id0,
      plainName("x"),
      intType,
      intType,
      TermShape.New("synthetic.unresolved.Widget", Nil)
    )
    assert(ScalametaTermShapeAuthoring.authorWithDefinitionBinders(
      TermShape.New("synthetic.unresolved.Widget", Nil),
      Vector(ScalametaTermShapeAuthoring.DefinitionBinder(id0, plainName("x")))
    ).isRight)
    assert(coreRejectedNew.isLeft)

    val n029Rejected = List(
      method(
        "typed",
        id0,
        "x",
        intType,
        intType,
        TermShape.Typed(bound(id0, "x"), "Int")
      ),
      method(
        "parenthesized",
        id0,
        "x",
        intType,
        intType,
        TermShape.Parenthesized(bound(id0, "x"))
      )
    )
    n029Rejected.foreach(assertErrorCode(_, "NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_TERM_UNSUPPORTED"))

  test("rejects free parameter capture directly and inside an admitted compound term"):
    val collisions = List(
      method("method", id0, "x", intType, intType, TermShape.Identifier("x", false)),
      method(
        "method",
        id0,
        "x",
        intType,
        intType,
        TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Identifier("x", false)))
      )
    )

    collisions.foreach(assertErrorCode(_, "NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_TERM_UNSUPPORTED"))

  test("maps malformed Term values to the bounded Term category"):
    val rejected = List(
      method("literal", id0, "x", intType, intType, TermShape.Literal("01")),
      method("identifier", id0, "x", intType, intType, TermShape.Identifier("bad-name", false))
    )

    rejected.foreach(assertErrorCode(_, "NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_TERM_UNSUPPORTED"))

  test("leaves method self-reference to the final N022 round-trip authority"):
    val recursive = method(
      "answer",
      id0,
      "x",
      intType,
      intType,
      TermShape.Identifier("answer", false)
    )
    val boundSameName = method("answer", id0, "answer", intType, intType, bound(id0, "answer"))
    val freeSameName = method(
      "answer",
      id0,
      "answer",
      intType,
      intType,
      TermShape.Identifier("answer", false)
    )

    assertErrorCode(recursive, "NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_ROUNDTRIP_FAILED")
    assertSemanticRoundTrip(boundSameName, project(author(boundSameName)), "bound same-name")
    assertErrorCode(freeSameName, "NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_TERM_UNSUPPORTED")

  private def author(shape: DefinitionShape.SingleParameterDef): Defn.Def =
    ScalametaTypedSingleParameterDefAuthoring.author(shape) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def project(definition: Defn.Def): ProjectedDefinitionShape =
    ScalametaTypedSingleParameterDefProjection.project(definition) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def single(projected: ProjectedDefinitionShape): DefinitionShape.SingleParameterDef =
    projected.shape match
      case value: DefinitionShape.SingleParameterDef => value
      case other => fail(s"expected SingleParameterDef, found ${other.render}")

  private def assertSemanticRoundTrip(
      expected: DefinitionShape.SingleParameterDef,
      actual: ProjectedDefinitionShape,
      label: String
  ): Unit =
    val projected = single(actual)
    assertEquals(projected.name, expected.name, clues(label))
    assertEquals(projected.parameterName, expected.parameterName, clues(label))
    assertEquals(projected.parameterType, expected.parameterType, clues(label))
    assertEquals(projected.resultType, expected.resultType, clues(label))
    assertEquals(
      TermShapeTraversal.alphaNormalizeInScope(projected.body, projected.parameterBinderId),
      TermShapeTraversal.alphaNormalizeInScope(expected.body, expected.parameterBinderId),
      clues(label)
    )
    assertEquals(projected, expected, clues(label))
    assertEquals(actual.sourceSpan, None, clues(label))

  private def method(
      methodName: String,
      binderId: BinderId,
      parameterName: String,
      parameterType: TypeShape,
      resultType: TypeShape,
      body: TermShape
  ): DefinitionShape.SingleParameterDef =
    method(plainName(methodName), binderId, plainName(parameterName), parameterType, resultType, body)

  private def method(
      methodName: DefinitionName,
      binderId: BinderId,
      parameterName: DefinitionName,
      parameterType: TypeShape,
      resultType: TypeShape,
      body: TermShape
  ): DefinitionShape.SingleParameterDef =
    DefinitionShape.singleParameterDef(
      methodName,
      binderId,
      parameterName,
      parameterType,
      resultType,
      body
    ) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def plainName(value: String): DefinitionName =
    DefinitionName.plain(value).toOption.get

  private def bound(binderId: BinderId, displayName: String): TermShape =
    TermShape.BoundReference(binderId, displayName)

  private def assertErrorCode(
      shape: DefinitionShape.SingleParameterDef,
      expected: String
  ): Unit =
    assertEquals(
      ScalametaTypedSingleParameterDefAuthoring.author(shape).left.toOption.map(_.code),
      Some(expected),
      clues(shape)
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
