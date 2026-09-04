package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionName, DefinitionNameSpelling, DefinitionShape}
import _root_.quasiquotes.parser.{BinderId, TermShape, TypeShape}
import _root_.quasiquotes.terms.TermShapeTraversal

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaTypedTwoParameterDefAuthoringTest extends munit.FunSuite:
  private final case class Fixture(label: String, shape: DefinitionShape.TwoParameterDef)

  private val id0 = BinderId(0)
  private val id1 = BinderId(1)
  private val intType = TypeShape.Identifier("Int")
  private val stringType = TypeShape.Identifier("String")
  private val booleanType = TypeShape.Identifier("Boolean")

  private val fixtures = List(
    Fixture(
      "canonical source order",
      method(
        "pair",
        id0,
        "left",
        intType,
        id1,
        "right",
        stringType,
        TypeShape.Tuple(List(intType, stringType)),
        TermShape.Tuple(List(bound(id0, "left"), bound(id1, "right")))
      )
    ),
    Fixture(
      "noncanonical binders reverse reference order and stale displays",
      method(
        "pair",
        BinderId(9),
        "left",
        intType,
        BinderId(2),
        "right",
        stringType,
        TypeShape.Tuple(List(stringType, intType)),
        TermShape.Tuple(List(bound(BinderId(2), "stale-second"), bound(BinderId(9), "stale-first")))
      )
    ),
    Fixture(
      "nested two-binder term family",
      method(
        "combined",
        BinderId(4),
        "left",
        intType,
        BinderId(8),
        "right",
        intType,
        stringType,
        TermShape.InterpolatedString(
          "s",
          List("value=", ""),
          List(
            TermShape.If(
              TermShape.Identifier("enabled", false),
              TermShape.Apply(
                TermShape.Select(bound(BinderId(4), "old-left"), "combine"),
                List(bound(BinderId(8), "old-right"))
              ),
              TermShape.Tuple(
                List(
                  TermShape.Infix(bound(BinderId(8), "old-right"), "+", TermShape.Literal("1")),
                  bound(BinderId(4), "old-left")
                )
              )
            )
          )
        )
      )
    ),
    Fixture(
      "unrelated free identifier",
      method("copy", id0, "x", intType, id1, "y", intType, intType, TermShape.Identifier("source", false))
    ),
    Fixture(
      "selected fields match parameters and method",
      method(
        "answer",
        id0,
        "x",
        intType,
        id1,
        "y",
        intType,
        TypeShape.Tuple(List(intType, intType, intType)),
        TermShape.Tuple(
          List(
            TermShape.Select(TermShape.Identifier("service", false), "x"),
            TermShape.Select(TermShape.Identifier("service", false), "y"),
            TermShape.Select(TermShape.Identifier("service", false), "answer")
          )
        )
      )
    ),
    Fixture(
      "method name equals first bound parameter",
      method("answer", id0, "answer", intType, id1, "y", intType, intType, bound(id0, "stale"))
    ),
    Fixture(
      "method name equals second bound parameter",
      method("answer", id0, "x", intType, id1, "answer", intType, intType, bound(id1, "stale"))
    ),
    Fixture(
      "distinct recursive Types",
      method(
        "nested",
        id0,
        "left",
        TypeShape.Apply(TypeShape.Identifier("List"), List(intType)),
        id1,
        "right",
        TypeShape.Apply(TypeShape.Identifier("Option"), List(stringType)),
        TypeShape.Apply(
          TypeShape.Identifier("Either"),
          List(booleanType, TypeShape.Tuple(List(stringType, intType)))
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
        id1,
        plainName("y"),
        intType,
        intType,
        bound(id0, "x")
      )
    ),
    Fixture(
      "backticked first parameter name",
      method(
        plainName("keywords"),
        id0,
        DefinitionName.backticked("`match`").toOption.get,
        intType,
        id1,
        plainName("y"),
        intType,
        intType,
        bound(id0, "stale")
      )
    ),
    Fixture(
      "backticked second parameter name",
      method(
        plainName("keywords"),
        id0,
        plainName("x"),
        intType,
        id1,
        DefinitionName.backticked("`type`").toOption.get,
        intType,
        intType,
        bound(id1, "stale")
      )
    )
  )

  test("authors the complete honest-intersection matrix to exact fresh ordinary topology"):
    fixtures.foreach { fixture =>
      val authored = author(fixture.shape)
      val group = authored.paramClauseGroups.head
      val clause = group.paramClauses.head
      val parameters = clause.values

      assertEquals(authored.productPrefix, "Defn.Def", clues(fixture.label))
      assertEquals(authored.mods, Nil, clues(fixture.label))
      assertEquals(authored.paramClauseGroups.size, 1, clues(fixture.label))
      assertEquals(group.tparamClause.values, Nil, clues(fixture.label))
      assertEquals(group.paramClauses.size, 1, clues(fixture.label))
      assertEquals(clause.mod, None, clues(fixture.label))
      assertEquals(parameters.size, 2, clues(fixture.label))
      assertEquals(
        parameters.map(_.name.value),
        List(fixture.shape.firstParameterName.decoded, fixture.shape.secondParameterName.decoded),
        clues(fixture.label)
      )
      parameters.foreach { parameter =>
        assertEquals(parameter.mods, Nil, clues(fixture.label))
        assertEquals(parameter.default, None, clues(fixture.label))
        assert(parameter.decltpe.nonEmpty, clues(fixture.label))
      }
      assert(authored.decltpe.nonEmpty, clues(fixture.label))
      assert(allTrees(authored).forall(_.pos == Position.None), clues(fixture.label))
    }

  test("round-trips every accepted row semantically through N023 and agrees with N025"):
    fixtures.foreach { fixture =>
      val authored = author(fixture.shape)
      val direct = project(authored)
      val dispatched = ScalametaDefinitionProjection.project(authored)

      assertSemanticRoundTrip(fixture.shape, direct, fixture.label)
      assertEquals(dispatched, Right(direct), clues(fixture.label))
    }

  test("canonicalizes arbitrary BinderIds and stale displays while preserving declaration and body order"):
    val original = fixtures.find(_.label.startsWith("noncanonical binders")).get.shape
    val authored = author(original)
    val projected = two(project(authored))
    val parameters = authored.paramClauseGroups.head.paramClauses.head.values

    assertEquals(original.firstParameterBinderId, BinderId(9))
    assertEquals(original.secondParameterBinderId, BinderId(2))
    assertEquals(parameters.map(_.name.value), List("left", "right"))
    assertEquals(
      parameters.map(_.decltpe.collect { case name: Type.Name => name.value }),
      List(Some("Int"), Some("String"))
    )
    assertEquals(projected.firstParameterBinderId, BinderId(0))
    assertEquals(projected.secondParameterBinderId, BinderId(1))
    assertEquals(projected.firstParameterName.decoded, "left")
    assertEquals(projected.secondParameterName.decoded, "right")
    assertEquals(
      projected.body,
      TermShape.Tuple(
        List(
          TermShape.BoundReference(BinderId(1), "right"),
          TermShape.BoundReference(BinderId(0), "left")
        )
      )
    )
    assertEquals(projected, original)

  test("alpha-equivalent input BinderId pairs author to equivalent source semantics"):
    val first = method(
      "pair",
      BinderId(2),
      "x",
      intType,
      BinderId(9),
      "y",
      intType,
      intType,
      TermShape.Infix(bound(BinderId(2), "old-x"), "+", bound(BinderId(9), "old-y"))
    )
    val second = method(
      "pair",
      BinderId(17),
      "x",
      intType,
      BinderId(3),
      "y",
      intType,
      intType,
      TermShape.Infix(bound(BinderId(17), "other-x"), "+", bound(BinderId(3), "other-y"))
    )
    val firstProjected = two(project(author(first)))
    val secondProjected = two(project(author(second)))

    assertEquals(first, second)
    assertEquals(firstProjected, secondProjected)
    assertEquals(
      Vector(firstProjected.firstParameterBinderId, firstProjected.secondParameterBinderId),
      Vector(BinderId(0), BinderId(1))
    )
    assertEquals(
      Vector(secondProjected.firstParameterBinderId, secondProjected.secondParameterBinderId),
      Vector(BinderId(0), BinderId(1))
    )

  test("preserves exact backticked method and parameter source spellings"):
    val methodKeyword = two(project(author(fixtures.find(_.label == "backticked method name").get.shape)))
    val firstKeyword = two(project(author(fixtures.find(_.label == "backticked first parameter name").get.shape)))
    val secondKeyword = two(project(author(fixtures.find(_.label == "backticked second parameter name").get.shape)))

    assertEquals(methodKeyword.name.source, "`type`")
    assertEquals(methodKeyword.name.spelling, DefinitionNameSpelling.BacktickedKeyword)
    assertEquals(firstKeyword.firstParameterName.source, "`match`")
    assertEquals(firstKeyword.firstParameterName.spelling, DefinitionNameSpelling.BacktickedKeyword)
    assertEquals(secondKeyword.secondParameterName.source, "`type`")
    assertEquals(secondKeyword.secondParameterName.spelling, DefinitionNameSpelling.BacktickedKeyword)

  test("rejects missing input and confirms invalid children remain outside the Core carrier"):
    assertEquals(
      ScalametaTypedTwoParameterDefAuthoring.author(null),
      Left(
        ScalametaTypedTwoParameterDefAuthoring.Error(
          "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_MISSING",
          "the two-parameter def shape must be present."
        )
      )
    )

    val name = plainName("method")
    val firstName = plainName("x")
    val secondName = plainName("y")
    val literal = TermShape.Literal("1")
    val unsupportedType = TypeShape.Unsupported("Type.Match", "outside N011")
    val coreRejected = List(
      DefinitionShape.twoParameterDef(name, id0, firstName, intType, id0, secondName, intType, intType, literal),
      DefinitionShape.twoParameterDef(name, id0, firstName, intType, id1, firstName, intType, intType, literal),
      DefinitionShape.twoParameterDef(name, id0, firstName, null, id1, secondName, intType, intType, literal),
      DefinitionShape.twoParameterDef(name, id0, firstName, unsupportedType, id1, secondName, intType, intType, literal),
      DefinitionShape.twoParameterDef(name, id0, firstName, intType, id1, secondName, null, intType, literal),
      DefinitionShape.twoParameterDef(name, id0, firstName, intType, id1, secondName, unsupportedType, intType, literal),
      DefinitionShape.twoParameterDef(name, id0, firstName, intType, id1, secondName, intType, null, literal),
      DefinitionShape.twoParameterDef(name, id0, firstName, intType, id1, secondName, intType, unsupportedType, literal),
      DefinitionShape.twoParameterDef(name, id0, firstName, intType, id1, secondName, intType, intType, null),
      DefinitionShape.twoParameterDef(
        name,
        id0,
        firstName,
        intType,
        id1,
        secondName,
        intType,
        intType,
        TermShape.New("synthetic.unresolved.Widget", Nil)
      )
    )

    assert(coreRejected.forall(_.isLeft))

  test("maps missing or overflowing binders and missing names without throwing"):
    val malformed = List(
      method(plainName("method"), null, plainName("x"), intType, id1, plainName("y"), intType, intType, TermShape.Literal("1")) ->
        "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_TERM_UNSUPPORTED",
      method(plainName("method"), id0, plainName("x"), intType, BinderId(Int.MaxValue), plainName("y"), intType, intType, TermShape.Literal("1")) ->
        "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_TERM_UNSUPPORTED",
      method(null, id0, plainName("x"), intType, id1, plainName("y"), intType, intType, TermShape.Literal("1")) ->
        "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_NAME_UNSUPPORTED",
      method(plainName("method"), id0, null, intType, id1, plainName("y"), intType, intType, TermShape.Literal("1")) ->
        "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_NAME_UNSUPPORTED",
      method(plainName("method"), id0, plainName("x"), intType, id1, null, intType, intType, TermShape.Literal("1")) ->
        "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_NAME_UNSUPPORTED"
    )

    malformed.foreach { case (shape, expected) => assertErrorCode(shape, expected) }

  test("keeps unknown binders and the Core versus N029 body-family intersection fail closed"):
    val binders = Vector(
      ScalametaTermShapeAuthoring.DefinitionBinder(id0, plainName("x")),
      ScalametaTermShapeAuthoring.DefinitionBinder(id1, plainName("y"))
    )
    val unknown = bound(BinderId(7), "unknown")
    assert(ScalametaTermShapeAuthoring.authorWithDefinitionBinders(unknown, binders).isLeft)
    assert(
      DefinitionShape.twoParameterDef(
        plainName("method"), id0, plainName("x"), intType,
        id1, plainName("y"), intType, intType, unknown
      ).isLeft
    )

    val newBody = TermShape.New("synthetic.unresolved.Widget", Nil)
    assert(ScalametaTermShapeAuthoring.authorWithDefinitionBinders(newBody, binders).isRight)
    assert(
      DefinitionShape.twoParameterDef(
        plainName("method"), id0, plainName("x"), intType,
        id1, plainName("y"), intType, intType, newBody
      ).isLeft
    )

    List(
      method("typed", id0, "x", intType, id1, "y", intType, intType, TermShape.Typed(bound(id0, "x"), "Int")),
      method("parenthesized", id0, "x", intType, id1, "y", intType, intType, TermShape.Parenthesized(bound(id1, "y")))
    ).foreach(assertErrorCode(_, "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_TERM_UNSUPPORTED"))

  test("rejects direct and nested free capture against either parameter"):
    val collisions = List(
      method("method", id0, "x", intType, id1, "y", intType, intType, TermShape.Identifier("x", false)),
      method("method", id0, "x", intType, id1, "y", intType, intType, TermShape.Identifier("y", false)),
      method(
        "method", id0, "x", intType, id1, "y", intType, intType,
        TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Identifier("x", false)))
      ),
      method(
        "method", id0, "x", intType, id1, "y", intType, intType,
        TermShape.If(TermShape.Identifier("flag", false), TermShape.Literal("1"), TermShape.Identifier("y", false))
      )
    )

    collisions.foreach(assertErrorCode(_, "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_TERM_UNSUPPORTED"))

  test("maps malformed Term leaves to the bounded Term category"):
    List(
      method("literal", id0, "x", intType, id1, "y", intType, intType, TermShape.Literal("01")),
      method("identifier", id0, "x", intType, id1, "y", intType, intType, TermShape.Identifier("bad-name", false))
    ).foreach(assertErrorCode(_, "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_TERM_UNSUPPORTED"))

  test("leaves free method self-reference to N023 while preserving shadowing distinctions"):
    val recursive = method(
      "answer", id0, "x", intType, id1, "y", intType, intType,
      TermShape.Identifier("answer", false)
    )
    val freeFirstShadow = method(
      "answer", id0, "answer", intType, id1, "y", intType, intType,
      TermShape.Identifier("answer", false)
    )
    val freeSecondShadow = method(
      "answer", id0, "x", intType, id1, "answer", intType, intType,
      TermShape.Identifier("answer", false)
    )

    assertErrorCode(recursive, "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_ROUNDTRIP_FAILED")
    assertErrorCode(freeFirstShadow, "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_TERM_UNSUPPORTED")
    assertErrorCode(freeSecondShadow, "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_TERM_UNSUPPORTED")

  private def author(shape: DefinitionShape.TwoParameterDef): Defn.Def =
    ScalametaTypedTwoParameterDefAuthoring.author(shape) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def project(definition: Defn.Def): ProjectedDefinitionShape =
    ScalametaTypedTwoParameterDefProjection.project(definition) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def two(projected: ProjectedDefinitionShape): DefinitionShape.TwoParameterDef =
    projected.shape match
      case value: DefinitionShape.TwoParameterDef => value
      case other => fail(s"expected TwoParameterDef, found ${other.render}")

  private def assertSemanticRoundTrip(
      expected: DefinitionShape.TwoParameterDef,
      actual: ProjectedDefinitionShape,
      label: String
  ): Unit =
    val projected = two(actual)
    assertEquals(projected.firstParameterBinderId, BinderId(0), clues(label))
    assertEquals(projected.secondParameterBinderId, BinderId(1), clues(label))
    assertEquals(projected.name, expected.name, clues(label))
    assertEquals(projected.firstParameterName, expected.firstParameterName, clues(label))
    assertEquals(projected.secondParameterName, expected.secondParameterName, clues(label))
    assertEquals(projected.firstParameterType, expected.firstParameterType, clues(label))
    assertEquals(projected.secondParameterType, expected.secondParameterType, clues(label))
    assertEquals(projected.resultType, expected.resultType, clues(label))
    assertEquals(
      TermShapeTraversal.alphaNormalizeInScope(
        projected.body,
        Vector(projected.firstParameterBinderId, projected.secondParameterBinderId)
      ),
      TermShapeTraversal.alphaNormalizeInScope(
        expected.body,
        Vector(expected.firstParameterBinderId, expected.secondParameterBinderId)
      ),
      clues(label)
    )
    assertEquals(projected, expected, clues(label))
    assertEquals(actual.sourceSpan, None, clues(label))

  private def method(
      methodName: String,
      firstBinderId: BinderId,
      firstName: String,
      firstType: TypeShape,
      secondBinderId: BinderId,
      secondName: String,
      secondType: TypeShape,
      resultType: TypeShape,
      body: TermShape
  ): DefinitionShape.TwoParameterDef =
    method(
      plainName(methodName),
      firstBinderId,
      plainName(firstName),
      firstType,
      secondBinderId,
      plainName(secondName),
      secondType,
      resultType,
      body
    )

  private def method(
      methodName: DefinitionName,
      firstBinderId: BinderId,
      firstName: DefinitionName,
      firstType: TypeShape,
      secondBinderId: BinderId,
      secondName: DefinitionName,
      secondType: TypeShape,
      resultType: TypeShape,
      body: TermShape
  ): DefinitionShape.TwoParameterDef =
    DefinitionShape.twoParameterDef(
      methodName,
      firstBinderId,
      firstName,
      firstType,
      secondBinderId,
      secondName,
      secondType,
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
      shape: DefinitionShape.TwoParameterDef,
      expected: String
  ): Unit =
    assertEquals(
      ScalametaTypedTwoParameterDefAuthoring.author(shape).left.toOption.map(_.code),
      Some(expected),
      clues(shape)
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
