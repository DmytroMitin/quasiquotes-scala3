package quasiquotes.matching

import quasiquotes.construct.SelectedMemberName
import quasiquotes.parser.TermShape

import scala.compiletime.testing.typeCheckErrors

final class SelectedNamePatternMatcherTest extends munit.FunSuite:
  private val transport = "__selected_name_transport"

  test("matches direct nullary and unary selected-member patterns with separate typed bindings"):
    val direct = validated(
      TermPattern.Select(TermPattern.Hole("receiver"), transport)
    )
    val nullary = validated(
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("receiver"), transport),
        Nil
      )
    )
    val unary = validated(
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("receiver"), transport),
        List(TermPattern.Hole("argument"))
      )
    )

    assertEquals(
      matched(direct, TermShape.Select(ident("service"), "ordinary")),
      NeutralSelectedNameMatchResult(
        Map("receiver" -> ident("service")),
        Map("member" -> selected("ordinary"))
      )
    )
    assertEquals(
      matched(
        nullary,
        TermShape.Apply(TermShape.Select(ident("service"), "member2"), Nil)
      ),
      NeutralSelectedNameMatchResult(
        Map("receiver" -> ident("service")),
        Map("member" -> selected("member2"))
      )
    )
    val unaryResult = matched(
      unary,
      TermShape.Apply(
        TermShape.Select(ident("service"), "_privateLike"),
        List(TermShape.Literal("1"))
      )
    )
    assertEquals(unaryResult.termBinding("$receiver"), Some(ident("service")))
    assertEquals(unaryResult.termBinding("argument"), Some(TermShape.Literal("1")))
    assertEquals(
      unaryResult.selectedNameBinding("$member"),
      Some(selected("_privateLike"))
    )

  test("uses root-first Select preorder and exact fixed selected names"):
    val pattern = TermPattern.Select(
      TermPattern.Select(TermPattern.Hole("receiver"), transport),
      "fixedOuter"
    )
    val target = TermShape.Select(
      TermShape.Select(ident("service"), "ordinary"),
      "fixedOuter"
    )
    val wrapper = validated(pattern, selectOrdinal = 1)

    assertEquals(
      matched(wrapper, target),
      NeutralSelectedNameMatchResult(
        Map("receiver" -> ident("service")),
        Map("member" -> selected("ordinary"))
      )
    )
    assertEquals(
      SelectedNamePatternMatcher.matchTarget(
        wrapper,
        target.copy(name = "FixedOuter")
      ).left.toOption.map(_.getClass),
      Some(classOf[NeutralSelectedNameMatchError.ShapeMismatch])
    )

  test("requires repeated ordinary Term captures to be structurally equal"):
    val wrapper = validated(
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("same"), transport),
        List(TermPattern.Hole("same"))
      )
    )
    val service = ident("service")

    assertEquals(
      matched(
        wrapper,
        TermShape.Apply(
          TermShape.Select(service, "ordinary"),
          List(service)
        )
      ).termBindings,
      Map("same" -> service)
    )
    assertEquals(
      SelectedNamePatternMatcher.matchTarget(
        wrapper,
        TermShape.Apply(
          TermShape.Select(service, "ordinary"),
          List(ident("other"))
        )
      ),
      Left(NeutralSelectedNameMatchError.RepeatedTermCaptureMismatch("same"))
    )

  test("matches only completed fixed identifiers and exact literals"):
    val identifierArgument = validated(
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Identifier("service"), transport),
        List(TermPattern.Identifier("argument"))
      )
    )
    val literalArgument = validated(
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Identifier("service"), transport),
        List(TermPattern.Literal("1"))
      )
    )

    assert(
      SelectedNamePatternMatcher
        .matchTarget(
          identifierArgument,
          TermShape.Apply(
            TermShape.Select(ident("service"), "ordinary"),
            List(ident("argument"))
          )
        )
        .isRight
    )
    List(
      TermShape.Apply(
        TermShape.Select(TermShape.Identifier("service", true), "ordinary"),
        List(ident("argument"))
      ),
      TermShape.Apply(
        TermShape.Select(ident("service"), "ordinary"),
        List(TermShape.Identifier("argument", true))
      ),
      TermShape.Apply(
        TermShape.Select(ident("different"), "ordinary"),
        List(ident("argument"))
      )
    ).foreach(target => assert(SelectedNamePatternMatcher.matchTarget(identifierArgument, target).isLeft))

    assert(
      SelectedNamePatternMatcher
        .matchTarget(
          literalArgument,
          TermShape.Apply(
            TermShape.Select(ident("service"), "ordinary"),
            List(TermShape.Literal("1"))
          )
        )
        .isRight
    )
    assert(
      SelectedNamePatternMatcher
        .matchTarget(
          literalArgument,
          TermShape.Apply(
            TermShape.Select(ident("service"), "ordinary"),
            List(TermShape.Literal("2"))
          )
        )
        .isLeft
    )

  test("retains the current neutral plain selected-name intersection"):
    val wrapper = validated(
      TermPattern.Select(TermPattern.Hole("receiver"), transport)
    )

    Vector("ordinary", "member2", "_privateLike").foreach { name =>
      assertEquals(
        matched(wrapper, TermShape.Select(ident("service"), name)).selectedNameBindings,
        Map("member" -> selected(name))
      )
    }

    Vector("+", "type", "safe spaced name", "_", "naïve", "$internal").foreach { name =>
      assertEquals(
        SelectedNamePatternMatcher.matchTarget(
          wrapper,
          TermShape.Select(ident("service"), name)
        ),
        Left(NeutralSelectedNameMatchError.SelectedNameLexicalUnsupported(name))
      )
    }

  test("validated metadata is transport-independent and category-sensitive"):
    val first = validated(
      TermPattern.Select(TermPattern.Hole("receiver"), "__transport_a"),
      transportValue = "__transport_a"
    )
    val second = validated(
      TermPattern.Select(TermPattern.Hole("receiver"), "__transport_b"),
      transportValue = "__transport_b"
    )
    val otherLogicalName = validated(
      TermPattern.Select(TermPattern.Hole("receiver"), "__transport_c"),
      logicalName = "other",
      transportValue = "__transport_c"
    )
    val captureOuter = validated(
      TermPattern.Select(
        TermPattern.Select(TermPattern.Hole("receiver"), "__inner"),
        "__outer"
      ),
      selectOrdinal = 0,
      transportValue = "__outer"
    )
    val captureInner = validated(
      TermPattern.Select(
        TermPattern.Select(TermPattern.Hole("receiver"), "__inner"),
        "__outer"
      ),
      selectOrdinal = 1,
      transportValue = "__inner"
    )

    assertEquals(first, second)
    assertEquals(first.hashCode, second.hashCode)
    assertNotEquals(first, otherLogicalName)
    assertNotEquals(captureOuter, captureInner)
    assertEquals(
      matched(first, TermShape.Select(ident("service"), "ordinary")),
      matched(second, TermShape.Select(ident("service"), "ordinary"))
    )

  test("rejects malformed selected-name metadata before matching"):
    val direct = TermPattern.Select(TermPattern.Hole("receiver"), transport)
    val duplicate = TermPattern.Select(
      TermPattern.Select(TermPattern.Hole("receiver"), transport),
      transport
    )

    assertInvalidPattern(null)
    assertInvalidOccurrence(direct, null)
    assertInvalidOccurrence(
      direct,
      SelectedNamePatternOccurrence("bad-name", 0, transport)
    )
    assertInvalidOccurrence(
      direct,
      SelectedNamePatternOccurrence("member", -1, transport)
    )
    assertInvalidOccurrence(
      direct,
      SelectedNamePatternOccurrence("member", 1, transport)
    )
    assertInvalidOccurrence(
      direct,
      SelectedNamePatternOccurrence("member", 0, "__different_transport")
    )
    assertInvalidOccurrence(
      TermPattern.Select(
        TermPattern.Select(TermPattern.Hole("receiver"), transport),
        "fixedOuter"
      ),
      SelectedNamePatternOccurrence("member", 0, transport)
    )
    assertEquals(
      ValidatedSelectedNamePattern.create(
        duplicate,
        SelectedNamePatternOccurrence("member", 0, transport)
      ),
      Left(NeutralSelectedNameMatchError.DuplicateSelectedNameCapture())
    )
    assertEquals(
      ValidatedSelectedNamePattern.create(
        TermPattern.Select(TermPattern.Hole("member"), transport),
        SelectedNamePatternOccurrence("member", 0, transport)
      ),
      Left(NeutralSelectedNameMatchError.CaptureCategoryConflict("member"))
    )

  test("rejects unsupported pattern families and invalid fixed selected names at validation"):
    val unsupportedRoots = List[TermPattern](
      TermPattern.Hole("root"),
      TermPattern.Apply(TermPattern.Identifier("f"), Nil),
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("receiver"), transport),
        List(TermPattern.Hole("a"), TermPattern.Hole("b"))
      ),
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("receiver"), transport),
        List(TermPattern.Apply(TermPattern.Identifier("nested"), Nil))
      ),
      TermPattern.Apply(
        TermPattern.Apply(
          TermPattern.Select(TermPattern.Hole("receiver"), transport),
          List(TermPattern.Hole("first"))
        ),
        List(TermPattern.Hole("second"))
      ),
      TermPattern.New("Widget", Nil),
      TermPattern.Infix(TermPattern.Identifier("a"), "+", TermPattern.Identifier("b"))
    )

    unsupportedRoots.foreach { pattern =>
      assert(
        ValidatedSelectedNamePattern
          .create(pattern, SelectedNamePatternOccurrence("member", 0, transport))
          .left
          .toOption
          .exists(_.isInstanceOf[NeutralSelectedNameMatchError.UnsupportedPatternShape]),
        clues(pattern.render)
      )
    }

    Vector("+", "type", "safe spaced name", "_", "naïve", "$internal").foreach { fixed =>
      val pattern = TermPattern.Select(
        TermPattern.Select(TermPattern.Hole("receiver"), transport),
        fixed
      )
      assertEquals(
        ValidatedSelectedNamePattern.create(
          pattern,
          SelectedNamePatternOccurrence("member", 1, transport)
        ),
        Left(NeutralSelectedNameMatchError.SelectedNameLexicalUnsupported(fixed))
      )
    }

  test("rejects target shape arity and null boundaries fail-closed"):
    val direct = validated(
      TermPattern.Select(TermPattern.Hole("receiver"), transport)
    )
    val nullary = validated(
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("receiver"), transport),
        Nil
      )
    )
    val unary = validated(
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("receiver"), transport),
        List(TermPattern.Hole("argument"))
      )
    )

    assertEquals(
      SelectedNamePatternMatcher.matchTarget(direct, null),
      Left(NeutralSelectedNameMatchError.InvalidTarget("target must be present"))
    )
    assert(SelectedNamePatternMatcher.matchTarget(null, ident("service")).isLeft)
    assert(SelectedNamePatternMatcher.matchTarget(direct, ident("service")).isLeft)
    assert(
      SelectedNamePatternMatcher
        .matchTarget(
          nullary,
          TermShape.Apply(
            TermShape.Select(ident("service"), "ordinary"),
            List(TermShape.Literal("1"))
          )
        )
        .isLeft
    )
    assert(
      SelectedNamePatternMatcher
        .matchTarget(
          unary,
          TermShape.Apply(TermShape.Select(ident("service"), "ordinary"), Nil)
        )
        .isLeft
    )
    assert(
      SelectedNamePatternMatcher
        .matchTarget(
          unary,
          TermShape.Apply(
            TermShape.Select(ident("service"), "ordinary"),
            List(TermShape.Literal("1"), TermShape.Literal("2"))
          )
        )
        .isLeft
    )

  test("public MatchResult and PatternSource remain unaware of selected-name metadata"):
    val receiver = ident("service")
    val publicResult = MatchResult[TermShape](Map("receiver" -> receiver))
    val mapped = PatternSource.synthesizeMapped("$receiver.fixed").toOption.get

    assertEquals(publicResult.binding("$receiver"), Some(receiver))
    assertEquals(mapped.patternSource.holes, Vector("receiver"))
    assertEquals(mapped.occurrences.map(_.name), Vector("receiver"))
    assert(
      typeCheckErrors(
        """val result = quasiquotes.matching.MatchResult[quasiquotes.parser.TermShape](Map("member" -> quasiquotes.construct.SelectedMemberName.from("ordinary").toOption.get))"""
      ).nonEmpty
    )

  private def validated(
      pattern: TermPattern,
      selectOrdinal: Int = 0,
      logicalName: String = "member",
      transportValue: String = transport
  ): ValidatedSelectedNamePattern =
    ValidatedSelectedNamePattern
      .create(
        pattern,
        SelectedNamePatternOccurrence(
          logicalName,
          selectOrdinal,
          transportValue
        )
      )
      .fold(error => fail(error.message), identity)

  private def matched(
      pattern: ValidatedSelectedNamePattern,
      target: TermShape
  ): NeutralSelectedNameMatchResult =
    SelectedNamePatternMatcher
      .matchTarget(pattern, target)
      .fold(error => fail(error.message), identity)

  private def assertInvalidPattern(pattern: TermPattern): Unit =
    assert(
      ValidatedSelectedNamePattern
        .create(
          pattern,
          SelectedNamePatternOccurrence("member", 0, transport)
        )
        .left
        .toOption
        .exists(_.isInstanceOf[NeutralSelectedNameMatchError.InvalidPattern])
    )

  private def assertInvalidOccurrence(
      pattern: TermPattern,
      occurrence: SelectedNamePatternOccurrence
  ): Unit =
    assert(
      ValidatedSelectedNamePattern
        .create(pattern, occurrence)
        .left
        .toOption
        .exists(_.isInstanceOf[NeutralSelectedNameMatchError.InvalidSelectedNameOccurrence])
    )

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def selected(name: String): SelectedMemberName =
    SelectedMemberName.from(name).fold(error => fail(error.message), identity)
