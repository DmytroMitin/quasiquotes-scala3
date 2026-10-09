package quasiquotes.matching

import quasiquotes.construct.SelectedMemberName
import quasiquotes.parser.TermShape

import scala.compiletime.testing.typeCheckErrors

final class N065PrivateTwoSelectedNameCaptureTest extends munit.FunSuite:
  private val firstTransport = "__n065_first"
  private val secondTransport = "__n065_second"

  test("matches two distinct selected names in root-first ordinal order"):
    val validated = twoValidated(twoCapturePattern())

    assertEquals(
      validated.occurrences.map(value => value.selectOrdinal -> value.name),
      Vector(0 -> "second", 1 -> "first")
    )
    assertEquals(
      SelectedNamePatternMatcher.matchTarget(
        validated,
        TermShape.Select(
          TermShape.Select(identifier("service"), "alpha"),
          "beta"
        )
      ),
      Right(
        NeutralSelectedNameMatchResult(
          Map("receiver" -> identifier("service")),
          Map("first" -> selected("alpha"), "second" -> selected("beta"))
        )
      )
    )

  test("preserves the single-occurrence create path and compatibility accessor"):
    val occurrence =
      SelectedNamePatternOccurrence("member", 0, firstTransport)
    val validated = ValidatedSelectedNamePattern
      .create(
        TermPattern.Select(TermPattern.Hole("receiver"), firstTransport),
        occurrence
      )
      .fold(error => fail(error.message), identity)

    assertEquals(validated.occurrence, occurrence)
    assertEquals(validated.occurrences, Vector(occurrence))
    assertEquals(
      SelectedNamePatternMatcher.matchTarget(
        validated,
        TermShape.Select(identifier("service"), "ordinary")
      ).toOption.get.selectedNameBindings,
      Map("member" -> selected("ordinary"))
    )

  test("matches two distinct captures in nullary and unary admitted shapes"):
    val nullary = twoValidated(
      TermPattern.Apply(twoCapturePattern(), Nil)
    )
    val unary = twoValidated(
      TermPattern.Apply(
        twoCapturePattern(),
        List(TermPattern.Hole("argument"))
      )
    )

    assertEquals(
      SelectedNamePatternMatcher.matchTarget(
        nullary,
        TermShape.Apply(twoCaptureTarget("alpha", "beta"), Nil)
      ).toOption.get.selectedNameBindings,
      Map("first" -> selected("alpha"), "second" -> selected("beta"))
    )
    assertEquals(
      SelectedNamePatternMatcher.matchTarget(
        unary,
        TermShape.Apply(
          twoCaptureTarget("alpha", "beta"),
          List(TermShape.Literal("1"))
        )
      ).toOption.get,
      NeutralSelectedNameMatchResult(
        Map(
          "receiver" -> identifier("service"),
          "argument" -> TermShape.Literal("1")
        ),
        Map("first" -> selected("alpha"), "second" -> selected("beta"))
      )
    )

  test("keeps a fixed selected name exact between non-adjacent captures"):
    val pattern = TermPattern.Select(
      TermPattern.Select(
        TermPattern.Select(TermPattern.Hole("receiver"), firstTransport),
        "fixed"
      ),
      secondTransport
    )
    val validated = twoValidated(
      pattern,
      first = SelectedNamePatternOccurrence("first", 2, firstTransport),
      second = SelectedNamePatternOccurrence("second", 0, secondTransport)
    )
    val target = TermShape.Select(
      TermShape.Select(
        TermShape.Select(identifier("service"), "alpha"),
        "fixed"
      ),
      "beta"
    )

    assertEquals(
      SelectedNamePatternMatcher.matchTarget(validated, target).toOption.get
        .selectedNameBindings,
      Map("first" -> selected("alpha"), "second" -> selected("beta"))
    )
    assert(
      SelectedNamePatternMatcher
        .matchTarget(
          validated,
          target.copy(
            qualifier = target.qualifier
              .asInstanceOf[TermShape.Select]
              .copy(name = "different")
          )
        )
        .left
        .toOption
        .exists(_.isInstanceOf[NeutralSelectedNameMatchError.ShapeMismatch])
    )

  test("repeated selected-name captures require exact typed equality"):
    val validated = twoValidated(
      twoCapturePattern(),
      first = SelectedNamePatternOccurrence("member", 1, firstTransport),
      second = SelectedNamePatternOccurrence("member", 0, secondTransport)
    )

    assertEquals(
      SelectedNamePatternMatcher
        .matchTarget(validated, twoCaptureTarget("same", "same"))
        .toOption
        .get
        .selectedNameBindings,
      Map("member" -> selected("same"))
    )
    assertEquals(
      SelectedNamePatternMatcher.matchTarget(
        validated,
        twoCaptureTarget("inner", "outer")
      ),
      Left(
        NeutralSelectedNameMatchError.RepeatedSelectedNameCaptureMismatch(
          "member"
        )
      )
    )

  test("ordinary repeated Term capture equality remains unchanged with two selected captures"):
    val pattern = TermPattern.Apply(
      TermPattern.Select(
        TermPattern.Select(TermPattern.Hole("same"), firstTransport),
        secondTransport
      ),
      List(TermPattern.Hole("same"))
    )
    val validated = twoValidated(pattern)
    val matchingTarget = TermShape.Apply(
      twoCaptureTarget("alpha", "beta"),
      List(identifier("service"))
    )

    assertEquals(
      SelectedNamePatternMatcher.matchTarget(validated, matchingTarget).toOption.get
        .termBindings,
      Map("same" -> identifier("service"))
    )
    assertEquals(
      SelectedNamePatternMatcher.matchTarget(
        validated,
        matchingTarget.copy(arguments = List(identifier("different")))
      ),
      Left(NeutralSelectedNameMatchError.RepeatedTermCaptureMismatch("same"))
    )

  test("two-capture semantic equality ignores transport spelling only"):
    val first = twoValidated(
      twoCapturePattern("__first_a", "__second_a"),
      first = SelectedNamePatternOccurrence("first", 1, "__first_a"),
      second = SelectedNamePatternOccurrence("second", 0, "__second_a")
    )
    val renamed = twoValidated(
      twoCapturePattern("__first_b", "__second_b"),
      first = SelectedNamePatternOccurrence("first", 1, "__first_b"),
      second = SelectedNamePatternOccurrence("second", 0, "__second_b")
    )
    val renamedLogical = twoValidated(
      twoCapturePattern("__first_c", "__second_c"),
      first = SelectedNamePatternOccurrence("other", 1, "__first_c"),
      second = SelectedNamePatternOccurrence("second", 0, "__second_c")
    )
    val moved = twoValidated(
      twoCapturePattern("__first_d", "__second_d"),
      first = SelectedNamePatternOccurrence("first", 0, "__second_d"),
      second = SelectedNamePatternOccurrence("second", 1, "__first_d")
    )
    val repeated = twoValidated(
      twoCapturePattern("__first_e", "__second_e"),
      first = SelectedNamePatternOccurrence("member", 1, "__first_e"),
      second = SelectedNamePatternOccurrence("member", 0, "__second_e")
    )

    assertEquals(first, renamed)
    assertEquals(first.hashCode, renamed.hashCode)
    assertNotEquals(first, renamedLogical)
    assertNotEquals(first, moved)
    assertNotEquals(repeated, ValidatedSelectedNamePattern.create(
      TermPattern.Select(TermPattern.Hole("receiver"), "__single"),
      SelectedNamePatternOccurrence("member", 0, "__single")
    ).toOption.get)

  test("two-capture validation uses stable address transport and category failures"):
    val pattern = twoCapturePattern()
    assertEquals(
      ValidatedSelectedNamePattern.createTwo(
        pattern,
        SelectedNamePatternOccurrence("first", 0, firstTransport),
        SelectedNamePatternOccurrence("second", 0, secondTransport)
      ),
      Left(
        NeutralSelectedNameMatchError.DuplicateSelectedNameOccurrenceAddress(0)
      )
    )
    assertEquals(
      ValidatedSelectedNamePattern.createTwo(
        TermPattern.Select(
          TermPattern.Select(TermPattern.Hole("receiver"), firstTransport),
          firstTransport
        ),
        SelectedNamePatternOccurrence("first", 1, firstTransport),
        SelectedNamePatternOccurrence("second", 0, firstTransport)
      ),
      Left(
        NeutralSelectedNameMatchError.SelectedNameTransportConflict(firstTransport)
      )
    )
    val repeatedTransportPattern = TermPattern.Select(
      TermPattern.Select(
        TermPattern.Select(TermPattern.Hole("receiver"), firstTransport),
        firstTransport
      ),
      secondTransport
    )
    assertEquals(
      ValidatedSelectedNamePattern.createTwo(
        repeatedTransportPattern,
        SelectedNamePatternOccurrence("first", 2, firstTransport),
        SelectedNamePatternOccurrence("second", 0, secondTransport)
      ),
      Left(
        NeutralSelectedNameMatchError.SelectedNameTransportConflict(firstTransport)
      )
    )
    assertInvalidOccurrence(
      ValidatedSelectedNamePattern.createTwo(
        pattern,
        SelectedNamePatternOccurrence("first", 3, firstTransport),
        SelectedNamePatternOccurrence("second", 0, secondTransport)
      )
    )
    assertInvalidOccurrence(
      ValidatedSelectedNamePattern.createTwo(
        pattern,
        SelectedNamePatternOccurrence("first", 0, firstTransport),
        SelectedNamePatternOccurrence("second", 1, secondTransport)
      )
    )
    assertEquals(
      ValidatedSelectedNamePattern.createTwo(
        TermPattern.Select(
          TermPattern.Select(TermPattern.Hole("first"), firstTransport),
          secondTransport
        ),
        SelectedNamePatternOccurrence("first", 1, firstTransport),
        SelectedNamePatternOccurrence("second", 0, secondTransport)
      ),
      Left(NeutralSelectedNameMatchError.CaptureCategoryConflict("first"))
    )

  test("two-capture entry point rejects absent metadata without arbitrary arity API"):
    assertInvalidOccurrence(
      ValidatedSelectedNamePattern.createTwo(twoCapturePattern(), null, null)
    )
    assertInvalidOccurrence(
      ValidatedSelectedNamePattern.createTwo(
        twoCapturePattern(),
        SelectedNamePatternOccurrence("first", 1, firstTransport),
        null
      )
    )
    assert(
      typeCheckErrors(
        """quasiquotes.matching.ValidatedSelectedNamePattern.createTwo(null, null, null, null)"""
      ).nonEmpty
    )

  test("final binding invariant reports missing and undeclared selected bindings"):
    val validated = twoValidated(twoCapturePattern())
    assertEquals(
      SelectedNamePatternMatcher.ensureDeclaredSelectedNameBindings(
        validated,
        Map("first" -> selected("alpha"))
      ),
      Left(
        NeutralSelectedNameMatchError.MissingSelectedNameCapture("second")
      )
    )
    assertInvalidBindingInvariant(
      SelectedNamePatternMatcher.ensureDeclaredSelectedNameBindings(
        validated,
        Map(
          "first" -> selected("alpha"),
          "second" -> selected("beta"),
          "third" -> selected("gamma")
        )
      )
    )

  test("retains lexical policy at both captures and uncaptured fixed names"):
    val validated = twoValidated(twoCapturePattern())
    Vector("+", "type", "two words", "_", "naïve", "$internal").foreach {
      invalid =>
        assertEquals(
          SelectedNamePatternMatcher.matchTarget(
            validated,
            twoCaptureTarget(invalid, "ordinary")
          ),
          Left(
            NeutralSelectedNameMatchError.SelectedNameLexicalUnsupported(
              invalid
            )
          )
        )
        assertEquals(
          SelectedNamePatternMatcher.matchTarget(
            validated,
            twoCaptureTarget("ordinary", invalid)
          ),
          Left(
            NeutralSelectedNameMatchError.SelectedNameLexicalUnsupported(
              invalid
            )
          )
        )
    }

    val invalidFixed = TermPattern.Select(
      TermPattern.Select(
        TermPattern.Select(TermPattern.Hole("receiver"), firstTransport),
        "+"
      ),
      secondTransport
    )
    assertEquals(
      ValidatedSelectedNamePattern.createTwo(
        invalidFixed,
        SelectedNamePatternOccurrence("first", 2, firstTransport),
        SelectedNamePatternOccurrence("second", 0, secondTransport)
      ),
      Left(NeutralSelectedNameMatchError.SelectedNameLexicalUnsupported("+"))
    )

  test("does not widen the direct nullary unary Apply envelope"):
    val multiArgument = TermPattern.Apply(
      twoCapturePattern(),
      List(TermPattern.Hole("a"), TermPattern.Hole("b"))
    )
    val nestedApply = TermPattern.Apply(
      TermPattern.Apply(
        twoCapturePattern(),
        List(TermPattern.Hole("firstArgument"))
      ),
      List(TermPattern.Hole("secondArgument"))
    )

    Vector(multiArgument, nestedApply).foreach { pattern =>
      assert(
        ValidatedSelectedNamePattern
          .createTwo(
            pattern,
            SelectedNamePatternOccurrence("first", 1, firstTransport),
            SelectedNamePatternOccurrence("second", 0, secondTransport)
          )
          .left
          .toOption
          .exists(
            _.isInstanceOf[
              NeutralSelectedNameMatchError.UnsupportedPatternShape
            ]
          )
      )
    }

  private def twoCapturePattern(
      first: String = firstTransport,
      second: String = secondTransport
  ): TermPattern =
    TermPattern.Select(
      TermPattern.Select(TermPattern.Hole("receiver"), first),
      second
    )

  private def twoCaptureTarget(
      first: String,
      second: String
  ): TermShape =
    TermShape.Select(
      TermShape.Select(identifier("service"), first),
      second
    )

  private def twoValidated(
      pattern: TermPattern,
      first: SelectedNamePatternOccurrence =
        SelectedNamePatternOccurrence("first", 1, firstTransport),
      second: SelectedNamePatternOccurrence =
        SelectedNamePatternOccurrence("second", 0, secondTransport)
  ): ValidatedSelectedNamePattern =
    ValidatedSelectedNamePattern
      .createTwo(pattern, first, second)
      .fold(error => fail(error.message), identity)

  private def assertInvalidOccurrence(
      result: Either[
        NeutralSelectedNameMatchError,
        ValidatedSelectedNamePattern
      ]
  ): Unit =
    assert(
      result.left.toOption.exists(
        _.isInstanceOf[
          NeutralSelectedNameMatchError.InvalidSelectedNameOccurrence
        ]
      )
    )

  private def assertInvalidBindingInvariant(
      result: Either[NeutralSelectedNameMatchError, Unit]
  ): Unit =
    assert(
      result.left.toOption.exists(
        _.isInstanceOf[
          NeutralSelectedNameMatchError.InvalidSelectedNameOccurrence
        ]
      )
    )

  private def identifier(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def selected(name: String): SelectedMemberName =
    SelectedMemberName.from(name).fold(error => fail(error.message), identity)
