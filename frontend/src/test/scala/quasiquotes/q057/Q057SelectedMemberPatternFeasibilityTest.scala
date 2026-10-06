package quasiquotes.q057

import scala.compiletime.testing.typeCheckErrors

import quasiquotes.q057.Q057SelectedMemberPatternProbeFactory.validateDecodedTargetName

final class Q057SelectedMemberPatternFeasibilityTest extends munit.FunSuite:
  test("captures direct, nullary, unary, normalized, and resolved-overload selected names"):
    assertEquals(
      Q057SelectedMemberCaptureProbe.observations,
      List(
        "direct:field",
        "nullary:nullary",
        "ordinary:ordinary",
        "symbolic:+",
        "keyword:type",
        "spaced:safe spaced name",
        "overloaded:overloaded",
        "ranked-mechanical:field",
        "null:fallthrough",
        "non-select:fallthrough",
        "unicode:fallthrough",
        "dollar-internal:fallthrough",
        "compiler-special:fallthrough"
      )
    )

  test("validates the already-decoded target name exactly once and fails closed"):
    assertEquals(validateDecodedTargetName("ordinary").map(_.decoded), Some("ordinary"))
    assertEquals(validateDecodedTargetName("+").map(_.decoded), Some("+"))
    assertEquals(validateDecodedTargetName("type").map(_.decoded), Some("type"))
    assertEquals(
      validateDecodedTargetName("safe spaced name").map(_.decoded),
      Some("safe spaced name")
    )
    assertEquals(validateDecodedTargetName("$plus"), None)
    assertEquals(validateDecodedTargetName("<init>"), None)
    assertEquals(validateDecodedTargetName("naïve"), None)
    assertEquals(validateDecodedTargetName(null), None)

  test("prototype exposes exact receiver, selected-name, and argument binder types"):
    val good = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.construct.SelectedMemberName
        import quasiquotes.q057.Q057SelectedMemberPatternProbe.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case q057qq"$receiver.$selectedName($argument)" =>
            val r: q.reflect.Term = receiver
            val n: SelectedMemberName = selectedName
            val a: q.reflect.Term = argument
            (r, n, a)
          case _ => throw new IllegalArgumentException("no match")
      }"""
    )
    assertEquals(good, Nil)

  test("prototype rejects reversed static binder assignments"):
    val selectedAsTerm = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.q057.Q057SelectedMemberPatternProbe.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case q057qq"$receiver.$selectedName($argument)" =>
            val wrong: q.reflect.Term = selectedName
          case _ => ()
      }"""
    )
    val selectedAsString = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.q057.Q057SelectedMemberPatternProbe.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case q057qq"$receiver.$selectedName($argument)" =>
            val wrong: String = selectedName
          case _ => ()
      }"""
    )
    val receiverAsName = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.construct.SelectedMemberName
        import quasiquotes.q057.Q057SelectedMemberPatternProbe.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case q057qq"$receiver.$selectedName($argument)" =>
            val wrong: SelectedMemberName = receiver
          case _ => ()
      }"""
    )
    val argumentAsName = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.construct.SelectedMemberName
        import quasiquotes.q057.Q057SelectedMemberPatternProbe.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case q057qq"$receiver.$selectedName($argument)" =>
            val wrong: SelectedMemberName = argument
          case _ => ()
      }"""
    )

    assert(selectedAsTerm.nonEmpty)
    assert(selectedAsString.nonEmpty)
    assert(receiverAsName.nonEmpty)
    assert(argumentAsName.nonEmpty)

  test("probe refuses shapes outside the first bounded selected-name slice"):
    val bare = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.q057.Q057SelectedMemberPatternProbe.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case q057qq"$selectedName" => selectedName
          case _ => ()
      }"""
    )
    val nested = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.q057.Q057SelectedMemberPatternProbe.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case q057qq"wrap($receiver.$selectedName)" => selectedName
          case _ => ()
      }"""
    )
    val twoNames = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.q057.Q057SelectedMemberPatternProbe.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case q057qq"$receiver.$first.$second" => (first, second)
          case _ => ()
      }"""
    )

    assert(bare.nonEmpty)
    assert(nested.nonEmpty)
    assert(twoNames.nonEmpty)
