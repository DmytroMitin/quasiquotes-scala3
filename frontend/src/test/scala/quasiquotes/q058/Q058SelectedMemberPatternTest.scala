package quasiquotes.q058

import scala.compiletime.testing.typeCheckErrors

final class Q058SelectedMemberPatternTest extends munit.FunSuite:
  test("standard qq captures direct, nullary, unary, normalized, and overloaded selected names"):
    assertEquals(
      Q058SelectedMemberPatternProbe.observations,
      List(
        "direct:field",
        "nullary:nullary",
        "ordinary:ordinary",
        "symbolic:+",
        "keyword:type",
        "spaced:safe spaced name",
        "overloaded:overloaded",
        "null:fallthrough",
        "non-select:fallthrough",
        "unicode:fallthrough",
        "dollar-internal:fallthrough",
        "compiler-special:fallthrough"
      )
    )

  test("selected-name templates expose exact public extractor and binder types"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.construct.SelectedMemberName
        import quasiquotes.matching.{QuasiPattern, TermPatternProductExtractor}
        import QuasiPattern.*

        def direct(using q: Quotes) =
          val extractor: TermPatternProductExtractor[
            q.reflect.Term,
            (q.reflect.Term, SelectedMemberName)
          ] = QuasiPattern.qq(StringContext("", ".", ""))(using q)
          extractor

        def unary(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName($argument)" =>
            val r: q.reflect.Term = receiver
            val n: SelectedMemberName = selectedName
            val a: q.reflect.Term = argument
            (r, n, a)
          case _ => throw new IllegalArgumentException("no match")
      }"""
    )

    assertEquals(errors, Nil)

  test("selected-name binders reject Term, String, and reversed semantic assignments"):
    val selectedAsTerm = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName($argument)" =>
            val wrong: q.reflect.Term = selectedName
          case _ => ()
      }"""
    )
    val selectedAsString = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName($argument)" =>
            val wrong: String = selectedName
          case _ => ()
      }"""
    )
    val receiverAsName = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.construct.SelectedMemberName
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName($argument)" =>
            val wrong: SelectedMemberName = receiver
          case _ => ()
      }"""
    )
    val argumentAsName = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.construct.SelectedMemberName
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName($argument)" =>
            val wrong: SelectedMemberName = argument
          case _ => ()
      }"""
    )

    assert(selectedAsTerm.nonEmpty)
    assert(selectedAsString.nonEmpty)
    assert(receiverAsName.nonEmpty)
    assert(argumentAsName.nonEmpty)

  test("positions outside the direct-root slice never acquire selected-name typing"):
    val bare = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.construct.SelectedMemberName
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$selectedName" =>
            val wrong: SelectedMemberName = selectedName
          case _ => ()
      }"""
    )
    val nested = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.construct.SelectedMemberName
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"wrap($receiver.$selectedName)" =>
            val wrong: SelectedMemberName = selectedName
          case _ => ()
      }"""
    )
    val twoNames = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second" => (first, second)
          case _ => ()
      }"""
    )
    val infix = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.construct.SelectedMemberName
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver $selectedName $argument" =>
            val wrong: SelectedMemberName = selectedName
          case _ => ()
      }"""
    )

    assert(bare.nonEmpty)
    assert(nested.nonEmpty)
    assert(twoNames.nonEmpty)
    assert(infix.nonEmpty)

  test("dynamic StringContext retains the scalar fallback result type"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.{QuasiPattern, TermPatternExtractor}
        def probe(using q: Quotes)(context: StringContext) =
          val extractor: TermPatternExtractor[q.reflect.Term] =
            QuasiPattern.qq(context)(using q)
          extractor
      }"""
    )

    assertEquals(errors, Nil)

  test("the general product extractor constructor remains matching-private"):
    val errors = typeCheckErrors(
      """{
        import quasiquotes.matching.TermPatternProductExtractor
        new TermPatternProductExtractor[Int, Int *: EmptyTuple](_ => Some(1 *: EmptyTuple))
      }"""
    )

    assert(errors.nonEmpty)
