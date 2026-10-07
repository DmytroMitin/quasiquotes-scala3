package quasiquotes.q060

import scala.compiletime.testing.typeCheckErrors

final class Q060SelectedMemberRankedPatternTest extends munit.FunSuite:
  test("mixed selected-name and rank-2 matching preserves names, cardinality, identity, and order"):
    assertEquals(
      Q060SelectedMemberRankedPatternProbe.observations,
      List(
        "zero:zero:0",
        "one:one:0",
        "ordinary:many:3",
        "symbolic:+:2",
        "keyword:type:2",
        "spaced:safe spaced name:2",
        "overloaded:overloaded:1",
        "unicode:fallthrough",
        "dollar-internal:fallthrough",
        "non-apply:fallthrough",
        "insufficient:fallthrough"
      )
    )

  test("all four mixed layouts expose exact heterogeneous product types"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.construct.SelectedMemberName
        import quasiquotes.matching.{QuasiPattern, TermPatternProductExtractor}
        import QuasiPattern.*

        def extractor(using q: Quotes) =
          val value: TermPatternProductExtractor[
            q.reflect.Term,
            (q.reflect.Term, SelectedMemberName, Seq[q.reflect.Term])
          ] = QuasiPattern.qq(StringContext("", ".", "(..", ")"))(using q)
          value

        def probe(using q: Quotes)(target: q.reflect.Term) =
          target match
            case qq"$receiver.$selectedName(..$arguments)" =>
              val r: q.reflect.Term = receiver
              val n: SelectedMemberName = selectedName
              val as: Seq[q.reflect.Term] = arguments
              (r, n, as)
            case qq"$receiver.$selectedName($head, ..$tail)" =>
              val r: q.reflect.Term = receiver
              val n: SelectedMemberName = selectedName
              val h: q.reflect.Term = head
              val ts: Seq[q.reflect.Term] = tail
              (r, n, h, ts)
            case qq"$receiver.$selectedName(..$init, $last)" =>
              val r: q.reflect.Term = receiver
              val n: SelectedMemberName = selectedName
              val is: Seq[q.reflect.Term] = init
              val l: q.reflect.Term = last
              (r, n, is, l)
            case qq"$receiver.$selectedName($first, ..$middle, $last)" =>
              val r: q.reflect.Term = receiver
              val n: SelectedMemberName = selectedName
              val f: q.reflect.Term = first
              val ms: Seq[q.reflect.Term] = middle
              val l: q.reflect.Term = last
              (r, n, f, ms, l)
            case _ => throw new IllegalArgumentException("no match")
      }"""
    )

    assertEquals(errors, Nil)

  test("mixed roles reject cross-category assignments"):
    val selectedAsTerm = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName(..$arguments)" =>
            val wrong: q.reflect.Term = selectedName
          case _ => ()
      }"""
    )
    val selectedAsString = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName(..$arguments)" =>
            val wrong: String = selectedName
          case _ => ()
      }"""
    )
    val sequenceAsTerm = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName(..$arguments)" =>
            val wrong: q.reflect.Term = arguments
          case _ => ()
      }"""
    )
    val scalarAsSequence = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName($head, ..$tail)" =>
            val wrong: Seq[q.reflect.Term] = head
          case _ => ()
      }"""
    )
    val receiverAsName = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.construct.SelectedMemberName
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName(..$arguments)" =>
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
          case qq"$receiver.$selectedName($head, ..$tail)" =>
            val wrong: SelectedMemberName = head
          case _ => ()
      }"""
    )

    assert(selectedAsTerm.nonEmpty)
    assert(selectedAsString.nonEmpty)
    assert(sequenceAsTerm.nonEmpty)
    assert(scalarAsSequence.nonEmpty)
    assert(receiverAsName.nonEmpty)
    assert(argumentAsName.nonEmpty)

  test("mixed source classification rejects multiple, rank-3, and non-direct argument sequence holes"):
    val multiple = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName(..$first, ..$second)" => ()
          case _ => ()
      }"""
    )
    val rank3 = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName(...$arguments)" => ()
          case _ => ()
      }"""
    )
    val nested = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName(wrap(..$arguments))" => ()
          case _ => ()
      }"""
    )
    val noCall = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName..$arguments" => ()
          case _ => ()
      }"""
    )
    val nestedSelection = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName.child(..$arguments)" => ()
          case _ => ()
      }"""
    )

    assert(multiple.exists(_.message.contains("only one rank-2")))
    assert(rank3.exists(_.message.contains("unsupported rank-3")))
    assert(nested.exists(_.message.contains("direct ordinary Apply argument")))
    assert(noCall.nonEmpty)
    assert(nestedSelection.exists(_.message.contains("direct ordinary Apply argument")))
