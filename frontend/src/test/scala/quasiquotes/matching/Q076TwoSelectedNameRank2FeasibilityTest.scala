package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors

final class Q076TwoSelectedNameRank2FeasibilityTest extends munit.FunSuite:
  test("Q083 public standard qq now admits the formerly private two-name rank-2 composition"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*

        def probe(using q: Quotes)(target: q.reflect.Term) =
          target match
            case qq"$receiver.$first.$second(..$arguments)" =>
              (receiver, first, second, arguments)
            case _ => ()
      }"""
    )

    assertEquals(errors, Nil)

  test("accepted one-name rank-2 public pattern remains a positive control"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*

        def probe(using q: Quotes)(target: q.reflect.Term) =
          target match
            case qq"$receiver.$selectedName(..$arguments)" =>
              val r: q.reflect.Term = receiver
              val as: Seq[q.reflect.Term] = arguments
              (r, selectedName, as)
            case _ => ()
      }"""
    )

    assertEquals(errors, Nil)

  test("source plan keeps two selected transports separate from four ranked holes"):
    val plan = Q076StandardMixedModel.plan().fold(fail(_), identity)

    assertEquals(plan.rankedHoleNames, Vector(
      "qqCapture0", "qqCapture1", "qqCapture2", "qqCapture3"
    ))
    assertEquals(plan.sequenceIndex, 3)
    assertEquals(plan.receiverHoleName, "qqCapture0")
    assertEquals(plan.sequenceHoleName, "qqCapture3")
    assertEquals(
      plan.selectedOccurrences.map(value => value.name -> value.selectOrdinal),
      Vector("first" -> 1, "second" -> 0)
    )
    assertEquals(plan.selectedOccurrences.map(_.transport).distinct.size, 2)
    plan.selectedOccurrences.foreach { occurrence =>
      assertEquals(plan.normalized.ordinaryHoleIndex.semanticNameFor(occurrence.transport), None)
    }
    assertEquals(plan.normalized.occurrences.map(_.name),
      Vector("receiver", "first", "second", "arguments"))
    assertEquals(
      plan.normalized.occurrences.map(_.originalSpan.start),
      plan.normalized.occurrences.map(_.originalSpan.start).sorted
    )
    assertEquals(
      plan.pattern,
      TermPattern.Apply(
        TermPattern.Select(
          TermPattern.Select(
            TermPattern.Hole("qqCapture0"),
            plan.selectedOccurrences(0).transport
          ),
          plan.selectedOccurrences(1).transport
        ),
        List(TermPattern.Hole("qqCapture3"))
      )
    )

  test("Q075 candidate-only route declines before source recovery and the Q060 route rejects topology"):
    var sourceRequested = false
    val twoName = TwoSelectedMemberPatternSource.classify(
      Q076StandardMixedModel.CandidateParts,
      {
        sourceRequested = true
        Q076StandardMixedModel.LogicalSource
      }
    )
    assertEquals(twoName, Right(None))
    assertEquals(sourceRequested, false)

    val oneNamePlan = SelectedMemberPatternSource
      .classifyIncludingRanked(Q076StandardMixedModel.CandidateParts)
      .fold(fail(_), _.getOrElse(fail("candidate was not classified by the one-name route")))
    val compiled = QuasiPattern.term(oneNamePlan.effectiveSource).fold(
      failure => fail(failure.message),
      _.pattern
    )
    assert(
      SelectedMemberPatternBridge
        .validatePlan(oneNamePlan, compiled)
        .left
        .toOption
        .exists(_.contains("direct root Apply"))
    )

  test("private standard model preserves ordered typed captures and exact Term identity"):
    assertEquals(
      Q076StandardMixedProbe.observations,
      List(
        "zero:first:second:0:true",
        "one:first:second:1:true",
        "many:first:second:3:true"
      )
    )

    assertEquals(
      Q076StandardMixedProbe.lexicalObservations,
      List(
        "symbolic:inner:+",
        "keyword:inner:type",
        "spaced:inner:safe spaced name",
        "generated:inner:__qqhole_second__selected_0",
        "unicode:fallthrough"
      )
    )
    assertEquals(
      Q076StandardMixedProbe.negativeObservations,
      List("non-apply:true", "curried:true", "null:true")
    )

  test("bad ranked and selected source roles fail atomically"):
    val missing = RankedPatternSource.classify(List("", ".", ".", "(", ")"))
    val tooMany = RankedPatternSource.classify(
      List("", ".", ".", "(..", ", ..", ")")
    )
    val rank3 = RankedPatternSource.classify(
      List("", ".", ".", "(...", ")")
    )
    assertEquals(missing.toOption.flatMap(_.sequenceIndex), None)
    assert(tooMany.left.toOption.exists(_.contains("only one rank-2")))
    assert(rank3.left.toOption.exists(_.contains("unsupported rank-3")))

    val plan = Q076StandardMixedModel.plan().fold(fail(_), identity)
    assert(
      ValidatedSelectedNamePattern.createTwo(
        plan.pattern,
        plan.selectedOccurrences(0).copy(selectOrdinal = 0),
        plan.selectedOccurrences(1).copy(selectOrdinal = 1)
      ).isLeft
    )
    assert(
      ValidatedSelectedNamePattern.createTwo(
        plan.pattern,
        plan.selectedOccurrences(0),
        plan.selectedOccurrences(1).copy(
          transport = plan.selectedOccurrences(0).transport
        )
      ).isLeft
    )
    assert(
      SelectedNameSourceNormalizer.normalize(
        "$first.$first.$second($arguments)",
        Vector(SelectedNameSourcePosition(1, 1), SelectedNameSourcePosition(2, 0))
      ).left.toOption.exists(_.message.contains("both an ordinary Term hole"))
    )
    Vector(
      "$receiver.$+.$second($arguments)",
      "$receiver.$type.$second($arguments)",
      "$receiver.$naïve.$second($arguments)"
    ).foreach { source =>
      assert(
        SelectedNameSourceNormalizer.normalize(
          source,
          Vector(SelectedNameSourcePosition(1, 1), SelectedNameSourcePosition(2, 0))
        ).isLeft,
        source
      )
    }

  test("nested rank-2 and additional argument clauses remain outside the bounded model"):
    val nested = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second(wrap(..$arguments))" => ()
          case _ => ()
      }"""
    )
    val additionalClause = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second()(..$arguments)" => ()
          case _ => ()
      }"""
    )

    assert(nested.nonEmpty, clues(nested))
    assert(additionalClause.nonEmpty, clues(additionalClause))

  test("foreign Quotes Terms are rejected by the dependent extractor type"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.construct.SelectedMemberName
        import quasiquotes.matching.TermPatternProductExtractor

        def rejectForeign(using q1: Quotes)(
            extractor: TermPatternProductExtractor[
              q1.reflect.Term,
              (q1.reflect.Term, SelectedMemberName, SelectedMemberName, Seq[q1.reflect.Term])
            ]
        )(using q2: Quotes)(target: q2.reflect.Term) =
          extractor.unapply(target)
      }"""
    )

    assert(errors.nonEmpty, clues(errors))

  test("native repeated binders and unreserved repeat marker remain rejected"):
    val repeated = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$same.$same(..$arguments)" => ()
          case _ => ()
      }"""
    )
    val marker = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.#repeat(..$arguments)" => ()
          case _ => ()
      }"""
    )

    assert(repeated.exists(_.message.contains("duplicate pattern variable: same")), clues(repeated))
    assert(marker.nonEmpty, clues(marker))
