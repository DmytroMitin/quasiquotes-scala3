package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors

final class Q076ScalametaTwoSelectedNameRank2FeasibilityTest extends munit.FunSuite:
  test("current public Scalameta qq rejects the unselected two-name rank-2 composition"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.scalameta.ScalametaQuasiPattern.qq

        def probe(using q: Quotes)(target: q.reflect.Term) =
          target match
            case qq"$receiver.$first.$second(..$arguments)" =>
              (receiver, first, second, arguments)
            case _ => ()
      }"""
    )

    assert(
      errors.exists(_.message.contains(
        "compiled mixed selected-name pattern must be a direct root Apply"
      )),
      clues(errors)
    )

  test("accepted one-name rank-2 Scalameta pattern remains a positive control"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.scalameta.ScalametaQuasiPattern.qq

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

  test("Scalameta-primary source plan matches the standard ranked and selected roles"):
    val plan = Q076ScalametaMixedModel.plan().fold(fail(_), identity)

    assertEquals(plan.engine, quasiquotes.scalameta.TermFrontend.Engine.Scalameta)
    assertEquals(plan.primaryFailure, None)
    assertEquals(plan.rankedHoleNames, Vector(
      "qqCapture0", "qqCapture1", "qqCapture2", "qqCapture3"
    ))
    assertEquals(plan.sequenceIndex, 3)
    assertEquals(
      plan.selectedOccurrences.map(value => value.name -> value.selectOrdinal),
      Vector("first" -> 1, "second" -> 0)
    )
    assertEquals(plan.selectedOccurrences.map(_.transport).distinct.size, 2)
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

  test("Q075 Scalameta candidate route declines before source recovery"):
    var sourceRequested = false
    val twoName = ScalametaTwoSelectedMemberPatternSource.classify(
      Q076ScalametaMixedModel.CandidateParts,
      {
        sourceRequested = true
        Q076ScalametaMixedModel.LogicalSource
      }
    )
    assertEquals(twoName, Right(None))
    assertEquals(sourceRequested, false)

  test("private Scalameta-primary model preserves ordered typed captures and exact Term identity"):
    assertEquals(
      Q076ScalametaMixedProbe.observations,
      List(
        "zero:first:second:0:true",
        "one:first:second:1:true",
        "many:first:second:3:true"
      )
    )

    assertEquals(
      Q076ScalametaMixedProbe.lexicalObservations,
      List(
        "symbolic:inner:+",
        "keyword:inner:type",
        "spaced:inner:safe spaced name",
        "generated:inner:__qqhole_second__selected_0",
        "unicode:fallthrough"
      )
    )
    assertEquals(
      Q076ScalametaMixedProbe.negativeObservations,
      List("non-apply:true", "curried:true", "null:true")
    )

  test("bad ranked and selected roles fail before a partial Scalameta match"):
    assert(
      RankedPatternSource
        .classify(List("", ".", ".", "(..", ", ..", ")"))
        .left.toOption.exists(_.contains("only one rank-2"))
    )
    assert(
      RankedPatternSource
        .classify(List("", ".", ".", "(...", ")"))
        .left.toOption.exists(_.contains("unsupported rank-3"))
    )

    val plan = Q076ScalametaMixedModel.plan().fold(fail(_), identity)
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
