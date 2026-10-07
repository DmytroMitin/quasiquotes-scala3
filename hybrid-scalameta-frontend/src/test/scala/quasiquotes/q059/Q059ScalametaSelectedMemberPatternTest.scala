package quasiquotes.q059

import scala.compiletime.testing.typeCheckErrors

import quasiquotes.matching.{SelectedMemberPatternBridge, SelectedMemberPatternSource, TermPattern}
import quasiquotes.scalameta.TermFrontend

final class Q059ScalametaSelectedMemberPatternTest extends munit.FunSuite:
  private inline def messages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  test("typed-Scalameta qq captures direct nullary unary and admitted selected names"):
    assertEquals(
      Q059ScalametaSelectedMemberPatternProbe.observations,
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

  test("selected-name templates expose the existing general product carrier and exact binder types"):
    val errors = messages(
      """import scala.quoted.*
        import quasiquotes.construct.SelectedMemberName
        import quasiquotes.matching.TermPatternProductExtractor
        import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def direct(using q: Quotes) =
          val extractor: TermPatternProductExtractor[
            q.reflect.Term,
            (q.reflect.Term, SelectedMemberName)
          ] = qq(StringContext("", ".", ""))(using q)
          extractor
        def unary(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName($argument)" =>
            val r: q.reflect.Term = receiver
            val n: SelectedMemberName = selectedName
            val a: q.reflect.Term = argument
            (r, n, a)
          case _ => throw new IllegalArgumentException("no match")
      """
    )
    assertEquals(errors, Nil)

  test("selected-name binders reject Term String and reversed semantic assignments"):
    val selectedAsTerm = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName($argument)" => val wrong: q.reflect.Term = selectedName
          case _ => ()"""
    )
    val selectedAsString = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName($argument)" => val wrong: String = selectedName
          case _ => ()"""
    )
    val receiverAsName = messages(
      """import scala.quoted.*; import quasiquotes.construct.SelectedMemberName
        import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName($argument)" => val wrong: SelectedMemberName = receiver
          case _ => ()"""
    )
    val argumentAsName = messages(
      """import scala.quoted.*; import quasiquotes.construct.SelectedMemberName
        import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName($argument)" => val wrong: SelectedMemberName = argument
          case _ => ()"""
    )
    assert(selectedAsTerm.nonEmpty)
    assert(selectedAsString.nonEmpty)
    assert(receiverAsName.nonEmpty)
    assert(argumentAsName.nonEmpty)

  test("selected-name diagnostics remain hybrid-owned and reject unsupported positions"):
    val bare = messages(
      """import scala.quoted.*; import quasiquotes.construct.SelectedMemberName
        import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$selectedName" => val wrong: SelectedMemberName = selectedName
          case _ => ()"""
    )
    val nested = messages(
      """import scala.quoted.*; import quasiquotes.construct.SelectedMemberName
        import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"wrap($receiver.$selectedName)" => val wrong: SelectedMemberName = selectedName
          case _ => ()"""
    )
    val twoNames = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second" => (first, second)
          case _ => ()"""
    )
    val infix = messages(
      """import scala.quoted.*; import quasiquotes.construct.SelectedMemberName
        import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver $selectedName $argument" => val wrong: SelectedMemberName = selectedName
          case _ => ()"""
    )
    val malformedDirect = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$selectedName(" => ()
          case _ => ()"""
    )

    assert(bare.nonEmpty)
    assert(nested.nonEmpty)
    assert(twoNames.exists(_.contains("Invalid Scalameta qq term-pattern template")))
    assert(infix.nonEmpty)
    assert(malformedDirect.exists(_.contains("Invalid Scalameta qq term-pattern template")))

  test("shared selected-name plans compile Scalameta-primary for all admitted topologies"):
    val parts = List(
      List("", ".", ""),
      List("", ".", "()"),
      List("", ".", "(", ")")
    )
    parts.foreach { sourceParts =>
      val plan = SelectedMemberPatternSource
        .classify(sourceParts)
        .fold(fail(_), _.getOrElse(fail(s"not classified: $sourceParts")))
      val compiled = TermFrontend.compile(plan.effectiveSource).fold(
        failure => fail(failure.message),
        identity
      )
      assertEquals(compiled.engine, TermFrontend.Engine.Scalameta)
      assertEquals(compiled.primaryFailure, None)
      assertEquals(SelectedMemberPatternBridge.validatePlan(plan, compiled.pattern), Right(()))
    }

  test("compiled topology validation rejects a mismatched selected-name role"):
    val plan = SelectedMemberPatternSource
      .classify(List("", ".", ""))
      .fold(fail(_), _.getOrElse(fail("direct source was not classified")))
    val mismatch = TermPattern.Apply(
      TermPattern.Select(TermPattern.Hole(plan.holeNames.head), plan.selectedNamePlaceholder),
      Nil
    )
    val result = SelectedMemberPatternBridge.validatePlan(plan, mismatch)
    assert(result.isLeft)

  test("dynamic StringContext keeps the existing scalar hybrid extractor"):
    val errors = messages(
      """import scala.quoted.*
        import quasiquotes.scalameta.{ScalametaQuasiPattern, ScalametaTermPatternExtractor}
        def probe(using q: Quotes)(context: StringContext) =
          val extractor: ScalametaTermPatternExtractor[q.reflect.Term] =
            ScalametaQuasiPattern.qq(context)(using q)
          extractor
      """
    )
    assertEquals(errors, Nil)
