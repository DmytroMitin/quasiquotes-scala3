package quasiquotes.hybrid

import scala.quoted.staging.{Compiler, run, withQuotes}
import scala.meta.dialects

import quasiquotes.construct.hybrid.{HybridTermFrontend, ScalametaTermFrontend}
import quasiquotes.matching.{TargetTermView, TermMatcher}
import quasiquotes.matching.hybrid.{HybridPatternFrontend, ScalametaPatternFrontend}

private object HybridIdentifierScope:
  private val namedValue = 41
  val alternateValue: Int = TermQ3Macros.identifierValue

class HybridTermFrontendTest extends munit.FunSuite:
  test("selected dialect follows the active supported compiler line"):
    val version = TermQ3DialectPolicy.compilerVersion
    val expected = if version.startsWith("3.8") then "Scala38" else "Scala3"
    assertEquals(TermQ3DialectPolicy.selectedName, expected)

  test("Scalameta construction preserves caller-owned holes and matches current structure"):
    val evidence = TermQ3Macros.constructionEvidence
    assertEquals(HybridIdentifierScope.alternateValue, 41)
    assert(evidence.exists(_.contains("literal current=Literal(42) scalameta=Literal(42)")), evidence.mkString("\n"))
    assert(evidence.exists(_.contains("ordinary-hole-original=true")), evidence.mkString("\n"))
    assert(evidence.exists(_.contains("multiple-holes-original=true,true")), evidence.mkString("\n"))
    assert(evidence.exists(_.contains("selection-application-equal=true")), evidence.mkString("\n"))
    assert(evidence.exists(_.contains("nested-equal=true")), evidence.mkString("\n"))
    assert(evidence.exists(_.contains("constructed-type-splice-equal=true")), evidence.mkString("\n"))

  test("Scalameta patterns preserve ordered original reflected capture identity"):
    val evidence = TermQ3Macros.matchingEvidence
    assert(evidence.exists(_.contains("one-capture-original=true")), evidence.mkString("\n"))
    assert(evidence.exists(_.contains("ordered-original=true,true")), evidence.mkString("\n"))
    assert(evidence.exists(_.contains("selection-application=true")), evidence.mkString("\n"))
    assert(evidence.exists(_.contains("nested-original=true")), evidence.mkString("\n"))
    assert(evidence.exists(_.contains("ordinary-mismatch=true")), evidence.mkString("\n"))
    assert(evidence.exists(_.contains("independent-target-original=true")), evidence.mkString("\n"))

  test("malformed pattern templates produce a controlled parser diagnostic"):
    val failure = ScalametaPatternFrontend.compile("$value +").swap.toOption.get
    assertEquals(failure.category, "EXACT_COMPILER_SYNTAX_REJECTED")
    assert(failure.start >= 0, failure)
    assert(failure.end >= failure.start, failure)

  test("generated NoSpan matching returns the original reflected capture"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*
      val generated = Literal(IntConstant(42))
      val pattern = ScalametaPatternFrontend.compile("$value").toOption.get
      val captured = TermMatcher.matchTerm(using q)(pattern, generated).toOption.get.bindings("value")
      val noSpan = quasiquotes.source.ReflectedPositionProvenance.usableBounds(using q)(generated.pos).isEmpty
      (captured.asInstanceOf[AnyRef] eq generated.asInstanceOf[AnyRef], noSpan)
    assertEquals(evidence, (true, true))

  test("synthetically restricted Scalameta dialect proves exact-Dotty parser fallback"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      HybridTermFrontend.build(Seq("if true then 1 else 2"), Nil, dialects.Scala213)
    assertEquals(result.map(_.engine), Right(HybridTermFrontend.Engine.CurrentDottyFallback))
    assert(result.toOption.flatMap(_.primaryFailure).exists(_.category == "SCALAMETA_PARSE_FAILURE"))

  test("a broader Scalameta dialect never overrides exact compiler syntax rejection"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      HybridTermFrontend.build(
        Seq("(0: (name: Int, age: Int))"),
        Nil,
        dialects.Scala3Future
      )
    if TermQ3DialectPolicy.compilerVersion.startsWith("3.3") then
      assert(result.swap.toOption.exists(_.category == "EXACT_COMPILER_SYNTAX_REJECTED"), result.toString)
    else
      assert(!result.exists(_.engine == HybridTermFrontend.Engine.Scalameta), result.toString)

  test("staging withQuotes and run exercise the alternate construction path"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val shape = withQuotes:
      HybridTermFrontend.build(Seq("42"), Nil).flatMap(result =>
        TargetTermView.fromTerm(result.term).left.map(error =>
          ScalametaTermFrontend.Failure.lowering(error.message)
        ).map(view => result.engine -> view.render)
      )
    assertEquals(shape, Right(HybridTermFrontend.Engine.Scalameta -> "Literal(42)"))

    val value = run:
      ScalametaTermFrontend.lower(Seq("42"), Nil).fold(
        failure => throw new IllegalArgumentException(failure.message),
        _.asExprOf[Int]
      )
    assertEquals(value, 42)

  test("current public qr and qq remain callable beside the experiment"):
    assertEquals(TermQ3Macros.currentEngineEvidence, (42, (20, 22)))

  test("pattern fallback remains callable without changing explicit QuasiPattern semantics"):
    val restricted = HybridPatternFrontend.compile("if true then $value else 0", dialects.Scala213)
    assertEquals(
      restricted.map(_.engine),
      Right(HybridPatternFrontend.Engine.CurrentDottyFallback),
      restricted
    )
    val repeated = quasiquotes.matching.QuasiPattern.term("($x, $x)")
    assert(repeated.isRight)
