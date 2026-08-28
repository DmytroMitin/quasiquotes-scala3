package quasiquotes.hybrid

import scala.quoted.staging.{Compiler, run, withQuotes}
import scala.meta.dialects

import quasiquotes.construct.hybrid.{HybridTermFrontend, ScalametaTermFrontend}
import quasiquotes.matching.{BlockPatternStatement, TargetTermView, TermMatcher, TermPattern}
import quasiquotes.matching.hybrid.{HybridPatternFrontend, ScalametaPatternFrontend}

private object HybridIdentifierScope:
  private val namedValue = 41
  val alternateValue: Int = TermQ3Macros.identifierValue

class HybridTermFrontendTest extends munit.FunSuite:
  test("selected dialect follows the active supported compiler line"):
    val version = TermQ3DialectPolicy.compilerVersion
    val expected = if version.startsWith("3.8") then "Scala38" else "Scala3"
    assertEquals(TermQ3DialectPolicy.selectedName, expected)

  test("authoritative public term matrix is unique, exhaustive, and singly classified"):
    val rows = TermQ3ParityMatrix.rows
    assertEquals(rows.map(_.id).distinct.size, rows.size)
    assertEquals(rows.map(_.id).toSet, TermQ3ParityMatrix.requiredIds)
    assertEquals(rows.size, 37)
    assertEquals(
      rows.count(_.classification == TermQ3ParityMatrix.Classification.HYBRID_SCALAMETA_SUPPORTED),
      31
    )
    assertEquals(
      rows.count(_.classification == TermQ3ParityMatrix.Classification.NOT_A_PUBLIC_TERM_CASE),
      6
    )
    assertEquals(
      rows.find(_.id == "p1-expression-block").map(_.classification),
      Some(TermQ3ParityMatrix.Classification.HYBRID_SCALAMETA_SUPPORTED)
    )
    assertEquals(
      rows.find(_.id == "p2-single-typed-local-immutable-val").map(_.classification),
      Some(TermQ3ParityMatrix.Classification.HYBRID_SCALAMETA_SUPPORTED)
    )

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

  test("full admitted construction and pattern corpus is structurally equal side by side"):
    val (construction, patterns) = TermQ3Macros.fullDifferentialEvidence
    assertEquals(construction.size, 13)
    assertEquals(patterns.size, 11)
    construction.foreach(row => assert(row.endsWith("=true"), construction.mkString("\n")))
    patterns.foreach(row => assert(row.endsWith("=true"), patterns.mkString("\n")))

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

  test("Scalameta local-definition syntax fails terminally without current-Dotty fallback"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      HybridTermFrontend.build(
        Seq("{ def boundedIdentity(value: Int): Int = value; boundedIdentity(1) }"),
        Nil
      )
    assertEquals(
      result.left.map(_.category),
      Left("SCALAMETA_LOWERING_UNSUPPORTED")
    )

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

  test("bounded supported-line dialect search finds no real exact-compiler syntax lag"):
    val searchMatrix = List(
      "42",
      "\"value\"",
      "true",
      "value",
      "value.size",
      "f(value)",
      "left + right",
      "-value",
      "(left, right)",
      "if condition then left else right",
      "(value: Int)",
      "s\"hello $name\"",
      "new java.lang.StringBuilder(16)",
      "(x: Int) => x",
      "f(g(value))"
    )
    val realLags = searchMatrix.filter { source =>
      quasiquotes.parser.TinyTermParser.parse(source).isRight &&
        ScalametaTermFrontend.parse(source).isLeft
    }
    assertEquals(
      realLags,
      Nil,
      "NO_REAL_SUPPORTED_LINE_LAG_FOUND_IN_SEARCH_MATRIX"
    )

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

  test("Scalameta construction covers current prefix unary, constructor, and Lambda1 rows"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*
      def renderPlain(source: String) =
        ScalametaTermFrontend.lower(using q)(Seq(source), Nil).flatMap(term =>
          TargetTermView.fromTerm(using q)(term).left.map(error =>
            ScalametaTermFrontend.Failure.lowering(error.message)
          ).map(_.render)
        )
      val operand = '{ 1 }.asTerm
      val unary = ScalametaTermFrontend.lower(using q)(Seq("-", ""), Seq(operand)).flatMap(term =>
        TargetTermView.fromTerm(using q)(term).left.map(error =>
          ScalametaTermFrontend.Failure.lowering(error.message)
        ).map(_.render)
      )
      (
        unary,
        renderPlain("new java.lang.StringBuilder(16)"),
        renderPlain("(x: Int) => x")
      )
    assertEquals(evidence._1, Right("Unary(-, Literal(1))"))
    assertEquals(evidence._2, Right("New(java.lang.StringBuilder, [Literal(16)])"))
    assertEquals(evidence._3, Right("Lambda1(x: Int, BoundRef(x))"))

  test("Scalameta matching covers current prefix unary, constructor, Lambda1, and interpolation rows"):
    assertEquals(
      ScalametaPatternFrontend.compile("-$value").map(_.render),
      Right("Unary(-, Hole($value))")
    )
    assertEquals(
      ScalametaPatternFrontend.compile("new java.lang.StringBuilder($capacity)").map(_.render),
      Right("New(java.lang.StringBuilder, [Hole($capacity)])")
    )
    assertEquals(
      ScalametaPatternFrontend.compile("(x: Int) => x").map(_.render),
      Right("Lambda1(x: Int, BoundRef(x))")
    )
    assertEquals(
      ScalametaPatternFrontend.compile("s\"hello $name\"").map(_.render),
      Right("InterpolatedString(s, [\"hello \", \"\"], [Hole($name)])")
    )
    assertEquals(
      ScalametaPatternFrontend.compile("s\"hello $$name\"").map(_.render),
      Right("InterpolatedString(s, [\"hello \", \"\"], [Ident(name)])")
    )
    assertEquals(
      ScalametaPatternFrontend.compile("($x, $x)").map(_.render),
      Right("Tuple([Hole($x), Hole($x)])")
    )

  test("Scalameta construction and matching share the P1 block structure and original captures"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      def render(term: Term) =
        TargetTermView.fromTerm(using q)(term).map(_.render)

      val current = quasiquotes.construct.QuasiquoteBuilder.build(using q)(Seq("{ 1; 2; 3 }"), Nil)
      val candidate = ScalametaTermFrontend.lower(using q)(Seq("{ 1; 2; 3 }"), Nil)
      val pattern = ScalametaPatternFrontend.compile("{ $prefix; $result }")
      val first = Literal(IntConstant(1))
      val result = Literal(IntConstant(2))
      val generated = Block(List(first), result)
      val identities = pattern.flatMap(compiled =>
        TermMatcher.matchTerm(using q)(compiled, generated)
          .left.map(error => ScalametaTermFrontend.Failure.lowering(error.message))
          .map(matched =>
            matched.bindings("prefix").asInstanceOf[AnyRef].eq(first.asInstanceOf[AnyRef]) &&
              matched.bindings("result").asInstanceOf[AnyRef].eq(result.asInstanceOf[AnyRef])
          )
      )

      (
        current.left.map(error => ScalametaTermFrontend.Failure.lowering(error.message)).flatMap(render),
        candidate.flatMap(term => render(term).left.map(error => ScalametaTermFrontend.Failure.lowering(error.message))),
        pattern.map(_.render),
        identities
      )

    val expected = "Block([Literal(1), Literal(2)], Literal(3))"
    assertEquals(evidence._1, Right(expected))
    assertEquals(evidence._2, Right(expected))
    assertEquals(
      evidence._3,
      Right("Block([Hole($prefix)], Hole($result))")
    )
    assertEquals(evidence._4, Right(true))

  test("Scalameta P2 local val lowering and patterns share binder and owner semantics"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val candidate = ScalametaTermFrontend.lower(using q)(Seq("{ val renamed: Int = 7; renamed }"), Nil)
      val pattern = ScalametaPatternFrontend.compile("{ val x: Int = $initializer; x }")
      val matched = for
        term <- candidate
        compiled <- pattern
        result <- TermMatcher.matchTerm(using q)(compiled, term)
          .left.map(error => ScalametaTermFrontend.Failure.lowering(error.message))
      yield
        val initializer = result.bindings("initializer")
        term match
          case Block((definition: ValDef) :: Nil, bound: Ident) =>
            (
              definition.symbol.owner == Symbol.spliceOwner,
              bound.symbol == definition.symbol,
              definition.rhs.exists(_.asInstanceOf[AnyRef].eq(initializer.asInstanceOf[AnyRef]))
            )
          case _ => (false, false, false)

      (
        candidate.flatMap(term => TargetTermView.fromTerm(using q)(term)
          .left.map(error => ScalametaTermFrontend.Failure.lowering(error.message))
          .map(_.render)),
        pattern.map(_.render),
        matched
      )

    assertEquals(
      evidence._1,
      Right("Block([LocalVal(renamed: Int = Literal(7))], BoundRef(renamed))")
    )
    assertEquals(
      evidence._2,
      Right("Block([LocalVal(x: Int = Hole($initializer))], BoundRef(x))")
    )
    assertEquals(evidence._3, Right((true, true, true)))

  test("Scalameta P2 accepts the existing applied declared-type subset"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*
      val initializer = '{ List.empty[Int] }.asTerm
      val candidate = ScalametaTermFrontend.lower(using q)(
        Seq("{ val xs: List[Int] = ", "; xs }"),
        Seq(initializer)
      )
      val pattern = ScalametaPatternFrontend.compile(
        "{ val renamed: List[Int] = $initializer; renamed }"
      )
      (
        candidate.map {
          case Block((definition: ValDef) :: Nil, bound: Ident) =>
            definition.tpt.tpe =:= TypeRepr.of[List[Int]] &&
              definition.rhs.exists(_.asInstanceOf[AnyRef].eq(initializer.asInstanceOf[AnyRef])) &&
              bound.symbol == definition.symbol
          case _ => false
        },
        pattern.map(_.render)
      )

    assertEquals(evidence._1, Right(true))
    assertEquals(
      evidence._2,
      Right("Block([LocalVal(renamed: List[Int] = Hole($initializer))], BoundRef(renamed))")
    )

  test("Scalameta P2 residual local forms fail closed with controlled diagnostics"):
    val rejectedPatterns = List(
      "{ val x = 1; x }",
      "{ var x: Int = 1; x }",
      "{ lazy val x: Int = 1; x }",
      "{ val (x, y) = (1, 2); x }",
      "{ val x: Int = 1; val y: Int = 2; y }",
      "{ def x: Int = 1; x }"
    )
    rejectedPatterns.foreach { source =>
      val failure = ScalametaPatternFrontend.compile(source).swap.toOption
      assert(failure.nonEmpty, source)
      assertEquals(failure.get.category, "SCALAMETA_PATTERN_LOWERING_UNSUPPORTED")
    }

  test("Scalameta construction rejects second P2 binders and P2-Lambda1 source shadowing"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val secondBinderDiagnostic = "Only one P2 local val binder is admitted per quasiquote tree"
    val shadowingDiagnostic = "P2 local val source-binder shadowing is unsupported"
    val cases = List(
      "{ val x: Int = 1; { val y: Int = 2; y } }" -> secondBinderDiagnostic,
      "(x: Int) => { val x: Int = 1; x }" -> shadowingDiagnostic,
      "{ val x: Int = 1; (x: Int) => x }" -> shadowingDiagnostic,
      "{ val x: Int = { val y: Int = 2; y }; x }" -> secondBinderDiagnostic,
      "{ val x: Int = 1; ({ val y: Int = 2; y }) }" -> secondBinderDiagnostic,
      "{ val x: Int = 1; { { val y: Int = 2; y }; x } }" -> secondBinderDiagnostic
    )

    cases.foreach { case (source, expected) =>
      val message = withQuotes {
        val q = summon[scala.quoted.Quotes]
        ScalametaTermFrontend.lower(using q)(Seq(source), Nil).fold(_.detail, _ => "accepted")
      }
      assert(message.contains(expected), s"$source: $message")
    }

  test("Scalameta pattern compilation rejects second P2 binders and P2-Lambda1 source shadowing"):
    val secondBinderDiagnostic = "Only one P2 local val binder is admitted per quasiquote tree"
    val shadowingDiagnostic = "P2 local val source-binder shadowing is unsupported"
    val cases = List(
      "{ val x: Int = 1; { val y: Int = 2; y } }" -> secondBinderDiagnostic,
      "(x: Int) => { val x: Int = 1; x }" -> shadowingDiagnostic,
      "{ val x: Int = 1; (x: Int) => x }" -> shadowingDiagnostic
    )

    cases.foreach { case (source, expected) =>
      val message = ScalametaPatternFrontend.compile(source).fold(_.detail, _ => "accepted")
      assert(message.contains(expected), s"$source: $message")
    }

  test("Scalameta paths retain one P2 binder combined with a distinct-name Lambda1 binder"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val sources = List(
      "(outer: Int) => { val x: Int = 1; x }",
      "{ val x: Int = 1; (inner: Int) => inner }"
    )

    sources.foreach { source =>
      val constructed = withQuotes {
        val q = summon[scala.quoted.Quotes]
        ScalametaTermFrontend.lower(using q)(Seq(source), Nil)
      }
      assert(constructed.isRight, s"$source: $constructed")
      assert(ScalametaPatternFrontend.compile(source).isRight, source)
    }

    val p2ThenLambda = ScalametaPatternFrontend
      .compile("{ val x: Int = 1; (inner: Int) => inner }")
      .toOption
      .get
    p2ThenLambda match
      case TermPattern.Block(
            List(local: BlockPatternStatement.LocalVal),
            TermPattern.Lambda1(lambdaId, _, _, _)
          ) => assertNotEquals(local.binderId, lambdaId)
      case other => fail(s"unexpected Scalameta P2/Lambda1 pattern: $other")

  test("pattern fallback remains callable without changing explicit QuasiPattern semantics"):
    val restricted = HybridPatternFrontend.compile("if true then $value else 0", dialects.Scala213)
    assertEquals(
      restricted.map(_.engine),
      Right(HybridPatternFrontend.Engine.CurrentDottyFallback),
      restricted
    )
    val repeated = quasiquotes.matching.QuasiPattern.term("($x, $x)")
    assert(repeated.isRight)

  test("unpublished selector is explicit, reversible, and reports fallback"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val selected = withQuotes:
      val current = TermQ3FrontendSelector.build(
        TermQ3FrontendSelector.Selection.CurrentDotty,
        Seq("42"),
        Nil
      )
      val candidate = TermQ3FrontendSelector.build(
        TermQ3FrontendSelector.Selection.ScalametaPrimary,
        Seq("42"),
        Nil
      )
      val fallback = TermQ3FrontendSelector.build(
        TermQ3FrontendSelector.Selection.ScalametaPrimary,
        Seq("if true then 1 else 2"),
        Nil,
        dialects.Scala213
      )
      (current.map(_.engine), candidate.map(_.engine), fallback.map(_.engine))
    assertEquals(
      selected,
      (
        Right(TermQ3FrontendSelector.Engine.CurrentDotty),
        Right(TermQ3FrontendSelector.Engine.Scalameta),
        Right(TermQ3FrontendSelector.Engine.CurrentDottyFallback)
      )
    )
    assertEquals(
      TermQ3FrontendSelector.compile(
        TermQ3FrontendSelector.Selection.CurrentDotty,
        "($left, $right)"
      ).map(_.engine),
      Right(TermQ3FrontendSelector.Engine.CurrentDotty)
    )
    assertEquals(
      TermQ3FrontendSelector.compile(
        TermQ3FrontendSelector.Selection.ScalametaPrimary,
        "($left, $right)"
      ).map(_.engine),
      Right(TermQ3FrontendSelector.Engine.Scalameta)
    )
