package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import quasiquotes.parser.{BinderId, BlockStatement, TermShape, TypeShape}
import quasiquotes.terms.{TermParameterSpec, TermShapeBindings}
import quasiquotes.types.TypeNormalForm

final class TermGeneratedOriginLoweringTest extends munit.FunSuite:
  private val intType = TypeNormalForm.STypeIdent("Int")
  private def ident(name: String): TermShape = TermShape.Identifier(name, false)
  private def lit(value: String): TermShape = TermShape.Literal(value)

  test("direct semantic families have deterministic fresh exact planned origins"):
    withContext:
      val fixtures = List(
        lit("42"), lit("true"), lit("\"λ😀\n\\\""), ident("value"), ident("type"),
        TermShape.Select(ident("service"), "type"),
        TermShape.Apply(ident("f"), Nil),
        TermShape.Apply(ident("f"), List(lit("1"), ident("x"))),
        TermShape.Infix(lit("1"), "+", lit("2")), TermShape.Unary("-", lit("-1")),
        TermShape.Tuple(List(ident("x"), lit("2"))),
        TermShape.If(ident("ready"), lit("1"), lit("2")),
        TermShape.InterpolatedString("s", List("λ😀=", "!"), List(ident("x"))),
        TermShape.InterpolatedString("s", List("", ""), List(TermShape.Infix(lit("1"), "+", lit("2")))),
        TermShape.New("java.lang.StringBuilder", List(lit("16"))),
        TermShape.Typed(lit("1"), "Int"),
        TermShape.Parenthesized(ident("x")),
        TermShape.Block(List(ident("first")), ident("last")),
        TermShape.Apply(TermShape.Parenthesized(TermShape.Apply(ident("f"), Nil)), Nil),
        TermShape.Select(TermShape.Parenthesized(lit("-1")), "abs"),
        TermShape.Select(TermShape.If(ident("ready"), ident("a"), ident("b")), "member")
      )
      fixtures.foreach(checkSuccess)
      val infix = lower(TermShape.Infix(lit("1"), "+", lit("2")))
      assertEquals(infix.tree.span.point, 2)

  test("public binders preserve declaration references spelling and alpha topology"):
    withContext:
      val lambda = identityLambda("x")
      val renamed = identityLambda("renamed")
      val value = TermShapeBindings.localValue("x", intType, lit("1")) { scope =>
        scope.reference(scope.declaredBinder.get)
      }.fold(p => fail(p.message), identity)
      val method = TermShapeBindings.localMethod("id", Vector(Vector(TermParameterSpec("x", intType))), intType) { scope =>
        scope.reference(scope.parameterBinders.head.head)
      } { scope => scope.reference(scope.declaredBinder.get) }.fold(p => fail(p.message), identity)
      val sameSpelling = TermShapeBindings.lambda(Vector(TermParameterSpec("x", intType))) { scope =>
        scope.reference(scope.parameterBinders.head.head).map(bound => TermShape.Tuple(List(bound, ident("x"))))
      }.fold(p => fail(p.message), identity)
      List(lambda, renamed, value, method, sameSpelling, TermShape.Parenthesized(method)).foreach(checkSuccess)
      assertEquals(alphaIdentity(lower(lambda).tree), alphaIdentity(lower(renamed).tree))
      assert(lower(renamed).generatedSource.contains("renamed"))
      assertNotEquals(lower(lambda).generatedSource, lower(renamed).generatedSource)
      var escaped: TermShape = null
      TermShapeBindings.lambda(Vector(TermParameterSpec("x", intType))) { scope =>
        scope.reference(scope.parameterBinders.head.head).map { ref => escaped = ref; ref }
      }
      assertEquals(failure(escaped).code, "MALFORMED_SEMANTIC_VALUE")

  test("source name roles and signed receiver exclusion leave source-free successes intact"):
    withContext:
      val names = List("_", "a.b", "x$", "+", "λ", "has space", "`type`")
      val excluded = names.flatMap(name => List(ident(name), TermShape.Select(ident("x"), name))) ++
        List(TermShape.New("foo.type", Nil), TermShape.New("foo._", Nil), TermShape.New("_.Foo", Nil),
          TermShape.Select(lit("-1"), "abs"), TermShape.Select(lit("-0"), "abs"))
      excluded.foreach { term =>
        assert(TermUntypedLowering.lower(term).isRight, clues(term))
        assertEquals(failure(term).code, "UNSUPPORTED_SEMANTIC_VALUE", clues(term))
      }
      checkSuccess(internalIdentity("type"))
      assertEquals(failure(internalIdentity("_")).code, "UNSUPPORTED_SEMANTIC_VALUE")
      assert(failure(TermShape.Select(ident("bad.name"), "bad.member")).detail.contains("selected member"))
      assert(failure(TermShape.New("_.type", Nil)).detail.contains("segment 0"))

  test("path preflight precedes semantics and all shared exact stages precede source admission"):
    withContext:
      assertEquals(failure(null, null).detail, "the semantic TermShape must be present.")
      assertEquals(failure(ident(null), null).detail, "the virtual source name must be present.")
      List("", " ", " leading", "trailing ", "a\u0000b", "a\rb", "a\nb").foreach { path =>
        assertEquals(failure(ident(null), path).code, "INVALID_VIRTUAL_SOURCE")
      }
      assertEquals(lower(ident("x"), "virtual path/term").virtualSourceName, "virtual path/term")
      val exact = TermShape.Tuple(List(ident("bad.name"), lit("3.14")))
      assertEquals(failure(exact).code, "EXACT_LOWERING_FAILED")
      assertEquals(failure(exact).detail, TermUntypedLowering.lower(exact).swap.toOption.get.detail)
      val completion = TermShape.Tuple(List(ident("bad.name"), TermShape.Typed(ident("x"), "Option[Int]")))
      assertEquals(failure(completion).detail, TermUntypedLowering.lower(completion).swap.toOption.get.detail)
      List(ident(null), TermShape.Parenthesized(null), TermShape.Apply(ident("f"), null),
        TermShape.Tuple(List(ident("x"), null)), TermShape.Select(null, "x")).foreach { term =>
        assertEquals(failure(term).code, "MALFORMED_SEMANTIC_VALUE")
        assertEquals(failure(term).detail, TermUntypedLowering.lower(term).swap.toOption.get.detail)
      }
      assertEquals(failure(TermShape.Apply(TermShape.Apply(ident("f"), Nil), Nil)).code, "UNSUPPORTED_SEMANTIC_VALUE")

  test("the completed and raw pair is preserved modulo exactly planned grouping shells"):
    withContext:
      val semantic = TermShape.Select(TermShape.If(ident("ready"), ident("a"), ident("b")), "member")
      val checked = CheckedTermUntypedLowering.lower(semantic).fold(p => fail(p.message), identity)
      val fragment = GeneratedOriginFragmentSupport.planTerm(checked.completed).fold(p => fail(p.message), identity)
      val source = dotty.tools.dotc.util.SourceFile.virtual("checked.scala", fragment.source)
      val positioned = GeneratedOriginFragmentSupport.positionTerm(checked.raw, fragment, source, 0).fold(p => fail(p.message), identity)
      def verify(tree: untpd.Tree) = GeneratedOriginFragmentSupport.validatePositionedTermAgainstCheckedRaw(checked.raw, tree, fragment)
      assert(verify(positioned).isRight)
      val selected = positioned.asInstanceOf[untpd.Select]
      val wrongName = untpd.cpy.Select(selected)(selected.qualifier, dotty.tools.dotc.core.Names.termName("changed"))
      assert(verify(wrongName).isLeft)
      val wrongPoint = positioned.cloneIn(source).withSpan(dotty.tools.dotc.util.Spans.Span(0, fragment.source.length, 0))
      assert(verify(wrongPoint).isLeft)
      assert(!checked.raw.source.exists)
      assert(!checked.raw.span.exists)

  test("binder source roles precede child roles and diagnostic reference spelling is not emission authority"):
    withContext:
      val malformedName = TermShape.Lambda1(BinderId(0), "_", "Int", ident("bad.child"))
      assert(TermUntypedLowering.lower(malformedName).isRight)
      assert(failure(malformedName).detail.contains("lambda parameter"))
      val diagnosticReference = TermShape.Lambda1(BinderId(0), "x", "Int", TermShape.BoundReference(BinderId(0), "bad.diagnostic"))
      checkSuccess(diagnosticReference)
      val local = TermShape.Block(List(BlockStatement.LocalDef(BinderId(0), "_", BinderId(1), "_",
        TypeShape.Identifier("Int"), TypeShape.Identifier("Int"), ident("bad.body"))), lit("0"))
      assert(TermUntypedLowering.lower(local).isRight)
      assert(failure(local).detail.contains("local method name"))
      val value = TermShape.Block(List(BlockStatement.LocalVal(BinderId(0), "_", "Int", ident("bad.initializer"))), lit("0"))
      assert(TermUntypedLowering.lower(value).isRight)
      assert(failure(value).detail.contains("local value"))

  test("impossible private result carriers and origin errors map structurally to stable categories"):
    val contain = CheckedTermUntypedLowering.getClass.getDeclaredMethods
      .find(method => method.getName == "contain" && method.getParameterCount == 2).getOrElse(fail("private checked containment missing"))
    contain.setAccessible(true)
    val classifier: TermUntypedLowering.Failure => TermUntypedLowering.Failure = identity
    List[Either[TermUntypedLowering.Failure, String]](null, Left(null), Right(null)).foreach { value =>
      val result = contain.invoke(CheckedTermUntypedLowering, value, classifier)
        .asInstanceOf[Either[TermUntypedLowering.Failure, String]]
      assertEquals(result.swap.toOption.get.code, "INTERNAL_INVARIANT_FAILED")
    }
    import ConstructedTermGeneratedOriginError.*
    val classify = TermGeneratedOriginLowering.getClass.getDeclaredMethods
      .find(method => method.getName == "classifyOriginFailure" && method.getParameterCount == 1).getOrElse(fail("private origin classifier missing"))
    classify.setAccessible(true)
    val cases = List[(ConstructedTermGeneratedOriginError, String)](
      UnrenderableName("identifier", "bad.name") -> "UNSUPPORTED_SEMANTIC_VALUE",
      MalformedBinderScope("broken") -> "MALFORMED_SEMANTIC_VALUE",
      UnsupportedLiteral("3.14") -> "EXACT_LOWERING_FAILED",
      IncompletePositionMap("broken") -> "GENERATED_ORIGIN_FAILED",
      MissingConstructedTerm -> "INTERNAL_INVARIANT_FAILED",
      RawLoweringFailure("impossible second lowering") -> "INTERNAL_INVARIANT_FAILED",
      (null, "INTERNAL_INVARIANT_FAILED")
    )
    cases.foreach { (problem, expected) =>
      val result = classify.invoke(TermGeneratedOriginLowering, problem).asInstanceOf[TermGeneratedOriginLowering.Failure]
      assertEquals(result.code, expected)
    }

  private def internalIdentity(name: String): TermShape =
    TermShape.Lambda1(BinderId(0), name, "Int", TermShape.BoundReference(BinderId(0), name))

  private def checkSuccess(term: TermShape)(using Context): Unit =
    val first = lower(term)
    val second = lower(term)
    assert(!(first eq second))
    assert(!(first.sourceFile eq second.sourceFile))
    assertEquals(first.generatedSource, second.generatedSource)
    assertEquals(first.virtualSourceName, "generated/Term.scala")
    assertEquals(first.sourceFile.content.mkString, first.generatedSource)
    assertEquals(first.tree.span.start, 0)
    assertEquals(first.tree.span.end, first.generatedSource.length)
    val nodes = GeneratedOriginFragmentSupport.allTrees(first.tree)
    val others = GeneratedOriginFragmentSupport.allTrees(second.tree)
    assertEquals(nodes.size, others.size)
    nodes.zip(others).foreach { (node, other) =>
      assert(!(node eq other))
      assert(node.source eq first.sourceFile)
      assertEquals(node.span, other.span)
      assertEquals(node.symbol, NoSymbol)
      assert(!node.isInstanceOf[untpd.TypedSplice])
    }

  private def identityLambda(name: String): TermShape =
    TermShapeBindings.lambda(Vector(TermParameterSpec(name, intType))) { scope =>
      scope.reference(scope.parameterBinders.head.head)
    }.fold(p => fail(p.message), identity)

  private def alphaIdentity(tree: untpd.Tree)(using Context): String = tree match
    case untpd.Function(List(parameter: untpd.ValDef), untpd.Ident(name)) if name == parameter.name => "lambda($0)->$0"
    case _ => fail("identity binder association lost")

  private def lower(term: TermShape, path: String = "generated/Term.scala")(using Context) =
    TermGeneratedOriginLowering.lower(term, path).fold(p => fail(p.message), identity)
  private def failure(term: TermShape, path: String = "generated/Term.scala")(using Context) =
    TermGeneratedOriginLowering.lower(term, path).swap.toOption.getOrElse(fail("unexpected success"))
  private def withContext[A](run: Context ?=> A): A = run(using (new ContextBase).initialCtx)
