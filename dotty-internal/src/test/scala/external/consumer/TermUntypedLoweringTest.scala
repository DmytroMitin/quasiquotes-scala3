package external.consumer

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.parser.TermShape
import quasiquotes.terms.{TermParameterSpec, TermShapeBindings}
import quasiquotes.terms.dotty.TermUntypedLowering
import quasiquotes.types.TypeNormalForm

final class TermUntypedLoweringTest extends munit.FunSuite:
  private val intType = TypeNormalForm.STypeIdent("Int")

  test("external consumer lowers direct and completed-only semantic Term values"):
    withContext:
      val fixtures = List(
        TermShape.Literal("42") -> "Number(42)",
        TermShape.Select(ident("service"), "answer") ->
          "Select(Ident(service),answer)",
        TermShape.Apply(ident("f"), List(TermShape.Literal("1"))) ->
          "Apply(Ident(f),[Number(1)])",
        TermShape.Infix(TermShape.Literal("1"), "+", TermShape.Literal("2")) ->
          "Infix(Number(1),Ident(+),Number(2))",
        TermShape.Unary("!", ident("ready")) ->
          "Prefix(Ident(!),Ident(ready))",
        TermShape.Tuple(List(ident("left"), TermShape.Literal("2"))) ->
          "Tuple([Ident(left),Number(2)])",
        TermShape.If(ident("ready"), TermShape.Literal("1"), TermShape.Literal("2")) ->
          "If(Ident(ready),Number(1),Number(2))",
        TermShape.InterpolatedString("s", List("value=", ""), List(ident("value"))) ->
          "Interpolation(s,[Thicket([Literal(value=),Ident(value)]),Literal()])",
        TermShape.New("java.lang.StringBuilder", List(TermShape.Literal("16"))) ->
          "Apply(Select(New(Select(Select(Ident(java),lang),StringBuilder)),<init>),[Number(16)])",
        TermShape.Block(List(ident("first")), ident("second")) ->
          "Block([Ident(first)],Ident(second))",
        TermShape.Typed(TermShape.Literal("1"), "Int") ->
          "Typed(Number(1),Ident(Int))",
        TermShape.Parenthesized(ident("wrapped")) ->
          "Parens(Ident(wrapped))",
        TermShape.Apply(
          TermShape.Select(ident("service"), "choose"),
          List(
            TermShape.Parenthesized(
              TermShape.If(ident("ready"), TermShape.Literal("1"), TermShape.Literal("2"))
            )
          )
        ) ->
          "Apply(Select(Ident(service),choose),[Parens(If(Ident(ready),Number(1),Number(2)))])"
      )

      fixtures.foreach { case (semantic, expected) =>
        val raw = lower(semantic)
        assertEquals(topology(raw), expected, clues(semantic))
        assertSourceFree(raw)
      }

  test("external consumer lowers binder-safe Lambda1 P2 and P3 through one facade"):
    withContext:
      val lambda = TermShapeBindings
        .lambda(Vector(TermParameterSpec("x", intType))) { scope =>
          scope.reference(scope.parameterBinders.head.head)
        }
        .fold(problem => fail(problem.message), identity)
      assertEquals(
        topology(lower(lambda)),
        "Function([ValDef(x,Ident(Int),Empty)],Ident(x))"
      )

      val localValue = TermShapeBindings
        .localValue("x", intType, TermShape.Literal("1")) { scope =>
          scope.reference(scope.declaredBinder.get)
        }
        .fold(problem => fail(problem.message), identity)
      assertEquals(
        topology(lower(localValue)),
        "Block([ValDef(x,Ident(Int),Number(1))],Ident(x))"
      )

      val localMethod = TermShapeBindings
        .localMethod(
          "id",
          Vector(Vector(TermParameterSpec("x", intType))),
          intType
        ) { scope =>
          scope.reference(scope.parameterBinders.head.head)
        } { scope =>
          scope.reference(scope.declaredBinder.get)
        }
        .fold(problem => fail(problem.message), identity)
      assertEquals(
        topology(lower(localMethod)),
        "Block([DefDef(id,[[ValDef(x,Ident(Int),Empty)]],Ident(Int),Ident(x))],Ident(id))"
      )

      List(lambda, localValue, localMethod).foreach(value => assertSourceFree(lower(value)))

  test("bound and free same-spelling identifiers remain distinct semantic inputs"):
    withContext:
      val semantic = TermShapeBindings
        .lambda(Vector(TermParameterSpec("x", intType))) { scope =>
          scope
            .reference(scope.parameterBinders.head.head)
            .map(bound => TermShape.Tuple(List(bound, ident("x"))))
        }
        .fold(problem => fail(problem.message), identity)

      assertEquals(
        topology(lower(semantic)),
        "Function([ValDef(x,Ident(Int),Empty)],Tuple([Ident(x),Ident(x)]))"
      )

  test("alpha-equivalent public binder graphs lower to equivalent binder topology"):
    withContext:
      val first = publicIdentity("left")
      val second = publicIdentity("right")

      assertEquals(alphaTopology(lower(first)), "Function([$0],$0)")
      assertEquals(alphaTopology(lower(second)), "Function([$0],$0)")

  test("each facade call returns a wholly fresh raw graph"):
    withContext:
      val semantic = TermShape.Apply(
        TermShape.Select(ident("service"), "answer"),
        List(TermShape.Parenthesized(TermShape.Typed(TermShape.Literal("1"), "Int")))
      )
      val first = allTrees(lower(semantic))
      val second = allTrees(lower(semantic))

      assertEquals(first.size, second.size)
      first.zip(second).foreach { case (left, right) =>
        assert(!(left eq right), clues(left, right))
      }

  test("foreign escaped binder references cannot be smuggled through the facade"):
    withContext:
      var escaped: TermShape = null
      val sourceGraph = TermShapeBindings
        .lambda(Vector(TermParameterSpec("x", intType))) { scope =>
          scope.reference(scope.parameterBinders.head.head).map { reference =>
            escaped = reference
            reference
          }
        }
      assert(sourceGraph.isRight)

      val failure = lowerFailure(TermShape.Apply(ident("consume"), List(escaped)))
      assertEquals(failure.code, "MALFORMED_SEMANTIC_VALUE")

  test("external consumer receives stable missing malformed unsupported and exact-stage codes"):
    withContext:
      assertEquals(lowerFailure(null).code, "MISSING_INPUT")

      val malformed = List[TermShape](
        TermShape.Identifier(null, isPlaceholder = false),
        TermShape.Literal(null),
        TermShape.Select(null, "member"),
        TermShape.Select(ident("value"), null),
        TermShape.Apply(null, Nil),
        TermShape.Apply(ident("f"), null),
        TermShape.New("java.lang.StringBuilder", null),
        TermShape.Infix(null, "+", TermShape.Literal("1")),
        TermShape.Unary("!", null),
        TermShape.InterpolatedString(null, List(""), Nil),
        TermShape.InterpolatedString("s", List(null), Nil),
        TermShape.Typed(null, "Int"),
        TermShape.Typed(ident("value"), null),
        TermShape.Tuple(null),
        TermShape.Tuple(List(ident("value"), null)),
        TermShape.If(null, TermShape.Literal("1"), TermShape.Literal("2")),
        TermShape.Block(List(null), ident("result")),
        TermShape.Parenthesized(null)
      )
      malformed.foreach { semantic =>
        val failure = lowerFailure(semantic)
        assertEquals(failure.code, "MALFORMED_SEMANTIC_VALUE", clues(semantic, failure))
      }

      val unsupported = List[TermShape](
        TermShape.Unsupported("future", "not admitted"),
        TermShape.Unary("await", ident("value")),
        TermShape.Tuple(List(ident("only"))),
        TermShape.Typed(ident("value"), "Option[Int]"),
        TermShape.Apply(TermShape.Apply(ident("f"), List(ident("x"))), List(ident("y")))
      )
      unsupported.foreach { semantic =>
        val failure = lowerFailure(semantic)
        assertEquals(failure.code, "UNSUPPORTED_SEMANTIC_VALUE", clues(semantic, failure))
      }

      val exact = lowerFailure(TermShape.Literal("3.14"))
      assertEquals(exact.code, "EXACT_LOWERING_FAILED")
      assert(exact.detail.contains("Unsupported constructed-term literal"), clues(exact))

  private def publicIdentity(name: String): TermShape =
    TermShapeBindings
      .lambda(Vector(TermParameterSpec(name, intType))) { scope =>
        scope.reference(scope.parameterBinders.head.head)
      }
      .fold(problem => fail(problem.message), identity)

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def lower(term: TermShape)(using Context): untpd.Tree =
    TermUntypedLowering
      .lower(term)
      .fold(problem => fail(problem.message), identity)

  private def lowerFailure(
      term: TermShape
  )(using Context): TermUntypedLowering.Failure =
    TermUntypedLowering
      .lower(term)
      .left
      .toOption
      .getOrElse(fail(s"semantic Term unexpectedly lowered: $term"))

  private def assertSourceFree(tree: untpd.Tree)(using Context): Unit =
    allTrees(tree).foreach { node =>
      assert(!node.source.exists, clues(node))
      assert(!node.span.exists, clues(node))
      assertEquals(node.symbol, NoSymbol, clues(node))
      assert(!node.isInstanceOf[untpd.TypedSplice], clues(node))
    }

  private def alphaTopology(tree: untpd.Tree)(using Context): String =
    tree match
      case untpd.Function(List(parameter: untpd.ValDef), untpd.Ident(reference))
          if parameter.name == reference =>
        "Function([$0],$0)"
      case other => s"Unexpected(${topology(other)})"

  private def topology(tree: untpd.Tree)(using Context): String =
    tree match
      case untpd.EmptyTree => "Empty"
      case untpd.Ident(name) => s"Ident($name)"
      case untpd.Number(value, untpd.NumberKind.Whole(10)) => s"Number($value)"
      case untpd.Literal(constant) => s"Literal(${constant.value})"
      case value: untpd.Select => s"Select(${topology(value.qualifier)},${value.name})"
      case value: untpd.New => s"New(${topology(value.tpt)})"
      case value: untpd.Apply =>
        s"Apply(${topology(value.fun)},[${value.args.map(topology).mkString(",")}])"
      case value: untpd.InfixOp =>
        s"Infix(${topology(value.left)},${topology(value.op)},${topology(value.right)})"
      case value: untpd.PrefixOp => s"Prefix(${topology(value.op)},${topology(value.od)})"
      case untpd.InterpolatedString(prefix, segments) =>
        s"Interpolation($prefix,[${segments.map(topology).mkString(",")}])"
      case value: untpd.Thicket => s"Thicket([${value.trees.map(topology).mkString(",")}])"
      case value: untpd.Typed => s"Typed(${topology(value.expr)},${topology(value.tpt)})"
      case value: untpd.Tuple => s"Tuple([${value.trees.map(topology).mkString(",")}])"
      case value: untpd.If =>
        s"If(${topology(value.cond)},${topology(value.thenp)},${topology(value.elsep)})"
      case value: untpd.Block =>
        s"Block([${value.stats.map(topology).mkString(",")}],${topology(value.expr)})"
      case value: untpd.Function =>
        s"Function([${value.args.map(topology).mkString(",")}],${topology(value.body)})"
      case value: untpd.ValDef =>
        s"ValDef(${value.name},${topology(value.tpt)},${topology(value.rhs)})"
      case value: untpd.DefDef =>
        s"DefDef(${value.name},[${value.paramss.map(row => s"[${row.map(topology).mkString(",")}]").mkString(",")}],${topology(value.tpt)},${topology(value.rhs)})"
      case value: untpd.Parens => s"Parens(${topology(value.t)})"
      case other => s"Unexpected(${other.getClass.getName})"

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree +: (tree match
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector.flatMap(allTrees) ++
          allTrees(value.tpt) ++ allTrees(value.rhs)
      case value: untpd.ValDef =>
        Vector(value.tpt, value.rhs).filterNot(_.isEmpty).flatMap(allTrees)
      case value: untpd.Select => allTrees(value.qualifier)
      case value: untpd.Apply => allTrees(value.fun) ++ value.args.toVector.flatMap(allTrees)
      case value: untpd.New => allTrees(value.tpt)
      case value: untpd.InfixOp => allTrees(value.left) ++ allTrees(value.op) ++ allTrees(value.right)
      case value: untpd.PrefixOp => allTrees(value.op) ++ allTrees(value.od)
      case value: untpd.InterpolatedString => value.segments.toVector.flatMap(allTrees)
      case value: untpd.Thicket => value.trees.toVector.flatMap(allTrees)
      case value: untpd.Block => value.stats.toVector.flatMap(allTrees) ++ allTrees(value.expr)
      case value: untpd.Typed => allTrees(value.expr) ++ allTrees(value.tpt)
      case value: untpd.Tuple => value.trees.toVector.flatMap(allTrees)
      case value: untpd.Function => value.args.toVector.flatMap(allTrees) ++ allTrees(value.body)
      case value: untpd.If => allTrees(value.cond) ++ allTrees(value.thenp) ++ allTrees(value.elsep)
      case value: untpd.Parens => allTrees(value.t)
      case _ => Vector.empty)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
