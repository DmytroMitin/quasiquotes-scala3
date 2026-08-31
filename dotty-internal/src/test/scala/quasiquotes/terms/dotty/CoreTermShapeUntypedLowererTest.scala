package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.neutral.ScalametaTermProjection
import quasiquotes.parser.{BinderId, TermShape, TermShapeInspector, TinyTermParser}

import scala.meta.*

class CoreTermShapeUntypedLowererTest extends munit.FunSuite:
  import CoreTermShapeUntypedLowererError.*

  private val canonicalIntegers = Vector(
    "0",
    "1",
    "-1",
    "42",
    "-987654321"
  )

  canonicalIntegers.foreach { value =>
    test(s"lowers canonical semantic integer without parsing source: $value") {
      withContext {
        val raw = lowerOrFail(TermShape.Literal(value))

        assertEquals(
          TermShapeInspector.rawStructure(raw),
          s"Number($value,Whole(10))"
        )
      }
    }
  }

  private val malformedIntegers = Vector(
    "",
    "+1",
    " 1",
    "1 ",
    "1.0",
    "1e2",
    "1L",
    "value",
    "00",
    "01",
    "-0",
    "-01"
  )

  malformedIntegers.foreach { value =>
    test(s"rejects noncanonical semantic integer text: ${value.replace(" ", "<space>")}") {
      withContext {
        assertEquals(
          CoreTermShapeUntypedLowerer.lower(TermShape.Literal(value)),
          Left(InvalidIntegerLiteral(value))
        )
      }
    }
  }

  test("rejects a null semantic integer value") {
    withContext {
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(TermShape.Literal(null)),
        Left(InvalidIntegerLiteral(null))
      )
    }
  }

  private val admittedOperators = Vector(
    "+",
    "-",
    "*",
    "/",
    "%",
    "==",
    "!=",
    "<",
    "<=",
    ">",
    ">="
  )

  admittedOperators.foreach { operator =>
    test(s"admits the bounded ordinary infix operator: $operator") {
      withContext {
        val raw = lowerOrFail(
          TermShape.Infix(
            TermShape.Literal("1"),
            operator,
            TermShape.Literal("2")
          )
        )

        raw match
          case untpd.InfixOp(_, untpd.Ident(actual), _) =>
            assertEquals(actual.toString, operator)
          case other =>
            fail(s"expected InfixOp, found ${other.getClass.getSimpleName}")
      }
    }
  }

  private val rejectedOperators = Vector("", "plus", "++", "&&", "/*", " +")

  rejectedOperators.foreach { operator =>
    test(s"rejects an operator outside the bounded exact-backend set: $operator") {
      withContext {
        assertEquals(
          CoreTermShapeUntypedLowerer.lower(
            TermShape.Infix(
              TermShape.Literal("1"),
              operator,
              TermShape.Literal("2")
            )
          ),
          Left(InvalidInfixOperator(operator))
        )
      }
    }
  }

  test("rejects a null infix operator") {
    withContext {
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(
          TermShape.Infix(
            TermShape.Literal("1"),
            null,
            TermShape.Literal("2")
          )
        ),
        Left(InvalidInfixOperator(null))
      )
    }
  }

  private val invalidSourceNames = Vector(
    "_",
    "if",
    "bad.name",
    "$hole",
    "<init>",
    "café"
  )

  invalidSourceNames.foreach { name =>
    test(s"rejects an invalid exact-backend Identifier name: $name") {
      withContext {
        assertEquals(
          CoreTermShapeUntypedLowerer.lower(
            TermShape.Identifier(name, isPlaceholder = false)
          ),
          Left(InvalidIdentifierName(name))
        )
      }
    }

    test(s"rejects an invalid exact-backend selected-member name: $name") {
      withContext {
        assertEquals(
          CoreTermShapeUntypedLowerer.lower(
            TermShape.Select(
              TermShape.Identifier("obj", isPlaceholder = false),
              name
            )
          ),
          Left(InvalidSelectedName(name))
        )
      }
    }
  }

  test("rejects null exact-backend Identifier and selected-member names") {
    withContext {
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(
          TermShape.Identifier(null, isPlaceholder = false)
        ),
        Left(InvalidIdentifierName(null))
      )
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(
          TermShape.Select(
            TermShape.Identifier("obj", isPlaceholder = false),
            null
          )
        ),
        Left(InvalidSelectedName(null))
      )
    }
  }

  test("rejects placeholder-marked Identifiers before raw construction") {
    withContext {
      val placeholders = List("value", "$hole", null)
      placeholders.foreach { name =>
        assertEquals(
          CoreTermShapeUntypedLowerer.lower(
            TermShape.Identifier(name, isPlaceholder = true)
          ),
          Left(PlaceholderIdentifier(name))
        )
      }
    }
  }

  test("admits valid exact-backend ASCII source names") {
    withContext {
      List("value", "_private", "A1").foreach { name =>
        assertEquals(
          TermShapeInspector.rawStructure(
            lowerOrFail(TermShape.Identifier(name, isPlaceholder = false))
          ),
          s"Ident($name)"
        )
      }
    }
  }

  test("rejects a direct nested Apply in function position as multiple application lists") {
    withContext {
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(
          TermShape.Apply(
            TermShape.Apply(
              TermShape.Identifier("f", isPlaceholder = false),
              TermShape.Literal("1") :: Nil
            ),
            TermShape.Literal("2") :: Nil
          )
        ),
        Left(MultipleApplicationLists)
      )
    }
  }

  test("allows Apply in argument and qualifier positions without adding another direct list") {
    val fixtures = Vector(
      "f(g(1), 2)" ->
        TermShape.Apply(
          TermShape.Identifier("f", isPlaceholder = false),
          List(
            TermShape.Apply(
              TermShape.Identifier("g", isPlaceholder = false),
              TermShape.Literal("1") :: Nil
            ),
            TermShape.Literal("2")
          )
        ),
      "f(1).g" ->
        TermShape.Select(
          TermShape.Apply(
            TermShape.Identifier("f", isPlaceholder = false),
            TermShape.Literal("1") :: Nil
          ),
          "g"
        ),
      "f(1).g(2)" ->
        TermShape.Apply(
          TermShape.Select(
            TermShape.Apply(
              TermShape.Identifier("f", isPlaceholder = false),
              TermShape.Literal("1") :: Nil
            ),
            "g"
          ),
          TermShape.Literal("2") :: Nil
        )
    )

    withContext {
      fixtures.foreach { (source, shape) =>
        assertEquals(
          TermShapeInspector.rawStructure(lowerOrFail(shape)),
          TinyTermParser.parseOrThrow(source).rawStructure,
          clues(source)
        )
      }
    }
  }

  test("rejects a missing ordinary Apply argument list") {
    withContext {
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(
          TermShape.Apply(
            TermShape.Identifier("f", isPlaceholder = false),
            null
          )
        ),
        Left(MissingApplyArguments)
      )
    }
  }

  test("fails closed when a recursively lowered call child is unsupported") {
    withContext {
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(
          TermShape.Apply(
            TermShape.Identifier("f", isPlaceholder = false),
            TermShape.New("Value", Nil) :: Nil
          )
        ),
        Left(UnsupportedTermShape("New"))
      )
    }
  }

  test("matches independent parser raw structure after ignoring parser metadata") {
    val fixtures = Vector(
      "1" -> TermShape.Literal("1"),
      "-1" -> TermShape.Literal("-1"),
      "1 + 1" ->
        TermShape.Infix(
          TermShape.Literal("1"),
          "+",
          TermShape.Literal("1")
        ),
      "1 + 2 * 3" ->
        TermShape.Infix(
          TermShape.Literal("1"),
          "+",
          TermShape.Infix(
            TermShape.Literal("2"),
            "*",
            TermShape.Literal("3")
          )
        ),
      "1 - 2" ->
        TermShape.Infix(
          TermShape.Literal("1"),
          "-",
          TermShape.Literal("2")
        ),
      "obj" -> TermShape.Identifier("obj", isPlaceholder = false),
      "obj.f" ->
        TermShape.Select(
          TermShape.Identifier("obj", isPlaceholder = false),
          "f"
        ),
      "f()" ->
        TermShape.Apply(TermShape.Identifier("f", isPlaceholder = false), Nil),
      "f(1)" ->
        TermShape.Apply(
          TermShape.Identifier("f", isPlaceholder = false),
          TermShape.Literal("1") :: Nil
        ),
      "obj.f(1 + 2, 3)" ->
        TermShape.Apply(
          TermShape.Select(
            TermShape.Identifier("obj", isPlaceholder = false),
            "f"
          ),
          List(
            TermShape.Infix(
              TermShape.Literal("1"),
              "+",
              TermShape.Literal("2")
            ),
            TermShape.Literal("3")
          )
        ),
      "f(g(1), 2)" ->
        TermShape.Apply(
          TermShape.Identifier("f", isPlaceholder = false),
          List(
            TermShape.Apply(
              TermShape.Identifier("g", isPlaceholder = false),
              TermShape.Literal("1") :: Nil
            ),
            TermShape.Literal("2")
          )
        )
    )

    withContext {
      fixtures.foreach { case (source, shape) =>
        val parserOracle = TinyTermParser.parseOrThrow(source)
        val raw = lowerOrFail(shape)

        assertEquals(
          TermShapeInspector.rawStructure(raw),
          parserOracle.rawStructure,
          clues(source)
        )
      }
    }
  }

  test("constructs only recursively source-free span-free symbol-free raw nodes without TypedSplice") {
    withContext {
      val raw = lowerOrFail(
        TermShape.Apply(
          TermShape.Select(
            TermShape.Apply(
              TermShape.Identifier("f", isPlaceholder = false),
              TermShape.Literal("1") :: Nil
            ),
            "g"
          ),
          List(
            TermShape.Infix(
              TermShape.Literal("-1"),
              "+",
              TermShape.Literal("2")
            ),
            TermShape.Apply(
              TermShape.Identifier("h", isPlaceholder = false),
              TermShape.Literal("3") :: Nil
            )
          )
        )
      )

      allTrees(raw).foreach { tree =>
        assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
        assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
        assertEquals(tree.symbol, NoSymbol, clues(tree.getClass.getSimpleName))
        assert(!tree.isInstanceOf[untpd.TypedSplice])
      }
    }
  }

  test("fails closed for every core TermShape family outside the bounded call backend") {
    val binder = BinderId(0)
    val literal = TermShape.Literal("1")
    val unsupported = Vector(
      TermShape.BoundReference(binder, "value") -> "BoundReference",
      TermShape.Lambda1(binder, "value", "Int", literal) -> "Lambda1",
      TermShape.New("Value", Nil) -> "New",
      TermShape.Unary("-", literal) -> "Unary",
      TermShape.InterpolatedString("s", List("", ""), List(literal)) ->
        "InterpolatedString",
      TermShape.Typed(literal, "Int") -> "Typed",
      TermShape.Tuple(List(literal)) -> "Tuple",
      TermShape.If(literal, literal, literal) -> "If",
      TermShape.Block(List(literal), literal) -> "Block",
      TermShape.Parenthesized(literal) -> "Parenthesized",
      TermShape.Unsupported("Hostile", "detail") -> "Unsupported"
    )

    withContext {
      unsupported.foreach { case (shape, nodeKind) =>
        assertEquals(
          CoreTermShapeUntypedLowerer.lower(shape),
          Left(UnsupportedTermShape(nodeKind)),
          clues(shape)
        )
      }
    }
  }

  test("fails closed for a missing core shape") {
    withContext {
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(null.asInstanceOf[TermShape]),
        Left(MissingTermShape)
      )
    }
  }

  test("composes the production Scalameta projector with the exact lowerer") {
    val fixtures = Vector(
      q"1 + 1" ->
        "InfixOp(Number(1,Whole(10)),Ident(+),Number(1,Whole(10)))",
      q"1 + 2 * 3" ->
        "InfixOp(Number(1,Whole(10)),Ident(+),InfixOp(Number(2,Whole(10)),Ident(*),Number(3,Whole(10))))",
      q"-1 + 2" ->
        "InfixOp(Number(-1,Whole(10)),Ident(+),Number(2,Whole(10)))",
      q"obj" -> "Ident(obj)",
      q"obj.f" -> "Select(Ident(obj), f)",
      q"f()" -> "Apply(Ident(f), [])",
      q"f(1)" -> "Apply(Ident(f), [Number(1,Whole(10))])",
      q"obj.f(1 + 2, 3)" ->
        "Apply(Select(Ident(obj), f), [InfixOp(Number(1,Whole(10)),Ident(+),Number(2,Whole(10))), Number(3,Whole(10))])",
      q"obj.inner.f(1 + 2 * 3)" ->
        "Apply(Select(Select(Ident(obj), inner), f), [InfixOp(Number(1,Whole(10)),Ident(+),InfixOp(Number(2,Whole(10)),Ident(*),Number(3,Whole(10))))])",
      q"f(g(1), 2)" ->
        "Apply(Ident(f), [Apply(Ident(g), [Number(1,Whole(10))]), Number(2,Whole(10))])"
    )

    withContext {
      fixtures.foreach { case (meta, expected) =>
        val projected = ScalametaTermProjection.project(meta).toOption.get
        val raw = lowerOrFail(projected.shape)

        assertEquals(TermShapeInspector.rawStructure(raw), expected)
      }
    }
  }

  private def withContext[A](body: Context ?=> A): A =
    val base = new ContextBase
    given Context = base.initialCtx
    body

  private def lowerOrFail(shape: TermShape)(using Context): untpd.Tree =
    CoreTermShapeUntypedLowerer.lower(shape).fold(
      error => fail(error.message),
      identity
    )

  private def allTrees(tree: untpd.Tree): List[untpd.Tree] =
    tree match
      case untpd.Select(qualifier, _) => tree :: allTrees(qualifier)
      case untpd.Apply(function, arguments) =>
        tree :: allTrees(function) ::: arguments.flatMap(allTrees)
      case untpd.InfixOp(left, operator, right) =>
        tree :: allTrees(left) ::: allTrees(operator) ::: allTrees(right)
      case _ => tree :: Nil
