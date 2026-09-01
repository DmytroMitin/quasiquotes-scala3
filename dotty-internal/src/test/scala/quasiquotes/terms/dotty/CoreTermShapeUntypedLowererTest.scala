package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.neutral.ScalametaTermProjection
import quasiquotes.parser.{BinderId, TermShape, TermShapeInspector, TinyTermParser}
import quasiquotes.terms.ConstructedTerm

import scala.meta.*
import scala.meta.dialects.Scala3

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

  private val unsupportedLiteralTexts = Vector(
    "",
    "+1",
    " 1",
    "1 ",
    "1.0",
    "1e2",
    "1L",
    "'x'",
    "null",
    "()",
    "1_000",
    "\"",
    "value",
    "00",
    "01",
    "-0",
    "-01"
  )

  unsupportedLiteralTexts.foreach { value =>
    test(s"rejects unsupported semantic literal text: ${value.replace(" ", "<space>")}") {
      withContext {
        assertEquals(
          CoreTermShapeUntypedLowerer.lower(TermShape.Literal(value)),
          Left(InvalidIntegerLiteral(value))
        )
      }
    }
  }

  test("rejects a null semantic literal payload") {
    withContext {
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(TermShape.Literal(null)),
        Left(InvalidIntegerLiteral(null))
      )
    }
  }

  test("lowers Boolean and semantic String literals without reparsing payload text") {
    withContext {
      val fixtures = Vector(
        TermShape.Literal("true") -> "Literal(Boolean(true))",
        TermShape.Literal("false") -> "Literal(Boolean(false))",
        TermShape.Literal("\"1\"") -> "Literal(String(\"1\"))",
        TermShape.Literal("\"a quoted \"value\" and \\ slash\nλ\"") ->
          "Literal(String(\"a quoted \"value\" and \\ slash\nλ\"))"
      )

      fixtures.foreach { case (shape, expected) =>
        assertEquals(
          TermShapeInspector.rawStructure(lowerOrFail(shape)),
          expected,
          clues(shape)
        )
      }
    }
  }

  test("lowers the selected unary tuple and explicit-if families recursively") {
    withContext {
      val shape =
        TermShape.If(
          TermShape.Unary("!", TermShape.Identifier("flag", false)),
          TermShape.Tuple(
            List(
              TermShape.Literal("\"yes\""),
              TermShape.Unary("~", TermShape.Identifier("mask", false)),
              TermShape.Apply(
                TermShape.Identifier("f", false),
                TermShape.Literal("1") :: Nil
              )
            )
          ),
          TermShape.Tuple(List(TermShape.Literal("false"), TermShape.Literal("0")))
        )

      assertEquals(
        TermShapeInspector.rawStructure(lowerOrFail(shape)),
        "If(PrefixOp(!,Ident(flag)),Tuple([Literal(String(\"yes\")), PrefixOp(~,Ident(mask)), Apply(Ident(f), [Number(1,Whole(10))])]),Tuple([Literal(Boolean(false)), Number(0,Whole(10))]))"
      )
    }
  }

  test("preserves integer and String literal identity and folded versus structural minus") {
    withContext {
      val integer = lowerOrFail(TermShape.Literal("1"))
      val string = lowerOrFail(TermShape.Literal("\"1\""))
      val folded = lowerOrFail(TermShape.Literal("-1"))
      val structural = lowerOrFail(TermShape.Unary("-", TermShape.Literal("1")))

      assertEquals(TermShapeInspector.rawStructure(integer), "Number(1,Whole(10))")
      assertEquals(TermShapeInspector.rawStructure(string), "Literal(String(\"1\"))")
      assertEquals(TermShapeInspector.rawStructure(folded), "Number(-1,Whole(10))")
      assertEquals(
        TermShapeInspector.rawStructure(structural),
        "PrefixOp(-,Number(1,Whole(10)))"
      )
    }
  }

  test("admits exactly the four selected unary operators") {
    withContext {
      Vector("+", "-", "!", "~").foreach { operator =>
        val raw = lowerOrFail(
          TermShape.Unary(operator, TermShape.Identifier("value", false))
        )
        assertEquals(
          TermShapeInspector.rawStructure(raw),
          s"PrefixOp($operator,Ident(value))"
        )
      }

      Vector("", "not", "++", "&", null).foreach { operator =>
        assertEquals(
          CoreTermShapeUntypedLowerer.lower(
            TermShape.Unary(operator, TermShape.Identifier("value", false))
          ),
          Left(InvalidUnaryOperator(operator))
        )
      }
    }
  }

  test("enforces Tuple arity 2 through 22 and recursive child admission") {
    withContext {
      Vector(2, 3, 22).foreach { arity =>
        val elements = (1 to arity).map(index => TermShape.Literal(index.toString)).toList
        lowerOrFail(TermShape.Tuple(elements)) match
          case untpd.Tuple(rawElements) => assertEquals(rawElements.size, arity)
          case other => fail(s"expected Tuple, found ${other.getClass.getSimpleName}")
      }

      Vector(0, 1, 23).foreach { arity =>
        val elements = List.fill(arity)(TermShape.Literal("1"))
        assertEquals(
          CoreTermShapeUntypedLowerer.lower(TermShape.Tuple(elements)),
          Left(InvalidTupleArity(arity))
        )
      }
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(TermShape.Tuple(null)),
        Left(MissingTupleElements)
      )
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(
          TermShape.Tuple(List(TermShape.Literal("1"), null))
        ),
        Left(MissingTermShape)
      )
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(
          TermShape.Tuple(List(TermShape.Literal("1"), TermShape.New("Value", Nil)))
        ),
        Left(UnsupportedTermShape("New"))
      )
    }
  }

  test("fails closed for missing or unsupported Unary and If children") {
    val literal = TermShape.Literal("1")
    val unsupported = TermShape.New("Value", Nil)
    withContext {
      Vector[TermShape](
        TermShape.Unary("!", null),
        TermShape.If(null, literal, literal),
        TermShape.If(literal, null, literal),
        TermShape.If(literal, literal, null)
      ).foreach(shape =>
        assertEquals(CoreTermShapeUntypedLowerer.lower(shape), Left(MissingTermShape))
      )
      Vector[TermShape](
        TermShape.Unary("!", unsupported),
        TermShape.If(unsupported, literal, literal),
        TermShape.If(literal, unsupported, literal),
        TermShape.If(literal, literal, unsupported)
      ).foreach(shape =>
        assertEquals(
          CoreTermShapeUntypedLowerer.lower(shape),
          Left(UnsupportedTermShape("New"))
        )
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
        TermShape.If(
          TermShape.Unary("!", TermShape.Literal("false")),
          TermShape.Tuple(
            List(
              TermShape.Apply(
                TermShape.Select(
                  TermShape.Identifier("service", isPlaceholder = false),
                  "answer"
                ),
                TermShape.Literal("1") :: Nil
              ),
              TermShape.Literal("\"text\""),
              TermShape.Unary(
                "~",
                TermShape.Select(
                  TermShape.Identifier("state", isPlaceholder = false),
                  "mask"
                )
              )
            )
          ),
          TermShape.Tuple(
            List(
              TermShape.Infix(
                TermShape.Literal("-1"),
                "+",
                TermShape.Literal("2")
              ),
              TermShape.Literal("true")
            ),
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

  test("fails closed for every core TermShape family outside the bounded direct backend") {
    val binder = BinderId(0)
    val literal = TermShape.Literal("1")
    val unsupported = Vector(
      TermShape.BoundReference(binder, "value") -> "BoundReference",
      TermShape.Lambda1(binder, "value", "Int", literal) -> "Lambda1",
      TermShape.New("Value", Nil) -> "New",
      TermShape.InterpolatedString("s", List("", ""), List(literal)) ->
        "InterpolatedString",
      TermShape.Typed(literal, "Int") -> "Typed",
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

  test("composes current neutral projection through every new exact family") {
    val sources = Vector(
      "\"text\"",
      "true",
      "!flag",
      "(x, true, \"value\")",
      "if cond then \"yes\" else \"no\"",
      "if !flag then (\"yes\", true) else (\"no\", false)"
    )

    withContext {
      sources.foreach { source =>
        val meta = source.parse[Term].get
        val projected = ScalametaTermProjection.project(meta).toOption.get
        val direct = lowerOrFail(projected.shape)
        val parserOracle = TinyTermParser.parseOrThrow(source)

        assertEquals(
          TermShapeInspector.rawStructure(direct),
          parserOracle.rawStructure,
          clues(source, projected.shape)
        )
      }
    }
  }

  test("agrees structurally with the richer exact backend on the bounded overlap") {
    val shapes = Vector(
      TermShape.Literal("true"),
      TermShape.Literal("false"),
      TermShape.Literal("\"a \"quote\" \\ newline\nλ\""),
      TermShape.Unary("!", TermShape.Identifier("flag", false)),
      TermShape.Tuple(
        List(TermShape.Identifier("x", false), TermShape.Literal("true"))
      ),
      TermShape.If(
        TermShape.Identifier("cond", false),
        TermShape.Literal("\"yes\""),
        TermShape.Literal("\"no\"")
      )
    )

    withContext {
      shapes.foreach { shape =>
        val direct = lowerOrFail(shape)
        val constructed = ConstructedTerm.fromShape(shape).toOption.get
        val richer = ConstructedTermUntypedBackend.lower(constructed).toOption.get
        assertEquals(
          TermShapeInspector.rawStructure(direct),
          TermShapeInspector.rawStructure(richer),
          clues(shape)
        )
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
      case untpd.PrefixOp(operator, operand) =>
        tree :: allTrees(operator) ::: allTrees(operand)
      case untpd.Tuple(elements) =>
        tree :: elements.flatMap(allTrees)
      case untpd.If(condition, thenBranch, elseBranch) =>
        tree :: allTrees(condition) ::: allTrees(thenBranch) ::: allTrees(elseBranch)
      case _ => tree :: Nil
