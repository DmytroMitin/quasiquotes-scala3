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

  test("constructs only source-free span-free symbol-free raw nodes without TypedSplice") {
    withContext {
      val raw = lowerOrFail(
        TermShape.Infix(
          TermShape.Literal("-1"),
          "+",
          TermShape.Infix(
            TermShape.Literal("2"),
            "*",
            TermShape.Literal("3")
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

  test("fails closed for every non-integer-infix core TermShape family") {
    val binder = BinderId(0)
    val literal = TermShape.Literal("1")
    val unsupported = Vector(
      TermShape.Identifier("value", isPlaceholder = false) -> "Identifier",
      TermShape.Identifier("$hole", isPlaceholder = true) -> "Identifier",
      TermShape.BoundReference(binder, "value") -> "BoundReference",
      TermShape.Lambda1(binder, "value", "Int", literal) -> "Lambda1",
      TermShape.Select(literal, "value") -> "Select",
      TermShape.Apply(literal, Nil) -> "Apply",
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
        "InfixOp(Number(-1,Whole(10)),Ident(+),Number(2,Whole(10)))"
    )

    withContext {
      fixtures.foreach { case (meta, expected) =>
        val projected = ScalametaTermProjection.project(meta).toOption.get
        val raw = lowerOrFail(projected.shape)

        assertEquals(TermShapeInspector.rawStructure(raw), expected)
      }
    }
  }

  test("rejects newly admitted neutral Identifier, Select, and Apply shapes at the unchanged exact boundary") {
    val fixtures = List(
      q"value" -> "Identifier",
      q"obj.value" -> "Select",
      q"obj.value(1)" -> "Apply"
    )

    withContext {
      fixtures.foreach { (source, expectedNodeKind) =>
        val projected = ScalametaTermProjection.project(source).fold(
          error => fail(error.message),
          identity
        )
        assertEquals(
          CoreTermShapeUntypedLowerer.lower(projected.shape),
          Left(UnsupportedTermShape(expectedNodeKind)),
          clues(source.syntax)
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
      case untpd.InfixOp(left, operator, right) =>
        tree :: allTrees(left) ::: allTrees(operator) ::: allTrees(right)
      case _ => tree :: Nil
