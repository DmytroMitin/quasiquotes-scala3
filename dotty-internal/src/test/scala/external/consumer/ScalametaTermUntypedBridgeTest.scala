package external.consumer

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.terms.dotty.ScalametaTermUntypedBridge

import scala.meta.*
import scala.meta.dialects.Scala3

class ScalametaTermUntypedBridgeTest extends munit.FunSuite:
  test("external consumer lowers the complete direct non-binder intersection") {
    withContext {
      assertNumber(lower("1"), "1")
      assertString(lower("\"text\""), "text")
      assertBoolean(lower("true"), true)

      lower("service.answer(1, 2)") match
        case untpd.Apply(
              untpd.Select(untpd.Ident(qualifier), selected),
              List(
                untpd.Number(first, untpd.NumberKind.Whole(10)),
                untpd.Number(second, untpd.NumberKind.Whole(10))
              )
            ) =>
          assertEquals(qualifier.toString, "service")
          assertEquals(selected.toString, "answer")
          assertEquals(first, "1")
          assertEquals(second, "2")
        case other => fail(s"expected selected Apply, found $other")

      lower("1 + 2 * 3") match
        case untpd.InfixOp(
              untpd.Number(first, untpd.NumberKind.Whole(10)),
              untpd.Ident(plus),
              untpd.InfixOp(
                untpd.Number(second, untpd.NumberKind.Whole(10)),
                untpd.Ident(times),
                untpd.Number(third, untpd.NumberKind.Whole(10))
              )
            ) =>
          assertEquals(first, "1")
          assertEquals(plus.toString, "+")
          assertEquals(second, "2")
          assertEquals(times.toString, "*")
          assertEquals(third, "3")
        case other => fail(s"expected nested infix tree, found $other")

      lower("!flag") match
        case untpd.PrefixOp(untpd.Ident(operator), untpd.Ident(operand)) =>
          assertEquals(operator.toString, "!")
          assertEquals(operand.toString, "flag")
        case other => fail(s"expected unary tree, found $other")

      lower("(left, 2, false)") match
        case untpd.Tuple(
              List(
                untpd.Ident(left),
                untpd.Number(number, untpd.NumberKind.Whole(10)),
                boolean
              )
            ) =>
          assertEquals(left.toString, "left")
          assertEquals(number, "2")
          assertBoolean(boolean, false)
        case other => fail(s"expected tuple tree, found $other")

      lower("if ready then \"yes\" else \"no\"") match
        case untpd.If(untpd.Ident(condition), thenBranch, elseBranch) =>
          assertEquals(condition.toString, "ready")
          assertString(thenBranch, "yes")
          assertString(elseBranch, "no")
        case other => fail(s"expected explicit-if tree, found $other")

      lower("s\"value=$value\"") match
        case untpd.InterpolatedString(prefix, segments) =>
          assertEquals(prefix.toString, "s")
          assertEquals(segments.size, 2)
        case other => fail(s"expected interpolation tree, found $other")

      lower("new java.lang.StringBuilder(16)") match
        case untpd.Apply(
              untpd.Select(untpd.New(_), constructor),
              List(untpd.Number(number, untpd.NumberKind.Whole(10)))
            ) =>
          assertEquals(constructor.toString, "<init>")
          assertEquals(number, "16")
        case other => fail(s"expected constructor tree, found $other")

      lower("{ first; second }") match
        case untpd.Block(List(untpd.Ident(first)), untpd.Ident(second)) =>
          assertEquals(first.toString, "first")
          assertEquals(second.toString, "second")
        case other => fail(s"expected binder-free block, found $other")

      lower("(wrapped)") match
        case untpd.Ident(name) => assertEquals(name.toString, "wrapped")
        case other => fail(s"expected transparent P0 identifier, found $other")
    }
  }

  test("external consumer receives stable neutral projection failures") {
    withContext {
      val sources = Vector(
        "f(1)(2)",
        "f[Int](1)",
        "f(value = 1)",
        "f(values*)",
        "f(using value)",
        "new StringBuilder(16)",
        "new Box[Int](1)",
        "new java.lang.StringBuilder(16)(32)",
        "new java.lang.Runnable { def run(): Unit = () }"
      )

      sources.foreach { source =>
        val failure = lowerFailure(source)
        assertEquals(failure.code, "NEUTRAL_PROJECTION_FAILED", clues(source, failure))
        assert(failure.detail.startsWith("NEUTRAL_"), clues(source, failure))
      }
    }
  }

  test("external consumer receives exact-lowering failures without richer fallback") {
    withContext {
      val sources = Vector(
        "(1: Int)" -> "Unsupported core TermShape",
        "(x: Int) => x" -> "Unsupported core TermShape",
        "{ val x: Int = 1; x }" -> "Malformed bounded exact-backend P1 Block",
        "{ def id(x: Int): Int = x; id }" ->
          "Malformed bounded exact-backend P1 Block"
      )

      sources.foreach { case (source, expectedDetailPrefix) =>
        val failure = lowerFailure(source)
        assertEquals(failure.code, "EXACT_LOWERING_FAILED", clues(source, failure))
        assert(
          failure.detail.startsWith(expectedDetailPrefix),
          clues(source, failure)
        )
      }
    }
  }

  test("external consumer receives missing-input failure and recursively source-free trees") {
    withContext {
      val missing = ScalametaTermUntypedBridge
        .lower(null)
        .left
        .toOption
        .getOrElse(fail("missing input unexpectedly lowered"))
      assertEquals(missing.code, "MISSING_INPUT")

      val tree = lower(
        "if !ready then (service.answer(1), s\"value=$value\") else (other, s\"none\")"
      )
      allTrees(tree).foreach { node =>
        assert(!node.source.exists, clues(node))
        assert(!node.span.exists, clues(node))
        assertEquals(node.symbol, NoSymbol, clues(node))
        assert(!node.isInstanceOf[untpd.TypedSplice], clues(node))
      }
    }
  }

  private def lower(source: String)(using Context): untpd.Tree =
    ScalametaTermUntypedBridge
      .lower(parse(source))
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

  private def lowerFailure(
      source: String
  )(using Context): ScalametaTermUntypedBridge.Failure =
    ScalametaTermUntypedBridge
      .lower(parse(source))
      .left
      .toOption
      .getOrElse(fail(s"unsupported source unexpectedly lowered: $source"))

  private def parse(source: String): Term =
    Scala3(source).parse[Term].get

  private def assertNumber(tree: untpd.Tree, expected: String): Unit =
    tree match
      case untpd.Number(number, untpd.NumberKind.Whole(10)) =>
        assertEquals(number, expected)
      case other => fail(s"expected Number($expected), found $other")

  private def assertString(tree: untpd.Tree, expected: String): Unit =
    tree match
      case untpd.Literal(constant) => assertEquals(constant, Constant(expected))
      case other => fail(s"expected String literal $expected, found $other")

  private def assertBoolean(tree: untpd.Tree, expected: Boolean): Unit =
    tree match
      case untpd.Literal(constant) => assertEquals(constant, Constant(expected))
      case other => fail(s"expected Boolean literal $expected, found $other")

  private def allTrees(tree: untpd.Tree): List[untpd.Tree] =
    tree match
      case untpd.InterpolatedString(_, segments) =>
        tree :: segments.flatMap(allTrees)
      case untpd.Thicket(trees) => tree :: trees.flatMap(allTrees)
      case untpd.Select(qualifier, _) => tree :: allTrees(qualifier)
      case fresh: untpd.New => tree :: allTrees(fresh.tpt)
      case untpd.Apply(function, arguments) =>
        tree :: allTrees(function) ::: arguments.flatMap(allTrees)
      case untpd.InfixOp(left, operator, right) =>
        tree :: allTrees(left) ::: allTrees(operator) ::: allTrees(right)
      case untpd.PrefixOp(operator, operand) =>
        tree :: allTrees(operator) ::: allTrees(operand)
      case untpd.Tuple(elements) => tree :: elements.flatMap(allTrees)
      case untpd.If(condition, thenBranch, elseBranch) =>
        tree :: allTrees(condition) ::: allTrees(thenBranch) ::: allTrees(elseBranch)
      case untpd.Block(statements, result) =>
        tree :: statements.flatMap(allTrees) ::: allTrees(result)
      case _ => tree :: Nil

  private def withContext[A](body: Context ?=> A): A =
    val base = new ContextBase
    body(using base.initialCtx)
