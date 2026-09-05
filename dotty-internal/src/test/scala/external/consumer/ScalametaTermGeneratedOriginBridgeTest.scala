package external.consumer

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.terms.dotty.ScalametaTermGeneratedOriginBridge

import scala.meta.*
import scala.meta.dialects.Scala3

final class ScalametaTermGeneratedOriginBridgeTest extends munit.FunSuite:
  test("external consumer receives positioned trees for the direct public intersection"):
    val fixtures = Vector(
      ("42", "42", "Number"),
      ("\"x\"", "\"x\"", "Literal"),
      ("true", "true", "Literal"),
      ("service.value", "service.value", "Select"),
      ("service.call(1)", "service.call(1)", "Apply"),
      ("(1, \"x\")", "(1, \"x\")", "Tuple"),
      ("if true then 1 else 2", "if true then 1 else 2", "If"),
      ("s\"x=$value\"", "s\"x=$value\"", "InterpolatedString"),
      (
        "new java.lang.StringBuilder(16)",
        "new java.lang.StringBuilder(16)",
        "Apply"
      )
    )

    withContext:
      fixtures.zipWithIndex.foreach { case ((source, expected, rootKind), index) =>
        val result = lower(source, s"<generated:term-direct-$index>")
        assertEquals(result.generatedSource, expected)
        assertEquals(result.tree.getClass.getSimpleName, rootKind, clues(source))
        assertPositioned(result, source)
      }

      lower("service.call(1)", "<generated:term-apply>").tree match
        case untpd.Apply(
              untpd.Select(untpd.Ident(qualifier), selected),
              List(untpd.Number(argument, untpd.NumberKind.Whole(10)))
            ) =>
          assertEquals(qualifier.toString, "service")
          assertEquals(selected.toString, "call")
          assertEquals(argument, "1")
        case other => fail(s"expected selected application, found $other")

      lower(
        "new java.lang.StringBuilder(16)",
        "<generated:term-constructor>"
      ).tree match
        case untpd.Apply(
              untpd.Select(untpd.New(_), constructor),
              List(untpd.Number(argument, untpd.NumberKind.Whole(10)))
            ) =>
          assertEquals(constructor.toString, "<init>")
          assertEquals(argument, "16")
        case other => fail(s"expected constructor application, found $other")

  test("external consumer receives the richer completed generated-origin intersection"):
    val fixtures = Vector(
      ("(wrapped)", "wrapped"),
      ("{ first; second }", "{ first; second }"),
      ("(1: Int)", "(1): Int"),
      ("(x: Int) => x + 1", "(x: Int) => x + 1"),
      ("{ val x: Int = 1; x + 1 }", "{ val x: Int = 1; x + 1 }"),
      (
        "{ def id(x: Int): Int = x; id }",
        "{ def id(x: Int): Int = x; id }"
      )
    )

    withContext:
      fixtures.zipWithIndex.foreach { case ((source, expected), index) =>
        val result = lower(source, s"<generated:term-richer-$index>")
        assertEquals(result.generatedSource, expected, clues(source))
        assertPositioned(result, source)
      }

      lower("(x: Int) => x + 1", "<generated:term-lambda>").tree match
        case untpd.Function(
              List(parameter: untpd.ValDef),
              untpd.InfixOp(untpd.Ident(reference), _, _)
            ) =>
          assertEquals(parameter.name.toString, "x")
          assertEquals(reference.toString, "x")
        case other => fail(s"expected Lambda1 topology, found $other")

      lower("{ val x: Int = 1; x + 1 }", "<generated:term-p2>").tree match
        case untpd.Block(
              List(local: untpd.ValDef),
              untpd.InfixOp(untpd.Ident(reference), _, _)
            ) =>
          assertEquals(local.name.toString, "x")
          assertEquals(reference.toString, "x")
        case other => fail(s"expected P2 local-val topology, found $other")

      lower(
        "{ def id(x: Int): Int = x; id }",
        "<generated:term-p3>"
      ).tree match
        case untpd.Block(
              List(local: untpd.DefDef),
              untpd.Ident(reference)
            ) =>
          assertEquals(local.name.toString, "id")
          assertEquals(local.paramss.map(_.map(_.name.toString)), List(List("x")))
          local.rhs match
            case untpd.Ident(parameterReference) =>
              assertEquals(parameterReference.toString, "x")
            case other => fail(s"expected local-def parameter reference, found $other")
          assertEquals(reference.toString, "id")
        case other => fail(s"expected P3 local-def topology, found $other")

  test("public failures identify projection completion and origin stages"):
    withContext:
      val missing = ScalametaTermGeneratedOriginBridge
        .lower(null, "<generated:term-missing>")
        .left
        .toOption
        .getOrElse(fail("missing input unexpectedly lowered"))
      assertEquals(missing.code, "MISSING_INPUT")

      Vector(
        "f(1)(2)",
        "f[Int](1)",
        "f(value = 1)",
        "f(values*)",
        "f(using value)",
        "raw\"x=$value\"",
        "new StringBuilder(16)"
      ).foreach: source =>
        val failure = lowerFailure(source, "<generated:term-projection>")
        assertEquals(failure.code, "NEUTRAL_PROJECTION_FAILED", clues(source))
        assert(failure.detail.startsWith("NEUTRAL_"), clues(source, failure))

      val completion =
        lowerFailure("(value: Option[Int])", "<generated:term-completion>")
      assertEquals(completion.code, "TERM_COMPLETION_FAILED")
      assert(
        completion.detail.contains("explicit completed-sidecar factory"),
        clues(completion)
      )

      Vector("", " padded ", "bad\nname", null).foreach: virtualName =>
        val failure = lowerFailure("42", virtualName)
        assertEquals(failure.code, "INVALID_VIRTUAL_SOURCE", clues(virtualName))
        assert(failure.detail.nonEmpty, clues(virtualName))

  private def lower(
      source: String,
      virtualName: String
  )(using Context): ScalametaTermGeneratedOriginBridge.Lowered =
    ScalametaTermGeneratedOriginBridge
      .lower(parsed(source), virtualName)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

  private def lowerFailure(
      source: String,
      virtualName: String
  )(using Context): ScalametaTermGeneratedOriginBridge.Failure =
    ScalametaTermGeneratedOriginBridge
      .lower(parsed(source), virtualName)
      .left
      .toOption
      .getOrElse(fail(s"unsupported Term unexpectedly lowered: $source"))

  private def parsed(source: String): Term =
    Scala3(source).parse[Term].get

  private def assertPositioned(
      result: ScalametaTermGeneratedOriginBridge.Lowered,
      source: String
  )(using Context): Unit =
    assertEquals(result.virtualSourceName, result.sourceFile.path)
    assertEquals(result.sourceFile.content.mkString, result.generatedSource)
    assertEquals(result.tree.span.start, 0)
    assertEquals(result.tree.span.end, result.generatedSource.length)
    allTrees(result.tree).foreach: node =>
      assert(node.source.exists, clues(source, node))
      assertEquals(node.source.path, result.virtualSourceName, clues(source, node))
      assert(node.span.exists, clues(source, node))
      assert(node.span.start >= 0, clues(source, node))
      assert(node.span.start <= node.span.point, clues(source, node))
      assert(node.span.point <= node.span.end, clues(source, node))
      assert(node.span.end <= result.generatedSource.length, clues(source, node))
      assertEquals(node.symbol, NoSymbol, clues(source, node))
      assert(!node.isInstanceOf[untpd.TypedSplice], clues(source, node))

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs)
      case value: untpd.ValDef =>
        Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.Select => Vector(value.qualifier)
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.InfixOp => Vector(value.left, value.op, value.right)
      case value: untpd.PrefixOp => Vector(value.op, value.od)
      case value: untpd.InterpolatedString => value.segments.toVector
      case value: untpd.Thicket => value.trees.toVector
      case value: untpd.Block => value.stats.toVector :+ value.expr
      case value: untpd.Typed => Vector(value.expr, value.tpt)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Tuple => value.trees.toVector
      case value: untpd.Function => value.args.toVector :+ value.body
      case value: untpd.If => Vector(value.cond, value.thenp, value.elsep)
      case value: untpd.Parens => Vector(value.t)
      case _ => Vector.empty

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
