package external.consumer

import dotty.tools.dotc.ast.untpd
import quasiquotes.types.dotty.ScalametaTypeUntypedBridge

import scala.meta.*
import scala.meta.dialects.Scala3

final class ScalametaTypeUntypedBridgeTest extends munit.FunSuite:
  test("external consumer lowers the complete bounded Type intersection without a Context"):
    val fixtures = List(
      "Int",
      "String",
      "Boolean",
      "List[Int]",
      "Option[(Int, String)]",
      "List[Int => String]",
      "Either[(Int, String), Boolean => Int]",
      "(Int, String, Boolean)",
      "(Int, String) => Boolean"
    )

    fixtures.foreach { source =>
      assertSourceFree(lower(source), source)
    }

    lower("Either[(Int, String), Boolean => Int]") match
      case untpd.AppliedTypeTree(
            untpd.Ident(either),
            List(
              untpd.Tuple(List(untpd.Ident(intType), untpd.Ident(stringType))),
              untpd.Function(List(untpd.Ident(booleanType)), untpd.Ident(resultType))
            )
          ) =>
        assertEquals(either.toString, "Either")
        assertEquals(intType.toString, "Int")
        assertEquals(stringType.toString, "String")
        assertEquals(booleanType.toString, "Boolean")
        assertEquals(resultType.toString, "Int")
      case other => fail(s"expected nested bounded Type topology, found $other")

  test("external consumer receives neutral failures without TupleN or FunctionN recovery"):
    val unsupported = List(
      "Long",
      "pkg.Type",
      "pkg.List[Int]",
      "Map[Int, String]",
      "(Int, String, Boolean, Int)",
      "(Int, String, Boolean) => Int",
      "Tuple2[Int, String]",
      "Function1[Int, String]",
      "Function2[Int, String, Boolean]",
      "Int { type Out = String }",
      "Int | String"
    )

    unsupported.foreach { source =>
      val failure = lowerFailure(source)
      assertEquals(failure.code, "NEUTRAL_PROJECTION_FAILED", clues(source, failure))
      assert(failure.detail.startsWith("NEUTRAL_TYPE_"), clues(source, failure))
    }

  test("external consumer receives exact-lowering failure for the wider neutral name"):
    val failure = lowerFailure("AnyVal")

    assertEquals(failure.code, "EXACT_LOWERING_FAILED")
    assertEquals(
      failure.detail,
      "Unsupported completed type at the exact-version untyped backend boundary: STypeIdent(AnyVal)."
    )

  test("external consumer receives missing-input failure"):
    val failure = ScalametaTypeUntypedBridge
      .lower(null)
      .left
      .toOption
      .getOrElse(fail("missing Type unexpectedly lowered"))

    assertEquals(failure.code, "MISSING_INPUT")
    assertEquals(failure.detail, "the Scalameta Type must be present.")

  private def lower(source: String): untpd.Tree =
    ScalametaTypeUntypedBridge
      .lower(parseType(source))
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

  private def lowerFailure(
      source: String
  ): ScalametaTypeUntypedBridge.Failure =
    ScalametaTypeUntypedBridge
      .lower(parseType(source))
      .left
      .toOption
      .getOrElse(fail(s"unsupported Type unexpectedly lowered: $source"))

  private def parseType(source: String): Type =
    Scala3(source).parse[Type].get

  private def assertSourceFree(tree: untpd.Tree, source: String): Unit =
    allTrees(tree).foreach { node =>
      assert(!node.source.exists, clues(source, node))
      assert(!node.span.exists, clues(source, node))
      assert(!node.isInstanceOf[untpd.TypedSplice], clues(source, node))
    }

  private def allTrees(tree: untpd.Tree): List[untpd.Tree] =
    tree match
      case untpd.AppliedTypeTree(constructor, arguments) =>
        tree :: allTrees(constructor) ::: arguments.flatMap(allTrees)
      case untpd.Tuple(elements) => tree :: elements.flatMap(allTrees)
      case untpd.Function(arguments, result) =>
        tree :: arguments.flatMap(allTrees) ::: allTrees(result)
      case _ => tree :: Nil
