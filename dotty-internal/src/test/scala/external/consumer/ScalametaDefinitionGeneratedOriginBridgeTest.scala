package external.consumer

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.dotty.ScalametaDefinitionGeneratedOriginBridge

import scala.meta.*
import scala.meta.dialects.Scala3

final class ScalametaDefinitionGeneratedOriginBridgeTest extends munit.FunSuite:
  test("external consumer receives positioned members and deterministic provenance for four ordinary families"):
    withContext:
      val fixtures = List(
        (
          "val answer: Int = 42",
          "<generated:c020-value>",
          "val answer: Int = 42",
          classOf[untpd.ValDef]
        ),
        (
          "def answer: Int = 42",
          "<generated:c020-parameterless>",
          "def answer: Int = 42",
          classOf[untpd.DefDef]
        ),
        (
          "def id(x: Int): Int = x",
          "<generated:c020-single>",
          "def id(x: Int): Int = x",
          classOf[untpd.DefDef]
        ),
        (
          "def pair(x: Int, y: Int): (Int, Int) = (x, y)",
          "<generated:c020-pair>",
          "def pair(x: Int, y: Int): (Int, Int) = (x, y)",
          classOf[untpd.DefDef]
        )
      )

      fixtures.foreach: (source, virtualName, expectedSource, expectedClass) =>
        val result = lower(source, virtualName)
        assert(expectedClass.isInstance(result.tree), clues(source, result.tree))
        assertEquals(result.generatedSource, expectedSource)
        assertEquals(result.virtualSourceName, virtualName)
        assertEquals(result.sourceFile.path, virtualName)
        assertEquals(result.sourceFile.content.mkString, expectedSource)
        assertEquals(result.tree.span.start, 0)
        assertEquals(result.tree.span.end, expectedSource.length)
        assertRecursivelyPositioned(result, source)

  test("external consumer preserves backticked binders and nested admitted source fragments"):
    withContext:
      val source =
        "def `type`(`match`: Option[Int]): Option[String] = service.convert(`match`)"
      val result = lower(source, "<generated:c020-nested>")

      assertEquals(result.generatedSource, source)
      result.tree match
        case method: untpd.DefDef =>
          assertEquals(method.name.toString, "type")
          assertEquals(method.paramss.map(_.map(_.name.toString)), List(List("match")))
          method.rhs match
            case untpd.Apply(
                  untpd.Select(untpd.Ident(qualifier), selected),
                  List(untpd.Ident(argument))
                ) =>
              assertEquals(qualifier.toString, "service")
              assertEquals(selected.toString, "convert")
              assertEquals(argument.toString, "match")
            case other => fail(s"expected selected application body, found $other")
        case other => fail(s"expected DefDef, found $other")

  test("generated-origin family and input failures remain stable and fail closed"):
    withContext:
      val missing = ScalametaDefinitionGeneratedOriginBridge
        .lower(null, "<generated:c020-missing>")
        .left
        .toOption
        .getOrElse(fail("missing definition unexpectedly lowered"))
      assertEquals(missing.code, "MISSING_INPUT")

      val projection = lowerFailure(
        "def unsupported(): Int = 1",
        "<generated:c020-projection>"
      )
      assertEquals(projection.code, "NEUTRAL_PROJECTION_FAILED")
      assert(
        projection.detail.startsWith("NEUTRAL_DEFINITION_FAMILY_UNSUPPORTED:"),
        clues(projection)
      )

      val alias = lowerFailure(
        "type Result = Option[Int]",
        "<generated:c020-alias>"
      )
      assertEquals(alias.code, "GENERATED_ORIGIN_FAMILY_UNSUPPORTED")
      assert(alias.detail.contains("SimpleTypeAlias"), clues(alias))

      val invalidSource = lowerFailure("val answer: Int = 42", "bad\nname")
      assertEquals(invalidSource.code, "INVALID_VIRTUAL_SOURCE")
      assert(invalidSource.detail.nonEmpty, clues(invalidSource))

  private def lower(
      source: String,
      virtualName: String
  )(using Context): ScalametaDefinitionGeneratedOriginBridge.Lowered =
    ScalametaDefinitionGeneratedOriginBridge
      .lower(parsed(source), virtualName)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

  private def lowerFailure(
      source: String,
      virtualName: String
  )(using Context): ScalametaDefinitionGeneratedOriginBridge.Failure =
    ScalametaDefinitionGeneratedOriginBridge
      .lower(parsed(source), virtualName)
      .left
      .toOption
      .getOrElse(fail(s"unsupported Definition unexpectedly lowered: $source"))

  private def parsed(source: String): Defn =
    Scala3(source).parse[Stat].get match
      case definition: Defn => definition
      case other => fail(s"expected Defn, found ${other.productPrefix}")

  private def assertRecursivelyPositioned(
      result: ScalametaDefinitionGeneratedOriginBridge.Lowered,
      source: String
  )(using Context): Unit =
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

  private def allTrees(tree: untpd.Tree)(using Context): List[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        tree :: value.paramss.flatten.flatMap(allTrees) :::
          allTrees(value.tpt) ::: allTrees(value.rhs)
      case value: untpd.ValDef =>
        tree :: allTrees(value.tpt) :::
          Option(value.unforcedRhs.asInstanceOf[untpd.Tree])
            .filterNot(_.isEmpty)
            .toList
            .flatMap(allTrees)
      case value: untpd.Select => tree :: allTrees(value.qualifier)
      case value: untpd.Apply => tree :: allTrees(value.fun) ::: value.args.flatMap(allTrees)
      case value: untpd.InfixOp =>
        tree :: allTrees(value.left) ::: allTrees(value.op) ::: allTrees(value.right)
      case value: untpd.AppliedTypeTree =>
        tree :: allTrees(value.tpt) ::: value.args.flatMap(allTrees)
      case value: untpd.Tuple => tree :: value.trees.flatMap(allTrees)
      case value: untpd.Function => tree :: value.args.flatMap(allTrees) ::: allTrees(value.body)
      case _ => tree :: Nil

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
