package external.consumer

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.dotty.ScalametaDefinitionUntypedBridge

import scala.meta.*
import scala.meta.dialects.Scala3

final class ScalametaDefinitionUntypedBridgeTest extends munit.FunSuite:
  test("external consumer lowers all five accepted Definition families to bounded members"):
    withContext:
      val fixtures = List(
        "val answer: Int = 42" -> classOf[untpd.ValDef],
        "def answer: Int = 42" -> classOf[untpd.DefDef],
        "def id(x: Int): Int = x" -> classOf[untpd.DefDef],
        "def pair(x: Int, y: Int): (Int, Int) = (x, y)" -> classOf[untpd.DefDef],
        "type Result = Option[Int]" -> classOf[untpd.TypeDef]
      )

      fixtures.foreach: (source, expectedClass) =>
        val member = lower(source)
        assert(expectedClass.isInstance(member), clues(source, member.getClass.getName))
        assertRecursivelySourceFree(member, source)

      assertEquals(lower("val answer: Int = 42").name.toString, "answer")
      assertEquals(lower("def answer: Int = 42").name.toString, "answer")
      assertEquals(lower("def id(x: Int): Int = x").name.toString, "id")
      assertEquals(
        lower("def pair(x: Int, y: Int): (Int, Int) = (x, y)").name.toString,
        "pair"
      )
      assertEquals(lower("type Result = Option[Int]").name.toString, "Result")

  test("external consumer observes bound parameters, renamed names, and nested admitted shapes"):
    withContext:
      lower("def `type`(`match`: Option[Int]): Option[String] = service.convert(`match`)") match
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

      lower("def pair(left: Int, right: Int): (Int, Int) = (right, left)") match
        case method: untpd.DefDef =>
          assertEquals(method.paramss.map(_.map(_.name.toString)), List(List("left", "right")))
          method.rhs match
            case untpd.Tuple(List(untpd.Ident(first), untpd.Ident(second))) =>
              assertEquals(first.toString, "right")
              assertEquals(second.toString, "left")
            case other => fail(s"expected reversed bound tuple, found $other")
        case other => fail(s"expected DefDef, found $other")

      lower("type `type` = Option[(Int, String)]") match
        case alias: untpd.TypeDef =>
          assertEquals(alias.name.toString, "type")
          assert(alias.name.isTypeName)
        case other => fail(s"expected TypeDef, found $other")

  test("external consumer receives stable missing, projection, and exact-lowering failures"):
    withContext:
      val missing = ScalametaDefinitionUntypedBridge
        .lower(null)
        .left
        .toOption
        .getOrElse(fail("missing definition unexpectedly lowered"))
      assertEquals(missing.code, "MISSING_INPUT")
      assertEquals(missing.detail, "the Scalameta Defn must be present.")

      val projection = lowerFailure("def unsupported(): Int = 1")
      assertEquals(projection.code, "NEUTRAL_PROJECTION_FAILED")
      assert(
        projection.detail.startsWith("NEUTRAL_DEFINITION_FAMILY_UNSUPPORTED:"),
        clues(projection)
      )

      val exact = lowerFailure("val wide: AnyVal = 42")
      assertEquals(exact.code, "EXACT_LOWERING_FAILED")
      assert(exact.detail.nonEmpty, clues(exact))

  private def lower(source: String)(using Context): untpd.MemberDef =
    ScalametaDefinitionUntypedBridge
      .lower(parsed(source))
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

  private def lowerFailure(
      source: String
  )(using Context): ScalametaDefinitionUntypedBridge.Failure =
    ScalametaDefinitionUntypedBridge
      .lower(parsed(source))
      .left
      .toOption
      .getOrElse(fail(s"unsupported Definition unexpectedly lowered: $source"))

  private def parsed(source: String): Defn =
    Scala3(source).parse[Stat].get match
      case definition: Defn => definition
      case other => fail(s"expected Defn, found ${other.productPrefix}")

  private def assertRecursivelySourceFree(
      tree: untpd.Tree,
      source: String
  )(using Context): Unit =
    allTrees(tree).foreach: node =>
      assert(!node.source.exists, clues(source, node))
      assert(!node.span.exists, clues(source, node))
      assertEquals(node.symbol, NoSymbol, clues(source, node))
      assert(!node.isInstanceOf[untpd.TypedSplice], clues(source, node))

  private def allTrees(tree: untpd.Tree)(using Context): List[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => tree :: allTrees(value.rhs)
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
