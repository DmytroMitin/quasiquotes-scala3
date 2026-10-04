package quasiquotes.neutral

import quasiquotes.definitions.ParameterlessDelegatedForwardingPlan
import quasiquotes.definitions.ParameterlessDelegatedForwardingPlan.*

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaParameterlessDelegatedForwardingMethodAuthoringTest
    extends munit.FunSuite:
  test("authors canonical and fully renamed plans with exact parsed structure"):
    val canonical = author(createPlan("empty", "A", "inst", "Empty"))
    val renamed = author(createPlan("obtain", "Element", "evidence", "Provider"))

    assertEquals(
      canonical.structure,
      parse("def empty[A](using inst: Empty[A]): A = inst.empty").structure
    )
    assertEquals(
      renamed.structure,
      parse(
        "def obtain[Element](using evidence: Provider[Element]): Element = evidence.obtain"
      ).structure
    )

  test("authors wholly fresh trees and reprojects an alpha-equivalent role snapshot"):
    val plan = createPlan("empty", "A", "inst", "Empty")
    val authored = author(plan)
    val projected = ScalametaParameterlessDelegatedForwardingMethodProjection
      .project(authored)
      .fold(problem => fail(problem.code + ": " + problem.detail), identity)

    assert(allTrees(authored).forall(_.pos == Position.None))
    assertEquals(projected.sourceSpan, None)
    assertNotEquals(declarationBinderIds(projected.plan), declarationBinderIds(plan))
    assertEquals(projected.plan.roleSnapshot, plan.roleSnapshot)
    assert(projected.plan.body.selectedMethodIdentity eq projected.plan.methodIdentity)

  test("authors all compiler-proven safe same-spelling cross-namespace controls"):
    val plans = List(
      createPlan("same", "A", "same", "Evidence"),
      createPlan("get", "A", "A", "Evidence"),
      createPlan("A", "A", "inst", "Evidence"),
      createPlan("Same", "A", "inst", "Same"),
      createPlan("get", "A", "Evidence", "Evidence")
    )

    plans.foreach { plan =>
      val projected = ScalametaParameterlessDelegatedForwardingMethodProjection
        .project(author(plan))
        .fold(problem => fail(problem.code + ": " + problem.detail), identity)
      assertEquals(projected.plan.roleSnapshot, plan.roleSnapshot)
    }

  test("rejects a missing plan with the stable private authoring category"):
    assertCode(
      ScalametaParameterlessDelegatedForwardingMethodAuthoring.author(null),
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_AUTHORING_MISSING"
    )

  test("fails closed for every Core-valid backticked role that fresh trees cannot retain"):
    val canonical = Vector("empty", "A", "inst", "Empty")
    val replacements = Vector("`def`", "`type`", "`given`", "`class`")

    canonical.indices.foreach { index =>
      val values = canonical.updated(index, replacements(index))
      assertCode(
        ScalametaParameterlessDelegatedForwardingMethodAuthoring.author(createFrom(values)),
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_AUTHORING_NAME_UNSUPPORTED"
      )
    }

  private def createFrom(values: Vector[String]): Plan =
    createPlan(values(0), values(1), values(2), values(3))

  private def createPlan(
      method: String,
      typeParameter: String,
      contextualParameter: String,
      contextualConstructor: String
  ): Plan =
    ParameterlessDelegatedForwardingPlan
      .create(method, typeParameter, contextualParameter, contextualConstructor)
      .fold(problem => fail(problem.message), identity)

  private def author(plan: Plan): Defn.Def =
    ScalametaParameterlessDelegatedForwardingMethodAuthoring
      .author(plan)
      .fold(problem => fail(problem.message), identity)

  private def declarationBinderIds(plan: Plan) =
    Vector(plan.typeParameter.binderId, plan.contextualParameter.binderId)

  private def assertCode[A](
      result: Either[ScalametaParameterlessDelegatedForwardingMethodAuthoring.Error, A],
      expected: String
  ): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, expected, clues(problem.detail))
      case Right(_) => fail("expected " + expected)

  private def parse(source: String): Defn.Def =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Def => definition
      case other => fail("expected Defn.Def, found " + other.productPrefix)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
