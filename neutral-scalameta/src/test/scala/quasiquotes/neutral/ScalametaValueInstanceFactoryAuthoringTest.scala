package quasiquotes.neutral

import quasiquotes.definitions.ValueInstanceFactoryPlan
import quasiquotes.definitions.ValueInstanceFactoryPlan.Plan

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaValueInstanceFactoryAuthoringTest extends munit.FunSuite:
  private val Canonical =
    """def instance[A](valueValue: A): HasValue[A] =
      |  new HasValue[A]:
      |    override val value: A = valueValue""".stripMargin

  test("authors canonical and fully renamed plans with exact parsed structure"):
    val canonical = author(createPlan("instance", "A", "valueValue", "HasValue", "value"))
    val renamed = author(
      createPlan("make", "Element", "elementCarrier", "Container", "element")
    )

    assertEquals(canonical.structure, parse(Canonical).structure)
    assertEquals(
      renamed.structure,
      parse(
        """def make[Element](elementCarrier: Element): Container[Element] =
          |  new Container[Element]:
          |    override val element: Element = elementCarrier""".stripMargin
      ).structure
    )

  test("authors wholly fresh trees and reprojects an alpha-equivalent role snapshot"):
    val plan = createPlan("instance", "A", "valueValue", "HasValue", "value")
    val authored = author(plan)
    val projected = ScalametaValueInstanceFactoryProjection
      .project(authored)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

    assert(allTrees(authored).forall(_.pos == Position.None))
    assertEquals(projected.sourceSpan, None)
    assertNotEquals(declarationBinderIds(projected.plan), declarationBinderIds(plan))
    assertEquals(projected.plan.roleSnapshot, plan.roleSnapshot)

  test("authors safe cross-namespace same-spelling controls"):
    val factoryTypeCarrier = author(createPlan("same", "same", "same", "Target", "member"))
    val factoryMember = author(createPlan("same", "A", "carrier", "Target", "same"))

    assertEquals(
      ScalametaValueInstanceFactoryProjection.project(factoryTypeCarrier).map(_.plan.roleSnapshot),
      Right(createPlan("same", "same", "same", "Target", "member").roleSnapshot)
    )
    assertEquals(
      ScalametaValueInstanceFactoryProjection.project(factoryMember).map(_.plan.roleSnapshot),
      Right(createPlan("same", "A", "carrier", "Target", "same").roleSnapshot)
    )

  test("rejects a missing plan with the stable private authoring category"):
    assertCode(
      ScalametaValueInstanceFactoryAuthoring.author(null),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_AUTHORING_MISSING"
    )

  test("fails closed for every Core-valid backticked role that fresh trees cannot retain"):
    val canonical = Vector("instance", "A", "valueValue", "HasValue", "value")
    val replacements = Vector("`def`", "`type`", "`val`", "`class`", "`object`")

    canonical.indices.foreach { index =>
      val values = canonical.updated(index, replacements(index))
      assertCode(
        ScalametaValueInstanceFactoryAuthoring.author(createFrom(values)),
        "NEUTRAL_VALUE_INSTANCE_FACTORY_AUTHORING_NAME_UNSUPPORTED"
      )
    }

  private def createFrom(values: Vector[String]): Plan =
    createPlan(values(0), values(1), values(2), values(3), values(4))

  private def createPlan(
      factory: String,
      typeParameter: String,
      carrier: String,
      target: String,
      member: String
  ): Plan =
    ValueInstanceFactoryPlan
      .create(factory, typeParameter, carrier, target, member)
      .fold(problem => fail(problem.message), identity)

  private def author(plan: Plan): Defn.Def =
    ScalametaValueInstanceFactoryAuthoring
      .author(plan)
      .fold(problem => fail(problem.message), identity)

  private def declarationBinderIds(plan: Plan) =
    Vector(plan.typeParameter.binderId, plan.strictCarrier.binderId)

  private def assertCode[A](
      result: Either[ScalametaValueInstanceFactoryAuthoring.Error, A],
      expected: String
  ): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, expected)
      case Right(_) => fail(s"expected $expected")

  private def parse(source: String): Defn.Def =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Def => definition
      case other => fail(s"expected Defn.Def, found ${other.productPrefix}")

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
