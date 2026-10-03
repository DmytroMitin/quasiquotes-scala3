package quasiquotes.neutral

import quasiquotes.definitions.TypeMemberInstanceFactoryPlan
import quasiquotes.definitions.TypeMemberInstanceFactoryPlan.Plan

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTypeMemberInstanceFactoryAuthoringTest extends munit.FunSuite:
  private val Canonical =
    """def instance[A, Out0]: HasOut[A] { type Out = Out0 } =
      |  new HasOut[A]:
      |    type Out = Out0""".stripMargin

  test("authors canonical and fully renamed plans with exact parsed structure"):
    val canonical = author(createPlan("instance", "A", "Out0", "HasOut", "Out"))
    val renamed = author(
      createPlan("make", "Element", "Result0", "Container", "Result")
    )

    assertEquals(canonical.structure, parse(Canonical).structure)
    assertEquals(
      renamed.structure,
      parse(
        """def make[Element, Result0]: Container[Element] { type Result = Result0 } =
          |  new Container[Element]:
          |    type Result = Result0""".stripMargin
      ).structure
    )

  test("authors wholly fresh trees and reprojects an alpha-equivalent role snapshot"):
    val plan = createPlan("instance", "A", "Out0", "HasOut", "Out")
    val authored = author(plan)
    val projected = ScalametaTypeMemberInstanceFactoryProjection
      .project(authored)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

    assert(allTrees(authored).forall(_.pos == Position.None))
    assertEquals(projected.sourceSpan, None)
    assertNotEquals(declarationBinderIds(projected.plan), declarationBinderIds(plan))
    assertEquals(projected.plan.roleSnapshot, plan.roleSnapshot)

  test("authors compiler-proven safe same-spelling controls"):
    val memberFirst = author(createPlan("instance", "A", "Out0", "HasOut", "A"))
    val factoryTypesAndMember = author(
      createPlan("same", "same", "Other", "Target", "same")
    )

    assertEquals(
      ScalametaTypeMemberInstanceFactoryProjection.project(memberFirst).map(_.plan.roleSnapshot),
      Right(createPlan("instance", "A", "Out0", "HasOut", "A").roleSnapshot)
    )
    assertEquals(
      ScalametaTypeMemberInstanceFactoryProjection
        .project(factoryTypesAndMember)
        .map(_.plan.roleSnapshot),
      Right(createPlan("same", "same", "Other", "Target", "same").roleSnapshot)
    )

  test("rejects a missing plan with the stable private authoring category"):
    assertCode(
      ScalametaTypeMemberInstanceFactoryAuthoring.author(null),
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_AUTHORING_MISSING"
    )

  test("fails closed for every Core-valid backticked role that fresh trees cannot retain"):
    val canonical = Vector("instance", "A", "Out0", "HasOut", "Out")
    val replacements = Vector("`def`", "`type`", "`val`", "`class`", "`object`")

    canonical.indices.foreach { index =>
      val values = canonical.updated(index, replacements(index))
      assertCode(
        ScalametaTypeMemberInstanceFactoryAuthoring.author(createFrom(values)),
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_AUTHORING_NAME_UNSUPPORTED"
      )
    }

  private def createFrom(values: Vector[String]): Plan =
    createPlan(values(0), values(1), values(2), values(3), values(4))

  private def createPlan(
      factory: String,
      firstTypeParameter: String,
      secondTypeParameter: String,
      target: String,
      member: String
  ): Plan =
    TypeMemberInstanceFactoryPlan
      .create(factory, firstTypeParameter, secondTypeParameter, target, member)
      .fold(problem => fail(problem.message), identity)

  private def author(plan: Plan): Defn.Def =
    ScalametaTypeMemberInstanceFactoryAuthoring
      .author(plan)
      .fold(problem => fail(problem.message), identity)

  private def declarationBinderIds(plan: Plan) =
    Vector(plan.firstTypeParameter.binderId, plan.secondTypeParameter.binderId)

  private def assertCode[A](
      result: Either[ScalametaTypeMemberInstanceFactoryAuthoring.Error, A],
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
