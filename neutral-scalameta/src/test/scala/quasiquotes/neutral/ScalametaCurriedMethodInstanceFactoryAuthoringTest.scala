package quasiquotes.neutral

import quasiquotes.definitions.CurriedMethodInstanceFactoryPlan
import quasiquotes.definitions.CurriedMethodInstanceFactoryPlan.Plan

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaCurriedMethodInstanceFactoryAuthoringTest extends munit.FunSuite:
  private val Canonical =
    """def instance[A](combineFunction: A => A => A): Curried[A] =
      |  new Curried[A]:
      |    override def combine(a: A)(b: A): A =
      |      combineFunction(a)(b)""".stripMargin

  test("authors canonical and fully renamed plans with exact parsed structure"):
    val canonical = author(createPlan("instance", "A", "combineFunction", "Curried", "combine", "a", "b"))
    val renamed = author(createPlan("make", "Element", "operation", "Combiner", "merge", "left", "right"))
    assertEquals(canonical.structure, parse(Canonical).structure)
    assertEquals(
      renamed.structure,
      parse(
        """def make[Element](operation: Element => Element => Element): Combiner[Element] =
          |  new Combiner[Element]:
          |    override def merge(left: Element)(right: Element): Element =
          |      operation(left)(right)""".stripMargin
      ).structure
    )

  test("authors wholly fresh trees and reprojects an alpha-equivalent snapshot"):
    val plan = createPlan("instance", "A", "combineFunction", "Curried", "combine", "a", "b")
    val authored = author(plan)
    val projected = ScalametaCurriedMethodInstanceFactoryProjection.project(authored).toOption.get
    assert(allTrees(authored).forall(_.pos == Position.None))
    assertEquals(projected.sourceSpan, None)
    assertNotEquals(declarationIds(projected.plan), declarationIds(plan))
    assertEquals(projected.plan.roleSnapshot, plan.roleSnapshot)

  test("authors compiler-proven safe same-spelling controls"):
    val rows = List(
      Vector("combine", "A", "f", "Curried", "combine", "a", "b"),
      Vector("instance", "A", "f", "Curried", "a", "a", "b"),
      Vector("instance", "A", "f", "Curried", "b", "a", "b"),
      Vector("same", "same", "same", "Target", "member", "left", "right"),
      Vector("Target", "A", "f", "Target", "combine", "a", "b")
    )
    rows.foreach(values => assert(ScalametaCurriedMethodInstanceFactoryProjection.project(author(createFrom(values))).isRight))

  test("rejects missing and every Core-valid backticked role"):
    assertCode(ScalametaCurriedMethodInstanceFactoryAuthoring.author(null), "NEUTRAL_CURRIED_METHOD_FACTORY_AUTHORING_MISSING")
    val canonical = Vector("instance", "A", "combineFunction", "Curried", "combine", "a", "b")
    val replacements = Vector("`def`", "`type`", "`val`", "`class`", "`object`", "`match`", "`given`")
    canonical.indices.foreach { index =>
      assertCode(
        ScalametaCurriedMethodInstanceFactoryAuthoring.author(createFrom(canonical.updated(index, replacements(index)))),
        "NEUTRAL_CURRIED_METHOD_FACTORY_AUTHORING_NAME_UNSUPPORTED"
      )
    }

  private def createFrom(values: Vector[String]): Plan = createPlan(values(0), values(1), values(2), values(3), values(4), values(5), values(6))
  private def createPlan(factory: String, tpe: String, carrier: String, target: String, member: String, first: String, second: String): Plan =
    CurriedMethodInstanceFactoryPlan.create(factory, tpe, carrier, target, member, first, second).fold(problem => fail(problem.message), identity)
  private def author(plan: Plan): Defn.Def = ScalametaCurriedMethodInstanceFactoryAuthoring.author(plan).fold(problem => fail(problem.message), identity)
  private def parse(source: String): Defn.Def = Scala3(source).parse[Stat].get.asInstanceOf[Defn.Def]
  private def allTrees(root: Tree): List[Tree] = root :: root.children.toList.flatMap(allTrees)
  private def declarationIds(plan: Plan) = Vector(plan.typeParameter.binderId, plan.strictCarrier.binderId, plan.firstParameter.binderId, plan.secondParameter.binderId)
  private def assertCode[A](result: Either[ScalametaCurriedMethodInstanceFactoryAuthoring.Error, A], expected: String): Unit =
    assertEquals(result.left.toOption.map(_.code), Some(expected), clues(result))
