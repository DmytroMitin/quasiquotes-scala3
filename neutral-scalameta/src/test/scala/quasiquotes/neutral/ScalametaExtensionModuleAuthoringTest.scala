package quasiquotes.neutral

import quasiquotes.definitions.ExtensionModulePlan
import quasiquotes.definitions.ExtensionModulePlan.Plan

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaExtensionModuleAuthoringTest extends munit.FunSuite:
  private val Canonical =
    """object syntax:
      |  extension [A](a: A)
      |    def combine(a1: A)(using inst: Monoid[A]): A =
      |      inst.combine(a, a1)""".stripMargin

  test("authors canonical and fully renamed plans with exact parsed structure"):
    val canonicalPlan = createPlan("syntax", "A", "a", "combine", "a1", "inst", "Monoid")
    val renamedPlan = createPlan(
      "operations",
      "Element",
      "left",
      "merge",
      "right",
      "evidence",
      "Combine"
    )
    val canonical = author(canonicalPlan)
    val renamed = author(renamedPlan)

    assertEquals(canonical.structure, parse(Canonical).structure)
    assertEquals(
      renamed.structure,
      parse(
        """object operations:
          |  extension [Element](left: Element)
          |    def merge(right: Element)(using evidence: Combine[Element]): Element =
          |      evidence.merge(left, right)""".stripMargin
      ).structure
    )

  test("authors wholly fresh trees and reprojects the independent role snapshot"):
    val plan = createPlan("syntax", "A", "a", "combine", "a1", "inst", "Monoid")
    val authored = author(plan)
    val projected = ScalametaExtensionModuleProjection
      .project(authored)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

    assert(allTrees(authored).forall(_.pos == Position.None))
    assertEquals(projected.sourceSpan, None)
    assertNotEquals(
      declarationBinderIds(projected.plan),
      declarationBinderIds(plan)
    )
    assertEquals(projected.plan.roleSnapshot, plan.roleSnapshot)

  test("authors safe cross-namespace and method-Term same-spelling controls"):
    val crossNamespace = author(createPlan("A", "A", "A", "A", "a1", "inst", "Monoid"))
    val methodTerm = author(
      createPlan("syntax", "A", "a", "combine", "combine", "inst", "Monoid")
    )

    assertEquals(
      ScalametaExtensionModuleProjection.project(crossNamespace).map(_.plan.roleSnapshot),
      Right(createPlan("A", "A", "A", "A", "a1", "inst", "Monoid").roleSnapshot)
    )
    assertEquals(
      ScalametaExtensionModuleProjection.project(methodTerm).map(_.plan.ordinaryArgument.displayName),
      Right("combine")
    )

  test("rejects a missing plan with the stable private authoring category"):
    assertCode(
      ScalametaExtensionModuleAuthoring.author(null),
      "NEUTRAL_EXTENSION_MODULE_AUTHORING_MISSING"
    )

  test("fails closed for every Core-valid backticked role that fresh trees cannot retain"):
    val canonical = Vector("syntax", "A", "a", "combine", "a1", "inst", "Monoid")
    val replacements = Vector(
      "`object`",
      "`type`",
      "`val`",
      "`def`",
      "`var`",
      "`given`",
      "`class`"
    )

    canonical.indices.foreach { index =>
      val values = canonical.updated(index, replacements(index))
      assertCode(
        ScalametaExtensionModuleAuthoring.author(createFrom(values)),
        "NEUTRAL_EXTENSION_MODULE_AUTHORING_NAME_UNSUPPORTED"
      )
    }

  private def createFrom(values: Vector[String]): Plan =
    createPlan(values(0), values(1), values(2), values(3), values(4), values(5), values(6))

  private def createPlan(
      module: String,
      typeParameter: String,
      receiver: String,
      method: String,
      ordinary: String,
      contextual: String,
      evidenceConstructor: String
  ): Plan =
    ExtensionModulePlan
      .create(
        module,
        typeParameter,
        receiver,
        method,
        ordinary,
        contextual,
        evidenceConstructor
      )
      .fold(problem => fail(problem.message), identity)

  private def author(plan: Plan): Defn.Object =
    ScalametaExtensionModuleAuthoring
      .author(plan)
      .fold(problem => fail(problem.message), identity)

  private def declarationBinderIds(plan: Plan) =
    Vector(
      plan.typeParameter.binderId,
      plan.receiverParameter.binderId,
      plan.ordinaryArgument.binderId,
      plan.contextualParameter.binderId
    )

  private def assertCode[A](
      result: Either[ScalametaExtensionModuleAuthoring.Error, A],
      expected: String
  ): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, expected)
      case Right(_) => fail(s"expected $expected")

  private def parse(source: String): Defn.Object =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Object => definition
      case other => fail(s"expected Defn.Object, found ${other.productPrefix}")

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
