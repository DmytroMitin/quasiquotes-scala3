package quasiquotes.neutral

import _root_.quasiquotes.definitions.*

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaSelfAbstractTypeMemberAuthoringTest extends munit.FunSuite:
  test("authors canonical renamed peer-collision and cross-namespace plans exactly"):
    val rows = List(
      validPlan(),
      validPlan("Element", "owner", "Domain"),
      validPlan("Element", "self$1", "Domain"),
      validPlan("Result", "self$12", "Carrier"),
      validPlan("Member", "Member", "ExternalBase")
    )

    rows.foreach { expected =>
      val authored = author(expected)
      assert(allTrees(authored).forall(_.pos == Position.None), clues(snapshot(expected)))
      val projected = project(
        authored,
        expected.memberName,
        expected.selfAlias.source,
        expected.upperBound.baseName
      )
      assertEquals(projected.sourceSpan, None)
      assertEquals(snapshot(projected.plan), snapshot(expected))
    }

  test("reports the stable missing category for a null plan"):
    assertErrorCode(
      ScalametaSelfAbstractTypeMemberAuthoring.author(null),
      "NEUTRAL_SELF_MEMBER_AUTHORING_MISSING"
    )

  test("rejects a member-capturing upper-base spelling while preserving Term-Type reuse"):
    assertErrorCode(
      ScalametaSelfAbstractTypeMemberAuthoring.author(
        validPlan("Member", "self", "Member")
      ),
      "NEUTRAL_SELF_MEMBER_AUTHORING_LEXICAL_ROLE_UNSUPPORTED"
    )

    val crossNamespace = validPlan("Member", "Member", "ExternalBase")
    assertEquals(snapshot(project(author(crossNamespace), "Member", "Member", "ExternalBase").plan), snapshot(crossNamespace))

  test("fails closed for Core-valid member and base spellings decoded by fresh Type names"):
    List(
      validPlan("`type`", "self", "Domain"),
      validPlan("Member", "self", "`type`")
    ).foreach { plan =>
      assertErrorCode(
        ScalametaSelfAbstractTypeMemberAuthoring.author(plan),
        "NEUTRAL_SELF_MEMBER_AUTHORING_NAME_UNSUPPORTED"
      )
    }

  test("direct malformed controls retain the existing projector as topology authority"):
    val canonical = author(validPlan())
    val Type.Singleton(lower: Term.Name) = canonical.bounds.lo.get: @unchecked
    val Type.Refine(Some(base: Type.Name), List(alias: Defn.Type)) =
      canonical.bounds.hi.get: @unchecked

    val typeParameter = Type.Param(
      Nil,
      Type.Name("A"),
      Type.ParamClause(Nil),
      Type.Bounds.empty
    )
    val wrongStatistic = Decl.Type(
      Nil,
      Type.Name("Other"),
      Type.ParamClause(Nil),
      Type.Bounds.empty
    )
    val secondAlias = alias.copy(name = Type.Name("Other"))

    List(
      canonical.copy(bounds = canonical.bounds.copy(lo = None)) ->
        "NEUTRAL_SELF_MEMBER_LOWER_BOUND_MISSING",
      canonical.copy(bounds = canonical.bounds.copy(lo = Some(Type.Name("String")))) ->
        "NEUTRAL_SELF_MEMBER_LOWER_BOUND_NOT_SINGLETON",
      canonical.copy(
        bounds = canonical.bounds.copy(
          lo = Some(Type.Singleton(Term.Name("other")))
        )
      ) -> "NEUTRAL_SELF_MEMBER_LOWER_ALIAS_MISMATCH",
      canonical.copy(bounds = canonical.bounds.copy(hi = None)) ->
        "NEUTRAL_SELF_MEMBER_UPPER_BOUND_MISSING",
      withUpper(canonical, Type.Refine(Some(Type.Name("Other")), Stat.Block(List(alias)))) ->
        "NEUTRAL_SELF_MEMBER_UPPER_BASE_MISMATCH",
      withUpper(canonical, Type.Refine(Some(base), Stat.Block(Nil))) ->
        "NEUTRAL_SELF_MEMBER_REFINEMENT_COUNT_UNSUPPORTED",
      withUpper(canonical, Type.Refine(Some(base), Stat.Block(List(alias, secondAlias)))) ->
        "NEUTRAL_SELF_MEMBER_REFINEMENT_COUNT_UNSUPPORTED",
      withUpper(canonical, Type.Refine(Some(base), Stat.Block(List(wrongStatistic)))) ->
        "NEUTRAL_SELF_MEMBER_REFINEMENT_MEMBER_UNSUPPORTED",
      withAlias(canonical, alias.copy(mods = List(Mod.Private(Name.Anonymous())))) ->
        "NEUTRAL_SELF_MEMBER_REFINEMENT_MEMBER_UNSUPPORTED",
      withAlias(canonical, alias.copy(tparams = List(typeParameter))) ->
        "NEUTRAL_SELF_MEMBER_REFINEMENT_MEMBER_UNSUPPORTED",
      withAlias(
        canonical,
        alias.copy(bounds = Type.Bounds(None, Some(Type.Name("Any")), Nil, Nil))
      ) -> "NEUTRAL_SELF_MEMBER_REFINEMENT_ALIAS_BOUNDS_UNSUPPORTED",
      withAlias(canonical, alias.copy(name = Type.Name("Other"))) ->
        "NEUTRAL_SELF_MEMBER_REFINEMENT_NAME_MISMATCH",
      withAlias(
        canonical,
        alias.copy(body = Type.Select(Term.Name("other"), Type.Name("Self")))
      ) -> "NEUTRAL_SELF_MEMBER_SELECTED_PREFIX_MISMATCH",
      withAlias(
        canonical,
        alias.copy(body = Type.Select(lower, Type.Name("Other")))
      ) -> "NEUTRAL_SELF_MEMBER_SELECTED_MEMBER_MISMATCH",
      withAlias(canonical, alias.copy(body = Type.Name("String"))) ->
        "NEUTRAL_SELF_MEMBER_REFINEMENT_RHS_UNSUPPORTED"
    ).foreach { case (declaration, code) =>
      assertProjectionCode(declaration, code)
    }

  test("a parsed control preserves positioned provenance distinct from fresh authoring"):
    val positioned = Scala3(
      "type Self >: self.type <: Nat { type Self = self.Self }"
    ).parse[Stat].get.asInstanceOf[Decl.Type]

    assert(project(positioned, "Self", "self", "Nat").sourceSpan.nonEmpty)

  private def validPlan(
      memberName: String = "Self",
      selfAlias: String = "self",
      upperBaseName: String = "Nat"
  ): SelfAbstractTypeMemberPlan =
    val expectation = SelfAbstractTypeMemberExpectation(
      memberName,
      selfAlias,
      upperBaseName
    )
    val observed = ObservedSelfAbstractTypeMember(
      memberName,
      selfAlias,
      upperBaseName,
      memberName,
      selfAlias,
      memberName
    )
    SelfAbstractTypeMemberPlan
      .create(observed, expectation)
      .fold(problem => fail(problem.message), identity)

  private def author(plan: SelfAbstractTypeMemberPlan): Decl.Type =
    ScalametaSelfAbstractTypeMemberAuthoring
      .author(plan)
      .fold(problem => fail(problem.message), identity)

  private def project(
      declaration: Decl.Type,
      memberName: String,
      selfAlias: String,
      upperBaseName: String
  ): ProjectedSelfAbstractTypeMember =
    ScalametaSelfAbstractTypeMemberProjection
      .project(declaration, memberName, selfAlias, upperBaseName)
      .fold(problem => fail(problem.message), identity)

  private def snapshot(plan: SelfAbstractTypeMemberPlan): Vector[String] =
    Vector(
      plan.memberName,
      plan.selfAlias.source,
      plan.lowerBound.alias.source,
      plan.upperBound.baseName,
      plan.upperBound.aliasName,
      plan.upperBound.rhs.alias.source,
      plan.upperBound.rhs.memberName
    )

  private def withUpper(declaration: Decl.Type, upper: Type): Decl.Type =
    declaration.copy(bounds = declaration.bounds.copy(hi = Some(upper)))

  private def withAlias(
      declaration: Decl.Type,
      replacement: Defn.Type
  ): Decl.Type =
    val Type.Refine(Some(base), _) = declaration.bounds.hi.get: @unchecked
    withUpper(declaration, Type.Refine(Some(base), Stat.Block(List(replacement))))

  private def assertProjectionCode(
      declaration: Decl.Type,
      expectedCode: String
  ): Unit =
    assertEquals(
      ScalametaSelfAbstractTypeMemberProjection
        .project(declaration, "Self", "self", "Nat")
        .left
        .toOption
        .map(_.code),
      Some(expectedCode)
    )

  private def assertErrorCode[A](
      result: Either[ScalametaSelfAbstractTypeMemberAuthoring.Error, A],
      expectedCode: String
  ): Unit =
    assertEquals(result.left.toOption.map(_.code), Some(expectedCode), clues(result))

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
