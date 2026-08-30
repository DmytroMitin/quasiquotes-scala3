package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3
import scala.meta.parsers.Parsed

@nowarn("cat=deprecation")
class ScalametaSelfAbstractTypeMemberProjectionTest extends munit.FunSuite:
  private final case class ExpectedNames(
      member: String,
      selfAlias: String,
      upperBase: String
  )

  test("characterizes the exact canonical Decl.Type tree structurally") {
    val declaration = parseDeclaration(
      "type Self >: self.type <: Nat { type Self = self.Self }"
    )

    assertEquals(declaration.mods, Nil)
    assertEquals(declaration.name.value, "Self")
    assertEquals(declaration.tparamClause.values, Nil)
    assertEquals(declaration.bounds.context, Nil)
    assertEquals(declaration.bounds.view, Nil)
    (declaration.bounds.lo, declaration.bounds.hi) match
      case (
            Some(Type.Singleton(lower: Term.Name)),
            Some(Type.Refine(Some(base: Type.Name), List(alias: Defn.Type)))
          ) =>
        assertEquals(lower.value, "self")
        assertEquals(base.value, "Nat")
        assertEquals(alias.mods, Nil)
        assertEquals(alias.name.value, "Self")
        assertEquals(alias.tparamClause.values, Nil)
        assertEquals(alias.bounds.lo, None)
        assertEquals(alias.bounds.hi, None)
        assertEquals(alias.bounds.context, Nil)
        assertEquals(alias.bounds.view, Nil)
        alias.body match
          case Type.Select(prefix: Term.Name, selected) =>
            assertEquals(prefix.value, "self")
            assertEquals(selected.value, "Self")
          case other => fail(s"expected direct selected-Type alias RHS, found $other")
      case other => fail(s"expected exact bounded abstract Decl.Type, found $other")
  }

  test("fully renamed legal names preserve the same exact categories") {
    val declaration = parseDeclaration(
      "type Element >: owner$2.type <: Domain { type Element = owner$2.Element }"
    )

    val projected = project(
      declaration,
      ExpectedNames("Element", "owner$2", "Domain")
    ).fold(error => fail(error.message), identity)

    assertEquals(projected.plan.memberName, "Element")
    assertEquals(projected.plan.selfAlias.source, "owner$2")
    assertEquals(projected.plan.lowerBound.alias, projected.plan.selfAlias)
    assertEquals(projected.plan.upperBound.baseName, "Domain")
    assertEquals(projected.plan.upperBound.aliasName, "Element")
    assertEquals(projected.plan.upperBound.rhs.alias, projected.plan.selfAlias)
    assertEquals(projected.plan.upperBound.rhs.memberName, "Element")
    assertEquals(
      projected.sourceSpan,
      Some(NeutralSourceSpan(declaration.pos.start, declaration.pos.end))
    )
  }

  test("classifies every independently parseable malformed near miss in stable order") {
    assertRejected(
      "type Self >: self.type <: Nat { type Self = self.Self }",
      ExpectedNames("Self", "type", "Nat"),
      "NEUTRAL_SELF_MEMBER_EXPECTED_SELF_ALIAS_INVALID"
    )
    assertRejected(
      "type Self >: self$0.type <: Nat { type Self = self$0.Self }",
      ExpectedNames("Self", "self$0", "Nat"),
      "NEUTRAL_SELF_MEMBER_EXPECTED_SELF_ALIAS_INVALID"
    )
    assertRejected(
      "type Self <: Nat { type Self = self.Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_LOWER_BOUND_MISSING"
    )
    assertRejected(
      "type Self >: String <: Nat { type Self = self.Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_LOWER_BOUND_NOT_SINGLETON"
    )
    assertRejected(
      "type Self >: other.type <: Nat { type Self = self.Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_LOWER_ALIAS_MISMATCH"
    )
    assertRejected(
      "type Self >: self.type <: Nat",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_UPPER_REFINEMENT_MISSING"
    )
    assertRejected(
      "type Self >: self.type <: Nat { type Self = self.Self; type Other = self.Other }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_REFINEMENT_COUNT_UNSUPPORTED"
    )
    assertRejected(
      "type Self >: self.type <: Nat { type Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_REFINEMENT_MEMBER_UNSUPPORTED"
    )
    assertRejected(
      "type Self >: self.type <: Nat { type Self = other.Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_SELECTED_PREFIX_MISMATCH"
    )
    assertRejected(
      "type Self >: self.type <: Nat { type Other = self.Other }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_REFINEMENT_NAME_MISMATCH"
    )
    assertRejected(
      "type Self >: self.type <: Nat { type Self = self.Other }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_SELECTED_MEMBER_MISMATCH"
    )
    assertRejected(
      "type Self >: self.type <: Other { type Self = self.Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_UPPER_BASE_MISMATCH"
    )
    assertRejected(
      "private type Self >: self.type <: Nat { type Self = self.Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_MODIFIERS_UNSUPPORTED"
    )
    assertRejected(
      "type Self[A] >: self.type <: Nat { type Self = self.Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_TYPE_PARAMETERS_UNSUPPORTED"
    )
  }

  test("illegal names fail in the Scala 3 parser before structural projection") {
    val parsed = Scala3(
      "type 1Self >: self.type <: Nat { type 1Self = self.1Self }"
    ).parse[Stat]
    assert(parsed.isInstanceOf[Parsed.Error], clues(parsed))
  }

  test("rejects expectation declaration auxiliary-bound and upper-base edges") {
    val canonical = parseDeclaration(
      "type Self >: self.type <: Nat { type Self = self.Self }"
    )
    assertProjectedRejected(
      canonical,
      ExpectedNames("bad-name", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_EXPECTED_MEMBER_INVALID"
    )
    assertProjectedRejected(
      canonical,
      ExpectedNames("Self", "self", "bad-name"),
      "NEUTRAL_SELF_MEMBER_EXPECTED_UPPER_BASE_INVALID"
    )
    assertProjectedRejected(
      canonical,
      ExpectedNames("Other", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_OUTER_NAME_MISMATCH"
    )
    assertRejected(
      "type Self >: self.type",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_UPPER_BOUND_MISSING"
    )
    assertRejected(
      "type Self >: self.type <: Nat[String] { type Self = self.Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_UPPER_BASE_UNSUPPORTED"
    )

    val withContextBound = canonical.copy(
      bounds = canonical.bounds.copy(context = List(Type.Name("Evidence")))
    )
    assertProjectedRejected(
      withContextBound,
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_CONTEXT_VIEW_BOUNDS_UNSUPPORTED"
    )

    val Type.Refine(Some(base), List(alias: Defn.Type)) =
      canonical.bounds.hi.get: @unchecked
    val boundedAlias = alias.copy(
      bounds = alias.bounds.copy(lo = Some(Type.Name("Nothing")))
    )
    val withAliasBounds = canonical.copy(
      bounds = canonical.bounds.copy(
        hi = Some(Type.Refine(Some(base), List(boundedAlias)))
      )
    )
    assertProjectedRejected(
      withAliasBounds,
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_REFINEMENT_ALIAS_BOUNDS_UNSUPPORTED"
    )
  }

  private def parseDeclaration(source: String): Decl.Type =
    Scala3(source).parse[Stat].get match
      case declaration: Decl.Type => declaration
      case other => fail(s"expected Decl.Type, found ${other.getClass.getSimpleName}")

  private def assertRejected(
      source: String,
      expected: ExpectedNames,
      expectedCode: String
  ): Unit =
    val result = project(parseDeclaration(source), expected)
    assertEquals(result.left.toOption.map(_.code), Some(expectedCode), clues(result))

  private def assertProjectedRejected(
      declaration: Decl.Type,
      expected: ExpectedNames,
      expectedCode: String
  ): Unit =
    val result = project(declaration, expected)
    assertEquals(result.left.toOption.map(_.code), Some(expectedCode), clues(result))

  private def project(
      declaration: Decl.Type,
      expected: ExpectedNames
  ): Either[NeutralProjectionError, ProjectedSelfAbstractTypeMember] =
    ScalametaSelfAbstractTypeMemberProjection.project(
      declaration,
      expected.member,
      expected.selfAlias,
      expected.upperBase
    )
