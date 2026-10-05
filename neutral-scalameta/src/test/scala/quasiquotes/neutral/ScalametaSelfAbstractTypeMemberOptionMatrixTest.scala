package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaSelfAbstractTypeMemberOptionMatrixTest extends munit.FunSuite:
  private final case class Row(
      label: String,
      source: String,
      member: String,
      selfAlias: String,
      upperBase: String,
      lowerPresent: Boolean,
      fBoundPresent: Boolean,
      treeCount: Int
  )

  private val rows = List(
    Row(
      "canonical both present",
      "type Self >: self.type <: Nat { type Self = self.Self }",
      "Self",
      "self",
      "Nat",
      lowerPresent = true,
      fBoundPresent = true,
      treeCount = 16
    ),
    Row(
      "canonical lower only",
      "type Self >: self.type <: Nat",
      "Self",
      "self",
      "Nat",
      lowerPresent = true,
      fBoundPresent = false,
      treeCount = 7
    ),
    Row(
      "canonical F-bound only",
      "type Self <: Nat { type Self = self.Self }",
      "Self",
      "self",
      "Nat",
      lowerPresent = false,
      fBoundPresent = true,
      treeCount = 14
    ),
    Row(
      "canonical neither",
      "type Self <: Nat",
      "Self",
      "self",
      "Nat",
      lowerPresent = false,
      fBoundPresent = false,
      treeCount = 5
    ),
    Row(
      "renamed both present",
      "type Element >: owner$2.type <: Domain { type Element = owner$2.Element }",
      "Element",
      "owner$2",
      "Domain",
      lowerPresent = true,
      fBoundPresent = true,
      treeCount = 16
    ),
    Row(
      "renamed lower only",
      "type Element >: owner$2.type <: Domain",
      "Element",
      "owner$2",
      "Domain",
      lowerPresent = true,
      fBoundPresent = false,
      treeCount = 7
    ),
    Row(
      "renamed F-bound only",
      "type Element <: Domain { type Element = owner$2.Element }",
      "Element",
      "owner$2",
      "Domain",
      lowerPresent = false,
      fBoundPresent = true,
      treeCount = 14
    ),
    Row(
      "renamed neither",
      "type Element <: Domain",
      "Element",
      "owner$2",
      "Domain",
      lowerPresent = false,
      fBoundPresent = false,
      treeCount = 5
    )
  )

  test("projects all eight parsed matrix rows with exact structural absence and role snapshots"):
    rows.foreach { row =>
      val declaration = parseDeclaration(row.source)
      val projected = projectMatrix(declaration, row)

      assertEquals(projected.plan.lowerBoundOption.nonEmpty, row.lowerPresent, row.label)
      assertEquals(projected.plan.fBoundRefinementOption.nonEmpty, row.fBoundPresent, row.label)
      assertEquals(projected.plan.roleSnapshot.lowerPresent, row.lowerPresent, row.label)
      assertEquals(projected.plan.roleSnapshot.fBoundPresent, row.fBoundPresent, row.label)
      assertEquals(declaration.bounds.lo.nonEmpty, row.lowerPresent, row.label)
      assertEquals(
        declaration.bounds.hi.exists(_.isInstanceOf[Type.Refine]),
        row.fBoundPresent,
        row.label
      )
      assertEquals(allTrees(declaration).size, row.treeCount, row.label)
      assertEquals(
        projected.sourceSpan,
        Some(NeutralSourceSpan(declaration.pos.start, declaration.pos.end)),
        row.label
      )
    }

  test("authors every matrix row directly and round-trips through the matrix projection"):
    rows.foreach { row =>
      val parsed = parseDeclaration(row.source)
      val expected = projectMatrix(parsed, row).plan
      val authored = ScalametaSelfAbstractTypeMemberAuthoring
        .author(expected)
        .fold(problem => fail(problem.message), identity)
      val projected = projectMatrix(authored, row)

      assertEquals(authored.structure, parsed.structure, row.label)
      assertEquals(allTrees(authored).size, row.treeCount, row.label)
      assert(allTrees(authored).forall(_.pos == Position.None), row.label)
      assertEquals(projected.sourceSpan, None, row.label)
      assertEquals(projected.plan.roleSnapshot, expected.roleSnapshot, row.label)
    }

  test("keeps the legacy projection both-present and fail-closed in historical order"):
    val both = rows.head
    assert(
      ScalametaSelfAbstractTypeMemberProjection
        .project(parseDeclaration(both.source), both.member, both.selfAlias, both.upperBase)
        .isRight
    )

    List(
      rows(1) -> "NEUTRAL_SELF_MEMBER_UPPER_REFINEMENT_MISSING",
      rows(2) -> "NEUTRAL_SELF_MEMBER_LOWER_BOUND_MISSING",
      rows(3) -> "NEUTRAL_SELF_MEMBER_LOWER_BOUND_MISSING"
    ).foreach { case (row, expectedCode) =>
      val result = ScalametaSelfAbstractTypeMemberProjection.project(
        parseDeclaration(row.source),
        row.member,
        row.selfAlias,
        row.upperBase
      )
      assertEquals(result.left.toOption.map(_.code), Some(expectedCode), row.label)
    }

  test("matrix projection rejects absent-edge substitutes and malformed direct or refined uppers"):
    val expected = rows.head
    List(
      "type Self >: Nothing <: Nat" ->
        "NEUTRAL_SELF_MEMBER_LOWER_BOUND_NOT_SINGLETON",
      "type Self >: self.type <: Other" ->
        "NEUTRAL_SELF_MEMBER_UPPER_BASE_MISMATCH",
      "type Self <: Nat {}" ->
        "NEUTRAL_SELF_MEMBER_REFINEMENT_COUNT_UNSUPPORTED",
      "type Self <: Nat[String]" ->
        "NEUTRAL_SELF_MEMBER_UPPER_BASE_UNSUPPORTED",
      "type Self <: { type Self = self.Self }" ->
        "NEUTRAL_SELF_MEMBER_UPPER_BASE_UNSUPPORTED"
    ).foreach { case (source, expectedCode) =>
      val result = ScalametaSelfAbstractTypeMemberProjection.projectMatrix(
        parseDeclaration(source),
        expected.member,
        expected.selfAlias,
        expected.upperBase
      )
      assertEquals(result.left.toOption.map(_.code), Some(expectedCode), source)
    }

  test("matrix projection validates expectations before null and authoring keeps lexical exclusions across all rows"):
    assertEquals(
      ScalametaSelfAbstractTypeMemberProjection
        .projectMatrix(null, "bad-name", "self", "Nat")
        .left
        .toOption
        .map(_.code),
      Some("NEUTRAL_SELF_MEMBER_EXPECTED_MEMBER_INVALID")
    )
    assertEquals(
      ScalametaSelfAbstractTypeMemberProjection
        .projectMatrix(null, "Self", "self", "Nat")
        .left
        .toOption
        .map(_.code),
      Some("NEUTRAL_SELF_MEMBER_DECLARATION_MISSING")
    )

    List(
      "type Member >: Member.type <: Member { type Member = Member.Member }",
      "type Member >: Member.type <: Member",
      "type Member <: Member { type Member = Member.Member }",
      "type Member <: Member"
    ).foreach { source =>
      val plan = ScalametaSelfAbstractTypeMemberProjection
        .projectMatrix(parseDeclaration(source), "Member", "Member", "Member")
        .fold(problem => fail(problem.message), _.plan)
      assertEquals(
        ScalametaSelfAbstractTypeMemberAuthoring
          .author(plan)
          .left
          .toOption
          .map(_.code),
        Some("NEUTRAL_SELF_MEMBER_AUTHORING_LEXICAL_ROLE_UNSUPPORTED"),
        source
      )
    }

  private def parseDeclaration(source: String): Decl.Type =
    Scala3(source).parse[Stat].get match
      case declaration: Decl.Type => declaration
      case other => fail("expected Decl.Type, found " + other.getClass.getSimpleName)

  private def projectMatrix(
      declaration: Decl.Type,
      row: Row
  ): ProjectedSelfAbstractTypeMember =
    ScalametaSelfAbstractTypeMemberProjection
      .projectMatrix(declaration, row.member, row.selfAlias, row.upperBase)
      .fold(problem => fail(problem.message), identity)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
