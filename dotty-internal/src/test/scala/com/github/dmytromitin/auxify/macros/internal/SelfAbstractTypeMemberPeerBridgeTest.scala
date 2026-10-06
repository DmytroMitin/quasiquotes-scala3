package com.github.dmytromitin.auxify.macros.internal

import scala.annotation.nowarn

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.dotty.SelfAbstractTypeMemberPeerBridge

import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
class SelfAbstractTypeMemberPeerBridgeTest extends munit.FunSuite:
  private final case class Names(member: String, selfAlias: String, upperBase: String)
  private final case class Row(label: String, lower: Boolean, fBound: Boolean, nodes: Int)

  private val nameSets = Vector(
    Names("Self", "self", "Nat"),
    Names("Element", "owner$2", "Domain")
  )
  private val rows = Vector(
    Row("both", lower = true, fBound = true, 9),
    Row("lower-only", lower = true, fBound = false, 5),
    Row("f-bound-only", lower = false, fBound = true, 7),
    Row("neither", lower = false, fBound = false, 3)
  )

  test("foreign AUXify package receives all canonical and renamed matrix rows") {
    withContext {
      nameSets.zipWithIndex.foreach { case (names, nameIndex) =>
        rows.zipWithIndex.foreach { case (row, rowIndex) =>
          val source = render(names, row)
          val declaration = parseDeclaration(source)
          val originalStructure = declaration.structure
          val originalSyntax = declaration.syntax
          val virtualName = s"AuxifySelfMatrix$nameIndex$rowIndex.scala"
          val first = lower(declaration, names, virtualName)
          val second = lower(declaration, names, virtualName)

          assertEquals(first.generatedSource, source, row.label)
          assertEquals(second.generatedSource, source, row.label)
          assertEquals(first.virtualSourceName, virtualName, row.label)
          assertRow(first.tree, names, row)
          assertRow(second.tree, names, row)
          assertPositioned(first.tree, source, virtualName, row.nodes)
          assertPositioned(second.tree, source, virtualName, row.nodes)
          assertEquals(spanSnapshot(first.tree), spanSnapshot(second.tree), row.label)
          assert(!(first.tree eq second.tree), row.label)
          assert(!(first.tree.source eq second.tree.source), row.label)
          assert(
            nonEmptyTrees(first.tree).zip(nonEmptyTrees(second.tree)).forall {
              case (left, right) => !(left eq right)
            },
            row.label
          )
          assertEquals(declaration.structure, originalStructure, row.label)
          assertEquals(declaration.syntax, originalSyntax, row.label)
        }
      }
    }
  }

  test("preserves exact historical both-present source topology and spans") {
    withContext {
      val canonical = lower(parseDeclaration(render(nameSets(0), rows(0))), nameSets(0), "Canonical.scala")
      val renamed = lower(parseDeclaration(render(nameSets(1), rows(0))), nameSets(1), "Renamed.scala")
      assertEquals(canonical.generatedSource, "type Self >: self.type <: Nat { type Self = self.Self }")
      assertEquals(renamed.generatedSource, "type Element >: owner$2.type <: Domain { type Element = owner$2.Element }")
      assertEquals(
        spanSnapshot(canonical.tree),
        Vector(
          ("TypeDef", 0, 5, 55),
          ("TypeBoundsTree", 13, 13, 55),
          ("SingletonTypeTree", 13, 13, 22),
          ("Ident", 13, 13, 17),
          ("RefinedTypeTree", 26, 26, 55),
          ("Ident", 26, 26, 29),
          ("TypeDef", 32, 37, 53),
          ("Select", 44, 49, 53),
          ("Ident", 44, 44, 48)
        )
      )
      assertEquals(
        spanSnapshot(renamed.tree),
        Vector(
          ("TypeDef", 0, 5, 73),
          ("TypeBoundsTree", 16, 16, 73),
          ("SingletonTypeTree", 16, 16, 28),
          ("Ident", 16, 16, 23),
          ("RefinedTypeTree", 32, 32, 73),
          ("Ident", 32, 32, 38),
          ("TypeDef", 41, 46, 71),
          ("Select", 56, 64, 71),
          ("Ident", 56, 56, 63)
        )
      )
    }
  }

  test("preserves compact public classifications for malformed present edges") {
    withContext {
      val canonical = parseDeclaration(render(nameSets.head, rows.head))
      val Type.Refine(Some(base), List(alias: Defn.Type)) = canonical.bounds.hi.get: @unchecked
      val noLower = canonical.copy(bounds = canonical.bounds.copy(lo = None))
      val malformed = Vector(
        parseDeclaration("type Self >: String <: Nat { type Self = self.Self }") ->
          ("NEUTRAL_SELF_MEMBER_LOWER_BOUND_NOT_SINGLETON", "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER"),
        parseDeclaration("type Self >: other.type <: Nat { type Self = self.Self }") ->
          ("NEUTRAL_SELF_MEMBER_LOWER_ALIAS_MISMATCH", "NEUTRAL_PROJECTION_FAILED"),
        parseDeclaration("type Self >: self.type") ->
          ("NEUTRAL_SELF_MEMBER_UPPER_BOUND_MISSING", "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER"),
        parseDeclaration("type Self <: Other") ->
          ("NEUTRAL_SELF_MEMBER_UPPER_BASE_MISMATCH", "NEUTRAL_PROJECTION_FAILED"),
        parseDeclaration("type Self <: Nat[String]") ->
          ("NEUTRAL_SELF_MEMBER_UPPER_BASE_UNSUPPORTED", "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER"),
        parseDeclaration("type Self <: Nat[String] { type Self = self.Self }") ->
          ("NEUTRAL_SELF_MEMBER_UPPER_BASE_UNSUPPORTED", "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER"),
        parseDeclaration("type Self <: Nat {}") ->
          ("NEUTRAL_SELF_MEMBER_REFINEMENT_COUNT_UNSUPPORTED", "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER"),
        parseDeclaration("type Self <: Nat { type Self = self.Self; type Other = self.Other }") ->
          ("NEUTRAL_SELF_MEMBER_REFINEMENT_COUNT_UNSUPPORTED", "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER"),
        parseDeclaration("type Self <: Nat { val value: Int }") ->
          ("NEUTRAL_SELF_MEMBER_REFINEMENT_MEMBER_UNSUPPORTED", "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER"),
        noLower.copy(bounds = noLower.bounds.copy(hi = Some(Type.Refine(
          Some(base),
          List(alias.copy(bounds = alias.bounds.copy(lo = Some(Type.Name("Nothing")))))
        )))) ->
          ("NEUTRAL_SELF_MEMBER_REFINEMENT_ALIAS_BOUNDS_UNSUPPORTED", "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER"),
        noLower.copy(bounds = noLower.bounds.copy(hi = Some(Type.Refine(
          Some(base),
          List(alias.copy(mods = List(Mod.Final())))
        )))) ->
          ("NEUTRAL_SELF_MEMBER_REFINEMENT_MEMBER_UNSUPPORTED", "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER"),
        noLower.copy(bounds = noLower.bounds.copy(hi = Some(Type.Refine(
          Some(base),
          List(alias.copy(tparams = List(tparam"A")))
        )))) ->
          ("NEUTRAL_SELF_MEMBER_REFINEMENT_MEMBER_UNSUPPORTED", "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER"),
        parseDeclaration("type Self <: Nat { type Other = self.Other }") ->
          ("NEUTRAL_SELF_MEMBER_REFINEMENT_NAME_MISMATCH", "NEUTRAL_PROJECTION_FAILED"),
        parseDeclaration("type Self <: Nat { type Self = String }") ->
          ("NEUTRAL_SELF_MEMBER_REFINEMENT_RHS_UNSUPPORTED", "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER"),
        parseDeclaration("type Self <: Nat { type Self = other.Self }") ->
          ("NEUTRAL_SELF_MEMBER_SELECTED_PREFIX_MISMATCH", "NEUTRAL_PROJECTION_FAILED"),
        parseDeclaration("type Self <: Nat { type Self = self.Other }") ->
          ("NEUTRAL_SELF_MEMBER_SELECTED_MEMBER_MISMATCH", "NEUTRAL_PROJECTION_FAILED"),
        parseDeclaration("private type Self <: Nat") ->
          ("NEUTRAL_SELF_MEMBER_MODIFIERS_UNSUPPORTED", "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER"),
        parseDeclaration("type Self[A] <: Nat") ->
          ("NEUTRAL_SELF_MEMBER_TYPE_PARAMETERS_UNSUPPORTED", "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER"),
        canonical.copy(bounds = canonical.bounds.copy(context = List(Type.Name("Evidence")))) ->
          ("NEUTRAL_SELF_MEMBER_CONTEXT_VIEW_BOUNDS_UNSUPPORTED", "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER"),
        canonical.copy(bounds = canonical.bounds.copy(view = List(Type.Name("Evidence")))) ->
          ("NEUTRAL_SELF_MEMBER_CONTEXT_VIEW_BOUNDS_UNSUPPORTED", "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER")
      )
      malformed.foreach { case (declaration, (privateCode, publicCode)) =>
        assertFailure(declaration, nameSets.head, privateCode, publicCode)
      }
      assertFailure(canonical, Names("Other", "self", "Nat"), "NEUTRAL_SELF_MEMBER_OUTER_NAME_MISMATCH", "NEUTRAL_PROJECTION_FAILED")
      assertFailure(null, nameSets.head, "NEUTRAL_SELF_MEMBER_DECLARATION_MISSING", "INVALID_SCALAMETA_DECLARATION")
      Vector(
        Names("bad-name", "self", "Nat") -> "NEUTRAL_SELF_MEMBER_EXPECTED_MEMBER_INVALID",
        Names("Self", "type", "Nat") -> "NEUTRAL_SELF_MEMBER_EXPECTED_SELF_ALIAS_INVALID",
        Names("Self", "self", "bad-name") -> "NEUTRAL_SELF_MEMBER_EXPECTED_UPPER_BASE_INVALID"
      ).foreach { case (expectations, privateCode) =>
        assertFailure(canonical, expectations, privateCode, "INVALID_EXPECTATION")
      }
      assertFailureCode(canonical, nameSets.head, "Bad\nName.scala", "INVALID_VIRTUAL_SOURCE_NAME")
      assertFailureCode(canonical, nameSets.head, null, "INVALID_VIRTUAL_SOURCE_NAME")
    }
  }

  private def render(names: Names, row: Row): String =
    val lower = Option.when(row.lower)(s" >: ${names.selfAlias}.type").getOrElse("")
    val refinement = Option.when(row.fBound)(s" { type ${names.member} = ${names.selfAlias}.${names.member} }").getOrElse("")
    s"type ${names.member}$lower <: ${names.upperBase}$refinement"

  private def parseDeclaration(source: String): Decl.Type =
    Scala3(source).parse[Stat].get match
      case declaration: Decl.Type => declaration
      case other => fail(s"expected Decl.Type, found ${other.getClass.getSimpleName}")

  private def lower(declaration: Decl.Type, names: Names, virtualName: String)(using Context): SelfAbstractTypeMemberPeerBridge.Lowered =
    SelfAbstractTypeMemberPeerBridge
      .lower(declaration, names.member, names.selfAlias, names.upperBase, virtualName)
      .fold(failure => fail(s"${failure.code}: ${failure.detail}"), identity)

  private def assertFailure(
      declaration: Decl.Type,
      expectations: Names,
      expectedPrivateCode: String,
      expectedPublicCode: String
  )(using Context): Unit =
    val result = SelfAbstractTypeMemberPeerBridge.lower(
      declaration,
      expectations.member,
      expectations.selfAlias,
      expectations.upperBase,
      "Generated.scala"
    )
    val failure = result.left.toOption.getOrElse(fail("malformed input unexpectedly lowered"))
    assertEquals(failure.code, expectedPublicCode, clues(failure))
    assert(failure.detail.startsWith(s"$expectedPrivateCode: "), clues(failure.detail))

  private def assertFailureCode(
      declaration: Decl.Type,
      expectations: Names,
      virtualName: String,
      expectedCode: String
  )(using Context): Unit =
    val result = SelfAbstractTypeMemberPeerBridge.lower(
      declaration,
      expectations.member,
      expectations.selfAlias,
      expectations.upperBase,
      virtualName
    )
    val failure = result.left.toOption.getOrElse(fail("malformed input unexpectedly lowered"))
    assertEquals(failure.code, expectedCode, clues(failure))
    assert(failure.detail.nonEmpty, clues(failure))

  private def assertRow(definition: untpd.TypeDef, names: Names, row: Row): Unit =
    assertEquals(definition.name.toString, names.member)
    assert(!definition.mods.hasFlags)
    definition.rhs match
      case bounds: untpd.TypeBoundsTree =>
        assert(bounds.alias.isEmpty)
        if row.lower then
          bounds.lo match
            case untpd.SingletonTypeTree(untpd.Ident(alias)) => assertEquals(alias.toString, names.selfAlias)
            case other => fail(s"expected singleton lower, found $other")
        else assert(bounds.lo.isEmpty, clues(bounds.lo))
        if row.fBound then
          bounds.hi match
            case untpd.RefinedTypeTree(untpd.Ident(base), List(member: untpd.TypeDef)) =>
              assertEquals(base.toString, names.upperBase)
              assertEquals(member.name.toString, names.member)
              member.rhs match
                case untpd.Select(untpd.Ident(prefix), selected) =>
                  assertEquals(prefix.toString, names.selfAlias)
                  assertEquals(selected.toString, names.member)
                case other => fail(s"expected selected RHS, found $other")
            case other => fail(s"expected refined upper, found $other")
        else
          bounds.hi match
            case untpd.Ident(base) => assertEquals(base.toString, names.upperBase)
            case other => fail(s"expected direct upper base, found $other")
      case other => fail(s"expected TypeBoundsTree, found $other")

  private def assertPositioned(
      root: untpd.TypeDef,
      source: String,
      virtualName: String,
      nodeCount: Int
  )(using Context): Unit =
    val trees = nonEmptyTrees(root)
    assertEquals(trees.size, nodeCount)
    trees.foreach { tree =>
      assert(tree.source.exists, clues(tree))
      assert(tree.source eq root.source, clues(tree))
      assertEquals(tree.source.path, virtualName, clues(tree))
      assertEquals(tree.source.content.mkString, source, clues(tree))
      assert(tree.span.exists, clues(tree))
      assert(tree.span.start >= 0, clues(tree))
      assert(tree.span.start <= tree.span.point, clues(tree))
      assert(tree.span.point <= tree.span.end, clues(tree))
      assert(tree.span.end <= source.length, clues(tree))
      assertEquals(tree.symbol, NoSymbol, clues(tree))
      assert(!tree.isInstanceOf[untpd.TypedSplice], clues(tree))
      directChildren(tree).foreach { child =>
        assert(child.span.start >= tree.span.start, clues(child))
        assert(child.span.end <= tree.span.end, clues(child))
      }
    }

  private def nonEmptyTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty else tree +: directChildren(tree).flatMap(nonEmptyTrees)

  private def spanSnapshot(tree: untpd.Tree)(using Context): Vector[(String, Int, Int, Int)] =
    nonEmptyTrees(tree).map(current =>
      (current.getClass.getSimpleName, current.span.start, current.span.point, current.span.end)
    )

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.TypeBoundsTree => Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.SingletonTypeTree => Vector(value.ref)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
