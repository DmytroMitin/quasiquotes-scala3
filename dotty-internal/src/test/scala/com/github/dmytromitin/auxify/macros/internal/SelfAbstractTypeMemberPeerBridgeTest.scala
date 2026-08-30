package com.github.dmytromitin.auxify.macros.internal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.dotty.SelfAbstractTypeMemberPeerBridge

import scala.meta.*

class SelfAbstractTypeMemberPeerBridgeTest extends munit.FunSuite:
  test("foreign AUXify package receives canonical and collision-renamed insertion-ready TypeDefs") {
    withContext {
      List(
        ("Self", "self", "Nat", "AuxifyGeneratedSelf.scala"),
        ("Element", "owner$2", "Domain", "AuxifyGeneratedElement.scala")
      ).foreach { case (member, alias, base, virtualSourceName) =>
        val declaration = targetDeclaration(member, alias, base)
        val lowered: SelfAbstractTypeMemberPeerBridge.Lowered =
          SelfAbstractTypeMemberPeerBridge
            .lower(declaration, member, alias, base, virtualSourceName)
            .fold(failure => fail(s"${failure.code}: ${failure.detail}"), identity)

        assertEquals(
          lowered.generatedSource,
          s"type $member >: $alias.type <: $base { type $member = $alias.$member }"
        )
        assertEquals(lowered.virtualSourceName, virtualSourceName)
        assertExactTypeDef(lowered.tree, member, alias, base)
        val trees = nonEmptyTrees(lowered.tree)
        assertEquals(trees.size, 9)
        trees.foreach { tree =>
          assert(tree.source.exists, clues(tree))
          assertEquals(tree.source.path, virtualSourceName, clues(tree))
          assertEquals(tree.source.content.mkString, lowered.generatedSource, clues(tree))
          assert(tree.span.exists, clues(tree))
          assert(tree.span.start >= 0, clues(tree))
          assert(tree.span.start <= tree.span.point, clues(tree))
          assert(tree.span.point <= tree.span.end, clues(tree))
          assert(tree.span.end <= lowered.generatedSource.length, clues(tree))
          assertEquals(tree.symbol, NoSymbol, clues(tree))
          assert(!tree.isInstanceOf[untpd.TypedSplice], clues(tree))
          directChildren(tree).foreach { child =>
            assert(child.span.start >= tree.span.start, clues(child))
            assert(child.span.end <= tree.span.end, clues(child))
          }
        }
      }
    }
  }

  test("foreign AUXify package receives compact deterministic boundary failures") {
    withContext {
      val canonical = targetDeclaration("Self", "self", "Nat")
      assertFailure(null, "Self", "self", "Nat", "Generated.scala", "INVALID_SCALAMETA_DECLARATION")
      assertFailure(canonical, "bad-name", "self", "Nat", "Generated.scala", "INVALID_EXPECTATION")
      assertFailure(canonical, "Self", "self$01", "Nat", "Generated.scala", "INVALID_EXPECTATION")
      assertFailure(
        targetDeclaration("Self", "other", "Nat"),
        "Self",
        "self",
        "Nat",
        "Generated.scala",
        "NEUTRAL_PROJECTION_FAILED"
      )
      assertFailure(
        q"type Self <: Nat { type Self = self.Self }".asInstanceOf[Decl.Type],
        "Self",
        "self",
        "Nat",
        "Generated.scala",
        "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER"
      )
      assertFailure(canonical, "Self", "self", "Nat", "Bad\nName.scala", "INVALID_VIRTUAL_SOURCE_NAME")
      assertFailure(canonical, "Self", "self", "Nat", null, "INVALID_VIRTUAL_SOURCE_NAME")
    }
  }

  private def targetDeclaration(
      member: String,
      alias: String,
      base: String
  ): Decl.Type =
    val memberName = Type.Name(member)
    val aliasName = Term.Name(alias)
    val baseName = Type.Name(base)
    val lower: Type = t"$aliasName.type"
    val selected: Type = t"$aliasName.$memberName"
    val equality: Defn.Type = q"type $memberName = $selected"
    val statistics: List[Stat] = equality :: Nil
    val upper: Type = t"$baseName { ..$statistics }"
    q"type $memberName >: $lower <: $upper"

  private def assertFailure(
      declaration: Decl.Type,
      member: String,
      alias: String,
      base: String,
      virtualSourceName: String,
      expectedCode: String
  )(using Context): Unit =
    val result = SelfAbstractTypeMemberPeerBridge.lower(
      declaration,
      member,
      alias,
      base,
      virtualSourceName
    )
    val failure = result.left.toOption.getOrElse(fail("malformed input unexpectedly lowered"))
    assertEquals(failure.code, expectedCode, clues(failure))
    assert(failure.detail.nonEmpty, clues(failure))

  private def assertExactTypeDef(
      definition: untpd.TypeDef,
      member: String,
      alias: String,
      base: String
  ): Unit =
    assertEquals(definition.name.toString, member)
    assert(!definition.mods.hasFlags)
    definition.rhs match
      case untpd.TypeBoundsTree(
            untpd.SingletonTypeTree(untpd.Ident(lowerAlias)),
            untpd.RefinedTypeTree(
              untpd.Ident(upperBase),
              List(refinementMember: untpd.TypeDef)
            ),
            boundsAlias
          ) =>
        assertEquals(lowerAlias.toString, alias)
        assertEquals(upperBase.toString, base)
        assert(boundsAlias.isEmpty)
        assertEquals(refinementMember.name.toString, member)
        refinementMember.rhs match
          case untpd.Select(untpd.Ident(prefix), selected) =>
            assertEquals(prefix.toString, alias)
            assertEquals(selected.toString, member)
          case other => fail(s"expected direct selected-Type alias RHS, found $other")
      case other => fail(s"expected exact bounded refined TypeDef, found $other")

  private def nonEmptyTrees(
      tree: untpd.Tree
  )(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(nonEmptyTrees)

  private def directChildren(
      tree: untpd.Tree
  )(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.SingletonTypeTree => Vector(value.ref)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
