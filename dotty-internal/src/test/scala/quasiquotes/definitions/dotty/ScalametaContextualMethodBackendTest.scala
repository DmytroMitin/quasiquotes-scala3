package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
class ScalametaContextualMethodBackendTest extends munit.FunSuite:
  private val CanonicalSource =
    "def apply[A](using inst: Show[A]): Show[A] = inst"

  test("lowers Scalameta Show.apply through validated IR to exact positioned untpd") {
    withContext {
      val neutral =
        q"def apply[A](using inst: Show[A]): Show[A] = inst".asInstanceOf[Defn.Def]

      val lowered = lower(neutral)
      val raw = lowered.tree.asInstanceOf[untpd.DefDef]

      assertEquals(lowered.generatedSource, CanonicalSource)
      assertEquals(raw.name.toString, "apply")
      assertEquals(raw.mods.flags, Flags.Method)
      assertEquals(raw.paramss.map(_.size), List(1, 1))
      assertEquals(raw.leadingTypeParams.map(_.name.toString), List("A"))
      val contextual = raw.trailingParamss.head.head.asInstanceOf[untpd.ValDef]
      assertEquals(contextual.name.toString, "inst")
      assertEquals(contextual.mods.flags, Flags.Param | Flags.Given)
      assertApplied(contextual.tpt, "Show", "A")
      assertApplied(raw.tpt, "Show", "A")
      raw.rhs match
        case untpd.Ident(name) => assertEquals(name.toString, "inst")
        case other => fail(s"expected body Ident(inst), found $other")
      assert(raw.source.exists)
      assert(raw.span.exists)
      assertEquals(raw.source.path, "NeutralShowApply.scala")
    }
  }

  test("projects exact untpd structurally to no-position Scalameta for matching") {
    withContext {
      val source =
        q"def apply[A](using inst: Show[A]): Show[A] = inst".asInstanceOf[Defn.Def]
      val raw = lower(source).tree.asInstanceOf[untpd.DefDef]

      val neutral = ScalametaContextualMethodBackend
        .project(raw)
        .fold(error => fail(error.message), identity)

      assertEquals(neutral.pos, Position.None)
      val matched = neutral match
        case q"def $name[..$tparams](...$paramss): $result = $body" =>
          name.value == "apply" &&
            tparams.map(_.name.value) == List("A") &&
            paramss.map(_.size) == List(1) &&
            paramss.head.head.name.value == "inst" &&
            result.exists(_.syntax == "Show[A]") &&
            body.syntax == "inst"
        case _ => false
      assert(matched, clues(neutral.structure))

      val group = neutral.paramClauseGroups.head
      assert(group.paramClauses.head.mod.exists(_.isInstanceOf[Mod.Using]))
      assertEquals(
        group.paramClauses.head.values.head.decltpe.map(_.syntax),
        Some("Show[A]")
      )
    }
  }

  test("rejects unsupported exact raw forms explicitly") {
    withContext {
      val source =
        q"def apply[A](using inst: Show[A]): Show[A] = inst".asInstanceOf[Defn.Def]
      val unsupported = lower(source).tree
        .asInstanceOf[untpd.DefDef]
        .withMods(untpd.Modifiers(Flags.EmptyFlags))
        .asInstanceOf[untpd.DefDef]

      assertEquals(
        ScalametaContextualMethodBackend.project(unsupported).left.toOption.map(_.code),
        Some("EXACT_METHOD_SHAPE_UNSUPPORTED")
      )
    }
  }

  private def lower(
      neutral: Defn.Def
  )(using Context): GeneratedOriginDefinitionResult =
    ScalametaContextualMethodBackend
      .lower(neutral, "NeutralShowApply.scala")
      .fold(error => fail(error.message), identity)

  private def assertApplied(
      tree: untpd.Tree,
      constructor: String,
      argument: String
  ): Unit =
    tree match
      case untpd.AppliedTypeTree(
            untpd.Ident(actualConstructor),
            List(untpd.Ident(actualArgument))
          ) =>
        assertEquals(actualConstructor.toString, constructor)
        assertEquals(actualArgument.toString, argument)
      case other =>
        fail(s"expected AppliedTypeTree($constructor, $argument), found $other")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
