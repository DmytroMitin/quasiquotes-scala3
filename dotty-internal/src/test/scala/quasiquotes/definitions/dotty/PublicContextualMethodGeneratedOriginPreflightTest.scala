package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

class PublicContextualMethodGeneratedOriginPreflightTest extends munit.FunSuite:
  private val source =
    "def apply[A](using instance: Show[A]): Show[A] = instance"

  test("records parser spans for the exact bounded public contextual method") {
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)
    val sourceFile =
      SourceFile.virtual(
        "PublicContextualMethodGeneratedOriginPreflight.scala",
        source
      )
    val parsed = new Parser(sourceFile).parse()

    assertEquals(reporter.pendingMessages.toList, Nil)
    val method = parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        packageDef.stats.head.asInstanceOf[untpd.DefDef]
      case other =>
        fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

    val typeParameter = method.leadingTypeParams match
      case (value: untpd.TypeDef) :: Nil => value
      case other => fail(s"expected one TypeDef, found $other")
    val contextualParameter = method.trailingParamss match
      case List(List(value: untpd.ValDef)) => value
      case other => fail(s"expected one contextual ValDef clause, found $other")

    println(
      s"CONTEXTUAL_METHOD_POSITION_PREFLIGHT source=$source length=${source.length} path=${sourceFile.path}"
    )
    observedTrees(method).foreach { case (role, tree) =>
      println(
        s"CONTEXTUAL_METHOD_POSITION_PREFLIGHT role=$role tree=${tree.getClass.getSimpleName} ${spanSummary(tree)} source=${tree.source.exists}:${tree.source.path} slice=${slice(tree)}"
      )
    }

    assertEquals(method.name.toString, "apply")
    assertEquals(method.mods.flags, Flags.Method)
    assertEquals(method.paramss.map(_.size), List(1, 1))
    assertEquals(typeParameter.name.toString, "A")
    assertEquals(typeParameter.mods.flags, Flags.Param)
    typeParameter.rhs match
      case untpd.WildcardTypeBoundsTree() => ()
      case other => fail(s"expected wildcard bounds, found $other")
    assertEquals(contextualParameter.name.toString, "instance")
    assertEquals(contextualParameter.mods.flags, Flags.Param | Flags.Given)
    assert(contextualParameter.rhs.isEmpty)
    assertEquals(method.span.start, 0)
    assertEquals(method.span.end, source.length)
  }

  private def observedTrees(
      method: untpd.DefDef
  )(using Context): Vector[(String, untpd.Tree)] =
    val typeParameter = method.leadingTypeParams.head
    val contextualParameter = method.trailingParamss.head.head
    Vector("root" -> method) ++
      walk("typeParameter", typeParameter) ++
      walk("contextualParameter", contextualParameter) ++
      walk("resultType", method.tpt) ++
      walk("body", method.rhs)

  private def walk(
      role: String,
      tree: untpd.Tree
  )(using Context): Vector[(String, untpd.Tree)] =
    Vector(role -> tree) ++ directChildren(tree).zipWithIndex.flatMap {
      case (child, index) => walk(s"$role.child$index", child)
    }

  private def directChildren(
      tree: untpd.Tree
  )(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs)
      case value: untpd.TypeBoundsTree => Vector(value.lo, value.hi)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case _ => Vector.empty

  private def spanSummary(tree: untpd.Tree): String =
    if tree.span.exists then
      s"span=${tree.span.start}..${tree.span.point}..${tree.span.end}"
    else "span=none"

  private def slice(tree: untpd.Tree): String =
    if tree.span.exists && tree.span.start >= 0 && tree.span.end <= source.length
    then source.slice(tree.span.start, tree.span.end)
    else "<none>"
