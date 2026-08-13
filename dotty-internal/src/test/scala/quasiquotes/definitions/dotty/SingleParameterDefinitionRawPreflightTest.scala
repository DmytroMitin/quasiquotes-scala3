package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

class SingleParameterDefinitionRawPreflightTest extends munit.FunSuite:
  private val fixtures = Vector(
    "def id(x: Int): Int = x",
    "def inc(x: Int): Int = x + 1",
    "def keep(x: String): String = x",
    "def choose(x: Boolean): Boolean = if x then false else true",
    "def ascribed(x: Int): Int = (x: Int)",
    "def built(x: Int): String = new java.lang.StringBuilder(x).toString"
  )

  fixtures.foreach { source =>
    test(s"records the raw single-parameter definition oracle: $source") {
      val base = new ContextBase
      val reporter = new StoreReporter(null)
      given Context = base.initialCtx.fresh.setReporter(reporter)
      val sourceFile = SourceFile.virtual("SingleParameterDefinitionRawPreflight.scala", source)
      val parsed = new Parser(sourceFile).parse()
      assertEquals(reporter.pendingMessages.toList, Nil)

      val method = parsed match
        case packageDef: untpd.PackageDef =>
          assertEquals(packageDef.stats.size, 1)
          packageDef.stats.head.asInstanceOf[untpd.DefDef]
        case other =>
          fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

      assertEquals(method.paramss.map(_.size), List(1))
      val parameter = method.paramss.head.head.asInstanceOf[untpd.ValDef]
      assert(parameter.rhs.isEmpty)
      assertEquals(method.symbol, NoSymbol)
      allDefinitionTrees(method).foreach(tree =>
        assertEquals(tree.symbol, NoSymbol)
      )

      val children = GeneratedOriginFragmentSupport.directChildren(method)
      println(s"SINGLE_PARAMETER_DEF_RAW_ORACLE source=$source")
      println(
        s"SINGLE_PARAMETER_DEF_RAW_ORACLE root=${summary(method, source)} flags=${method.mods.flags} paramClauses=${method.paramss.map(_.size)} children=${children.map(_.getClass.getSimpleName)}"
      )
      println(
        s"SINGLE_PARAMETER_DEF_RAW_ORACLE parameter=${summary(parameter, source)} flags=${parameter.mods.flags} rhsEmpty=${parameter.rhs.isEmpty}"
      )
      allDefinitionTrees(method).foreach(tree =>
        println(
          s"SINGLE_PARAMETER_DEF_RAW_ORACLE node=${summary(tree, source)} noSymbol=${tree.symbol == NoSymbol}"
        )
      )
    }
  }

  private def summary(tree: untpd.Tree, source: String): String =
    val span = tree.span
    val slice =
      if span.exists then source.slice(span.start, span.end)
      else "<none>"
    val detail = tree match
      case method: untpd.DefDef => s" name=${method.name}"
      case value: untpd.ValDef => s" name=${value.name}"
      case value: untpd.Ident => s" name=${value.name}"
      case value: untpd.Select => s" name=${value.name}"
      case _ => ""
    s"${tree.getClass.getName}$detail span=${span.start}..${span.point}..${span.end} slice=$slice"

  private def allDefinitionTrees(method: untpd.DefDef)(using Context): List[untpd.Tree] =
    List(method) ++
      method.paramss.flatten.flatMap(GeneratedOriginFragmentSupport.allTrees) ++
      GeneratedOriginFragmentSupport.allTrees(method.tpt) ++
      GeneratedOriginFragmentSupport.allTrees(method.rhs)
