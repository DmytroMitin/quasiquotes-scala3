package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

class TwoParameterDefinitionRawPreflightTest extends munit.FunSuite:
  private val fixtures = Vector(
    "def first(x: Int, y: Int): Int = x",
    "def second(x: Int, y: Int): Int = y",
    "def mixed(x: Int, y: String): Int = x",
    "def plus(x: Int, y: Int): Int = x + y"
  )

  fixtures.foreach { source =>
    test(s"records the raw exact-two definition oracle: $source") {
      val base = new ContextBase
      val reporter = new StoreReporter(null)
      given Context = base.initialCtx.fresh.setReporter(reporter)
      val sourceFile = SourceFile.virtual("TwoParameterDefinitionRawPreflight.scala", source)
      val parsed = new Parser(sourceFile).parse()
      assertEquals(reporter.pendingMessages.toList, Nil)

      val method = parsed match
        case packageDef: untpd.PackageDef =>
          assertEquals(packageDef.stats.size, 1)
          packageDef.stats.head.asInstanceOf[untpd.DefDef]
        case other =>
          fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

      assertEquals(method.paramss.map(_.size), List(2))
      assertEquals(method.mods.flags, Flags.Method)
      val parameters = method.paramss.head.map(_.asInstanceOf[untpd.ValDef])
      assertEquals(parameters.map(_.name.toString), List("x", "y"))
      parameters.foreach { parameter =>
        assert(parameter.rhs.isEmpty)
        assertEquals(parameter.mods.flags, Flags.Param)
      }
      allDefinitionTrees(method).foreach(tree => assertEquals(tree.symbol, NoSymbol))

      val open = source.indexOf('(')
      val comma = source.indexOf(", ", open)
      val close = source.indexOf(')', comma)
      val resultStart = source.indexOf(": ", close) + 2
      val bodyStart = source.indexOf(" = ", resultStart) + 3
      assertSpan(method, 0, 4, source.length)
      assertSpan(parameters(0), open + 1, open + 1, comma)
      assertSpan(parameters(1), comma + 2, comma + 2, close)
      assertSpan(
        parameters(0).tpt,
        source.indexOf(": ", open) + 2,
        source.indexOf(": ", open) + 2,
        comma
      )
      assertSpan(
        parameters(1).tpt,
        source.indexOf(": ", comma) + 2,
        source.indexOf(": ", comma) + 2,
        close
      )
      assertSpan(method.tpt, resultStart, resultStart, source.indexOf(" = ", resultStart))
      val expectedBodyPoint =
        if source.contains(" + ") then source.indexOf(" + ", bodyStart) + 1
        else bodyStart
      assertSpan(method.rhs, bodyStart, expectedBodyPoint, source.length)

      val children = GeneratedOriginFragmentSupport.directChildren(method)
      println(s"TWO_PARAMETER_DEF_RAW_ORACLE source=$source")
      println(
        s"TWO_PARAMETER_DEF_RAW_ORACLE root=${summary(method, source)} flags=${method.mods.flags} paramClauses=${method.paramss.map(_.size)} children=${children.map(_.getClass.getSimpleName)}"
      )
      parameters.zipWithIndex.foreach { case (parameter, index) =>
        println(
          s"TWO_PARAMETER_DEF_RAW_ORACLE parameter=$index ${summary(parameter, source)} flags=${parameter.mods.flags} rhsEmpty=${parameter.rhs.isEmpty} type=${summary(parameter.tpt, source)}"
        )
      }
      println(
        s"TWO_PARAMETER_DEF_RAW_ORACLE resultType=${summary(method.tpt, source)} body=${summary(method.rhs, source)}"
      )
      allDefinitionTrees(method).foreach(tree =>
        println(
          s"TWO_PARAMETER_DEF_RAW_ORACLE node=${summary(tree, source)} noSymbol=${tree.symbol == NoSymbol}"
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
      case _ => ""
    s"${tree.getClass.getName}$detail span=${span.start}..${span.point}..${span.end} slice=$slice"

  private def assertSpan(
      tree: untpd.Tree,
      expectedStart: Int,
      expectedPoint: Int,
      expectedEnd: Int
  ): Unit =
    assertEquals(
      (tree.span.start, tree.span.point, tree.span.end),
      (expectedStart, expectedPoint, expectedEnd)
    )

  private def allDefinitionTrees(method: untpd.DefDef)(using Context): List[untpd.Tree] =
    List(method) ++
      method.paramss.flatten.flatMap(GeneratedOriginFragmentSupport.allTrees) ++
      GeneratedOriginFragmentSupport.allTrees(method.tpt) ++
      GeneratedOriginFragmentSupport.allTrees(method.rhs)
