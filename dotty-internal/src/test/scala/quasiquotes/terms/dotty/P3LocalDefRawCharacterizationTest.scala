package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.parser.TinyTermParser

class P3LocalDefRawCharacterizationTest extends munit.FunSuite:
  private final case class Fixture(
      method: String,
      parameter: String,
      tpe: String
  ):
    val source =
      s"{ def $method($parameter: $tpe): $tpe = $parameter; $method }"

  private val fixtures = Vector(
    Fixture("id", "x", "Int"),
    Fixture("id", "x", "String"),
    Fixture("id", "x", "Boolean"),
    Fixture("renamed", "argument", "Int")
  )

  fixtures.foreach { fixture =>
    test(s"pins the raw P3 parser oracle: ${fixture.source}") {
      val base = new ContextBase
      given Context = base.initialCtx
      val parsed = TinyTermParser.parseOrThrow(fixture.source)
      val block = parsed.rawTree.asInstanceOf[untpd.Block]

      assertEquals(block.stats.size, 1)
      val method = block.stats.head.asInstanceOf[untpd.DefDef]
      val finalReference = block.expr.asInstanceOf[untpd.Ident]
      assertEquals(method.name.toString, fixture.method)
      assertEquals(method.mods.flags, Flags.Method)
      assertEquals(method.leadingTypeParams, Nil)
      assertEquals(method.paramss.map(_.size), List(1))
      assertEquals(method.trailingParamss.map(_.size), List(1))

      val parameter = method.paramss.head.head.asInstanceOf[untpd.ValDef]
      assertEquals(parameter.name.toString, fixture.parameter)
      assertEquals(parameter.mods.flags, Flags.Param)
      assert(parameter.rhs.isEmpty)
      assertTypeIdent(parameter.tpt, fixture.tpe)
      assertTypeIdent(method.tpt, fixture.tpe)
      assertIdent(method.rhs, fixture.parameter)
      assertIdent(finalReference, fixture.method)

      val expectedSlices = Vector(
        block -> fixture.source,
        method -> s"def ${fixture.method}(${fixture.parameter}: ${fixture.tpe}): ${fixture.tpe} = ${fixture.parameter}",
        parameter -> s"${fixture.parameter}: ${fixture.tpe}",
        parameter.tpt -> fixture.tpe,
        method.tpt -> fixture.tpe,
        method.rhs -> fixture.parameter,
        finalReference -> fixture.method
      )
      expectedSlices.foreach { case (tree, expected) =>
        assert(tree.span.exists)
        assertEquals(
          fixture.source.slice(tree.span.start, tree.span.end),
          expected,
          clues(tree.getClass.getSimpleName, tree.span)
        )
        assert(tree.span.start <= tree.span.point)
        assert(tree.span.point <= tree.span.end)
        assertEquals(tree.source.path, block.source.path)
        assertEquals(tree.symbol, NoSymbol)
      }

      assertEquals(block.span.point, 0)
      assertEquals(method.span.point, fixture.source.indexOf(fixture.method))
      assertEquals(parameter.span.point, fixture.source.indexOf(fixture.parameter))
      assertEquals(parameter.tpt.span.point, fixture.source.indexOf(fixture.tpe))
      assertEquals(
        method.tpt.span.point,
        fixture.source.indexOf(fixture.tpe, parameter.tpt.span.end)
      )
      assertEquals(method.rhs.span.point, fixture.source.lastIndexOf(fixture.parameter))
      assertEquals(finalReference.span.point, fixture.source.lastIndexOf(fixture.method))

      val allTrees =
        Vector(block, method, parameter, parameter.tpt, method.tpt, method.rhs, finalReference)
      assert(allTrees.forall(_.symbol == NoSymbol))
    }
  }

  private def assertTypeIdent(tree: untpd.Tree, expected: String): Unit =
    tree match
      case value: untpd.Ident => assertEquals(value.name.toString, expected)
      case other => fail(s"expected type Ident($expected), found $other")

  private def assertIdent(tree: untpd.Tree, expected: String): Unit =
    tree match
      case value: untpd.Ident => assertEquals(value.name.toString, expected)
      case other => fail(s"expected Ident($expected), found $other")
