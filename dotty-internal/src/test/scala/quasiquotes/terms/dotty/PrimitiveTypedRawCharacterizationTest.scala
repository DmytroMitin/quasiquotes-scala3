package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.parser.TinyTermParser

class PrimitiveTypedRawCharacterizationTest extends munit.FunSuite:
  private val fixtures = Vector(
    "(x: Int)" -> "Parens(Typed(Ident(x),Ident(Int)))",
    "(\"x\": String)" -> "Parens(Typed(Literal(String(\"x\")),Ident(String)))",
    "(true: Boolean)" ->
      "Parens(Typed(Literal(Boolean(true)),Ident(Boolean)))",
    "(f(1): Int)" ->
      "Parens(Typed(Apply(Ident(f), [Number(1,Whole(10))]),Ident(Int)))",
    "((1, 2): String)" ->
      "Parens(Typed(Tuple([Number(1,Whole(10)), Number(2,Whole(10))]),Ident(String)))"
  )

  fixtures.foreach { case (source, expectedStructure) =>
    test(s"characterizes primitive Typed parser topology: $source") {
      given Context = new ContextBase().initialCtx
      val parsed = TinyTermParser.parseOrThrow(source)

      assertEquals(parsed.rawStructure, expectedStructure)
      parsed.rawTree match
        case untpd.Parens(typed: untpd.Typed) =>
          typed.tpt match
            case untpd.Ident(name) =>
              assert(Set("Int", "String", "Boolean").contains(name.toString))
              assert(name.isTypeName)
            case other =>
              fail(s"expected primitive type Ident, found ${other.getClass.getSimpleName}")
        case other =>
          fail(s"expected Parens(Typed), found ${other.getClass.getSimpleName}")

      allTrees(parsed.rawTree).foreach { tree =>
        assert(tree.span.exists, clues(source, tree.getClass.getSimpleName))
        assert(!tree.source.exists, clues(source, tree.getClass.getSimpleName))
        assertEquals(tree.symbol, NoSymbol, clues(source, tree.getClass.getSimpleName))
        assert(!tree.isInstanceOf[untpd.TypedSplice])
      }
    }
  }

  private def allTrees(tree: untpd.Tree): Vector[untpd.Tree] =
    tree match
      case untpd.Typed(expression, typeTree) =>
        tree +: (allTrees(expression) ++ allTrees(typeTree))
      case untpd.Apply(function, arguments) =>
        tree +: (allTrees(function) ++ arguments.toVector.flatMap(allTrees))
      case untpd.Tuple(elements) => tree +: elements.toVector.flatMap(allTrees)
      case untpd.Parens(inner) => tree +: allTrees(inner)
      case _ => Vector(tree)
