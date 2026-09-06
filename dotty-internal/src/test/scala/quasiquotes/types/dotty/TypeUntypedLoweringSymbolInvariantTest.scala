package quasiquotes.types.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.types.TypeNormalForm
import quasiquotes.types.TypeNormalForm.*

final class TypeUntypedLoweringSymbolInvariantTest extends munit.FunSuite:
  test("fresh context-free result has NoSymbol when inspected in a Context"):
    withContext {
      val semantic = STypeFunction(
        List(STypeTuple(List(STypeIdent("Int"), STypeIdent("String")))),
        STypeApply(
          STypeIdent("Either"),
          List(STypeIdent("Boolean"), STypeIdent("Int"))
        )
      )
      val raw = TypeUntypedLowering
        .lower(semantic)
        .fold(problem => fail(problem.message), identity)

      allTrees(raw).foreach(node => assertEquals(node.symbol, NoSymbol, clues(node)))
    }

  private def allTrees(tree: untpd.Tree): List[untpd.Tree] =
    tree match
      case untpd.AppliedTypeTree(constructor, arguments) =>
        tree :: allTrees(constructor) ::: arguments.flatMap(allTrees)
      case untpd.Tuple(elements) => tree :: elements.flatMap(allTrees)
      case untpd.Function(arguments, result) =>
        tree :: arguments.flatMap(allTrees) ::: allTrees(result)
      case _ => tree :: Nil

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
