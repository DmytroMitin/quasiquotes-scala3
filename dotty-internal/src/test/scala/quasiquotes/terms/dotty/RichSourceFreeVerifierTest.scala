package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.util.{NoSource, SourceFile}

final class RichSourceFreeVerifierTest extends munit.FunSuite:
  test("shared verifier recursively rejects contaminated richer Term nodes"):
    withContext:
      given SourceFile = NoSource
      val symbol = newSymbol(NoSymbol, termName("bad"), EmptyFlags, NoType)
      val contaminated = untpd.Ident(termName("bad")).withType(symbol.termRef)
      val cleanType = untpd.Ident(typeName("Int"))

      val richer = List[untpd.Tree](
        untpd.Function(Nil, contaminated),
        untpd.ValDef(termName("x"), contaminated, untpd.EmptyTree),
        untpd.DefDef(termName("f"), Nil, cleanType, contaminated),
        untpd.Parens(contaminated)
      )

      richer.foreach { tree =>
        val failure = CoreTermShapeUntypedLowerer
          .verifySourceFreeForTest(tree)
          .left
          .toOption
          .getOrElse(fail(s"contaminated richer node unexpectedly passed: $tree"))
        assertEquals(
          failure,
          CoreTermShapeUntypedLowererError.SourceFreeInvariantViolation(
            "Ident",
            "the node has a symbol."
          )
        )
      }

  test("shared verifier rejects null descendants of richer definitions"):
    withContext:
      given SourceFile = NoSource
      val malformed = List[untpd.Tree](
        untpd.Function(Nil, null),
        untpd.ValDef(termName("x"), null, untpd.EmptyTree),
        untpd.DefDef(termName("f"), Nil, null, untpd.EmptyTree),
        untpd.Parens(null)
      )

      malformed.foreach { tree =>
        val failure = CoreTermShapeUntypedLowerer
          .verifySourceFreeForTest(tree)
          .left
          .toOption
          .getOrElse(fail(s"null richer descendant unexpectedly passed: $tree"))
        assertEquals(
          failure,
          CoreTermShapeUntypedLowererError.SourceFreeInvariantViolation(
            "null",
            "the node is null."
          )
        )
      }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
