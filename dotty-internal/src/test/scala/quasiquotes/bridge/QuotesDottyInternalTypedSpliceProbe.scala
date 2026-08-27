package quasiquotes.bridge

import scala.quoted.*
import scala.quoted.runtime.impl.QuotesImpl

import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.toTermName
import dotty.tools.dotc.typer.Typer

/** Test-only feasibility probe; this is deliberately not a supported bridge. */
object QuotesDottyInternalTypedSpliceProbe:
  inline def add(left: Int, right: Int): Int = ${ addImpl('left, 'right) }

  private def addImpl(left: Expr[Int], right: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*

    given Context = q.asInstanceOf[QuotesImpl].ctx

    val shell = untpd.Apply(
      untpd.Select(
        untpd.TypedSplice(left.asTerm.asInstanceOf[tpd.Tree]),
        "+".toTermName
      ),
      untpd.TypedSplice(right.asTerm.asInstanceOf[tpd.Tree]) :: Nil
    )
    val typed = new Typer().typedExpr(shell)

    typed.asInstanceOf[q.reflect.Term].asExprOf[Int]
