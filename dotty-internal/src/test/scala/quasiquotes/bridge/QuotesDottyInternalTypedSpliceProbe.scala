package quasiquotes.bridge

import scala.quoted.*
import scala.quoted.runtime.impl.QuotesImpl

import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.toTermName
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.typer.Typer
import dotty.tools.dotc.util.SourceFile

/** Test-only feasibility probe; this is deliberately not a supported bridge. */
object QuotesDottyInternalTypedSpliceProbe:
  inline def add(left: Int, right: Int): Int = ${ addImpl('left, 'right) }

  inline def addTypedOverloaded(left: Int, right: Int): Int =
    ${ addTypedOverloadedImpl('left, 'right) }

  def addUntpdTree(
      left: untpd.Tree,
      right: untpd.Tree
  )(using SourceFile): untpd.Tree =
    untpd.Apply(
      untpd.Select(left, "+".toTermName),
      right :: Nil
    )

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

  private def addTypedOverloadedImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using q: Quotes): Expr[Int] =
    import q.reflect.*

    given Context = q.asInstanceOf[QuotesImpl].ctx

    val typed = tpd.applyOverloaded(
      left.asTerm.asInstanceOf[tpd.Tree],
      "+".toTermName,
      right.asTerm.asInstanceOf[tpd.Tree] :: Nil,
      Nil,
      Types.WildcardType
    )

    typed.asInstanceOf[q.reflect.Term].asExprOf[Int]
