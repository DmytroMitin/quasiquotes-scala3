package quasiquotes.terms.dotty

import scala.quoted.*
import scala.quoted.runtime.impl.QuotesImpl

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.typer.Typer
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.parser.TermShape

private[quasiquotes] object CoreTermShapeUntypedLowererTyperProbe:
  inline def onePlusOne: Int = ${ onePlusOneImpl }

  inline def nestedPrecedence: Int = ${ nestedPrecedenceImpl }

  inline def negativePlusTwo: Int = ${ negativePlusTwoImpl }

  private def onePlusOneImpl(using Quotes): Expr[Int] =
    typeAndReturn(
      TermShape.Infix(
        TermShape.Literal("1"),
        "+",
        TermShape.Literal("1")
      )
    )

  private def nestedPrecedenceImpl(using Quotes): Expr[Int] =
    typeAndReturn(
      TermShape.Infix(
        TermShape.Literal("1"),
        "+",
        TermShape.Infix(
          TermShape.Literal("2"),
          "*",
          TermShape.Literal("3")
        )
      )
    )

  private def negativePlusTwoImpl(using Quotes): Expr[Int] =
    typeAndReturn(
      TermShape.Infix(
        TermShape.Literal("-1"),
        "+",
        TermShape.Literal("2")
      )
    )

  private def typeAndReturn(shape: TermShape)(using q: Quotes): Expr[Int] =
    import q.reflect.*

    given Context = q.asInstanceOf[QuotesImpl].ctx
    val raw = CoreTermShapeUntypedLowerer.lower(shape).fold(
      error => report.errorAndAbort(error.message),
      identity
    )
    val typed = new Typer().typedExpr(typingShell(raw))

    typed.asInstanceOf[Term].asExprOf[Int]

  private def typingShell(tree: untpd.Tree): untpd.Tree =
    given SourceFile = NoSource

    tree match
      case untpd.InfixOp(left, operator, right) =>
        untpd.Apply(
          untpd.Select(typingShell(left), operator.name),
          typingShell(right) :: Nil
        )
      case other => other
