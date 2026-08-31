package quasiquotes.terms.dotty

import scala.quoted.*
import scala.quoted.runtime.impl.QuotesImpl

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.typer.Typer
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.parser.TermShape

private[quasiquotes] object CoreTermShapeUntypedLowererTyperFixture:
  val value: Int = 7
  def zero(): Int = 0
  def identity(value: Int): Int = value
  def add(left: Int, right: Int): Int = left + right

private[quasiquotes] object CoreTermShapeUntypedLowererTyperProbe:
  inline def onePlusOne: Int = ${ onePlusOneImpl }

  inline def nestedPrecedence: Int = ${ nestedPrecedenceImpl }

  inline def negativePlusTwo: Int = ${ negativePlusTwoImpl }

  inline def identifierViable: Boolean = ${ identifierViableImpl }

  inline def selectViable: Boolean = ${ selectViableImpl }

  inline def emptyApplyViable: Boolean = ${ emptyApplyViableImpl }

  inline def oneArgumentApplyViable: Boolean = ${ oneArgumentApplyViableImpl }

  inline def multiArgumentApplyViable: Boolean = ${ multiArgumentApplyViableImpl }

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

  private def identifierViableImpl(using Quotes): Expr[Boolean] =
    typeForViability(fixtureIdentifier)

  private def selectViableImpl(using Quotes): Expr[Boolean] =
    typeForViability(TermShape.Select(fixtureIdentifier, "value"))

  private def emptyApplyViableImpl(using Quotes): Expr[Boolean] =
    typeForViability(
      TermShape.Apply(
        TermShape.Select(fixtureIdentifier, "zero"),
        Nil
      )
    )

  private def oneArgumentApplyViableImpl(using Quotes): Expr[Boolean] =
    typeForViability(
      TermShape.Apply(
        TermShape.Select(fixtureIdentifier, "identity"),
        TermShape.Literal("11") :: Nil
      )
    )

  private def multiArgumentApplyViableImpl(using Quotes): Expr[Boolean] =
    typeForViability(
      TermShape.Apply(
        TermShape.Select(fixtureIdentifier, "add"),
        List(TermShape.Literal("2"), TermShape.Literal("3"))
      )
    )

  private def fixtureIdentifier: TermShape =
    TermShape.Identifier(
      "CoreTermShapeUntypedLowererTyperFixture",
      isPlaceholder = false
    )

  private def typeForViability(shape: TermShape)(using q: Quotes): Expr[Boolean] =
    given Context = q.asInstanceOf[QuotesImpl].ctx
    val raw = CoreTermShapeUntypedLowerer.lower(shape).fold(
      error => quotes.reflect.report.errorAndAbort(error.message),
      identity
    )
    val typed = new Typer().typedExpr(raw)
    if typed.tpe.isError then
      quotes.reflect.report.errorAndAbort(
        s"ordinary Typer produced an error type for ${shape.render}"
      )
    Expr(true)

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
