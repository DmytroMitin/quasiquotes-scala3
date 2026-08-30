package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.parser.TermShape

private[quasiquotes] object CoreTermShapeUntypedLowerer:
  import CoreTermShapeUntypedLowererError.*

  private val CanonicalInteger = "(?:0|[1-9][0-9]*|-[1-9][0-9]*)".r
  private val AdmittedOperators = Set(
    "+",
    "-",
    "*",
    "/",
    "%",
    "==",
    "!=",
    "<",
    "<=",
    ">",
    ">="
  )

  def lower(
      shape: TermShape
  )(using Context): Either[CoreTermShapeUntypedLowererError, untpd.Tree] =
    given SourceFile = NoSource

    lowerAdmitted(shape).flatMap { tree =>
      verifySourceFree(tree).map(_ => tree)
    }

  private def lowerAdmitted(
      shape: TermShape
  )(using SourceFile): Either[CoreTermShapeUntypedLowererError, untpd.Tree] =
    Option(shape) match
      case None => Left(MissingTermShape)
      case Some(TermShape.Literal(value)) =>
        Option(value) match
          case Some(CanonicalInteger()) =>
            Right(untpd.Number(value, untpd.NumberKind.Whole(10)))
          case _ => Left(InvalidIntegerLiteral(value))
      case Some(TermShape.Infix(left, operator, right)) =>
        if !Option(operator).exists(AdmittedOperators) then
          Left(InvalidInfixOperator(operator))
        else
          for
            rawLeft <- lowerAdmitted(left)
            rawRight <- lowerAdmitted(right)
          yield untpd.InfixOp(
            rawLeft,
            untpd.Ident(termName(operator)),
            rawRight
          )
      case Some(other) =>
        Left(UnsupportedTermShape(nodeKind(other)))

  private def nodeKind(shape: TermShape): String =
    shape match
      case _: TermShape.Identifier => "Identifier"
      case _: TermShape.BoundReference => "BoundReference"
      case _: TermShape.Lambda1 => "Lambda1"
      case _: TermShape.Literal => "Literal"
      case _: TermShape.Select => "Select"
      case _: TermShape.Apply => "Apply"
      case _: TermShape.New => "New"
      case _: TermShape.Infix => "Infix"
      case _: TermShape.Unary => "Unary"
      case _: TermShape.InterpolatedString => "InterpolatedString"
      case _: TermShape.Typed => "Typed"
      case _: TermShape.Tuple => "Tuple"
      case _: TermShape.If => "If"
      case _: TermShape.Block => "Block"
      case _: TermShape.Parenthesized => "Parenthesized"
      case _: TermShape.Unsupported => "Unsupported"

  private def verifySourceFree(
      tree: untpd.Tree
  )(using Context): Either[CoreTermShapeUntypedLowererError, Unit] =
    val nodeKind = tree.getClass.getSimpleName
    if tree.source.exists then
      Left(SourceFreeInvariantViolation(nodeKind, "the node has a source."))
    else if tree.span.exists then
      Left(SourceFreeInvariantViolation(nodeKind, "the node has a span."))
    else if tree.symbol != NoSymbol then
      Left(SourceFreeInvariantViolation(nodeKind, "the node has a symbol."))
    else if tree.isInstanceOf[untpd.TypedSplice] then
      Left(SourceFreeInvariantViolation(nodeKind, "the tree contains TypedSplice."))
    else
      tree match
        case untpd.InfixOp(left, operator, right) =>
          for
            _ <- verifySourceFree(left)
            _ <- verifySourceFree(operator)
            _ <- verifySourceFree(right)
          yield ()
        case _ => Right(())
