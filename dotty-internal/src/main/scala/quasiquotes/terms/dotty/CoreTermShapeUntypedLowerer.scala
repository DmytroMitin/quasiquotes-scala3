package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.parser.{BlockStatement, TermShape}

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
  private val AdmittedUnaryOperators = Set("+", "-", "!", "~")
  private val PlainSourceName = "[A-Za-z_][A-Za-z0-9_]*".r
  private val Scala3Keywords = Set(
    "abstract",
    "as",
    "case",
    "catch",
    "class",
    "def",
    "derives",
    "do",
    "else",
    "end",
    "enum",
    "export",
    "extends",
    "extension",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    "given",
    "if",
    "implicit",
    "import",
    "infix",
    "inline",
    "lazy",
    "macro",
    "match",
    "new",
    "null",
    "object",
    "opaque",
    "open",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "then",
    "this",
    "throw",
    "trait",
    "transparent",
    "true",
    "try",
    "type",
    "using",
    "val",
    "var",
    "while",
    "with",
    "yield"
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
          case Some("true") => Right(untpd.Literal(Constant(true)))
          case Some("false") => Right(untpd.Literal(Constant(false)))
          case Some(semanticString)
              if semanticString.length >= 2 &&
                semanticString.head == '"' &&
                semanticString.last == '"' =>
            Right(
              untpd.Literal(
                Constant(
                  semanticString.substring(1, semanticString.length - 1)
                )
              )
            )
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
      case Some(TermShape.Identifier(name, isPlaceholder)) =>
        if isPlaceholder then Left(PlaceholderIdentifier(name))
        else
          validateSourceName(name, InvalidIdentifierName.apply)
            .map(validName => untpd.Ident(termName(validName)))
      case Some(TermShape.Select(qualifier, name)) =>
        for
          validName <- validateSourceName(name, InvalidSelectedName.apply)
          rawQualifier <- lowerAdmitted(qualifier)
        yield untpd.Select(rawQualifier, termName(validName))
      case Some(TermShape.Apply(_: TermShape.Apply, _)) =>
        Left(MultipleApplicationLists)
      case Some(TermShape.Apply(function, arguments)) =>
        Option(arguments) match
          case None => Left(MissingApplyArguments)
          case Some(argumentList) =>
            for
              rawFunction <- lowerAdmitted(function)
              rawArguments <- traverse(argumentList)(lowerAdmitted)
            yield untpd.Apply(rawFunction, rawArguments)
      case Some(TermShape.Unary(operator, operand)) =>
        if !Option(operator).exists(AdmittedUnaryOperators) then
          Left(InvalidUnaryOperator(operator))
        else
          lowerAdmitted(operand).map(rawOperand =>
            untpd.PrefixOp(
              untpd.Ident(termName(operator)),
              rawOperand
            )
          )
      case Some(TermShape.Tuple(elements)) =>
        Option(elements) match
          case None => Left(MissingTupleElements)
          case Some(elementList)
              if elementList.size < 2 || elementList.size > 22 =>
            Left(InvalidTupleArity(elementList.size))
          case Some(elementList) =>
            traverse(elementList)(lowerAdmitted).map(untpd.Tuple.apply)
      case Some(TermShape.If(condition, thenBranch, elseBranch)) =>
        for
          rawCondition <- lowerAdmitted(condition)
          rawThen <- lowerAdmitted(thenBranch)
          rawElse <- lowerAdmitted(elseBranch)
        yield untpd.If(rawCondition, rawThen, rawElse)
      case Some(TermShape.Block(statements, result)) =>
        lowerBlock(statements, result)
      case Some(other) =>
        Left(UnsupportedTermShape(nodeKind(other)))

  private def lowerBlock(
      statements: List[BlockStatement],
      result: TermShape
  )(using SourceFile): Either[CoreTermShapeUntypedLowererError, untpd.Tree] =
    val expressionPrefix =
      statements.zipWithIndex.foldLeft[
        Either[CoreTermShapeUntypedLowererError, List[TermShape]]
      ](Right(Nil)) { case (accumulated, (statement, index)) =>
        accumulated.flatMap { values =>
          statement match
            case expression: TermShape => Right(expression :: values)
            case null => Left(MalformedBlock(s"prefix entry $index is null."))
            case other =>
              Left(
                MalformedBlock(
                  s"prefix entry $index is ${blockStatementKind(other)}; expected a binder-free Term expression."
                )
              )
        }
      }.map(_.reverse)

    for
      prefix <- expressionPrefix
      rawPrefix <- traverse(prefix)(lowerAdmitted)
      rawResult <- lowerAdmitted(result)
    yield untpd.Block(rawPrefix, rawResult)

  private def blockStatementKind(statement: BlockStatement): String =
    statement match
      case _: BlockStatement.LocalVal => "LocalVal"
      case _: BlockStatement.LocalDef => "LocalDef"
      case _: TermShape => "TermShape"

  private def validateSourceName(
      name: String,
      invalid: String => CoreTermShapeUntypedLowererError
  ): Either[CoreTermShapeUntypedLowererError, String] =
    Either.cond(
      Option(name).exists(value =>
        value != "_" && PlainSourceName.matches(value) && !Scala3Keywords(value)
      ),
      name,
      invalid(name)
    )

  private def traverse[A, B](
      values: List[A]
  )(lowerValue: A => Either[CoreTermShapeUntypedLowererError, B])
      : Either[CoreTermShapeUntypedLowererError, List[B]] =
    values.foldRight(
      Right(Nil): Either[CoreTermShapeUntypedLowererError, List[B]]
    ) { (value, rest) =>
      for
        head <- lowerValue(value)
        tail <- rest
      yield head :: tail
    }

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
        case untpd.Select(qualifier, _) => verifySourceFree(qualifier)
        case untpd.Apply(function, arguments) =>
          for
            _ <- verifySourceFree(function)
            _ <- traverse(arguments)(verifySourceFree)
          yield ()
        case untpd.InfixOp(left, operator, right) =>
          for
            _ <- verifySourceFree(left)
            _ <- verifySourceFree(operator)
            _ <- verifySourceFree(right)
          yield ()
        case untpd.PrefixOp(operator, operand) =>
          for
            _ <- verifySourceFree(operator)
            _ <- verifySourceFree(operand)
          yield ()
        case untpd.Tuple(elements) =>
          traverse(elements)(verifySourceFree).map(_ => ())
        case untpd.If(condition, thenBranch, elseBranch) =>
          for
            _ <- verifySourceFree(condition)
            _ <- verifySourceFree(thenBranch)
            _ <- verifySourceFree(elseBranch)
          yield ()
        case untpd.Block(statements, result) =>
          for
            _ <- traverse(statements)(verifySourceFree)
            _ <- verifySourceFree(result)
          yield ()
        case _ => Right(())
