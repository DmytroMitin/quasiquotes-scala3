package quasiquotes.parser

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.parsing.Tokens
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

object Scala3ParserBridge:
  /** Compiler-coupled entry point.
    *
    * This object is intentionally the only place that knows about Dotty parser setup.
    * Future quasiquote work can keep using the same bridge while changing only the
    * post-parse lowering pipeline.
    */
  def parseExpression(source: String): Either[ParseError, ParsedExpression] =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)

    val parser = new Parser(SourceFile.virtual("Expr.scala", source))
    val rawTree = parser.expr()
    val messages = reporter.pendingMessages.map(_.message).toList

    if messages.nonEmpty then Left(ParseError.syntax(source, messages))
    else if parser.in.token != Tokens.EOF then
      Left(
        ParseError.trailing(
          source = source,
          offset = parser.in.offset,
          trailingSnippet = source.drop(parser.in.offset).trim,
          tokenDescription = Tokens.tokenString(parser.in.token)
        )
      )
    else
      val preliminaryShape = TermShapeInspector.inspect(rawTree)
      val inspected = SourceOwnedLocalDefAdmission.validate(preliminaryShape) match
        case Left(violation) => TermShape.Unsupported("Block", violation.message)
        case Right(_) => P2LocalValUntypedAdmission.validate(rawTree) match
          case Left(violation) => TermShape.Unsupported("Block", violation.message)
          case Right(_) => preliminaryShape match
            case _: TermShape.Lambda1 if isContextFunction(source, rawTree) =>
              TermShape.Unsupported(
                "Lambda1",
                Lambda1DiagnosticMessages.ContextFunction
              )
            case _: TermShape.InterpolatedString if source.contains("s\"\"\"") =>
              TermShape.Unsupported(
                "InterpolatedStringSurface",
                "triple-quoted interpolation is outside the bounded s tranche"
              )
            case other => other
      Right(
        ParsedExpression(
          source = source,
          rawTree = rawTree,
          shape = inspected,
          rawStructure = TermShapeInspector.rawStructure(rawTree)
        )
      )

  private def isContextFunction(source: String, tree: untpd.Tree): Boolean =
    tree match
      case untpd.Function((parameter: untpd.ValDef) :: Nil, body) =>
        (for
          parameterSpan <- DottySourceSpanAdapter.fromTree(parameter)
          bodySpan <- DottySourceSpanAdapter.fromTree(body)
        yield source.slice(parameterSpan.end, bodySpan.start).contains("?=>"))
          .getOrElse(false)
      case _ => false

  def parseType(source: String): Either[ParseError, ParsedType] =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)

    val parser = new Parser(SourceFile.virtual("Type.scala", source))
    val rawTree = parser.typ()
    val messages = reporter.pendingMessages.map(_.message).toList

    if messages.nonEmpty then Left(ParseError.syntax(source, messages))
    else if parser.in.token != Tokens.EOF then
      Left(
        ParseError.trailing(
          source = source,
          offset = parser.in.offset,
          trailingSnippet = source.drop(parser.in.offset).trim,
          tokenDescription = Tokens.tokenString(parser.in.token)
        )
      )
    else
      Right(
        ParsedType(
          source = source,
          rawTree = rawTree,
          shape = TypeShapeInspector.inspect(rawTree),
          rawStructure = TypeShapeInspector.rawStructure(rawTree)
        )
      )
