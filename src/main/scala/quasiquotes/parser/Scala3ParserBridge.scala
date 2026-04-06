package quasiquotes.parser

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers.Parser
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

    if messages.nonEmpty then Left(ParseError(source, messages))
    else
      Right(
        ParsedExpression(
          source = source,
          rawTree = rawTree,
          shape = TermShapeInspector.inspect(rawTree),
          rawStructure = TermShapeInspector.rawStructure(rawTree)
        )
      )
