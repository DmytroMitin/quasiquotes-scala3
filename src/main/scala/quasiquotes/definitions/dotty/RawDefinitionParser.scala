package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

import quasiquotes.definitions.LocatedDefinitionShape
import quasiquotes.parser.DottySourceSpanAdapter
import quasiquotes.source.*

private[quasiquotes] object RawDefinitionParser:
  def parseStandalone(
      source: String,
      sourceId: SourceId
  ): Either[LocatedDiagnostic[RawDefinitionAdapterError], LocatedDefinitionShape] =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)

    val parsed = new Parser(SourceFile.virtual("RawDefinition.scala", source)).parse()
    reporter.pendingMessages.toList.headOption match
      case Some(diagnostic) =>
        val location =
          DottySourceSpanAdapter
            .fromSpan(diagnostic.pos.span)
            .filter(!_.isEmpty)
            .flatMap(DiagnosticLocation.direct(sourceId, _, DiagnosticPrecision.ExactOccurrence))
        Left(LocatedDiagnostic(RawDefinitionAdapterError.DefinitionParseFailure, location))
      case None =>
        val statements = parsed match
          case packageDef: untpd.PackageDef
              if packageDef.pid.name == nme.EMPTY_PACKAGE =>
            packageDef.stats
          case untpd.EmptyTree => Nil
          case other => List(other)
        statements match
          case definition :: Nil =>
            RawDefinitionAdapter.adaptIsolated(definition, source, sourceId)
          case other =>
            val location =
              Option
                .when(source.nonEmpty)(SourceSpan(0, source.length))
                .flatMap(DiagnosticLocation.direct(sourceId, _, DiagnosticPrecision.WholeSource))
            Left(
              LocatedDiagnostic(
                RawDefinitionAdapterError.ExpectedExactlyOneDefinition(other.size),
                location
              )
            )
