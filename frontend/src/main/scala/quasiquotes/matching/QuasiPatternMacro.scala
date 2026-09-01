package quasiquotes.matching

import scala.quoted.*

private[matching] object QuasiPatternMacro:
  def extractor(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        partExpressions.toList.map(_.valueOrAbort)
      case _ =>
        return '{ QuasiPattern.scalarExtractor($context)(using $callerQuotes) }

    RankedPatternSource.classify(parts) match
      case Left(detail) =>
        quotes.reflect.report.errorAndAbort(
          s"Invalid qq term-pattern template: $detail",
          context
        )
      case Right(template) =>
        template.sequenceIndex match
          case None => '{ QuasiPattern.scalarExtractor($context)(using $callerQuotes) }
          case Some(sequenceIndex) =>
            RankedPatternSource.compile(parts, sequenceIndex) match
              case Left(detail) =>
                quotes.reflect.report.errorAndAbort(
                  s"Invalid qq term-pattern template: $detail",
                  context
                )
              case Right(_) =>
                val tupleCons = TypeRepr.of[Any *: EmptyTuple] match
                  case AppliedType(constructor, _) => constructor
                  case other =>
                    quotes.reflect.report.errorAndAbort(
                      s"Unable to resolve Scala tuple constructor: ${other.show}",
                      context
                    )
                val kinds = template.holeNames.indices.foldRight(TypeRepr.of[EmptyTuple]) {
                  case (index, tail) =>
                    val head =
                      if index == sequenceIndex then TypeRepr.of[SequenceTermCapture]
                      else TypeRepr.of[ScalarTermCapture]
                    AppliedType(tupleCons, List(head, tail))
                }
                kinds.asType match
                  case '[captureKinds] =>
                    '{
                      RankedTermPatternExtractorFactory
                        .extractor[captureKinds & Tuple]($context, ${ Expr(sequenceIndex) })(using $callerQuotes)
                    }
