package quasiquotes.matching

import scala.quoted.*

import quasiquotes.scalameta.{ScalametaQuasiPattern, TermFrontend}

private[quasiquotes] object ScalametaQuasiPatternMacro:
  def extractor(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        partExpressions.toList.map(_.valueOrAbort)
      case _ =>
        return '{ ScalametaQuasiPattern.scalarExtractor($context)(using $callerQuotes) }

    RankedTermPatternBridge.classify(parts) match
      case Left(detail) => abort(detail, context)
      case Right(layout) =>
        layout.sequenceIndex match
          case None =>
            '{ ScalametaQuasiPattern.scalarExtractor($context)(using $callerQuotes) }
          case Some(sequenceIndex) =>
            val sequenceHoleName = layout.sequenceHoleName.getOrElse(
              quotes.reflect.report.errorAndAbort(
                "Ranked Scalameta qq layout lost its sequence capture",
                context
              )
            )
            val compiled = TermFrontend.compile(layout.effectiveSource).fold(
              failure => abort(failure.message, context),
              identity
            )
            RankedTermPatternBridge
              .validateSequencePosition(compiled.pattern, sequenceHoleName)
              .fold(detail => abort(detail, context), identity)

            val tupleCons = TypeRepr.of[Any *: EmptyTuple] match
              case AppliedType(constructor, _) => constructor
              case other =>
                quotes.reflect.report.errorAndAbort(
                  s"Unable to resolve Scala tuple constructor: ${other.show}",
                  context
                )
            val kinds = layout.holeNames.indices.foldRight(TypeRepr.of[EmptyTuple]) {
              case (index, tail) =>
                val head =
                  if index == sequenceIndex then TypeRepr.of[SequenceTermCapture]
                  else TypeRepr.of[ScalarTermCapture]
                AppliedType(tupleCons, List(head, tail))
            }
            val pattern = RankedTermPatternBridge.patternExpr(compiled.pattern)
            val holeNames = RankedTermPatternBridge.holeNamesExpr(layout.holeNames)
            val sequenceName = Expr(sequenceHoleName)
            kinds.asType match
              case '[captureKinds] =>
                val directSingletonNewSequence =
                  layout.holeNames.size == 1 &&
                    RankedTermPatternBridge.containsDirectNewSequenceHole(
                      compiled.pattern,
                      sequenceHoleName
                    )
                if directSingletonNewSequence then
                  '{
                    RankedTermPatternBridge.singleSequenceExtractor(
                      $pattern,
                      $holeNames,
                      $sequenceName
                    )(using $callerQuotes)
                  }
                else
                  '{
                    RankedTermPatternBridge.extractor[captureKinds & Tuple](
                      $pattern,
                      $holeNames,
                      $sequenceName
                    )(using $callerQuotes)
                  }

  private def abort(detail: String, context: Expr[StringContext])(using Quotes): Nothing =
    quotes.reflect.report.errorAndAbort(
      s"Invalid Scalameta qq term-pattern template: $detail",
      context
    )
