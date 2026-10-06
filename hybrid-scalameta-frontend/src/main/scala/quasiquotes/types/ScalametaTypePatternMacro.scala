package quasiquotes.types

import scala.quoted.*

import quasiquotes.scalameta.{ScalametaQuasiPattern, TypeFrontend}

private[quasiquotes] object ScalametaTypePatternMacro:
  def extractor(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        partExpressions.toList.map(_.valueOrAbort)
      case _ =>
        return '{
          ScalametaQuasiPattern.scalarTypeExtractor($context)(using $callerQuotes)
        }

    RankedTypePatternSupport.classify(parts) match
      case Left(detail) => abort(detail, context)
      case Right(layout) =>
        layout.sequenceIndex match
          case None =>
            TypeFrontend.compileRanked(parts, 0).foreach { compiled =>
              compiled.pattern match
                case TypePattern.TPApply(TypePattern.TPHole(_), _) =>
                  abort(
                    "dynamic Type-constructor capture requires one rank-2 Type-argument capture",
                    context
                  )
                case _ => ()
            }
            '{
              ScalametaQuasiPattern.scalarTypeExtractor($context)(using $callerQuotes)
            }
          case Some(sequenceIndex) =>
            val effectiveParts =
              parts.updated(sequenceIndex, parts(sequenceIndex).dropRight(2))
            val compiled = TypeFrontend.compileRanked(effectiveParts, sequenceIndex).fold(
              failure => abort(failure.message, context),
              identity
            )
            val dynamicRoot = compiled.pattern match
              case TypePattern.TPApply(TypePattern.TPHole(_), _) => true
              case TypePattern.TPApply(TypePattern.TPIdent(_), _) => false
              case TypePattern.TPApply(_, _) =>
                abort(
                  "dynamic or selected Type constructors are outside the fixed-constructor tqq slice",
                  context
                )
              case _ =>
                abort(
                  "rank-2 capture is supported only in a direct fixed Type constructor argument list",
                  context
                )
            if dynamicRoot then
              RankedTypePatternSupport
                .compileDynamicPattern(
                  compiled.pattern,
                  compiled.captureNames,
                  sequenceIndex
                )
                .fold(detail => abort(detail, context), identity)
            else
              RankedTypePatternSupport
                .compilePattern(compiled.pattern, compiled.captureNames, sequenceIndex)
                .fold(detail => abort(detail, context), identity)

            val tupleCons = TypeRepr.of[Any *: EmptyTuple] match
              case AppliedType(constructor, _) => constructor
              case other =>
                report.errorAndAbort(
                  s"Unable to resolve Scala tuple constructor: ${other.show}",
                  context
                )
            val kinds = compiled.captureNames.indices.foldRight(TypeRepr.of[EmptyTuple]) {
              case (index, tail) =>
                val head =
                  if index == sequenceIndex then TypeRepr.of[SequenceTypeCapture]
                  else TypeRepr.of[ScalarTypeCapture]
                AppliedType(tupleCons, List(head, tail))
            }
            val pattern = patternExpr(compiled.pattern, context)
            val holeNames =
              '{ Vector.from(${ Expr.ofList(compiled.captureNames.toList.map(Expr(_))) }) }
            kinds.asType match
              case '[captureKinds] =>
                if dynamicRoot then
                  '{
                    RankedTypePatternSupport.dynamicRankedExtractorFromPattern[
                      captureKinds & Tuple
                    ](
                      $pattern,
                      $holeNames,
                      ${ Expr(sequenceIndex) }
                    )(using $callerQuotes)
                  }
                else if compiled.captureNames.size == 1 then
                  '{
                    RankedTypePatternSupport.singleSequenceExtractorFromPattern(
                      $pattern,
                      $holeNames,
                      ${ Expr(sequenceIndex) }
                    )(using $callerQuotes)
                  }
                else
                  '{
                    RankedTypePatternSupport.rankedExtractorFromPattern[captureKinds & Tuple](
                      $pattern,
                      $holeNames,
                      ${ Expr(sequenceIndex) }
                    )(using $callerQuotes)
                  }

  private def patternExpr(
      pattern: TypePattern,
      context: Expr[StringContext]
  )(using Quotes): Expr[TypePattern] =
    pattern match
      case TypePattern.TPHole(name) => '{ TypePattern.TPHole(${ Expr(name) }) }
      case TypePattern.TPIdent(name) => '{ TypePattern.TPIdent(${ Expr(name) }) }
      case TypePattern.TPApply(constructor, arguments) =>
        '{
          TypePattern.TPApply(
            ${ patternExpr(constructor, context) },
            ${ Expr.ofList(arguments.map(patternExpr(_, context))) }
          )
        }
      case TypePattern.TPTuple(elements) =>
        '{ TypePattern.TPTuple(${ Expr.ofList(elements.map(patternExpr(_, context))) }) }
      case TypePattern.TPFunction(arguments, result) =>
        '{
          TypePattern.TPFunction(
            ${ Expr.ofList(arguments.map(patternExpr(_, context))) },
            ${ patternExpr(result, context) }
          )
        }
      case TypePattern.TPResolved(id) =>
        quotes.reflect.report.errorAndAbort(
          s"Invalid Scalameta tqq type-pattern template: dynamic or selected Type constructors are outside the fixed-constructor tqq slice (${id.canonicalSource})",
          context
        )

  private def abort(detail: String, context: Expr[StringContext])(using Quotes): Nothing =
    quotes.reflect.report.errorAndAbort(
      s"Invalid Scalameta tqq type-pattern template: $detail",
      context
    )
