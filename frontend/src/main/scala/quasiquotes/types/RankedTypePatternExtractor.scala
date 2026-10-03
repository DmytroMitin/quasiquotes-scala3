package quasiquotes.types

import scala.compiletime.erasedValue
import scala.quoted.*

import quasiquotes.parser.{TinyTypeParser, TypeShape}
import quasiquotes.syntax.RankMarkerScanner.{hasSplitDoubleDot, unquotedDotRuns}

/** A typed product extractor for one bounded sequence-Type capture. */
final class RankedTypePatternExtractor[T, Captures <: Tuple](
    extract: T => Option[Captures]
):
  def unapply(value: T): Option[Captures] = extract(value)

private[quasiquotes] final class RankedSingleSequenceTypePatternExtractor[T](
    extract: T => Option[Seq[T]]
):
  def unapply(value: T): Option[Seq[T]] = extract(value)

private[quasiquotes] sealed trait TypeCaptureKind
private[quasiquotes] sealed trait ScalarTypeCapture extends TypeCaptureKind
private[quasiquotes] sealed trait SequenceTypeCapture extends TypeCaptureKind

private[quasiquotes] type TypeCaptureTypes[T, Kinds <: Tuple] <: Tuple = Kinds match
  case EmptyTuple => EmptyTuple
  case ScalarTypeCapture *: tail => T *: TypeCaptureTypes[T, tail]
  case SequenceTypeCapture *: tail => Seq[T] *: TypeCaptureTypes[T, tail]

private[quasiquotes] object RankedTypePatternSupport:
  final case class Layout(
      source: String,
      holeNames: Vector[String],
      sequenceIndex: Option[Int]
  )

  final case class Compiled(
      constructor: TypePattern,
      prefix: List[TypePattern],
      suffix: List[TypePattern],
      holeNames: Vector[String],
      sequenceHoleName: String
  )

  private final case class MatchData[T](
      scalarBindings: Map[String, T],
      sequenceBindings: Map[String, Seq[T]],
      holeNames: Vector[String]
  ):
    def scalar(index: Int): T = scalarBindings(holeNames(index))
    def sequence(index: Int): Seq[T] = sequenceBindings(holeNames(index))

  private[types] transparent inline def singleSequenceExtractor(
      context: StringContext,
      sequenceIndex: Int
  )(using q: Quotes): RankedSingleSequenceTypePatternExtractor[q.reflect.TypeRepr] =
    val compiled = compileOrAbort(context.parts.toList, sequenceIndex)
    new RankedSingleSequenceTypePatternExtractor(target =>
      matchCompiled(using q)(compiled, target).map(_.sequence(sequenceIndex))
    )

  private[types] transparent inline def rankedExtractor[Kinds <: Tuple](
      context: StringContext,
      sequenceIndex: Int
  )(using q: Quotes): RankedTypePatternExtractor[
    q.reflect.TypeRepr,
    TypeCaptureTypes[q.reflect.TypeRepr, Kinds]
  ] =
    val compiled = compileOrAbort(context.parts.toList, sequenceIndex)
    new RankedTypePatternExtractor(target =>
      matchCompiled(using q)(compiled, target).map(result =>
        captureTuple[q.reflect.TypeRepr, Kinds](result, 0)
      )
    )

  def classify(parts: List[String]): Either[String, Layout] =
    if parts.isEmpty then Left("StringContext must contain at least one part")
    else
      val partVector = parts.toVector
      val runs = unquotedDotRuns(partVector)
      if hasSplitDoubleDot(partVector, runs) then
        Left("orphan or malformed rank-marker spelling; `..` must be adjacent")
      else
        runs.find(_._3 >= 3) match
          case Some((partIndex, _, _)) =>
            Left(s"unsupported rank-3 capture marker in literal part $partIndex")
          case None =>
            val rankTwo = runs.filter(_._3 == 2)
            val consumed = rankTwo.filter { case (partIndex, offset, _) =>
              partIndex < partVector.size - 1 &&
              offset == partVector(partIndex).length - 2
            }
            if consumed.size > 1 then
              Left("only one rank-2 Type-argument capture is supported")
            else
              rankTwo.find(marker => !consumed.contains(marker)) match
                case Some((partIndex, _, _)) =>
                  Left(s"orphan or malformed `..` rank marker in literal part $partIndex")
                case None =>
                  val effectiveParts = consumed.headOption match
                    case Some((partIndex, _, _)) =>
                      partVector.updated(partIndex, partVector(partIndex).dropRight(2))
                    case None => partVector
                  val holeNames =
                    Vector.tabulate(parts.size - 1)(index => s"tqqCapture$index")
                  val source = effectiveParts.zipWithIndex.foldLeft(new StringBuilder) {
                    case (builder, (part, index)) =>
                      builder.append(part)
                      holeNames.lift(index).foreach(name =>
                        builder.append('$').append(name)
                      )
                      builder
                  }.toString
                  Right(Layout(source, holeNames, consumed.headOption.map(_._1)))

  private[types] def compile(
      parts: List[String],
      sequenceIndex: Int
  ): Either[String, Compiled] =
    classify(parts).flatMap { layout =>
      if layout.sequenceIndex != Some(sequenceIndex) then
        Left("ranked tqq template classification changed before compilation")
      else
        val mapped = TypePattern.rewriteSourceMapped(layout.source)
        TinyTypeParser.parse(mapped.generatedSource).left.map(_.summary).flatMap { parsed =>
          val sequenceName = layout.holeNames(sequenceIndex)
          val generatedSequenceName =
            mapped.generatedHoleIndex.generatedNameFor(sequenceName).getOrElse("")
          parsed.shape match
            case TypeShape.Apply(TypeShape.Identifier(constructorName), arguments) =>
              mapped.generatedHoleIndex.semanticNameFor(constructorName) match
                case Some(_) =>
                  Left(
                    "dynamic Type-constructor capture is outside the fixed-constructor tqq slice"
                  )
                case None =>
                  AppliedTypeConstructorPolicy.named(constructorName) match
                    case None =>
                      Left(
                        s"Unsupported applied Type constructor `$constructorName` in the fixed-constructor tqq slice"
                      )
                    case Some(policy) =>
                      val sequencePositions = arguments.zipWithIndex.collect {
                        case (TypeShape.Identifier(name), index)
                            if name == generatedSequenceName =>
                          index
                      }
                      sequencePositions match
                        case sequencePosition :: Nil =>
                          val fixedCount = arguments.size - 1
                          if fixedCount > policy.requiredArity then
                            Left(
                              s"fixed Type arguments exceed `$constructorName` arity ${policy.requiredArity}"
                            )
                          else
                            val compiledArguments =
                              arguments.zipWithIndex.collect {
                                case (argument, index) if index != sequencePosition =>
                                  TypePattern
                                    .fromShapeWithHoles(
                                      argument,
                                      mapped.generatedHoleIndex
                                    )
                                    .left
                                    .map(_.message)
                              }
                            collect(compiledArguments).map { fixed =>
                              Compiled(
                                TypePattern.TPIdent(constructorName),
                                fixed.take(sequencePosition),
                                fixed.drop(sequencePosition),
                                layout.holeNames,
                                sequenceName
                              )
                            }
                        case _ =>
                          Left(
                            "rank-2 capture is supported only once in a direct fixed Type constructor argument list"
                          )
            case TypeShape.Apply(_, _) =>
              Left(
                "dynamic or selected Type constructors are outside the fixed-constructor tqq slice"
              )
            case _ =>
              Left(
                "rank-2 capture is supported only in a direct fixed Type constructor argument list"
              )
        }
    }

  def compilePattern(
      pattern: TypePattern,
      holeNames: Vector[String],
      sequenceIndex: Int
  ): Either[String, Compiled] =
    holeNames.lift(sequenceIndex).toRight(
      "ranked tqq template classification changed before compilation"
    ).flatMap { sequenceName =>
      pattern match
        case TypePattern.TPApply(TypePattern.TPIdent(constructorName), arguments) =>
          AppliedTypeConstructorPolicy.named(constructorName) match
            case None =>
              Left(
                s"Unsupported applied Type constructor `$constructorName` in the fixed-constructor tqq slice"
              )
            case Some(policy) =>
              val sequencePositions = arguments.zipWithIndex.collect {
                case (TypePattern.TPHole(name), index) if name == sequenceName => index
              }
              sequencePositions match
                case sequencePosition :: Nil =>
                  val fixedCount = arguments.size - 1
                  if fixedCount > policy.requiredArity then
                    Left(
                      s"fixed Type arguments exceed `$constructorName` arity ${policy.requiredArity}"
                    )
                  else
                    Right(
                      Compiled(
                        TypePattern.TPIdent(constructorName),
                        arguments.take(sequencePosition),
                        arguments.drop(sequencePosition + 1),
                        holeNames,
                        sequenceName
                      )
                    )
                case _ =>
                  Left(
                    "rank-2 capture is supported only once in a direct fixed Type constructor argument list"
                  )
        case TypePattern.TPApply(TypePattern.TPHole(_), _) =>
          Left(
            "dynamic Type-constructor capture is outside the fixed-constructor tqq slice"
          )
        case TypePattern.TPApply(_, _) =>
          Left(
            "dynamic or selected Type constructors are outside the fixed-constructor tqq slice"
          )
        case _ =>
          Left(
            "rank-2 capture is supported only in a direct fixed Type constructor argument list"
          )
    }

  transparent inline def singleSequenceExtractorFromPattern(
      pattern: TypePattern,
      holeNames: Vector[String],
      sequenceIndex: Int
  )(using q: Quotes): RankedSingleSequenceTypePatternExtractor[q.reflect.TypeRepr] =
    val compiled = compilePatternOrAbort(pattern, holeNames, sequenceIndex)
    new RankedSingleSequenceTypePatternExtractor(target =>
      matchCompiled(using q)(compiled, target).map(_.sequence(sequenceIndex))
    )

  transparent inline def rankedExtractorFromPattern[Kinds <: Tuple](
      pattern: TypePattern,
      holeNames: Vector[String],
      sequenceIndex: Int
  )(using q: Quotes): RankedTypePatternExtractor[
    q.reflect.TypeRepr,
    TypeCaptureTypes[q.reflect.TypeRepr, Kinds]
  ] =
    val compiled = compilePatternOrAbort(pattern, holeNames, sequenceIndex)
    new RankedTypePatternExtractor(target =>
      matchCompiled(using q)(compiled, target).map(result =>
        captureTuple[q.reflect.TypeRepr, Kinds](result, 0)
      )
    )

  private def compileOrAbort(using q: Quotes)(
      parts: List[String],
      sequenceIndex: Int
  ): Compiled =
    compile(parts, sequenceIndex).fold(
      detail =>
        q.reflect.report.errorAndAbort(
          s"Invalid tqq type-pattern template: $detail"
        ),
      identity
    )

  private def compilePatternOrAbort(using q: Quotes)(
      pattern: TypePattern,
      holeNames: Vector[String],
      sequenceIndex: Int
  ): Compiled =
    compilePattern(pattern, holeNames, sequenceIndex).fold(
      detail =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta tqq type-pattern template: $detail"
        ),
      identity
    )

  private def matchCompiled(using q: Quotes)(
      compiled: Compiled,
      target: q.reflect.TypeRepr
  ): Option[MatchData[q.reflect.TypeRepr]] =
    TargetTypeReprInspector.inspectWithOrigins(target).toOption.flatMap {
      inspection =>
        inspection.normalForm match
          case TypeNormalForm.STypeApply(targetConstructor, targetArguments)
              if targetArguments.size >= compiled.prefix.size + compiled.suffix.size &&
                TypePattern
                  .matchNormalForm(compiled.constructor, targetConstructor)
                  .isDefined =>
            val prefixTargets = targetArguments.take(compiled.prefix.size)
            val suffixStart = targetArguments.size - compiled.suffix.size
            val suffixTargets = targetArguments.drop(suffixStart)
            val initial =
              Option(
                (
                  Map.empty[String, TypeNormalForm],
                  Map.empty[String, q.reflect.TypeRepr]
                )
              )
            val afterPrefix =
              compiled.prefix
                .zip(prefixTargets)
                .zipWithIndex
                .foldLeft(initial) {
                  case (state, ((pattern, normalForm), index)) =>
                    state.flatMap(matchChild(inspection, pattern, normalForm, index, _))
                }
            val afterSuffix =
              compiled.suffix
                .zip(suffixTargets)
                .zipWithIndex
                .foldLeft(afterPrefix) {
                  case (state, ((pattern, normalForm), offset)) =>
                    state.flatMap(
                      matchChild(
                        inspection,
                        pattern,
                        normalForm,
                        suffixStart + offset,
                        _
                      )
                    )
                }
            afterSuffix.flatMap { case (_, scalarBindings) =>
              val sequence =
                (compiled.prefix.size until suffixStart).foldLeft(
                  Option(Vector.empty[q.reflect.TypeRepr])
                ) { (current, index) =>
                  current.flatMap(values =>
                    inspection.originalsByPath
                      .get(Vector(index))
                      .map(values :+ _)
                  )
                }
              sequence.map(values =>
                MatchData(
                  scalarBindings,
                  Map(compiled.sequenceHoleName -> values),
                  compiled.holeNames
                )
              )
            }
          case _ => None
    }

  private def matchChild[T](
      inspection: TargetTypeReprInspector.Inspection[T],
      pattern: TypePattern,
      target: TypeNormalForm,
      targetIndex: Int,
      state: (Map[String, TypeNormalForm], Map[String, T])
  ): Option[(Map[String, TypeNormalForm], Map[String, T])] =
    TypePattern.matchNormalFormWithPaths(pattern, target).flatMap { trace =>
      val mergedNormalForms =
        trace.result.bindings.foldLeft(Option(state._1)) {
          case (Some(current), (name, value)) =>
            current.get(name) match
              case Some(existing) if existing != value => None
              case _ => Some(current.updated(name, value))
          case (None, _) => None
        }
      mergedNormalForms.flatMap { normalForms =>
        trace.holePaths.foldLeft(Option(state._2)) {
          case (Some(current), (name, relativePath)) =>
            inspection.originalsByPath
              .get(Vector(targetIndex) ++ relativePath)
              .map(value => current.updated(name, value))
          case (None, _) => None
        }.map(normalForms -> _)
      }
    }

  private inline def captureTuple[T, Kinds <: Tuple](
      result: MatchData[T],
      index: Int
  ): TypeCaptureTypes[T, Kinds] =
    inline erasedValue[Kinds] match
      case _: EmptyTuple => EmptyTuple
      case _: (ScalarTypeCapture *: tail) =>
        result.scalar(index) *: captureTuple[T, tail](result, index + 1)
      case _: (SequenceTypeCapture *: tail) =>
        result.sequence(index) *: captureTuple[T, tail](result, index + 1)

  private def collect[A](values: List[Either[String, A]]): Either[String, List[A]] =
    values.foldRight[Either[String, List[A]]](Right(Nil)) {
      (value, accumulated) =>
        for
          head <- value
          tail <- accumulated
        yield head :: tail
    }

private[types] object QuasiTypePatternMacro:
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
          QuasiTypequotes.scalarExtractor($context)(using $callerQuotes)
        }

    RankedTypePatternSupport.classify(parts) match
      case Left(detail) =>
        report.errorAndAbort(
          s"Invalid tqq type-pattern template: $detail",
          context
        )
      case Right(layout) =>
        layout.sequenceIndex match
          case None =>
            '{
              QuasiTypequotes.scalarExtractor($context)(using $callerQuotes)
            }
          case Some(sequenceIndex) =>
            RankedTypePatternSupport
              .compile(parts, sequenceIndex)
              .fold(
                detail =>
                  report.errorAndAbort(
                    s"Invalid tqq type-pattern template: $detail",
                    context
                  ),
                _ => ()
              )
            val tupleCons = TypeRepr.of[Any *: EmptyTuple] match
              case AppliedType(constructor, _) => constructor
              case other =>
                report.errorAndAbort(
                  s"Unable to resolve Scala tuple constructor: ${other.show}",
                  context
                )
            val holeCount = parts.size - 1
            val kinds = (0 until holeCount).foldRight(TypeRepr.of[EmptyTuple]) {
              case (index, tail) =>
                val head =
                  if index == sequenceIndex then TypeRepr.of[SequenceTypeCapture]
                  else TypeRepr.of[ScalarTypeCapture]
                AppliedType(tupleCons, List(head, tail))
            }
            kinds.asType match
              case '[captureKinds] =>
                if holeCount == 1 then
                  '{
                    RankedTypePatternSupport.singleSequenceExtractor(
                      $context,
                      ${ Expr(sequenceIndex) }
                    )(using $callerQuotes)
                  }
                else
                  '{
                    RankedTypePatternSupport.rankedExtractor[captureKinds & Tuple](
                      $context,
                      ${ Expr(sequenceIndex) }
                    )(using $callerQuotes)
                  }
