package quasiquotes.types.q054

import scala.compiletime.erasedValue
import scala.quoted.*

import quasiquotes.parser.{TinyTypeParser, TypeShape}
import quasiquotes.types.*

final class Q054Box[A]
type Q054Alias[A] = List[A]

/** Test-only R2 prototype. It leaves the dynamic application shell outside
  * TypeNormalForm, while delegating every fixed scalar argument constraint to
  * the existing TypePattern/TargetTypeReprInspector semantics.
  */
object Q054TypePatternProbe:
  private[q054] final case class Compiled(
      constructorHoleName: String,
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

  extension (inline context: StringContext)
    transparent inline def tqq(using q: Quotes) =
      ${ Q054TypePatternProbeMacro.extractor('context, 'q) }

  private[q054] transparent inline def rankedExtractor[Kinds <: Tuple](
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

  private[q054] def compile(
      parts: List[String],
      sequenceIndex: Int
  ): Either[String, Compiled] =
    RankedTypePatternSupport.classify(parts).flatMap { layout =>
      layout.sequenceIndex match
        case None =>
          dynamicRootShape(layout.source) match
            case Some(TypeShape.Identifier(_)) =>
              Left(
                "dynamic Type-constructor capture is supported only in the root applied Type position with one rank-2 argument capture"
              )
            case _ =>
              Left(
                "dynamic Type-constructor capture requires exactly one rank-2 Type-argument capture"
              )
        case Some(actualIndex) if actualIndex != sequenceIndex =>
          Left("ranked tqq template classification changed before compilation")
        case Some(_) => compileDynamic(layout)
    }

  private def compileDynamic(
      layout: RankedTypePatternSupport.Layout
  ): Either[String, Compiled] =
    val sequenceIndex = layout.sequenceIndex.get
    val mapped = TypePattern.rewriteSourceMapped(layout.source)
    TinyTypeParser.parse(mapped.generatedSource).left.map(_.summary).flatMap { parsed =>
      val sequenceName = layout.holeNames(sequenceIndex)
      val generatedSequenceName =
        mapped.generatedHoleIndex.generatedNameFor(sequenceName).getOrElse("")
      parsed.shape match
        case TypeShape.Apply(TypeShape.Identifier(constructorName), arguments) =>
          mapped.generatedHoleIndex.semanticNameFor(constructorName) match
            case None =>
              Left("dynamic Type-constructor capture requires a root constructor hole")
            case Some(constructorHoleName) =>
              val sequencePositions = arguments.zipWithIndex.collect {
                case (TypeShape.Identifier(name), index)
                    if name == generatedSequenceName => index
              }
              sequencePositions match
                case sequencePosition :: Nil =>
                  val fixed = arguments.zipWithIndex.collect {
                    case (argument, index) if index != sequencePosition =>
                      TypePattern
                        .fromShapeWithHoles(argument, mapped.generatedHoleIndex)
                        .left
                        .map(_.message)
                  }
                  collect(fixed).map { compiledArguments =>
                    Compiled(
                      constructorHoleName,
                      compiledArguments.take(sequencePosition),
                      compiledArguments.drop(sequencePosition),
                      layout.holeNames,
                      sequenceName
                    )
                  }
                case _ =>
                  Left(
                    "rank-2 capture is supported only once in the root applied Type argument list"
                  )
        case TypeShape.Apply(_, _) =>
          Left("dynamic Type-constructor capture requires a direct root constructor hole")
        case _ =>
          Left(
            "dynamic Type-constructor capture is supported only in the root applied Type position"
          )
    }

  private def dynamicRootShape(source: String): Option[TypeShape] =
    val mapped = TypePattern.rewriteSourceMapped(source)
    TinyTypeParser.parse(mapped.generatedSource).toOption.map(_.shape)

  private def compileOrAbort(using q: Quotes)(
      parts: List[String],
      sequenceIndex: Int
  ): Compiled =
    compile(parts, sequenceIndex).fold(
      detail =>
        q.reflect.report.errorAndAbort(
          s"Invalid Q054 tqq type-pattern template: $detail"
        ),
      identity
    )

  private def matchCompiled(using q: Quotes)(
      compiled: Compiled,
      target: q.reflect.TypeRepr
  ): Option[MatchData[q.reflect.TypeRepr]] =
    import q.reflect.*

    Option(target).flatMap {
      case AppliedType(constructor, arguments)
          if arguments.size >= compiled.prefix.size + compiled.suffix.size =>
        val initial = Option(
          (
            Map.empty[String, TypeNormalForm],
            Map(compiled.constructorHoleName -> constructor)
          )
        )
        val prefixTargets = arguments.take(compiled.prefix.size)
        val suffixStart = arguments.size - compiled.suffix.size
        val suffixTargets = arguments.drop(suffixStart)
        val afterPrefix = compiled.prefix.zip(prefixTargets).foldLeft(initial) {
          case (state, (pattern, argument)) =>
            state.flatMap(matchArgument(pattern, argument, _))
        }
        val afterSuffix = compiled.suffix.zip(suffixTargets).foldLeft(afterPrefix) {
          case (state, (pattern, argument)) =>
            state.flatMap(matchArgument(pattern, argument, _))
        }
        afterSuffix.map { case (_, scalarBindings) =>
          MatchData(
            scalarBindings,
            Map(
              compiled.sequenceHoleName ->
                arguments.slice(compiled.prefix.size, suffixStart)
            ),
            compiled.holeNames
          )
        }
      case _ => None
    }

  private def matchArgument(using q: Quotes)(
      pattern: TypePattern,
      argument: q.reflect.TypeRepr,
      state: (Map[String, TypeNormalForm], Map[String, q.reflect.TypeRepr])
  ): Option[(Map[String, TypeNormalForm], Map[String, q.reflect.TypeRepr])] =
    TargetTypeReprInspector.inspectWithOrigins(argument).toOption.flatMap { inspection =>
      TypePattern.matchNormalFormWithPaths(pattern, inspection.normalForm).flatMap { trace =>
        val mergedNormalForms = trace.result.bindings.foldLeft(Option(state._1)) {
          case (Some(current), (name, value)) =>
            current.get(name) match
              case Some(existing) if existing != value => None
              case _ => Some(current.updated(name, value))
          case (None, _) => None
        }
        mergedNormalForms.flatMap { normalForms =>
          trace.holePaths.foldLeft(Option(state._2)) {
            case (Some(current), (name, path)) =>
              inspection.originalsByPath
                .get(path)
                .map(value => current.updated(name, value))
            case (None, _) => None
          }.map(normalForms -> _)
        }
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

private object Q054TypePatternProbeMacro:
  def extractor(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        partExpressions.toList.map(_.valueOrAbort)
      case _ =>
        report.errorAndAbort(
          "Q054 direct-reflection probe requires a static StringContext",
          context
        )

    RankedTypePatternSupport.classify(parts) match
      case Left(detail) =>
        report.errorAndAbort(s"Invalid Q054 tqq type-pattern template: $detail", context)
      case Right(layout) =>
        layout.sequenceIndex match
          case None =>
            val detail = Q054TypePatternProbe.compile(parts, 0).left.getOrElse(
              "dynamic Type-constructor capture requires one rank-2 capture"
            )
            report.errorAndAbort(s"Invalid Q054 tqq type-pattern template: $detail", context)
          case Some(sequenceIndex) =>
            Q054TypePatternProbe.compile(parts, sequenceIndex).fold(
              detail =>
                report.errorAndAbort(
                  s"Invalid Q054 tqq type-pattern template: $detail",
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
                '{
                  Q054TypePatternProbe.rankedExtractor[captureKinds & Tuple](
                    $context,
                    ${ Expr(sequenceIndex) }
                  )(using $callerQuotes)
                }
