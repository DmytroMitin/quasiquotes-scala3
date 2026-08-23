package quasiquotes.scalameta

import scala.meta.Dialect
import scala.quoted.Quotes

import quasiquotes.hybrid.TypeQ3DialectPolicy
import quasiquotes.types.TypePattern
import quasiquotes.types.hybrid.{HybridTypeFrontend, ScalametaTypeFrontend}

/** Explicit programmatic entry point for the Scalameta-primary Type frontend.
  * Importing or calling this object never changes the ordinary current-Dotty
  * `tqr` / `tqq` frontend.
  */
object TypeFrontend:
  enum Engine derives CanEqual:
    case Scalameta
    case CurrentDottyFallback

  final case class Failure(
      category: String,
      start: Int,
      end: Int,
      detail: String
  ) derives CanEqual:
    def message: String = s"$category[$start..$end]: $detail"

  final case class BuildResult[T](
      typeRepr: T,
      engine: Engine,
      primaryFailure: Option[Failure]
  )

  final case class CompileResult(
      pattern: TypePattern,
      captureNames: Vector[String],
      engine: Engine,
      primaryFailure: Option[Failure]
  )

  final case class MatchResult[T](
      captures: Vector[T],
      engine: Engine,
      primaryFailure: Option[Failure]
  )

  val defaultDialectName: String = TypeQ3DialectPolicy.selectedName

  def build(using q: Quotes)(
      parts: Seq[String],
      arguments: Seq[q.reflect.TypeRepr],
      dialect: Dialect = TypeQ3DialectPolicy.selected
  ): Either[Failure, BuildResult[q.reflect.TypeRepr]] =
    HybridTypeFrontend
      .construct(parts, arguments, dialect)
      .left
      .map(fromInternalFailure)
      .map(result =>
        BuildResult(
          result.value,
          fromInternalEngine(result.engine),
          result.primaryFailure.map(fromInternalFailure)
        )
      )

  /** Compile an interpolator-shaped pattern from source parts. Each boundary
    * between parts becomes one ordered capture slot.
    */
  def compile(
      parts: Seq[String],
      dialect: Dialect = TypeQ3DialectPolicy.selected
  ): Either[Failure, CompileResult] =
    HybridTypeFrontend
      .compile(parts, dialect)
      .left
      .map(fromInternalFailure)
      .map(fromInternalCompileResult)

  /** Compile a programmatic pattern whose `$name` holes retain semantic name
    * identity, including repeated-hole structural equality.
    */
  def compilePattern(
      source: String,
      dialect: Dialect = TypeQ3DialectPolicy.selected
  ): Either[Failure, CompileResult] =
    HybridTypeFrontend
      .compileProgrammatic(source, dialect)
      .left
      .map(fromInternalFailure)
      .map(fromInternalCompileResult)

  /** Match once-inspected target structure while returning the original
    * caller-owned reflected subtrees for successful captures.
    */
  def matchPattern(using q: Quotes)(
      compiled: CompileResult,
      target: q.reflect.TypeRepr
  ): Either[Failure, Option[MatchResult[q.reflect.TypeRepr]]] =
    val internal = HybridTypeFrontend.CompiledPattern(
      compiled.pattern,
      compiled.captureNames,
      toInternalEngine(compiled.engine),
      compiled.primaryFailure.map(toInternalFailure)
    )
    HybridTypeFrontend
      .matchPattern(internal, target)
      .left
      .map(fromInternalFailure)
      .map(_.map(captures => MatchResult(captures, compiled.engine, compiled.primaryFailure)))

  private def fromInternalCompileResult(
      result: HybridTypeFrontend.CompiledPattern
  ): CompileResult =
    CompileResult(
      result.value,
      result.captureNames,
      fromInternalEngine(result.engine),
      result.primaryFailure.map(fromInternalFailure)
    )

  private def fromInternalEngine(engine: HybridTypeFrontend.Engine): Engine =
    engine match
      case HybridTypeFrontend.Engine.Scalameta => Engine.Scalameta
      case HybridTypeFrontend.Engine.CurrentDottyFallback => Engine.CurrentDottyFallback

  private def toInternalEngine(engine: Engine): HybridTypeFrontend.Engine =
    engine match
      case Engine.Scalameta => HybridTypeFrontend.Engine.Scalameta
      case Engine.CurrentDottyFallback => HybridTypeFrontend.Engine.CurrentDottyFallback

  private def fromInternalFailure(failure: ScalametaTypeFrontend.Failure): Failure =
    Failure(failure.category, failure.start, failure.end, failure.detail)

  private def toInternalFailure(failure: Failure): ScalametaTypeFrontend.Failure =
    ScalametaTypeFrontend.Failure(
      failure.category,
      failure.start,
      failure.end,
      failure.detail
    )
