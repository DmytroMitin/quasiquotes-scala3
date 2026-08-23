package quasiquotes.types.hybrid

import scala.meta.*
import scala.meta.parsers.Parsed

import _root_.quasiquotes.hybrid.TypeQ3DialectPolicy
import _root_.quasiquotes.parser.{TinyTypeParser, TypeShape}
import _root_.quasiquotes.source.HoleSourceRewriter
import _root_.quasiquotes.types.*

/** Public-Scalameta parsing followed by project-owned Type semantics. This
  * remains private to the unpublished side-by-side module.
  */
private[quasiquotes] object ScalametaTypeFrontend:
  final case class Failure(
      category: String,
      start: Int,
      end: Int,
      detail: String
  ) derives CanEqual:
    def message: String = s"$category[$start..$end]: $detail"

  object Failure:
    def parse(start: Int, end: Int, detail: String): Failure =
      Failure("SCALAMETA_PARSE_FAILURE", start, end, detail)

    def exactCompiler(detail: String): Failure =
      Failure("EXACT_COMPILER_SYNTAX_REJECTED", 0, 0, detail)

    def unsupported(sourceLength: Int, detail: String): Failure =
      Failure("SCALAMETA_TYPE_LOWERING_UNSUPPORTED", 0, sourceLength, detail)

    def spliceInspection(detail: String): Failure =
      Failure("TYPE_SPLICE_INSPECTION_FAILURE", 0, 0, detail)

    def targetInspection(detail: String): Failure =
      Failure("TYPE_TARGET_INSPECTION_FAILURE", 0, 0, detail)

    def construction(detail: String): Failure =
      Failure("TYPE_CONSTRUCTION_FAILURE", 0, 0, detail)

  def parseShape(
      source: String,
      dialect: Dialect = TypeQ3DialectPolicy.selected
  ): Either[Failure, TypeShape] =
    dialect(source).parse[scala.meta.Type] match
      case Parsed.Success(tree) =>
        validateExactCompiler(source).map(_ => ScalametaTypeShapeMapper.map(tree))
      case error: Parsed.Error =>
        Left(Failure.parse(error.pos.start, error.pos.end, error.message))

  def validateExactCompiler(source: String): Either[Failure, Unit] =
    TinyTypeParser.parse(source) match
      case Right(_) => Right(())
      case Left(error) => Left(Failure.exactCompiler(s"${error.kind}: ${error.summary}"))

  def normalForm(
      source: String,
      dialect: Dialect = TypeQ3DialectPolicy.selected
  ): Either[Failure, TypeNormalForm] =
    parseShape(source, dialect).flatMap { shape =>
      TypeNormalForm
        .fromShape(shape)
        .left
        .map(error => Failure.unsupported(source.length, error.message))
    }

  def template(
      source: String,
      dialect: Dialect = TypeQ3DialectPolicy.selected
  ): Either[Failure, TypeTemplate] =
    val mapped = TypeTemplate.rewriteSourceMapped(source)
    parseShape(mapped.generatedSource, dialect).flatMap { shape =>
      TypeTemplate
        .fromShapeWithHoles(shape, mapped.generatedHoleIndex)
        .left
        .map(error =>
          Failure.unsupported(
            source.length,
            HoleSourceRewriter.restoreSemanticHoleIdentifiers(
              error.message,
              mapped,
              allowUnicodeIdentifiers = false
            )
          )
        )
    }

  def pattern(
      source: String,
      dialect: Dialect = TypeQ3DialectPolicy.selected
  ): Either[Failure, TypePattern] =
    val mapped = TypePattern.rewriteSourceMapped(source)
    parseShape(mapped.generatedSource, dialect).flatMap { shape =>
      TypePattern
        .fromShapeWithHoles(shape, mapped.generatedHoleIndex)
        .left
        .map(error =>
          Failure.unsupported(
            source.length,
            HoleSourceRewriter.restoreSemanticHoleIdentifiers(
              error.message,
              mapped,
              allowUnicodeIdentifiers = false
            )
          )
        )
    }

private[quasiquotes] object HybridTypeFrontend:
  enum Engine derives CanEqual:
    case Scalameta
    case CurrentDottyFallback

  final case class Result[A](
      value: A,
      engine: Engine,
      primaryFailure: Option[ScalametaTypeFrontend.Failure]
  )

  final case class CompiledPattern(
      value: TypePattern,
      captureNames: Vector[String],
      engine: Engine,
      primaryFailure: Option[ScalametaTypeFrontend.Failure]
  )

  def normalForm(
      source: String,
      dialect: Dialect = TypeQ3DialectPolicy.selected
  ): Either[ScalametaTypeFrontend.Failure, Result[TypeNormalForm]] =
    resolveNormalForm(source, ScalametaTypeFrontend.normalForm(source, dialect))

  private[quasiquotes] def resolveNormalForm(
      source: String,
      primaryResult: Either[ScalametaTypeFrontend.Failure, TypeNormalForm]
  ): Either[ScalametaTypeFrontend.Failure, Result[TypeNormalForm]] =
    primaryResult match
      case Right(value) => Right(Result(value, Engine.Scalameta, None))
      case Left(primary) if primary.category == "SCALAMETA_PARSE_FAILURE" =>
        TypeNormalFormSource.fromSource(source) match
          case Right(value) => Right(Result(value, Engine.CurrentDottyFallback, Some(primary)))
          case Left(fallback) => Left(primary.copy(detail = s"${primary.detail}; current parser: ${fallback.message}"))
      case Left(failure) => Left(failure)

  def construct(using q: scala.quoted.Quotes)(
      parts: Seq[String],
      arguments: Seq[q.reflect.TypeRepr],
      dialect: Dialect = TypeQ3DialectPolicy.selected
  ): Either[ScalametaTypeFrontend.Failure, Result[q.reflect.TypeRepr]] =
    checkedSource(parts, arguments.size, "tqr").flatMap { source =>
      inspectBindings(arguments, "tqrSlot").flatMap { bindings =>
        ScalametaTypeFrontend.template(source, dialect) match
          case Right(template) =>
            constructTemplate(template, bindings).map(Result(_, Engine.Scalameta, None))
          case Left(primary) if primary.category == "SCALAMETA_PARSE_FAILURE" =>
            TypeTemplateSource.fromSource(source) match
              case Right(template) =>
                constructTemplate(template, bindings)
                  .map(Result(_, Engine.CurrentDottyFallback, Some(primary)))
              case Left(fallback) =>
                Left(primary.copy(detail = s"${primary.detail}; current parser: ${fallback.message}"))
          case Left(failure) => Left(failure)
      }
    }

  def compile(
      parts: Seq[String],
      dialect: Dialect = TypeQ3DialectPolicy.selected
  ): Either[ScalametaTypeFrontend.Failure, CompiledPattern] =
    val count = parts.size - 1
    checkedSource(parts, count, "tqq").flatMap(source =>
      compileSource(source, Vector.tabulate(count)(index => s"tqqSlot$index"), dialect)
    )

  def compileProgrammatic(
      source: String,
      dialect: Dialect = TypeQ3DialectPolicy.selected
  ): Either[ScalametaTypeFrontend.Failure, CompiledPattern] =
    val names = TypePattern.rewriteSourceMapped(source).occurrences.map(_.name).distinct
    compileSource(source, names, dialect)

  def matchPattern(using q: scala.quoted.Quotes)(
      compiled: CompiledPattern,
      target: q.reflect.TypeRepr
  ): Either[ScalametaTypeFrontend.Failure, Option[Vector[q.reflect.TypeRepr]]] =
    TargetTypeReprInspector
      .inspectWithOrigins(target)
      .left
      .map(error => ScalametaTypeFrontend.Failure.targetInspection(error.message))
      .map { inspection =>
        TypePattern.matchNormalFormWithPaths(compiled.value, inspection.normalForm).flatMap { trace =>
          compiled.captureNames.foldLeft(Option(Vector.empty[q.reflect.TypeRepr])) {
            case (captures, name) =>
              captures.flatMap(current =>
                trace.holePaths
                  .get(name)
                  .flatMap(inspection.originalsByPath.get)
                  .map(current :+ _)
              )
          }
        }
      }

  def extract(using q: scala.quoted.Quotes)(
      compiled: CompiledPattern,
      target: q.reflect.TypeRepr
  ): Option[Vector[q.reflect.TypeRepr]] =
    matchPattern(compiled, target).toOption.flatten

  private def compileSource(
      source: String,
      names: Vector[String],
      dialect: Dialect
  ): Either[ScalametaTypeFrontend.Failure, CompiledPattern] =
    ScalametaTypeFrontend.pattern(source, dialect) match
      case Right(pattern) => Right(CompiledPattern(pattern, names, Engine.Scalameta, None))
      case Left(primary) if primary.category == "SCALAMETA_PARSE_FAILURE" =>
        TypePatternSource.fromSource(source) match
          case Right(pattern) =>
            Right(CompiledPattern(pattern, names, Engine.CurrentDottyFallback, Some(primary)))
          case Left(fallback) =>
            Left(primary.copy(detail = s"${primary.detail}; current parser: ${fallback.message}"))
      case Left(failure) => Left(failure)

  private def inspectBindings(using q: scala.quoted.Quotes)(
      arguments: Seq[q.reflect.TypeRepr],
      prefix: String
  ): Either[ScalametaTypeFrontend.Failure, Map[String, TypeNormalForm]] =
    arguments.zipWithIndex.foldLeft(
      Right(Map.empty): Either[ScalametaTypeFrontend.Failure, Map[String, TypeNormalForm]]
    ) { case (accumulated, (argument, index)) =>
      for
        bindings <- accumulated
        normalForm <- TargetTypeReprInspector
          .inspect(argument)
          .left
          .map(error => ScalametaTypeFrontend.Failure.spliceInspection(error.message))
      yield bindings.updated(s"$prefix$index", normalForm)
    }

  private def constructTemplate(using q: scala.quoted.Quotes)(
      template: TypeTemplate,
      bindings: Map[String, TypeNormalForm]
  ): Either[ScalametaTypeFrontend.Failure, q.reflect.TypeRepr] =
    for
      normalForm <- TypeTemplate
        .construct(template, bindings)
        .left
        .map(error => ScalametaTypeFrontend.Failure.construction(error.message))
      _ <- TypeTemplate
        .validateConstructed(normalForm)
        .left
        .map(error => ScalametaTypeFrontend.Failure.construction(error.message))
      lowered <- TypeReprLowerer
        .lowerNormalForm(normalForm)
        .left
        .map(error => ScalametaTypeFrontend.Failure.construction(error.message))
    yield lowered

  private def checkedSource(
      parts: Seq[String],
      argumentCount: Int,
      prefix: String
  ): Either[ScalametaTypeFrontend.Failure, String] =
    if parts.isEmpty then
      Left(ScalametaTypeFrontend.Failure.construction(s"$prefix requires at least one source part"))
    else if parts.size - 1 != argumentCount then
      Left(
        ScalametaTypeFrontend.Failure.construction(
          s"$prefix expected ${parts.size - 1} slot(s), received $argumentCount"
        )
      )
    else
      Right(
        parts.zipWithIndex.foldLeft(new StringBuilder) { case (builder, (part, index)) =>
          builder.append(part)
          if index < argumentCount then builder.append('$').append(prefix).append("Slot").append(index)
          builder
        }.toString
      )
