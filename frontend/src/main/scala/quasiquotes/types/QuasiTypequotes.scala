package quasiquotes.types

import scala.quoted.*
import scala.annotation.targetName
import scala.util.control.NonFatal

import quasiquotes.parser.{TinyTypeParser, TypeShape}
import quasiquotes.matching.RankedPatternSource

object QuasiTypequotes:
  /** Recommended research-facing pattern convenience; this is a function, not an interpolator. */
  def tqq(source: String)(using Quotes): Either[TypeQuasiquoteError, QuasiTypePattern] =
    QuasiTypePattern.pattern(source)

  /** Recommended research-facing construction convenience; this is a function, not an interpolator. */
  def tqr(
      templateSource: String,
      bindings: (String, TypeNormalForm)*
  ): Either[TypeQuasiquoteError, ConstructedType] =
    QuasiTypeConstruct.fromTemplate(templateSource, bindings*)

  /** Sequence-shaped compatibility overload for eta-expansion beside the interpolator overload. */
  @targetName("tqrFromSeq")
  def tqr(
      templateSource: String,
      bindings: Seq[(String, TypeNormalForm)]
  ): Either[TypeQuasiquoteError, ConstructedType] =
    QuasiTypeConstruct.fromTemplate(templateSource, bindings*)

  extension (sc: StringContext)
    def tqr(using q: Quotes)(args: q.reflect.TypeRepr*): q.reflect.TypeRepr =
      import q.reflect.*

      val parts = checkedParts(sc, "Invalid tqr type template:")
      val expectedArity = parts.size - 1
      if args.size != expectedArity then
        report.errorAndAbort(
          s"Invalid tqr type template: expected $expectedArity TypeRepr splice(s), but received ${args.size}."
        )

      val holeNames = Vector.tabulate(expectedArity)(index => s"tqrSlot$index")
      val source = synthesize(parts, holeNames)
      val bindings = args.zip(holeNames).foldLeft(Map.empty[String, TypeNormalForm]) {
        case (current, (argument, name)) =>
          TargetTypeReprInspector.inspect(argument) match
            case Left(error) =>
              report.errorAndAbort(
                s"Invalid tqr type template: unsupported TypeRepr splice `$name`: ${error.message}"
              )
            case Right(normalForm) => current.updated(name, normalForm)
      }

      lowerCanonicalSelectedTerminal(source, expectedArity) match
        case Some(Left(error)) =>
          report.errorAndAbort(s"Invalid tqr type template: ${error.message}")
        case Some(Right(typeRepr)) => typeRepr
        case None =>
          QuasiTypeConstruct.fromTemplateLocated(source, bindings) match
            case Left(failure) =>
              report.errorAndAbort(
                s"Invalid tqr type template: ${failure.diagnostic.message}"
              )
            case Right(constructed) =>
              constructed.toTypeRepr match
                case Left(error) =>
                  report.errorAndAbort(s"Invalid tqr type template: ${error.message}")
                case Right(typeRepr) => typeRepr

    def tqq(using q: Quotes): TypePatternExtractor[q.reflect.TypeRepr] =
      import q.reflect.*

      val parts = checkedParts(sc, "Invalid tqq type-pattern template:")
      RankedPatternSource
        .unsupportedFamilyRankDiagnostic(parts, "Type")
        .foreach(detail => report.errorAndAbort(s"Invalid tqq type-pattern template: $detail"))
      val holeNames = Vector.tabulate(parts.size - 1)(index => s"tqqSlot$index")
      val source = synthesize(parts, holeNames)

      QuasiTypePattern.patternLocated(source) match
        case Left(failure) =>
          report.errorAndAbort(
            s"Invalid tqq type-pattern template: ${failure.diagnostic.message}"
          )
        case Right(pattern) =>
          new TypePatternExtractor[q.reflect.TypeRepr](target =>
            TargetTypeReprInspector.inspectWithOrigins(target).toOption.flatMap { inspection =>
              TypePattern.matchNormalFormWithPaths(pattern.typePattern, inspection.normalForm).flatMap { trace =>
                holeNames.foldLeft(Option(Vector.empty[q.reflect.TypeRepr])) {
                  case (captures, name) =>
                    captures.flatMap { current =>
                      trace.holePaths.get(name).flatMap(inspection.originalsByPath.get).map(current :+ _)
                    }
                }
              }
            }
          )

  private def checkedParts(sc: StringContext, prefix: String)(using q: Quotes): Seq[String] =
    import q.reflect.report

    if sc == null then report.errorAndAbort(s"$prefix StringContext must not be null.")
    val parts = sc.parts
    if parts == null || parts.isEmpty then
      report.errorAndAbort(s"$prefix StringContext must contain at least one part.")
    parts

  private def lowerCanonicalSelectedTerminal(using q: Quotes)(
      source: String,
      expectedArity: Int
  ): Option[Either[TypeQuasiquoteError, q.reflect.TypeRepr]] =
    if expectedArity != 0 || !isSelectedTerminal(source) then None
    else
      import q.reflect.*
      Some(
        try
          val witness = Symbol.requiredClass(source).typeRef
          for
            environment <- GlobalSelectedTypeEnvironment.fromWitnesses(using q)(witness)
            resolved <- GlobalSelectedTypeFrontend.construct(using q)(
              source,
              environment
            )
          yield resolved
        catch
          case NonFatal(error) =>
            Left(
              TypeQuasiquoteError(
                s"Canonical selected class `$source` could not be resolved: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}"
              )
            )
      )

  private def isSelectedTerminal(source: String): Boolean =
    def selected(shape: TypeShape): Boolean =
      shape match
        case TypeShape.Select(qualifier, _) =>
          qualifier match
            case TypeShape.Identifier(_) => true
            case nested: TypeShape.Select => selected(nested)
            case _ => false
        case _ => false

    TinyTypeParser.parse(source).toOption.exists(parsed => selected(parsed.shape))

  private def synthesize(parts: Seq[String], holeNames: Seq[String]): String =
    parts.zipWithIndex.foldLeft(new StringBuilder) {
      case (builder, (part, index)) =>
        builder.append(part)
        holeNames.lift(index).foreach(name => builder.append('$').append(name))
        builder
    }.toString
