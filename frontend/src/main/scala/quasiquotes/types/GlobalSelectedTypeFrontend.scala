package quasiquotes.types

import quasiquotes.parser.TinyTypeParser
import scala.quoted.*

/** A compiled pattern for the explicit canonical global selected-Type API. */
final case class GlobalSelectedTypePattern(
    source: String,
    pattern: TypePattern,
    captureNames: Vector[String]
)

/** Explicit programmatic selected-Type construction and matching. Ordinary
  * current-Dotty `tqr` / `tqq` entry points remain unchanged.
  */
object GlobalSelectedTypeFrontend:
  def construct(using q: Quotes)(
      source: String,
      environment: GlobalSelectedTypeEnvironment[q.reflect.TypeRepr],
      bindings: (String, q.reflect.TypeRepr)*
  ): Either[TypeQuasiquoteError, q.reflect.TypeRepr] =
    for
      template <- resolvedTemplate(source, environment.semanticEnvironment)
      _ <- rejectExtraBindings(template, bindings)
      normalBindings <- inspectBindings(bindings, environment)
      normalForm <- TypeTemplate.construct(template, normalBindings)
      _ <- TypeTemplate.validateConstructed(normalForm)
      lowered <- lowerResolved(normalForm, environment)
    yield lowered

  def compilePattern(using q: Quotes)(
      source: String,
      environment: GlobalSelectedTypeEnvironment[q.reflect.TypeRepr]
  ): Either[TypeQuasiquoteError, GlobalSelectedTypePattern] =
    val mapped = TypePattern.rewriteSourceMapped(source)
    for
      parsed <- TinyTypeParser
        .parse(mapped.generatedSource)
        .left
        .map(error => TypeQuasiquoteError(error.summary))
      pattern <- TypePattern.fromShapeResolvedWithHoles(
        parsed.shape,
        mapped.generatedHoleIndex,
        environment.semanticEnvironment
      )
    yield GlobalSelectedTypePattern(
      source,
      pattern,
      mapped.occurrences.map(_.name).distinct
    )

  def matchPattern(using q: Quotes)(
      compiled: GlobalSelectedTypePattern,
      target: q.reflect.TypeRepr,
      environment: GlobalSelectedTypeEnvironment[q.reflect.TypeRepr]
  ): Either[TypeQuasiquoteError, Option[Map[String, q.reflect.TypeRepr]]] =
    TargetTypeReprInspector
      .inspectResolvedWithOrigins(target, environment)
      .map { inspection =>
        TypePattern
          .matchNormalFormWithPaths(compiled.pattern, inspection.normalForm)
          .flatMap { trace =>
            compiled.captureNames.foldLeft(
              Option(Map.empty[String, q.reflect.TypeRepr])
            ) { (captures, name) =>
              captures.flatMap { current =>
                trace.holePaths
                  .get(name)
                  .flatMap(inspection.originalsByPath.get)
                  .map(value => current.updated(name, value))
              }
            }
          }
      }

  private[quasiquotes] def resolvedTemplate(
      source: String,
      environment: ResolvedTypeEnvironment
  ): Either[TypeQuasiquoteError, TypeTemplate] =
    val mapped = TypeTemplate.rewriteSourceMapped(source)
    for
      parsed <- TinyTypeParser
        .parse(mapped.generatedSource)
        .left
        .map(error => TypeQuasiquoteError(error.summary))
      template <- TypeTemplate.fromShapeResolvedWithHoles(
        parsed.shape,
        mapped.generatedHoleIndex,
        environment
      )
    yield template

  private[quasiquotes] def lowerResolved(using q: Quotes)(
      normalForm: TypeNormalForm,
      environment: GlobalSelectedTypeEnvironment[q.reflect.TypeRepr]
  ): Either[TypeQuasiquoteError, q.reflect.TypeRepr] =
    import q.reflect.*

    normalForm match
      case TypeNormalForm.STypeResolved(id) =>
        environment.binding(id) match
          case Some(GlobalSelectedTypeEnvironment.Binding(_, witness, GlobalSelectedTypeEnvironment.WitnessRole.Terminal)) =>
            Right(witness)
          case _ =>
            Left(TypeQuasiquoteError(TypeNameResolutionDiagnostics.resolvedFamilyUnsupported(id.canonicalSource)))
      case TypeNormalForm.STypeApply(TypeNormalForm.STypeResolved(id), arguments) =>
        environment.binding(id) match
          case Some(GlobalSelectedTypeEnvironment.Binding(_, constructor, GlobalSelectedTypeEnvironment.WitnessRole.Constructor(arity)))
              if arity == arguments.size && AppliedTypeConstructorPolicy.forResolved(id, arity).isDefined =>
            collect(arguments.map(lowerResolved(_, environment))).map(AppliedType(constructor, _))
          case _ =>
            Left(TypeQuasiquoteError(TypeNameResolutionDiagnostics.constructorPolicyMismatch(id, arguments.size)))
      case TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent(name), arguments)
          if AppliedTypeConstructorPolicy.forConstruction(name, arguments.size).isDefined =>
        collect(arguments.map(lowerResolved(_, environment))).flatMap(lowerFixedApplied(name, _))
      case TypeNormalForm.STypeApply(constructor, arguments) =>
        for
          constructorRepr <- lowerResolved(constructor, environment)
          argumentReprs <- collect(arguments.map(lowerResolved(_, environment)))
        yield AppliedType(constructorRepr, argumentReprs)
      case TypeNormalForm.STypeTuple(elements) =>
        TypeReprLowerer.lowerNormalForm(TypeNormalForm.STypeTuple(elements))
      case TypeNormalForm.STypeFunction(arguments, result) =>
        TypeReprLowerer.lowerNormalForm(TypeNormalForm.STypeFunction(arguments, result))
      case fixed =>
        TypeReprLowerer.lowerNormalForm(fixed)

  private def lowerFixedApplied(using q: Quotes)(
      name: String,
      arguments: List[q.reflect.TypeRepr]
  ): Either[TypeQuasiquoteError, q.reflect.TypeRepr] =
    import q.reflect.*

    (name, arguments) match
      case ("List", argument :: Nil) =>
        argument.asType match
          case '[a] => Right(TypeRepr.of[List[a]])
      case ("Option", argument :: Nil) =>
        argument.asType match
          case '[a] => Right(TypeRepr.of[Option[a]])
      case ("Either", first :: second :: Nil) =>
        first.asType match
          case '[a] =>
            second.asType match
              case '[b] => Right(TypeRepr.of[Either[a, b]])
      case _ =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedAppliedConstructor(name, arguments.size)))

  private def inspectBindings(using q: Quotes)(
      bindings: Seq[(String, q.reflect.TypeRepr)],
      environment: GlobalSelectedTypeEnvironment[q.reflect.TypeRepr]
  ): Either[TypeQuasiquoteError, Map[String, TypeNormalForm]] =
    bindings.foldLeft[Either[TypeQuasiquoteError, Map[String, TypeNormalForm]]](Right(Map.empty)) {
      case (accumulated, (name, witness)) =>
        for
          current <- accumulated
          normalForm <- TargetTypeReprInspector.inspectResolved(witness, environment)
        yield current.updated(name, normalForm)
    }

  private def rejectExtraBindings[T](
      template: TypeTemplate,
      bindings: Seq[(String, T)]
  ): Either[TypeQuasiquoteError, Unit] =
    val expectedNames = TypeTemplate.holeNames(template)
    val extraNames = bindings.iterator.map(_._1).toSet.diff(expectedNames).toList.sorted
    if extraNames.isEmpty then Right(())
    else
      Left(
        TypeQuasiquoteError(
          s"Unexpected type-construction binding(s): ${extraNames.map(name => s"`$$$name`").mkString(", ")}. Remove bindings that do not occur in the template."
        )
      )

  private def collect[A](
      values: List[Either[TypeQuasiquoteError, A]]
  ): Either[TypeQuasiquoteError, List[A]] =
    values.foldRight[Either[TypeQuasiquoteError, List[A]]](Right(Nil)) {
      (value, accumulated) =>
        for
          head <- value
          tail <- accumulated
        yield head :: tail
    }
