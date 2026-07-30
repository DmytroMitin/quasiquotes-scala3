package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

import quasiquotes.definitions.*
import quasiquotes.parser.{
  DottySourceSpanAdapter,
  TermShapeInspector,
  TypeShapeInspector
}
import quasiquotes.source.*

private[dotty] object RawDefinitionAdapter:
  def adaptIsolated(
      tree: untpd.Tree,
      source: String,
      sourceId: SourceId
  )(using Context): Either[LocatedDiagnostic[RawDefinitionAdapterError], LocatedDefinitionShape] =
    extractEnvelope(tree, source, sourceId).flatMap(adaptEnvelope(_, sourceId))

  private[quasiquotes] def extractEnvelope(
      tree: untpd.Tree,
      source: String,
      sourceId: SourceId
  )(using Context): Either[
    LocatedDiagnostic[RawDefinitionAdapterError],
    RawDefinitionEnvelope
  ] =
    tree match
      case definition: untpd.DefDef =>
        extractDef(definition, source, sourceId)
      case definition: untpd.ValDef =>
        extractVal(definition, source, sourceId)
      case _: untpd.PatDef =>
        Left(located(RawDefinitionAdapterError.UnsupportedPatternValue, wholeLocation(source, sourceId)))
      case _ =>
        Left(located(RawDefinitionAdapterError.UnsupportedRawDefinitionKind, wholeLocation(source, sourceId)))

  private[quasiquotes] def adaptEnvelope(
      envelope: RawDefinitionEnvelope,
      sourceId: SourceId
  ): Either[
    LocatedDiagnostic[RawDefinitionAdapterError],
    LocatedDefinitionShape
  ] =
    given Context = envelope.context
    val definitionLocation =
      exactLocation(sourceId, envelope.components.definition)
    val typeShape = TypeShapeInspector.inspect(envelope.definitionType)
    val termShape = TermShapeInspector.inspect(envelope.body)
    val create =
      envelope.variant match
        case RawDefinitionVariant.ParameterlessDef =>
          DefinitionShape.parameterlessDef
        case RawDefinitionVariant.ImmutableVal =>
          DefinitionShape.immutableVal
    for
      shape <- create(envelope.name, typeShape, termShape).left.map {
        case error: DefinitionError.UnsupportedDefinitionType =>
          LocatedDiagnostic(
            RawDefinitionAdapterError.UnsupportedDefinitionType(error),
            exactLocation(sourceId, envelope.components.declaredType)
          )
        case error: DefinitionError.UnsupportedDefinitionBody =>
          LocatedDiagnostic(
            RawDefinitionAdapterError.UnsupportedDefinitionBody(error),
            exactLocation(sourceId, envelope.components.body)
          )
        case error =>
          LocatedDiagnostic(
            RawDefinitionAdapterError.IndefensibleComponentSpan(error.message),
            definitionLocation
          )
      }
      locatedShape <- liftDefinition(
        LocatedDefinitionShape.create(
          shape,
          sourceId,
          envelope.components
        ),
        error =>
          RawDefinitionAdapterError.IndefensibleComponentSpan(error.message),
        definitionLocation
      )
    yield locatedShape

  private def extractDef(
      definition: untpd.DefDef,
      source: String,
      sourceId: SourceId
  )(using Context): Either[
    LocatedDiagnostic[RawDefinitionAdapterError],
    RawDefinitionEnvelope
  ] =
    val definitionLocation = treeLocation(definition, sourceId, DiagnosticPrecision.WholeSource)
    if hasUnsupportedMethodModifiers(definition) then
      Left(located(RawDefinitionAdapterError.UnsupportedDefinitionModifiers, definitionLocation))
    else if definition.leadingTypeParams.nonEmpty then
      Left(located(RawDefinitionAdapterError.UnsupportedTypeParameters, definitionLocation))
    else if definition.trailingParamss.nonEmpty then
      Left(located(RawDefinitionAdapterError.UnsupportedParameterClauses, definitionLocation))
    else
      extractComponents(
        definition = definition,
        typeTree = definition.tpt,
        bodyTree = definition.rhs,
        keyword = "def",
        decodedName = definition.name.toString,
        source = source,
        sourceId = sourceId,
        variant = RawDefinitionVariant.ParameterlessDef
      )

  private def extractVal(
      definition: untpd.ValDef,
      source: String,
      sourceId: SourceId
  )(using Context): Either[
    LocatedDiagnostic[RawDefinitionAdapterError],
    RawDefinitionEnvelope
  ] =
    val definitionLocation = treeLocation(definition, sourceId, DiagnosticPrecision.WholeSource)
    if definition.mods.is(Flags.Mutable) then
      Left(located(RawDefinitionAdapterError.UnsupportedMutableValue, definitionLocation))
    else if definition.mods.is(Flags.Lazy) then
      Left(located(RawDefinitionAdapterError.UnsupportedLazyValue, definitionLocation))
    else if definition.mods.hasFlags || definition.mods.hasAnnotations || definition.mods.hasPrivateWithin then
      Left(located(RawDefinitionAdapterError.UnsupportedDefinitionModifiers, definitionLocation))
    else
      extractComponents(
        definition = definition,
        typeTree = definition.tpt,
        bodyTree = definition.rhs,
        keyword = "val",
        decodedName = definition.name.toString,
        source = source,
        sourceId = sourceId,
        variant = RawDefinitionVariant.ImmutableVal
      )

  private def extractComponents(
      definition: untpd.Tree,
      typeTree: untpd.Tree,
      bodyTree: untpd.Tree,
      keyword: String,
      decodedName: String,
      source: String,
      sourceId: SourceId,
      variant: RawDefinitionVariant
  )(using Context): Either[
    LocatedDiagnostic[RawDefinitionAdapterError],
    RawDefinitionEnvelope
  ] =
    val definitionSpan = DottySourceSpanAdapter.fromTree(definition).filter(!_.isEmpty)
    val typeSpan = explicitTypeSpan(typeTree)
    val bodySpan = explicitBodySpan(bodyTree)
    val definitionLocation =
      definitionSpan.flatMap(DiagnosticLocation.direct(sourceId, _, DiagnosticPrecision.WholeSource))

    if typeSpan.isEmpty then
      Left(
        located(
          RawDefinitionAdapterError.MissingExplicitType,
          treeLocation(typeTree, sourceId, DiagnosticPrecision.ExactOccurrence).orElse(definitionLocation)
        )
      )
    else if bodySpan.isEmpty then
      Left(
        located(
          RawDefinitionAdapterError.MissingDefinitionBody,
          treeLocation(bodyTree, sourceId, DiagnosticPrecision.ExactOccurrence).orElse(definitionLocation)
        )
      )
    else
      (definitionSpan, typeSpan, bodySpan) match
        case (Some(complete), Some(declaredType), Some(body)) =>
          for
            nameEvidence <- liftAdapter(
              RawDefinitionSpanAdapter.nameEvidence(source, complete, declaredType, keyword, decodedName),
              definitionLocation
            )
            name <- liftDefinition(
              DefinitionName.fromSource(nameEvidence.sourceSpelling),
              RawDefinitionAdapterError.InvalidDefinitionName.apply,
              exactLocation(sourceId, nameEvidence.span)
            )
            components <- liftDefinition(
              DefinitionComponentSpans.create(complete, nameEvidence.span, declaredType, body),
              error => RawDefinitionAdapterError.IndefensibleComponentSpan(error.message),
              definitionLocation
            )
          yield
            RawDefinitionEnvelope(
              variant,
              definition,
              name,
              typeTree,
              bodyTree,
              components,
              summon[Context]
            )
        case _ =>
          Left(
            located(
              RawDefinitionAdapterError.IndefensibleComponentSpan("source metadata"),
              definitionLocation
            )
          )

  private def explicitTypeSpan(tree: untpd.Tree): Option[SourceSpan] =
    tree match
      case _: untpd.TypeTree | untpd.EmptyTree => None
      case _ => DottySourceSpanAdapter.fromTree(tree).filter(!_.isEmpty)

  private def hasUnsupportedMethodModifiers(definition: untpd.DefDef): Boolean =
    definition.mods.flags != Flags.Method ||
      definition.mods.hasAnnotations ||
      definition.mods.hasPrivateWithin

  private def explicitBodySpan(tree: untpd.Tree): Option[SourceSpan] =
    tree match
      case untpd.EmptyTree => None
      case _ => DottySourceSpanAdapter.fromTree(tree).filter(!_.isEmpty)

  private def treeLocation(
      tree: untpd.Tree,
      sourceId: SourceId,
      precision: DiagnosticPrecision
  ): Option[DiagnosticLocation] =
    DottySourceSpanAdapter
      .fromTree(tree)
      .filter(!_.isEmpty)
      .flatMap(DiagnosticLocation.direct(sourceId, _, precision))

  private def wholeLocation(source: String, sourceId: SourceId): Option[DiagnosticLocation] =
    Option
      .when(source.nonEmpty)(SourceSpan(0, source.length))
      .flatMap(DiagnosticLocation.direct(sourceId, _, DiagnosticPrecision.WholeSource))

  private def exactLocation(sourceId: SourceId, span: SourceSpan): Option[DiagnosticLocation] =
    DiagnosticLocation.direct(sourceId, span, DiagnosticPrecision.ExactOccurrence)

  private def located(
      error: RawDefinitionAdapterError,
      location: Option[DiagnosticLocation]
  ): LocatedDiagnostic[RawDefinitionAdapterError] =
    LocatedDiagnostic(error, location)

  private def liftDefinition[A](
      result: Either[DefinitionError, A],
      adapt: DefinitionError => RawDefinitionAdapterError,
      location: Option[DiagnosticLocation]
  ): Either[LocatedDiagnostic[RawDefinitionAdapterError], A] =
    result.left.map(error => LocatedDiagnostic(adapt(error), location))

  private def liftAdapter[A](
      result: Either[RawDefinitionAdapterError, A],
      location: Option[DiagnosticLocation]
  ): Either[LocatedDiagnostic[RawDefinitionAdapterError], A] =
    result.left.map(error => LocatedDiagnostic(error, location))
