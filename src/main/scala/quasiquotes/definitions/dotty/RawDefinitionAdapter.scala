package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

import quasiquotes.definitions.*
import quasiquotes.parser.{DottySourceSpanAdapter, TermShape, TermShapeInspector, TypeShape, TypeShapeInspector}
import quasiquotes.source.*

private[dotty] object RawDefinitionAdapter:
  def adaptIsolated(
      tree: untpd.Tree,
      source: String,
      sourceId: SourceId
  )(using Context): Either[LocatedDiagnostic[RawDefinitionAdapterError], LocatedDefinitionShape] =
    tree match
      case definition: untpd.DefDef => adaptDef(definition, source, sourceId)
      case definition: untpd.ValDef => adaptVal(definition, source, sourceId)
      case _: untpd.PatDef =>
        Left(located(RawDefinitionAdapterError.UnsupportedPatternValue, wholeLocation(source, sourceId)))
      case _ =>
        Left(located(RawDefinitionAdapterError.UnsupportedRawDefinitionKind, wholeLocation(source, sourceId)))

  private def adaptDef(
      definition: untpd.DefDef,
      source: String,
      sourceId: SourceId
  )(using Context): Either[LocatedDiagnostic[RawDefinitionAdapterError], LocatedDefinitionShape] =
    val definitionLocation = treeLocation(definition, sourceId, DiagnosticPrecision.WholeSource)
    if hasUnsupportedMethodModifiers(definition) then
      Left(located(RawDefinitionAdapterError.UnsupportedDefinitionModifiers, definitionLocation))
    else if definition.leadingTypeParams.nonEmpty then
      Left(located(RawDefinitionAdapterError.UnsupportedTypeParameters, definitionLocation))
    else if definition.trailingParamss.nonEmpty then
      Left(located(RawDefinitionAdapterError.UnsupportedParameterClauses, definitionLocation))
    else
      adaptComponents(
        definition = definition,
        typeTree = definition.tpt,
        bodyTree = definition.rhs,
        keyword = "def",
        decodedName = definition.name.toString,
        source = source,
        sourceId = sourceId,
        create = DefinitionShape.parameterlessDef
      )

  private def adaptVal(
      definition: untpd.ValDef,
      source: String,
      sourceId: SourceId
  )(using Context): Either[LocatedDiagnostic[RawDefinitionAdapterError], LocatedDefinitionShape] =
    val definitionLocation = treeLocation(definition, sourceId, DiagnosticPrecision.WholeSource)
    if definition.mods.is(Flags.Mutable) then
      Left(located(RawDefinitionAdapterError.UnsupportedMutableValue, definitionLocation))
    else if definition.mods.is(Flags.Lazy) then
      Left(located(RawDefinitionAdapterError.UnsupportedLazyValue, definitionLocation))
    else if definition.mods.hasFlags || definition.mods.hasAnnotations || definition.mods.hasPrivateWithin then
      Left(located(RawDefinitionAdapterError.UnsupportedDefinitionModifiers, definitionLocation))
    else
      adaptComponents(
        definition = definition,
        typeTree = definition.tpt,
        bodyTree = definition.rhs,
        keyword = "val",
        decodedName = definition.name.toString,
        source = source,
        sourceId = sourceId,
        create = DefinitionShape.immutableVal
      )

  private def adaptComponents(
      definition: untpd.Tree,
      typeTree: untpd.Tree,
      bodyTree: untpd.Tree,
      keyword: String,
      decodedName: String,
      source: String,
      sourceId: SourceId,
      create: (DefinitionName, TypeShape, TermShape) => Either[DefinitionError, DefinitionShape]
  )(using Context): Either[LocatedDiagnostic[RawDefinitionAdapterError], LocatedDefinitionShape] =
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
            typeShape = TypeShapeInspector.inspect(typeTree)
            termShape = TermShapeInspector.inspect(bodyTree)
            shape <- create(name, typeShape, termShape).left.map {
              case error: DefinitionError.UnsupportedDefinitionType =>
                LocatedDiagnostic(
                  RawDefinitionAdapterError.UnsupportedDefinitionType(error),
                  exactLocation(sourceId, declaredType)
                )
              case error: DefinitionError.UnsupportedDefinitionBody =>
                LocatedDiagnostic(
                  RawDefinitionAdapterError.UnsupportedDefinitionBody(error),
                  exactLocation(sourceId, body)
                )
              case error =>
                LocatedDiagnostic(
                  RawDefinitionAdapterError.IndefensibleComponentSpan(error.message),
                  definitionLocation
                )
            }
            components <- liftDefinition(
              DefinitionComponentSpans.create(complete, nameEvidence.span, declaredType, body),
              error => RawDefinitionAdapterError.IndefensibleComponentSpan(error.message),
              definitionLocation
            )
            locatedShape <- liftDefinition(
              LocatedDefinitionShape.create(shape, sourceId, components),
              error => RawDefinitionAdapterError.IndefensibleComponentSpan(error.message),
              definitionLocation
            )
          yield locatedShape
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
