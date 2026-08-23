package quasiquotes.terms.parser

import dotty.tools.dotc.ast.untpd

import quasiquotes.parser.{
  DiagnosticLocationMapper,
  DottySourceSpanAdapter,
  ParseError,
  ParsedExpression,
  TermShape,
  TinyTermParser,
  TypeShape,
  TypeShapeInspector
}
import quasiquotes.source.*
import quasiquotes.terms.{
  LocatedTermHoleOccurrence,
  LocatedTermTemplate,
  TermConstructionError,
  TermHoleOccurrence,
  TermShapeTraversal,
  TermTemplate
}
import quasiquotes.types.{TypeQuasiquoteError, TypeTemplate}

private[quasiquotes] object TermTemplateSourceAdapter:
  import TermTemplateHoleCategory.*
  import TermTemplateSourceAdapterError.*

  private val TermPrefix = "__qq_tt_term_"
  private val TypePrefix = "__qq_tt_type_"

  private[parser] final case class RawIdentifier(
      name: String,
      span: Option[SourceSpan]
  )

  private[parser] final case class RawField(name: String)

  def parse(
      source: String,
      occurrences: Vector[CategorizedHoleOccurrence]
  ): Either[TermTemplateSourceAdapterError, TermTemplate] =
    parseLocated(source, occurrences).left.map(_.diagnostic).map(_.template)

  def parseLocated(
      source: String,
      occurrences: Vector[CategorizedHoleOccurrence]
  ): Either[
    LocatedDiagnostic[TermTemplateSourceAdapterError],
    LocatedTermTemplate
  ] =
    parseLocatedUsing(source, occurrences)(TinyTermParser.parse)

  private[parser] def parseLocatedUsing(
      source: String,
      occurrences: Vector[CategorizedHoleOccurrence]
  )(
      parseGenerated: String => Either[ParseError, ParsedExpression]
  ): Either[
    LocatedDiagnostic[TermTemplateSourceAdapterError],
    LocatedTermTemplate
  ] =
    parseLocatedUsingPrefixes(
      source,
      occurrences,
      TermPrefix,
      TypePrefix
    )(parseGenerated)

  private[parser] def parseLocatedUsingPrefixes(
      source: String,
      occurrences: Vector[CategorizedHoleOccurrence],
      termPrefix: String,
      typePrefix: String
  )(
      parseGenerated: String => Either[ParseError, ParsedExpression]
  ): Either[
    LocatedDiagnostic[TermTemplateSourceAdapterError],
    LocatedTermTemplate
  ] =
    val scan =
      HoleSourceRewriter.scan(source, allowUnicodeIdentifiers = false)

    validatePlan(source, scan, occurrences).flatMap { roles =>
      val mapped = HoleSourceRewriter.rewriteScannedCategorized(
        source,
        scan,
        roles,
        {
          case HoleRole.TermTemplate => termPrefix
          case HoleRole.TypeTemplate => typePrefix
          case other => s"__qq_tt_${other.toString.toLowerCase}_"
        },
        SourceId.TermConstructionTemplate,
        SourceId.VirtualExpressionParserInput
      )

      validateSourceMap(mapped).flatMap { _ =>
        parseGenerated(mapped.generatedSource) match
          case Left(error) =>
            Left(
              LocatedDiagnostic(
                ParserFailure(
                  HoleSourceRewriter.restoreSemanticHoleIdentifiers(
                    error.summary,
                    mapped,
                    allowUnicodeIdentifiers = false
                  )
                ),
                DiagnosticLocationMapper
                  .fromParseError(error, mapped.originMap)
                  .orElse(wholeLocation(mapped.originMap))
              )
            )
          case Right(parsed) =>
            DottySourceSpanAdapter.fromTree(parsed.rawTree) match
              case Some(componentSpan) =>
                RawTermTemplateAdapter.adapt(
                  scan,
                  mapped,
                  parsed.rawTree,
                  parsed.shape,
                  HoleRole.TermTemplate,
                  HoleRole.TypeTemplate,
                  componentSpan,
                  Vector(termPrefix, typePrefix)
                )
              case None =>
                Left(
                  LocatedDiagnostic(
                    InvalidSourceMetadata(
                      "the parsed expression lacks an exact component span"
                    ),
                    wholeLocation(mapped.originMap)
                  )
                )
      }
    }

  private def validatePlan(
      source: String,
      scan: HoleSourceRewriter.SourceScan,
      occurrences: Vector[CategorizedHoleOccurrence]
  ): Either[
    LocatedDiagnostic[TermTemplateSourceAdapterError],
    Vector[HoleRole]
  ] =
    occurrences.zipWithIndex
      .find { case (occurrence, _) => !isValidHoleName(occurrence.name) }
      .map { case (occurrence, index) =>
        Left(
          LocatedDiagnostic(
            InvalidHoleName(index, occurrence.name),
            wholeOriginalLocation(source)
          )
        )
      }
      .orElse(
        scan.invalidDollarSpans.headOption.map { span =>
          Left(
            LocatedDiagnostic(
              InvalidDollarSyntax(source.slice(span.start, span.end)),
              DiagnosticLocation.direct(
                SourceId.TermConstructionTemplate,
                span,
                DiagnosticPrecision.ExactOccurrence
              )
            )
          )
        }
      )
      .orElse(
        Option
          .when(scan.holes.size != occurrences.size) {
            val location =
              scan.holes
                .lift(occurrences.size)
                .flatMap(hole =>
                  DiagnosticLocation.direct(
                    SourceId.TermConstructionTemplate,
                    SourceSpan(hole.start, hole.end),
                    DiagnosticPrecision.ExactOccurrence
                  )
                )
                .orElse(wholeOriginalLocation(source))
            Left(
              LocatedDiagnostic(
                OccurrenceCountMismatch(
                  expected = scan.holes.size,
                  actual = occurrences.size
                ),
                location
              )
            )
          }
      )
      .orElse(
        scan.holes
          .zip(occurrences)
          .zipWithIndex
          .collectFirst {
            case ((hole, occurrence), index)
                if hole.name != occurrence.name =>
              Left(
                LocatedDiagnostic(
                  OccurrenceNameMismatch(
                    index,
                    hole.name,
                    occurrence.name
                  ),
                  DiagnosticLocation.direct(
                    SourceId.TermConstructionTemplate,
                    SourceSpan(hole.start, hole.end),
                    DiagnosticPrecision.ExactOccurrence
                  )
                )
              )
          }
      )
      .getOrElse {
        Right(
          occurrences.map {
            case CategorizedHoleOccurrence(_, Term) =>
              HoleRole.TermTemplate
            case CategorizedHoleOccurrence(_, Type) =>
              HoleRole.TypeTemplate
          }
        )
      }

  private def validateSourceMap(
      mapped: CategorizedMappedHoleSource
  ): Either[
    LocatedDiagnostic[TermTemplateSourceAdapterError],
    Unit
  ] =
    val spans = mapped.originMap.segments.map(_.generatedSpan)
    val complete =
      if mapped.generatedSource.isEmpty then spans.isEmpty
      else
        spans.nonEmpty &&
          spans.head.start == 0 &&
          spans.last.end == mapped.generatedSource.length &&
          spans.zip(spans.drop(1)).forall { case (left, right) =>
            left.end == right.start
          }
    Either.cond(
      complete,
      (),
      LocatedDiagnostic(
        InvalidSourceMetadata(
          "the generated source map does not cover the complete source"
        ),
        wholeLocation(mapped.originMap)
      )
    )

  private def wholeLocation(
      sourceMap: GeneratedSourceMap
  ): Option[DiagnosticLocation] =
    DiagnosticLocationMapper.wholeSource(sourceMap)

  private def wholeOriginalLocation(
      source: String
  ): Option[DiagnosticLocation] =
    Option
      .when(source.nonEmpty)(SourceSpan(0, source.length))
      .flatMap(
        DiagnosticLocation.direct(
          SourceId.TermConstructionTemplate,
          _,
          DiagnosticPrecision.WholeSource
        )
      )

  private def isValidHoleName(name: String): Boolean =
    name.nonEmpty &&
      isIdentifierStart(name.head) &&
      name.tail.forall(isIdentifierPart)

  private def isIdentifierStart(char: Char): Boolean =
    char == '_' ||
      ('A' <= char && char <= 'Z') ||
      ('a' <= char && char <= 'z')

  private def isIdentifierPart(char: Char): Boolean =
    isIdentifierStart(char) || ('0' <= char && char <= '9')

private[quasiquotes] object RawTermTemplateAdapter:
  import TermTemplateSourceAdapter.{RawField, RawIdentifier}
  import TermTemplateSourceAdapterError.*

  def adapt(
      scan: HoleSourceRewriter.SourceScan,
      mapped: CategorizedMappedHoleSource,
      rawTree: untpd.Tree,
      shape: TermShape,
      termRole: HoleRole,
      typeRole: HoleRole,
      componentSpan: SourceSpan,
      generatedPrefixes: Vector[String]
  ): Either[
    LocatedDiagnostic[TermTemplateSourceAdapterError],
    LocatedTermTemplate
  ] =
    val termIndex = mapped.generatedHoleIndex(termRole)
    val typeIndex = mapped.generatedHoleIndex(typeRole)
    val typedTypeTrees = rawTypedTypeTrees(rawTree)
    val typeTreeSpans =
      typedTypeTrees.flatMap(DottySourceSpanAdapter.fromTree)

    def failure(
        error: TermTemplateSourceAdapterError,
        occurrence: Option[HoleOccurrence] = None
    ): Left[
      LocatedDiagnostic[TermTemplateSourceAdapterError],
      Nothing
    ] =
      Left(
        LocatedDiagnostic(
          error,
          occurrence
            .flatMap(item => exactLocation(mapped.originMap, item.generatedSpan))
            .orElse(wholeLocation(mapped.originMap))
        )
      )

    val outsideComponent =
      mapped.occurrences.find(occurrence =>
        (occurrence.role == termRole || occurrence.role == typeRole) &&
          !contains(componentSpan, occurrence.generatedSpan)
      )
    val duplicateGenerated =
      (termIndex.generatedNames intersect typeIndex.generatedNames)
        .toVector
        .sorted
        .headOption
    outsideComponent match
      case Some(occurrence) =>
        failure(
          InvalidSourceMetadata(
            s"hole `$$${occurrence.name}` is outside the adapted raw component"
          ),
          Some(occurrence)
        )
      case None =>
        duplicateGenerated match
          case Some(occurrence) =>
            failure(DuplicateGeneratedIdentity(occurrence))
          case None =>
            val termInsideType =
              mapped.occurrences.find(occurrence =>
                occurrence.role == termRole &&
                  typeTreeSpans.exists(contains(_, occurrence.generatedSpan))
              )
            termInsideType match
              case Some(occurrence) =>
                failure(
                  TermMarkerInsideType(occurrence.name),
                  Some(occurrence)
                )
              case None =>
                val typeOutsideAscription =
                  mapped.occurrences.find(occurrence =>
                    occurrence.role == typeRole &&
                      !typeTreeSpans.exists(
                        contains(_, occurrence.generatedSpan)
                      )
                  )
                typeOutsideAscription match
                  case Some(occurrence) =>
                    val rawTermNames =
                      rawTermIdentifiers(rawTree).map(_.name).toSet
                    if rawTermNames(occurrence.generatedName) then
                      failure(
                        TypeMarkerInTermPosition(occurrence.name),
                        Some(occurrence)
                      )
                    else
                      failure(
                        TypeMarkerOutsideAscription(occurrence.name),
                        Some(occurrence)
                      )
                  case None =>
                    adaptOwnedParsed(
                      scan,
                      mapped,
                      rawTree,
                      shape,
                      termIndex,
                      typeIndex,
                      typedTypeTrees,
                      termRole,
                      typeRole,
                      generatedPrefixes,
                      failure
                    )

  private def adaptOwnedParsed(
      scan: HoleSourceRewriter.SourceScan,
      mapped: CategorizedMappedHoleSource,
      rawTree: untpd.Tree,
      shape: TermShape,
      termIndex: GeneratedHoleIndex,
      typeIndex: GeneratedHoleIndex,
      typedTypeTrees: Vector[untpd.Tree],
      termRole: HoleRole,
      typeRole: HoleRole,
      generatedPrefixes: Vector[String],
      failure: (
          TermTemplateSourceAdapterError,
          Option[HoleOccurrence]
      ) => Left[
        LocatedDiagnostic[TermTemplateSourceAdapterError],
        Nothing
      ]
  ): Either[
    LocatedDiagnostic[TermTemplateSourceAdapterError],
    LocatedTermTemplate
  ] =
    firstUnsupported(shape) match
      case Some(detail) =>
        failure(UnsupportedTermShape(detail), None)
      case None =>
        val rawIdentifiers = rawTermIdentifiers(rawTree)
        val shapeIdentifiers =
          TermShapeTraversal.identifierEntries(shape)
        if rawIdentifiers.map(_.name) != shapeIdentifiers.map(_.name) then
          failure(
            SidecarOrderMismatch(
              "raw-tree and TermShape identifier preorder disagree"
            ),
            None
          )
        else
          val invalidTermField =
            rawTermFields(rawTree)
              .find(field => termIndex.semanticNameFor(field.name).nonEmpty)
          invalidTermField match
            case Some(field) =>
              val semanticName =
                termIndex.semanticNameFor(field.name).get
              failure(
                TermMarkerInInvalidPosition(semanticName),
                firstOccurrence(mapped, field.name)
              )
            case None =>
              val typeInTermField =
                rawTermFields(rawTree)
                  .find(field =>
                    typeIndex.semanticNameFor(field.name).nonEmpty
                  )
              typeInTermField match
                case Some(field) =>
                  val semanticName =
                    typeIndex.semanticNameFor(field.name).get
                  failure(
                    TypeMarkerInTermPosition(semanticName),
                    firstOccurrence(mapped, field.name)
                  )
                case None =>
                  val unowned =
                    (rawIdentifiers.map(_.name) ++
                      rawTermFields(rawTree).map(_.name))
                      .find(name =>
                        isGeneratedName(name, generatedPrefixes) &&
                          !scan.literalIdentifiers(name) &&
                          termIndex.semanticNameFor(name).isEmpty &&
                          typeIndex.semanticNameFor(name).isEmpty
                      )
                  unowned match
                    case Some(name) =>
                      failure(UnknownGeneratedMarker(name), None)
                    case None =>
                      buildTemplate(
                        mapped,
                        shape,
                        rawIdentifiers,
                        shapeIdentifiers,
                        typedTypeTrees,
                        termIndex,
                        typeIndex,
                        termRole,
                        typeRole,
                        failure
                      )

  private def buildTemplate(
      mapped: CategorizedMappedHoleSource,
      shape: TermShape,
      rawIdentifiers: Vector[RawIdentifier],
      shapeIdentifiers: Vector[TermShapeTraversal.IdentifierEntry],
      typedTypeTrees: Vector[untpd.Tree],
      termIndex: GeneratedHoleIndex,
      typeIndex: GeneratedHoleIndex,
      termRole: HoleRole,
      typeRole: HoleRole,
      failure: (
          TermTemplateSourceAdapterError,
          Option[HoleOccurrence]
      ) => Left[
        LocatedDiagnostic[TermTemplateSourceAdapterError],
        Nothing
      ]
  ): Either[
    LocatedDiagnostic[TermTemplateSourceAdapterError],
    LocatedTermTemplate
  ] =
    val termOccurrences =
      rawIdentifiers.zip(shapeIdentifiers).flatMap {
        case (raw, shape) =>
          termIndex.semanticNameFor(raw.name).map { semanticName =>
            val sourceOccurrence =
              exactOccurrence(mapped, raw.name, raw.span)
            semanticName -> (shape.ordinal -> sourceOccurrence)
          }
      }

    termOccurrences.collectFirst {
      case (name, (_, None)) => name
    } match
      case Some(name) =>
        failure(
          InvalidSourceMetadata(
            s"term hole `$$$name` lacks exact parser-span ownership"
          ),
          firstSemanticOccurrence(mapped, name, termRole)
        )
      case None =>
        val semanticTermOccurrences =
          termOccurrences.map { case (name, (ordinal, _)) =>
            TermHoleOccurrence(name, ordinal)
          }
        val locatedTermOccurrences =
          termOccurrences.map { case (name, (ordinal, sourceOccurrence)) =>
            LocatedTermHoleOccurrence(
              TermHoleOccurrence(name, ordinal),
              sourceOccurrence.get
            )
          }
        val mappedTerms =
          mapped.occurrences.filter(_.role == termRole)
        if locatedTermOccurrences.map(_.source) != mappedTerms then
          val missing = mappedTerms.find(item =>
            !locatedTermOccurrences.exists(_.source == item)
          )
          failure(
            TermMarkerInInvalidPosition(
              missing.map(_.name).getOrElse("<unknown>")
            ),
            missing
          )
        else
          buildSidecars(
            mapped,
            shape,
            typedTypeTrees,
            termIndex,
            typeIndex,
            semanticTermOccurrences,
            locatedTermOccurrences,
            termRole,
            typeRole,
            failure
          )

  private def buildSidecars(
      mapped: CategorizedMappedHoleSource,
      shape: TermShape,
      typedTypeTrees: Vector[untpd.Tree],
      termIndex: GeneratedHoleIndex,
      typeIndex: GeneratedHoleIndex,
      semanticTermOccurrences: Vector[TermHoleOccurrence],
      locatedTermOccurrences: Vector[LocatedTermHoleOccurrence],
      termRole: HoleRole,
      typeRole: HoleRole,
      failure: (
          TermTemplateSourceAdapterError,
          Option[HoleOccurrence]
      ) => Left[
        LocatedDiagnostic[TermTemplateSourceAdapterError],
        Nothing
      ]
  ): Either[
    LocatedDiagnostic[TermTemplateSourceAdapterError],
    LocatedTermTemplate
  ] =
    val sidecarResults = typedTypeTrees.map { typeTree =>
      val shape = TypeShapeInspector.inspect(typeTree)
      firstOwnedTypeField(shape, termIndex) match
        case Some(generatedName) =>
          Left(
            TermMarkerInsideType(
              termIndex.semanticNameFor(generatedName).get
            )
          )
        case None =>
          TypeTemplate
            .fromShapeWithHoles(shape, typeIndex)
            .left
            .map(error => UnsupportedTypeTemplateShape(error.message))
    }

    sidecarResults.collectFirst { case Left(error) => error } match
      case Some(error @ TermMarkerInsideType(name)) =>
        failure(
          error,
          firstSemanticOccurrence(mapped, name, termRole)
        )
      case Some(error) =>
        failure(error, None)
      case None =>
        val sidecars =
          sidecarResults.collect { case Right(sidecar) => sidecar }
        val orderedTypeOccurrences =
          typedTypeTrees.flatMap { typeTree =>
            DottySourceSpanAdapter
              .fromTree(typeTree)
              .toVector
              .flatMap { span =>
                mapped.occurrences
                  .filter(occurrence =>
                    occurrence.role == typeRole &&
                      contains(span, occurrence.generatedSpan)
                  )
                  .sortBy(_.generatedSpan.start)
              }
          }
        val expectedTypeNames =
          sidecars.flatMap(TermShapeTraversal.typeHoleOccurrences)
        if orderedTypeOccurrences.map(_.name) != expectedTypeNames then
          failure(
            SidecarOrderMismatch(
              s"expected ${expectedTypeNames.mkString("[", ", ", "]")} but extracted ${orderedTypeOccurrences.map(_.name).mkString("[", ", ", "]")}"
            ),
            None
          )
        else if
          orderedTypeOccurrences.toSet !=
            mapped.occurrences
              .filter(_.role == typeRole)
              .toSet
        then
          failure(
            SidecarOrderMismatch(
              "not every categorized type occurrence belongs to exactly one typed-node sidecar"
            ),
            None
          )
        else
          TermTemplate.create(
            shape,
            termIndex,
            semanticTermOccurrences,
            typeIndex,
            sidecars
          ) match
            case Left(error) =>
              failure(mapConstructionError(error, shape), None)
            case Right(template) =>
              LocatedTermTemplate.create(
                template,
                mapped.originMap,
                locatedTermOccurrences,
                orderedTypeOccurrences,
                termRole,
                typeRole
              ) match
                case Left(error) =>
                  failure(InvalidSourceMetadata(error.message), None)
                case Right(located) =>
                  Right(located)

  private def mapConstructionError(
      error: TermConstructionError,
      shape: TermShape
  ): TermTemplateSourceAdapterError =
    error match
      case TermConstructionError.UnsupportedTermShape() =>
        UnsupportedTermShape(shape.render)
      case TermConstructionError.DuplicateGeneratedIdentifier(name) =>
        DuplicateGeneratedIdentity(name)
      case TermConstructionError.InvalidTermHolePosition(name) =>
        TermMarkerInInvalidPosition(name)
      case TermConstructionError.TypeHoleMarkerInTermPosition(_) =>
        TypeMarkerInTermPosition("<unknown>")
      case other =>
        DownstreamConstructionFailure(other.message)

  private def rawTermIdentifiers(tree: untpd.Tree): Vector[RawIdentifier] =
    def loop(current: untpd.Tree, boundNames: List[String]): Vector[RawIdentifier] =
      current match
        case untpd.Function((parameter: untpd.ValDef) :: Nil, body) =>
          loop(body, parameter.name.toString :: boundNames)
        case ident @ untpd.Ident(name) if !boundNames.contains(name.toString) =>
          Vector(RawIdentifier(name.toString, DottySourceSpanAdapter.fromTree(ident)))
        case _: untpd.Ident =>
          Vector.empty
        case untpd.Select(qualifier, _) =>
          loop(qualifier, boundNames)
        case untpd.Apply(function, arguments) =>
          loop(function, boundNames) ++
            arguments.toVector.flatMap(loop(_, boundNames))
        case untpd.InfixOp(left, _, right) =>
          loop(left, boundNames) ++ loop(right, boundNames)
        case untpd.PrefixOp(_, operand) =>
          loop(operand, boundNames)
        case untpd.Typed(expression, _) =>
          loop(expression, boundNames)
        case untpd.Tuple(elements) =>
          elements.toVector.flatMap(loop(_, boundNames))
        case untpd.If(condition, thenBranch, elseBranch) =>
          loop(condition, boundNames) ++
            loop(thenBranch, boundNames) ++
            loop(elseBranch, boundNames)
        case untpd.Block(statements, result) =>
          statements.toVector.flatMap(loop(_, boundNames)) ++ loop(result, boundNames)
        case untpd.Parens(inner) =>
          loop(inner, boundNames)
        case untpd.TypedSplice(inner) =>
          loop(inner, boundNames)
        case _ =>
          Vector.empty

    loop(tree, Nil)

  private def rawTermFields(tree: untpd.Tree): Vector[RawField] =
    tree match
      case untpd.Function(_ :: Nil, body) =>
        rawTermFields(body)
      case untpd.Select(qualifier, name) =>
        RawField(name.toString) +: rawTermFields(qualifier)
      case untpd.Apply(function, arguments) =>
        rawTermFields(function) ++
          arguments.toVector.flatMap(rawTermFields)
      case untpd.InfixOp(left, operator, right) =>
        RawField(operator.name.toString) +:
          (rawTermFields(left) ++ rawTermFields(right))
      case untpd.PrefixOp(untpd.Ident(operator), operand) =>
        RawField(operator.toString) +: rawTermFields(operand)
      case untpd.Typed(expression, _) =>
        rawTermFields(expression)
      case untpd.Tuple(elements) =>
        elements.toVector.flatMap(rawTermFields)
      case untpd.If(condition, thenBranch, elseBranch) =>
        rawTermFields(condition) ++
          rawTermFields(thenBranch) ++
          rawTermFields(elseBranch)
      case untpd.Block(statements, result) =>
        statements.toVector.flatMap(rawTermFields) ++ rawTermFields(result)
      case untpd.Parens(inner) =>
        rawTermFields(inner)
      case untpd.TypedSplice(inner) =>
        rawTermFields(inner)
      case _ =>
        Vector.empty

  private def rawTypedTypeTrees(tree: untpd.Tree): Vector[untpd.Tree] =
    tree match
      case untpd.Function((parameter: untpd.ValDef) :: Nil, body) =>
        parameter.tpt +: rawTypedTypeTrees(body)
      case untpd.Select(qualifier, _) =>
        rawTypedTypeTrees(qualifier)
      case untpd.Apply(function, arguments) =>
        rawTypedTypeTrees(function) ++
          arguments.toVector.flatMap(rawTypedTypeTrees)
      case untpd.InfixOp(left, _, right) =>
        rawTypedTypeTrees(left) ++ rawTypedTypeTrees(right)
      case untpd.PrefixOp(_, operand) =>
        rawTypedTypeTrees(operand)
      case interpolation: untpd.InterpolatedString =>
        interpolation.segments.toVector.flatMap(rawTypedTypeTrees)
      case thicket: untpd.Thicket =>
        thicket.trees.toVector.flatMap(rawTypedTypeTrees)
      case untpd.Typed(expression, typeTree) =>
        typeTree +: rawTypedTypeTrees(expression)
      case untpd.Tuple(elements) =>
        elements.toVector.flatMap(rawTypedTypeTrees)
      case untpd.If(condition, thenBranch, elseBranch) =>
        rawTypedTypeTrees(condition) ++
          rawTypedTypeTrees(thenBranch) ++
          rawTypedTypeTrees(elseBranch)
      case untpd.Block(statements, result) =>
        statements.toVector.flatMap(rawTypedTypeTrees) ++ rawTypedTypeTrees(result)
      case untpd.Parens(inner) =>
        rawTypedTypeTrees(inner)
      case untpd.TypedSplice(inner) =>
        rawTypedTypeTrees(inner)
      case _ =>
        Vector.empty

  private def firstOwnedTypeField(
      shape: TypeShape,
      index: GeneratedHoleIndex
  ): Option[String] =
    shape match
      case TypeShape.Identifier(name) =>
        Option.when(index.semanticNameFor(name).nonEmpty)(name)
      case TypeShape.Select(qualifier, name) =>
        firstOwnedTypeField(qualifier, index)
          .orElse(Option.when(index.semanticNameFor(name).nonEmpty)(name))
      case TypeShape.Apply(constructor, arguments) =>
        firstOwnedTypeField(constructor, index)
          .orElse(arguments.iterator.flatMap(firstOwnedTypeField(_, index)).nextOption())
      case TypeShape.Tuple(elements) =>
        elements.iterator.flatMap(firstOwnedTypeField(_, index)).nextOption()
      case TypeShape.Function(arguments, result) =>
        arguments.iterator
          .flatMap(firstOwnedTypeField(_, index))
          .nextOption()
          .orElse(firstOwnedTypeField(result, index))
      case TypeShape.Parenthesized(inner) =>
        firstOwnedTypeField(inner, index)
      case TypeShape.Unsupported(_, _) =>
        None

  private def firstUnsupported(shape: TermShape): Option[String] =
    shape match
      case TermShape.Unsupported(nodeKind, detail) =>
        Some(s"$nodeKind: $detail")
      case TermShape.Select(qualifier, _) =>
        firstUnsupported(qualifier)
      case TermShape.Apply(function, arguments) =>
        firstUnsupported(function)
          .orElse(arguments.iterator.flatMap(firstUnsupported).nextOption())
      case TermShape.New(_, arguments) =>
        arguments.iterator.flatMap(firstUnsupported).nextOption()
      case TermShape.Infix(left, _, right) =>
        firstUnsupported(left).orElse(firstUnsupported(right))
      case TermShape.Unary(_, operand) =>
        firstUnsupported(operand)
      case TermShape.InterpolatedString(_, _, arguments) =>
        arguments.iterator.flatMap(firstUnsupported).nextOption()
      case TermShape.Typed(expression, _) =>
        firstUnsupported(expression)
      case TermShape.Tuple(elements) =>
        elements.iterator.flatMap(firstUnsupported).nextOption()
      case TermShape.If(condition, thenBranch, elseBranch) =>
        firstUnsupported(condition)
          .orElse(firstUnsupported(thenBranch))
          .orElse(firstUnsupported(elseBranch))
      case TermShape.Block(prefix, result) =>
        prefix.iterator.flatMap(firstUnsupported).nextOption()
          .orElse(firstUnsupported(result))
      case TermShape.Parenthesized(expression) =>
        firstUnsupported(expression)
      case TermShape.Lambda1(_, _, _, body) =>
        firstUnsupported(body)
      case TermShape.Identifier(_, _) | TermShape.BoundReference(_, _) | TermShape.Literal(_) =>
        None

  private def exactOccurrence(
      mapped: CategorizedMappedHoleSource,
      generatedName: String,
      span: Option[SourceSpan]
  ): Option[HoleOccurrence] =
    span.flatMap(exactSpan =>
      mapped.occurrences.find(occurrence =>
        occurrence.generatedName == generatedName &&
          occurrence.generatedSpan == exactSpan
      )
    )

  private def firstOccurrence(
      mapped: CategorizedMappedHoleSource,
      generatedName: String
  ): Option[HoleOccurrence] =
    mapped.occurrences.find(_.generatedName == generatedName)

  private def firstSemanticOccurrence(
      mapped: CategorizedMappedHoleSource,
      semanticName: String,
      role: HoleRole
  ): Option[HoleOccurrence] =
    mapped.occurrences.find(occurrence =>
      occurrence.name == semanticName && occurrence.role == role
    )

  private def exactLocation(
      sourceMap: GeneratedSourceMap,
      span: SourceSpan
  ): Option[DiagnosticLocation] =
    DiagnosticLocation.fromGeneratedMap(
      sourceMap,
      span,
      DiagnosticPrecision.ExactOccurrence
    )

  private def wholeLocation(
      sourceMap: GeneratedSourceMap
  ): Option[DiagnosticLocation] =
    DiagnosticLocationMapper.wholeSource(sourceMap)

  private def contains(outer: SourceSpan, inner: SourceSpan): Boolean =
    outer.start <= inner.start && inner.end <= outer.end

  private def isGeneratedName(
      name: String,
      prefixes: Vector[String]
  ): Boolean =
    prefixes.exists(name.startsWith)
