package quasiquotes.definitions

import quasiquotes.source.{GeneratedSourceMap, SourceId, SourceSpan}

private[quasiquotes] final class DefinitionComponentSpans private (
    val definition: SourceSpan,
    val name: SourceSpan,
    val declaredType: SourceSpan,
    val body: SourceSpan
) derives CanEqual:
  def render: String =
    s"DefinitionComponentSpans(definition=${span(definition)}, name=${span(name)}, type=${span(declaredType)}, body=${span(body)})"

  override def equals(other: Any): Boolean =
    other match
      case that: DefinitionComponentSpans =>
        definition == that.definition &&
          name == that.name &&
          declaredType == that.declaredType &&
          body == that.body
      case _ => false

  override def hashCode: Int = (definition, name, declaredType, body).hashCode
  override def toString: String = render

  private def span(value: SourceSpan): String = s"[${value.start}, ${value.end})"

private[quasiquotes] object DefinitionComponentSpans:
  def create(
      definition: SourceSpan,
      name: SourceSpan,
      declaredType: SourceSpan,
      body: SourceSpan
  ): Either[DefinitionError, DefinitionComponentSpans] =
    if definition.isEmpty then invalid("the complete definition span must be nonempty")
    else if name.isEmpty then invalid("the name span must be nonempty")
    else if declaredType.isEmpty then invalid("the explicit type span must be nonempty")
    else if body.isEmpty then invalid("the body or right-hand-side span must be nonempty")
    else if !contains(definition, name) then invalid("the name span must be contained by the definition span")
    else if !contains(definition, declaredType) then invalid("the explicit type span must be contained by the definition span")
    else if !contains(definition, body) then invalid("the body or right-hand-side span must be contained by the definition span")
    else if name.end > declaredType.start then invalid("component spans must be ordered and non-overlapping: name before type")
    else if declaredType.end > body.start then invalid("component spans must be ordered and non-overlapping: type before body")
    else Right(new DefinitionComponentSpans(definition, name, declaredType, body))

  private def contains(container: SourceSpan, child: SourceSpan): Boolean =
    container.start <= child.start && child.end <= container.end

  private def invalid(reason: String): Left[DefinitionError, Nothing] =
    Left(DefinitionError.InvalidSourceMetadata(reason))

private[quasiquotes] final class LocatedDefinitionShape private (
    val shape: DefinitionShape,
    val sourceId: SourceId,
    val components: DefinitionComponentSpans,
    val originMap: Option[GeneratedSourceMap]
) derives CanEqual:
  def render: String =
    s"LocatedDefinitionShape(shape=${shape.render}, sourceId=${sourceId.value}, components=${components.render}, originMap=${originMap.fold("none")(_ => "present")})"

  override def equals(other: Any): Boolean =
    other match
      case that: LocatedDefinitionShape =>
        shape == that.shape &&
          sourceId == that.sourceId &&
          components == that.components &&
          originMap == that.originMap
      case _ => false

  override def hashCode: Int = (shape, sourceId, components, originMap).hashCode
  override def toString: String = render

private[quasiquotes] object LocatedDefinitionShape:
  def create(
      shape: DefinitionShape,
      sourceId: SourceId,
      components: DefinitionComponentSpans,
      originMap: Option[GeneratedSourceMap] = None
  ): Either[DefinitionError, LocatedDefinitionShape] =
    shape match
      case _: DefinitionShape.SingleParameterDef =>
        invalid(
          "single-parameter definitions require separate parameter-name and parameter-type evidence"
        )
      case _ => createSupported(shape, sourceId, components, originMap)

  private def createSupported(
      shape: DefinitionShape,
      sourceId: SourceId,
      components: DefinitionComponentSpans,
      originMap: Option[GeneratedSourceMap]
  ): Either[DefinitionError, LocatedDefinitionShape] =
    originMap match
      case None =>
        Right(new LocatedDefinitionShape(shape, sourceId, components, None))
      case Some(map) if map.generatedSourceId != sourceId =>
        invalid("the generated source map identity must equal the located definition source identity")
      case Some(map) if components.definition.end > map.generatedSource.length =>
        invalid("the complete definition span must fit inside the generated source")
      case Some(map) if !fullyCovered(map, components.definition) =>
        invalid("the generated source map must cover the complete definition span without holes")
      case Some(map) =>
        Right(new LocatedDefinitionShape(shape, sourceId, components, Some(map)))

  private def fullyCovered(map: GeneratedSourceMap, span: SourceSpan): Boolean =
    val covered = map.originsFor(span).map(_.generatedSpan)
    covered.nonEmpty &&
      covered.head.start == span.start &&
      covered.last.end == span.end &&
      covered.zip(covered.drop(1)).forall { case (left, right) => left.end == right.start }

  private def invalid(reason: String): Left[DefinitionError, Nothing] =
    Left(DefinitionError.InvalidSourceMetadata(reason))
