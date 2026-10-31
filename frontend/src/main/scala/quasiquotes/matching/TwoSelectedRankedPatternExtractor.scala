package quasiquotes.matching

import scala.collection.mutable
import scala.quoted.*

import quasiquotes.construct.SelectedMemberName
import quasiquotes.definitions.DefinitionName
import quasiquotes.parser.{TermShape, TinyTermParser}
import quasiquotes.source.{HoleSourceRewriter, ReflectedPositionProvenance, SourceOrigin, SourceSpan}

/** Standard-only N2 terminal ordinary-Apply source grammar. The mapped input
  * still contains the original rank marker: no rank-erased offsets are used.
  */
private[matching] object TwoSelectedRankedPatternSource:
  val Parts: List[String] = List("", ".", ".", "(..", ")")

  final case class Plan(
      mapped: MappedPatternSource,
      rankMarkerSpan: SourceSpan,
      effectiveSource: String,
      pattern: TermPattern,
      receiverName: String,
      sequenceName: String,
      selectedOccurrences: Vector[SelectedNamePatternOccurrence]
  )

  def classify(parts: List[String], source: => String): Either[String, Option[Plan]] =
    if parts != Parts then Right(None)
    else planSource(source).map(Some.apply)

  def planSource(source: String): Either[String, Plan] =
    for
      present <- Option(source).toRight("original two-selected-name rank-2 source must be present")
      mapped <- PatternSource.synthesizeMapped(present).left.map(_.message)
      marker <- validateOriginal(present, mapped)
      names = mapped.occurrences.map(_.name)
      used = mutable.Set.from(HoleSourceRewriter.scan(present, allowUnicodeIdentifiers = true)
        .literalIdentifiers ++ mapped.occurrences.map(_.generatedName) ++ names)
      // The neutral two-name validator uses conservative ordinary hole keys.
      // Retain original ordinary binder semantics/spans in mapped, transporting
      // only nonplain keys to collision-safe ASCII ordinary Term/sequence holes.
      ordinary = Vector(names(0) -> "qqRank2Receiver", names(3) -> "qqRank2Arguments").map { (name, base) =>
        if DefinitionName.plain(name).isRight then name
        else
          var transport = base
          var suffix = 0
          while used(transport) do
            suffix += 1
            transport = s"${base}_$suffix"
          used += transport
          transport
      }
      selected = mapped.occurrences.slice(1, 3).zipWithIndex.map { (occurrence, index) =>
        val ordinal = 1 - index
        val base = s"${occurrence.generatedName}__selected_rank2_$ordinal"
        var transport = base
        var suffix = 0
        while used(transport) do
          suffix += 1
          transport = s"${base}_$suffix"
        used += transport
        SelectedNamePatternOccurrence(occurrence.name, ordinal, transport)
      }
      receiver = ordinary(0)
      sequence = ordinary(1)
      effective = s"$$$receiver.${selected(0).transport}.${selected(1).transport}($$$sequence)"
      compiled <- QuasiPattern.term(effective).left.map(_.message).map(_.pattern)
      _ <- validateCompiled(compiled, receiver, sequence, selected)
    yield Plan(mapped, marker, effective, compiled, receiver, sequence, selected)

  private[matching] def validateOriginal(
      source: String, mapped: MappedPatternSource
  ): Either[String, SourceSpan] =
    val occurrences = mapped.occurrences
    for
      _ <- Either.cond(occurrences.size == 4, (), "two-selected-name rank-2 requires exactly four source roles")
      _ <- Either.cond(occurrences.slice(1, 3).forall(o => DefinitionName.plain(o.name).isRight), (),
        "two-selected-name rank-2 selected source binders must be conservative plain names")
      _ <- Either.cond(occurrences.map(_.name).distinct.size == 4, (),
        "two-selected-name rank-2 source binders must be distinct across capture categories")
      _ <- Either.cond(occurrences.forall(o => o.originalSpan.end <= source.length &&
        source.slice(o.originalSpan.start, o.originalSpan.end) == "$" + o.name), (),
        "two-selected-name rank-2 original hole spans do not match their binders")
      _ <- Either.cond(occurrences.zip(occurrences.drop(1)).forall((left, right) =>
        left.originalSpan.end <= right.originalSpan.start), (),
        "two-selected-name rank-2 original hole spans must be ordered")
      _ <- Either.cond(occurrences.head.originalSpan.start == 0 &&
        source.substring(occurrences.last.originalSpan.end) == ")" &&
        occurrences.sliding(2).zip(List(".", ".", "(..")).forall { (pair, gap) =>
          source.substring(pair(0).originalSpan.end, pair(1).originalSpan.start) == gap
        }, (), "two-selected-name rank-2 original prefix, marker or closing span is malformed")
      _ <- Either.cond(occurrences.forall { o =>
        mapped.patternSource.source.slice(o.generatedSpan.start, o.generatedSpan.end) == o.generatedName &&
          mapped.originMap.originsFor(o.generatedSpan).map(_.origin) == Vector(
            SourceOrigin.RewrittenHole(quasiquotes.source.SourceId.TermPattern,
              o.originalSpan, o.name, o.role))
      }, (), "two-selected-name rank-2 generated hole mapping differs from original source")
      originalSpans = mapped.originMap.segments.flatMap { segment => segment.origin match
        case SourceOrigin.OriginalText(_, span) => Some(span)
        case SourceOrigin.RewrittenHole(_, span, _, _) => Some(span)
        case _ => None
      }
      _ <- Either.cond(originalSpans.size == mapped.originMap.segments.size &&
        originalSpans.headOption.exists(_.start == 0) && originalSpans.lastOption.exists(_.end == source.length) &&
        originalSpans.zip(originalSpans.drop(1)).forall((left, right) => left.end == right.start) &&
        mapped.originMap.segments.forall { segment => segment.origin match
          case SourceOrigin.OriginalText(_, original) => original.end <= source.length &&
            mapped.patternSource.source.slice(segment.generatedSpan.start, segment.generatedSpan.end) ==
              source.slice(original.start, original.end)
          case _ => true
        }, (), "two-selected-name rank-2 literal mapping must cover the complete original source, including closing parenthesis")
      marker = SourceSpan(occurrences(3).originalSpan.start - 2, occurrences(3).originalSpan.start)
      _ <- Either.cond(mapped.originMap.segments.exists {
        case quasiquotes.source.GeneratedSegment(span, SourceOrigin.OriginalText(_, original)) =>
          original.start <= marker.start && original.end >= marker.end &&
            source.slice(marker.start, marker.end) == ".." &&
            mapped.patternSource.source.slice(span.start, span.end) == source.slice(original.start, original.end)
        case _ => false
      }, (), "two-selected-name rank-2 marker lost its original literal source mapping")
    yield marker

  private[matching] def validateCompiled(
      pattern: TermPattern, receiver: String, sequence: String,
      selected: Vector[SelectedNamePatternOccurrence]
  ): Either[String, Unit] =
    for
      _ <- Either.cond(selected.size == 2 && selected.map(_.selectOrdinal) == Vector(1, 0) &&
        selected.map(_.transport).distinct.size == 2 && selected.forall(o =>
          DefinitionName.plain(o.transport).isRight && o.transport != receiver &&
            o.transport != sequence && !selected.exists(_.name == o.transport)), (),
        "two-selected-name rank-2 selected transports or ordinals are invalid")
      _ <- Either.cond(pattern == TermPattern.Apply(
        TermPattern.Select(TermPattern.Select(TermPattern.Hole(receiver), selected(0).transport), selected(1).transport),
        List(TermPattern.Hole(sequence))), (),
        "two-selected-name rank-2 compiler pattern must have exactly two Selects and one direct sequence role")
      _ <- RankedPatternSource.validateSequencePosition(pattern, sequence)
      _ <- ValidatedSelectedNamePattern.createTwo(pattern, selected(0), selected(1)).left.map(_.message)
    yield ()

/** One bounded matcher. Admission uses the existing raw target view; captures
  * come exclusively from the original caller's Apply and Select nodes.
  */
private[matching] object TwoSelectedRankedPatternBridge:
  def matchTarget(using q: Quotes)(target: q.reflect.Term): Option[
    (q.reflect.Term, SelectedMemberName, SelectedMemberName, Seq[q.reflect.Term])
  ] =
    import q.reflect.*
    def strip(term: Term): Term = term match
      case Inlined(_, Nil, body) => strip(body)
      case other => other
    def ordinary(method: Term): Boolean =
      method.tpe.widen match
        case mt: MethodType => !mt.isImplicit && (mt.resType match
          case _: MethodType | _: PolyType => false
          case _ => true)
        case _ => false
    def ordinarySource(term: Term, method: Term, prefix: Term, inner: String, outer: String): Boolean =
      def unwrap(shape: TermShape): TermShape = shape match
        case TermShape.Parenthesized(inner) => unwrap(inner)
        case other => other
      val original = ReflectedPositionProvenance.sourceCode(term.pos)
      // An inserted Function.apply can repeat the two literal target names.
      // Its Select shares its qualifier's source span; a written terminal
      // selector extends that span. Keep the receiver hole entirely opaque.
      val writtenTerminal = (
        ReflectedPositionProvenance.usableBounds(method.pos),
        ReflectedPositionProvenance.usableBounds(prefix.pos)
      ) match
        case (Some((start, end)), Some((prefixStart, prefixEnd))) =>
          (method.pos.sourceFile eq prefix.pos.sourceFile) && start <= prefixStart && end > prefixEnd
        case _ => original.isEmpty
      // Reflected infix calls can have the same Apply/Select encoding as a
      // dotted call. Recoverable syntax must prove the ordinary Apply shell
      // and both selected names; genuinely generated NoSpan trees have none.
      writtenTerminal && original.forall { source =>
        TinyTermParser.parse(source).toOption.map(value => unwrap(value.shape)).exists {
          case TermShape.Apply(function, _) => unwrap(function) match
            case TermShape.Select(qualifier, sourceOuter) => unwrap(qualifier) match
              case TermShape.Select(_, sourceInner) => sourceInner == inner && sourceOuter == outer
              case _ => false
            case _ => false
          case _ => false
        }
      }
    if target == null then None
    else strip(target) match
      case applied @ Apply(method @ Select(prefix @ Select(receiver, inner), outer), arguments)
          if ordinary(method) && ordinarySource(applied, method, prefix, inner, outer) && !arguments.exists {
            case NamedArg(_, _) => true
            case _ => false
          } =>
        for
          first <- SelectedMemberName.from(inner).toOption
          second <- SelectedMemberName.from(outer).toOption
          // Require the same bounded Apply/Select topology after admission.
          // The view may normalize an inner local val, so never capture from it.
          _ <- TargetTermView.fromTerm(applied).toOption.collect {
            case TargetTermView.Apply(TargetTermView.Select(TargetTermView.Select(_, `inner`, _), `outer`, _), args, _)
                if args.size == arguments.size => ()
          }
        yield (receiver, first, second, arguments)
      case _ => None

private[matching] object TwoSelectedRankedPatternExtractorFactory:
  def extractor(using q: Quotes): TermPatternProductExtractor[
    q.reflect.Term,
    (q.reflect.Term, SelectedMemberName, SelectedMemberName, Seq[q.reflect.Term])
  ] = TermPatternProductExtractorFactory.productExtractor(target =>
    TwoSelectedRankedPatternBridge.matchTarget(target))
