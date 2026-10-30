package quasiquotes.matching

import scala.collection.mutable
import scala.compiletime.erasedValue
import scala.quoted.*

import quasiquotes.construct.SelectedMemberName
import quasiquotes.definitions.DefinitionName
import quasiquotes.source.HoleSourceRewriter

/** Standard frontend admission only; source roles never depend on target names. */
private[matching] object SelectedChainPatternSource:
  private val MaximumDepth = 8

  final case class Plan(
      mapped: MappedPatternSource,
      effectiveSource: String,
      pattern: TermPattern,
      receiverName: String,
      selectedOccurrences: Vector[SelectedNamePatternOccurrence]
  )

  def classify(parts: List[String], source: => String): Either[String, Option[Plan]] =
    val depth = parts.size - 2
    if depth < 3 || parts != List("") ++ List.fill(depth)(".") ++ List("") then Right(None)
    else if depth > MaximumDepth then
      Left(s"selected-chain capacity: direct qq supports at most $MaximumDepth selected names, got $depth")
    else planSource(source, parts).map(Some.apply)

  def planSource(source: String, parts: List[String]): Either[String, Plan] =
    for
      present <- Option(source).toRight("original selected-chain source must be present")
      mapped <- PatternSource.synthesizeMapped(present).left.map(_.message)
      depth = mapped.occurrences.size - 1
      _ <- Either.cond(
        depth >= 3 && depth <= MaximumDepth &&
          parts == List("") ++ List.fill(depth)(".") ++ List(""),
        (), "selected-chain source occurrences do not match the admitted direct parts"
      )
      _ <- mapped.occurrences.find(value => DefinitionName.plain(value.name).isLeft)
        .map(value => s"unsupported selected-chain plain source binder ${value.name}").toLeft(())
      names = mapped.occurrences.map(_.name)
      _ <- Either.cond(names.distinct.size == names.size, (),
        "selected-chain source binders must be distinct across Term and selected-name categories")
      _ <- Either.cond(
        mapped.occurrences.head.originalSpan.start == 0 &&
          mapped.occurrences.last.originalSpan.end == present.length,
        (), "selected-chain original source must be the complete direct capture chain"
      )
      // Check the recovered original separators, rather than synthesizing a
      // valid source from a malformed original with the same hole count.
      _ <- Either.cond(mapped.occurrences.sliding(2).forall { pair =>
        present.substring(pair(0).originalSpan.end, pair(1).originalSpan.start) == "."
      }, (), "selected-chain original source has malformed direct separators")
      receiver = mapped.occurrences.head.name
      used = mutable.Set.from(
        HoleSourceRewriter.scan(present, allowUnicodeIdentifiers = true).literalIdentifiers ++
          mapped.occurrences.map(_.generatedName) ++ names
      )
      roles = mapped.occurrences.tail.zipWithIndex.map { (occurrence, index) =>
        val ordinal = depth - index - 1
        val base = s"${occurrence.generatedName}__selected_chain_$ordinal"
        var transport = base
        var suffix = 0
        while used(transport) do
          suffix += 1
          transport = s"${base}_$suffix"
        used += transport
        SelectedNamePatternOccurrence(occurrence.name, ordinal, transport)
      }
      effective = s"$$$receiver." + roles.map(_.transport).mkString(".")
      pattern <- QuasiPattern.term(effective).left.map(_.message).map(_.pattern)
      _ <- validate(pattern, receiver, roles)
    yield Plan(mapped, effective, pattern, receiver, roles)

  def validate(
      pattern: TermPattern,
      receiverName: String,
      occurrences: Vector[SelectedNamePatternOccurrence]
  ): Either[String, Unit] =
    def scan(current: TermPattern, names: Vector[String]): (TermPattern, Vector[String]) =
      current match
        case TermPattern.Select(prefix, name) => scan(prefix, names :+ name)
        case other => other -> names
    val (base, rootFirst) = scan(pattern, Vector.empty)
    val transports = occurrences.map(_.transport)
    for
      _ <- Either.cond(occurrences.nonEmpty &&
        occurrences.map(_.selectOrdinal) == occurrences.indices.reverse.toVector,
        (), "selected-chain source-order ordinals must cover each Select exactly once")
      _ <- Either.cond(transports.distinct.size == transports.size &&
        transports.forall(name => DefinitionName.plain(name).isRight) &&
        !transports.contains(receiverName) &&
        !transports.exists(name => occurrences.exists(_.name == name)),
        (), "selected-chain transports must be legal, unique and disjoint from capture names")
      _ <- Either.cond(base == TermPattern.Hole(receiverName) && rootFirst == transports.reverse,
        (), "selected-chain compiled categories or transport positions differ from original source roles")
    yield ()

/** Peels raw Selects and delegates ordinary receiver admission to TermMatcher.
  * Only project-owned IR is concretized; caller Terms are never reconstructed.
  */
private[matching] object SelectedChainPatternBridge:
  final case class Result[T](receiver: T, names: Vector[SelectedMemberName])

  def matchTarget(using q: Quotes)(
      pattern: TermPattern,
      receiverName: String,
      depth: Int,
      target: q.reflect.Term
  ): Option[Result[q.reflect.Term]] =
    import q.reflect.*
    def strip(term: Term): Term = term match
      case Inlined(_, Nil, body) => strip(body)
      case other => other
    def peel(term: Term, remaining: Int, names: Vector[String]): Option[(Term, Vector[String])] =
      if remaining == 0 then Some(term -> names)
      else term match
        case Select(prefix, name) => peel(prefix, remaining - 1, names :+ name)
        case _ => None
    def concretize(current: TermPattern, names: List[String]): Option[TermPattern] =
      (current, names) match
        case (TermPattern.Select(prefix, _), name :: tail) =>
          concretize(prefix, tail).map(TermPattern.Select(_, name))
        case (hole @ TermPattern.Hole(`receiverName`), Nil) => Some(hole)
        case _ => None

    if target == null || depth < 1 then None
    else
      val prepared = strip(target)
      for
        peeled <- peel(prepared, depth, Vector.empty)
        (receiver, rootFirst) = peeled
        decoded <- rootFirst.reverse.foldLeft(Option(Vector.empty[SelectedMemberName])) { (result, name) =>
          for
            previous <- result
            selected <- SelectedMemberName.from(name).toOption
          yield previous :+ selected
        }
        concrete <- concretize(pattern, rootFirst.toList)
        matched <- TermMatcher.matchTermRaw(concrete, prepared).toOption
        // The legacy view may unwrap an inner receiver or follow a local val
        // to its RHS. Use it for admission, but capture the original prefix.
        _ <- matched.bindings.get(receiverName)
      yield Result(receiver, decoded)

/** One role-specialized tuple algorithm, expanded at the caller's static type. */
private[matching] object SelectedChainTupleFactory:
  type Captures[T, Kinds <: Tuple] <: Tuple = Kinds match
    case EmptyTuple => EmptyTuple
    case ProductTermCapture *: tail => T *: Captures[T, tail]
    case ProductSelectedNameCapture *: tail => SelectedMemberName *: Captures[T, tail]

  private inline def tuple[T, Kinds <: Tuple](
      receiver: T, names: Vector[SelectedMemberName], index: Int
  ): Captures[T, Kinds] =
    inline erasedValue[Kinds] match
      case _: EmptyTuple => EmptyTuple
      case _: (ProductTermCapture *: tail) => receiver *: tuple[T, tail](receiver, names, index)
      case _: (ProductSelectedNameCapture *: tail) =>
        names(index) *: tuple[T, tail](receiver, names, index + 1)

  private inline def nameCount[Kinds <: Tuple]: Int =
    inline erasedValue[Kinds] match
      case _: EmptyTuple => 0
      case _: (ProductTermCapture *: tail) => nameCount[tail]
      case _: (ProductSelectedNameCapture *: tail) => 1 + nameCount[tail]

  inline def checkedTuple[T, Kinds <: Tuple](
      receiver: T, names: Vector[SelectedMemberName]
  ): Option[Captures[T, Kinds]] =
    if names.size != nameCount[Kinds] then None
    else Some(tuple[T, Kinds](receiver, names, 0))

  transparent inline def extractor[Kinds <: Tuple](using q: Quotes)(
      pattern: TermPattern, receiverName: String, depth: Int
  ): TermPatternProductExtractor[q.reflect.Term, Captures[q.reflect.Term, Kinds]] =
    TermPatternProductExtractorFactory.productExtractor(target =>
      SelectedChainPatternBridge.matchTarget(pattern, receiverName, depth, target).flatMap(result =>
        checkedTuple[q.reflect.Term, Kinds](result.receiver, result.names)
      )
    )
