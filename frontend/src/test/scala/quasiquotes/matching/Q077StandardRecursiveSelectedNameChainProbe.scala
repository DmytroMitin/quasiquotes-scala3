package quasiquotes.matching

import scala.collection.mutable
import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*

import quasiquotes.construct.SelectedMemberName
import quasiquotes.definitions.DefinitionName

object Q077StandardChainTarget:
  object root:
    object alpha:
      object beta:
        object gamma:
          object delta:
            object epsilon:
              object zeta:
                object eta:
                  object theta
        def applied(value: Int): Int = value
        def generic[A](value: A): A = value
        def contextual(value: Int)(using String): Int = value
    object same:
      object same
    object lexical:
      object `+`:
        object `type`:
          object `safe spaced name`
      object naïve

  transparent inline def wrapped = root.alpha.beta

/** Q077 test-only generic flat selected-chain planner and matcher.  The
  * algorithm is depth-independent; the four factory methods below exist only
  * to prove the fixed source arities have fixed tuple types.
  */
private[matching] object Q077StandardFlatModel:
  private val FixtureNames = Vector("first", "second", "third", "fourth")

  final case class Plan(
      source: String,
      parts: List[String],
      effectiveSource: String,
      pattern: TermPattern,
      receiverName: String,
      selectedOccurrences: Vector[SelectedNamePatternOccurrence],
      sourceStarts: Vector[Int]
  )

  final case class Result[T](
      receiver: T,
      names: Vector[SelectedMemberName]
  )

  def plan(depth: Int): Either[String, Plan] =
    if depth < 1 || depth > FixtureNames.size then
      Left(s"Q077 fixture depth must be 1 through ${FixtureNames.size}, got $depth")
    else
      planSource(
        "$receiver" + FixtureNames.take(depth).map(name => s".$$${name}").mkString,
        List("") ++ List.fill(depth)(".") ++ List("")
      )

  def planSource(source: String, parts: List[String]): Either[String, Plan] =
    for
      present <- Option(source).toRight("original flat selected-chain source must be present")
      mapped <- PatternSource.synthesizeMapped(present).left.map(_.message)
      depth = mapped.occurrences.size - 1
      _ <- Either.cond(depth >= 1, (), "flat selected-chain requires a receiver and at least one name")
      _ <- Either.cond(
        parts == (List("") ++ List.fill(depth)(".") ++ List("")),
        (),
        s"flat selected-chain parts do not encode exactly $depth selections: $parts"
      )
      receiver <- mapped.occurrences.headOption.toRight("flat selected-chain receiver is missing")
      selectedSource = mapped.occurrences.tail
      _ <- selectedSource
        .find(value => DefinitionName.plain(value.name).isLeft)
        .map(value => s"unsupported flat selected source name ${value.name}")
        .toLeft(())
      allNames = mapped.occurrences.map(_.name)
      _ <- allNames
        .groupBy(identity)
        .collectFirst { case (name, values) if values.size > 1 =>
          s"flat selected-chain source binder $name is repeated or changes capture category"
        }
        .toLeft(())
      roles = selectedRoles(mapped, selectedSource)
      effectiveSource =
        s"$$${receiver.name}." + roles.map(_.transport).mkString(".")
      compiled <- QuasiPattern.term(effectiveSource).left.map(_.message).map(_.pattern)
      _ <- validate(compiled, receiver.name, roles)
    yield Plan(
      present,
      parts,
      effectiveSource,
      compiled,
      receiver.name,
      roles,
      mapped.occurrences.map(_.originalSpan.start)
    )

  private def selectedRoles(
      mapped: MappedPatternSource,
      selectedSource: Vector[quasiquotes.source.HoleOccurrence]
  ): Vector[SelectedNamePatternOccurrence] =
    val used = mutable.Set.from(mapped.occurrences.map(_.generatedName))

    def fresh(base: String): String =
      var candidate = base
      var suffix = 0
      while used(candidate) do
        suffix += 1
        candidate = s"${base}_$suffix"
      used += candidate
      candidate

    val depth = selectedSource.size
    selectedSource.zipWithIndex.map { case (occurrence, index) =>
      val ordinal = depth - index - 1
      SelectedNamePatternOccurrence(
        occurrence.name,
        ordinal,
        fresh(s"${occurrence.generatedName}__q077_selected_$ordinal")
      )
    }

  def validate(
      pattern: TermPattern,
      receiverName: String,
      occurrences: Vector[SelectedNamePatternOccurrence]
  ): Either[String, Unit] =
    for
      _ <- Either.cond(occurrences.nonEmpty, (), "selected occurrences must be present")
      _ <- Either.cond(
        occurrences.map(_.selectOrdinal).sorted == occurrences.indices.toVector,
        (),
        "selected ordinals must cover every root-first Select exactly once"
      )
      _ <- Either.cond(
        occurrences.map(_.transport).distinct.size == occurrences.size,
        (),
        "selected transports must be unique"
      )
      scanned <- scanPattern(pattern)
      (base, rootFirstNames) = scanned
      _ <- Either.cond(
        base == TermPattern.Hole(receiverName),
        (),
        s"flat selected-chain base is not receiver hole $receiverName: $base"
      )
      expected = occurrences.sortBy(_.selectOrdinal).map(_.transport)
      _ <- Either.cond(
        rootFirstNames == expected,
        (),
        s"flat selected-chain transports differ: $rootFirstNames != $expected"
      )
    yield ()

  private def scanPattern(
      pattern: TermPattern
  ): Either[String, (TermPattern, Vector[String])] =
    def loop(current: TermPattern, names: Vector[String]): (TermPattern, Vector[String]) =
      current match
        case TermPattern.Select(qualifier, name) => loop(qualifier, names :+ name)
        case other => other -> names

    val (base, rootFirst) = loop(pattern, Vector.empty)
    Right(base -> rootFirst)

  def matchTarget(using q: Quotes)(
      plan: Plan,
      target: q.reflect.Term
  ): Option[Result[q.reflect.Term]] =
    import q.reflect.*

    if target == null then None
    else
      val prepared = stripInlined(target)
      val depth = plan.selectedOccurrences.size
      peelRaw(prepared, depth).flatMap { case (_, rootFirstNames) =>
        val decoded = rootFirstNames.reverse.map(SelectedMemberName.from)
        if decoded.exists(_.isLeft) then None
        else
          for
            concrete <- replaceNames(plan.pattern, rootFirstNames)
            matched <- TermMatcher.matchTermRaw(concrete, prepared).toOption
            receiver <- matched.bindings.get(plan.receiverName)
          yield Result(receiver, decoded.flatMap(_.toOption))
      }

  private def peelRaw(using q: Quotes)(
      target: q.reflect.Term,
      depth: Int
  ): Option[(q.reflect.Term, Vector[String])] =
    import q.reflect.*

    def loop(
        current: Term,
        remaining: Int,
        names: Vector[String]
    ): Option[(Term, Vector[String])] =
      if remaining == 0 then Some(current -> names)
      else
        current match
          case Select(prefix, name) => loop(prefix, remaining - 1, names :+ name)
          case _ => None

    loop(target, depth, Vector.empty)

  private def replaceNames(
      pattern: TermPattern,
      rootFirstNames: Vector[String]
  ): Option[TermPattern] =
    def loop(current: TermPattern, ordinal: Int): Option[(TermPattern, Int)] =
      current match
        case TermPattern.Select(qualifier, _) =>
          for
            name <- rootFirstNames.lift(ordinal)
            replaced <- loop(qualifier, ordinal + 1)
          yield TermPattern.Select(replaced._1, name) -> replaced._2
        case other => Some(other -> ordinal)

    loop(pattern, 0).collect {
      case (result, consumed) if consumed == rootFirstNames.size => result
    }

  def one(using q: Quotes)(plan: Plan): TermPatternProductExtractor[
      q.reflect.Term,
      (q.reflect.Term, SelectedMemberName)
  ] =
    TermPatternProductExtractorFactory.productExtractor(target =>
      matchTarget(plan, target).collect {
        case Result(receiver, Vector(first)) => (receiver, first)
      }
    )

  def two(using q: Quotes)(plan: Plan): TermPatternProductExtractor[
      q.reflect.Term,
      (q.reflect.Term, SelectedMemberName, SelectedMemberName)
  ] =
    TermPatternProductExtractorFactory.productExtractor(target =>
      matchTarget(plan, target).collect {
        case Result(receiver, Vector(first, second)) => (receiver, first, second)
      }
    )

  def three(using q: Quotes)(plan: Plan): TermPatternProductExtractor[
      q.reflect.Term,
      (q.reflect.Term, SelectedMemberName, SelectedMemberName, SelectedMemberName)
  ] =
    TermPatternProductExtractorFactory.productExtractor(target =>
      matchTarget(plan, target).collect {
        case Result(receiver, Vector(first, second, third)) =>
          (receiver, first, second, third)
      }
    )

  def four(using q: Quotes)(plan: Plan): TermPatternProductExtractor[
      q.reflect.Term,
      (
          q.reflect.Term,
          SelectedMemberName,
          SelectedMemberName,
          SelectedMemberName,
          SelectedMemberName
      )
  ] =
    TermPatternProductExtractorFactory.productExtractor(target =>
      matchTarget(plan, target).collect {
        case Result(receiver, Vector(first, second, third, fourth)) =>
          (receiver, first, second, third, fourth)
      }
    )

  private def stripInlined(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term match
      case Inlined(_, Nil, body) => stripInlined(body)
      case other => other

object Q077StandardChainProbe:
  inline def publicCompositionObservations: List[String] =
    ${ publicCompositionObservationsImpl }
  inline def recursivePeelObservations: List[String] =
    ${ recursivePeelObservationsImpl }
  inline def flatModelObservations: List[String] =
    ${ flatModelObservationsImpl }
  inline def rankedCompositionObservations: List[String] =
    ${ rankedCompositionObservationsImpl }
  inline def negativeObservations: List[String] =
    ${ negativeObservationsImpl }

  private def nestedPublic(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName, SelectedMemberName, q.reflect.Term)] =
    import QuasiPattern.*
    target match
      case qq"$prefix.$second" =>
        prefix match
          case qq"$receiver.$first" => Some((receiver, first, second, prefix))
          case _ => None
      case _ => None

  private def flatPublic(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName, SelectedMemberName)] =
    import QuasiPattern.*
    target match
      case qq"$receiver.$first.$second" => Some((receiver, first, second))
      case _ => None

  private def peel(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName)] =
    import QuasiPattern.*
    target match
      case qq"$prefix.$name" => Some(prefix -> name)
      case _ => None

  private final case class Peeled[T](
      receiver: T,
      names: Vector[SelectedMemberName],
      prefixesRootFirst: Vector[T]
  )

  private def peelN(using q: Quotes)(
      target: q.reflect.Term,
      depth: Int
  ): Option[Peeled[q.reflect.Term]] =
    def loop(
        current: q.reflect.Term,
        remaining: Int,
        namesRootFirst: Vector[SelectedMemberName],
        prefixes: Vector[q.reflect.Term]
    ): Option[Peeled[q.reflect.Term]] =
      if remaining == 0 then
        Some(Peeled(current, namesRootFirst.reverse, prefixes))
      else
        peel(current).flatMap { case (prefix, name) =>
          loop(prefix, remaining - 1, namesRootFirst :+ name, prefixes :+ prefix)
        }

    if target == null || depth < 0 then None
    else loop(target, depth, Vector.empty, Vector.empty)

  private def rawPeel(using q: Quotes)(
      target: q.reflect.Term,
      depth: Int
  ): Option[(q.reflect.Term, Vector[q.reflect.Term])] =
    import q.reflect.*

    def loop(
        current: Term,
        remaining: Int,
        prefixes: Vector[Term]
    ): Option[(Term, Vector[Term])] =
      if remaining == 0 then Some(current -> prefixes)
      else
        current match
          case Select(prefix, _) => loop(prefix, remaining - 1, prefixes :+ prefix)
          case _ => None

    loop(target, depth, Vector.empty)

  private def publicCompositionObservationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    def observe(label: String, raw: Term): String =
      val target = stripInlined(raw)
      val expected = target match
        case Select(prefix @ Select(receiver, _), _) => Some(receiver -> prefix)
        case _ => None
      val nested = nestedPublic(target)
      val flat = flatPublic(target)
      (expected, nested, flat) match
        case (
              Some((expectedReceiver, expectedPrefix)),
              Some((nestedReceiver, first, second, nestedPrefix)),
              Some((flatReceiver, flatFirst, flatSecond))
            ) =>
          val namesEqual = first == flatFirst && second == flatSecond
          val receiverIdentity =
            (nestedReceiver eq expectedReceiver) && (flatReceiver eq expectedReceiver)
          val prefixIdentity = nestedPrefix eq expectedPrefix
          s"$label:${first.decoded}:${second.decoded}:$namesEqual:$receiverIdentity:$prefixIdentity"
        case other => report.errorAndAbort(s"Q077 standard composition failed: $label / $other")

    val distinct = observe(
      "distinct",
      '{ Q077StandardChainTarget.root.alpha.beta }.asTerm
    )
    val equal = observe(
      "equal",
      '{ Q077StandardChainTarget.root.same.same }.asTerm
    )
    val invalidApply = nestedPublic(
      stripInlined('{ Q077StandardChainTarget.root.alpha.beta.applied(1) }.asTerm)
    ).isEmpty && flatPublic(
      stripInlined('{ Q077StandardChainTarget.root.alpha.beta.applied(1) }.asTerm)
    ).isEmpty
    val invalidUnicode = peel(
      stripInlined('{ Q077StandardChainTarget.root.lexical.naïve }.asTerm)
    ).isEmpty

    Expr.ofList(
      List(
        distinct,
        equal,
        s"invalid-apply:$invalidApply",
        s"invalid-unicode:$invalidUnicode"
      ).map(Expr(_))
    )

  private def recursivePeelObservationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    def observation(
        label: String,
        raw: Term,
        depth: Int,
        expectedNames: Vector[String]
    ): String =
      val target = stripInlined(raw)
      val expected = rawPeel(target, depth).getOrElse(
        report.errorAndAbort(s"Q077 raw fixture is shorter than $depth for $label")
      )
      val result = peelN(target, depth).getOrElse(
        report.errorAndAbort(s"Q077 public peel failed for $label")
      )
      val identity =
        (result.receiver eq expected._1) &&
          result.prefixesRootFirst.zip(expected._2).forall { case (left, right) => left eq right }
      val names = result.names.map(_.decoded)
      val symbols = result.receiver.symbol.exists && result.prefixesRootFirst.forall(_.symbol.exists)
      s"$label:${names.mkString(",")}:${identity && names == expectedNames}:$symbols"

    val depth0 = observation(
      "depth-0",
      '{ Q077StandardChainTarget.root }.asTerm,
      0,
      Vector.empty
    )
    val depth1 = observation(
      "depth-1",
      '{ Q077StandardChainTarget.root.alpha }.asTerm,
      1,
      Vector("alpha")
    )
    val depth2 = observation(
      "depth-2",
      '{ Q077StandardChainTarget.root.alpha.beta }.asTerm,
      2,
      Vector("alpha", "beta")
    )
    val depth3 = observation(
      "depth-3",
      '{ Q077StandardChainTarget.root.alpha.beta.gamma }.asTerm,
      3,
      Vector("alpha", "beta", "gamma")
    )
    val depth4 = observation(
      "depth-4",
      '{ Q077StandardChainTarget.root.alpha.beta.gamma.delta }.asTerm,
      4,
      Vector("alpha", "beta", "gamma", "delta")
    )
    val depth8 = observation(
      "depth-8",
      '{
        Q077StandardChainTarget.root.alpha.beta.gamma.delta.epsilon.zeta.eta.theta
      }.asTerm,
      8,
      Vector("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta")
    )
    val short = !peelN(
      stripInlined('{ Q077StandardChainTarget.root.alpha.beta }.asTerm),
      3
    ).exists(_.names.map(_.decoded) == Vector("alpha", "beta", "gamma"))
    val long = peelN(
      stripInlined('{ Q077StandardChainTarget.root.alpha.beta.gamma }.asTerm),
      2
    ).getOrElse(report.errorAndAbort("Q077 arbitrary-prefix long-chain peel failed"))
    val longArbitrary = long.names.map(_.decoded) == Vector("beta", "gamma")
    val longFixedRejected = long.receiver.symbol.name != "root"
    val lexical = peelN(
      stripInlined('{
        Q077StandardChainTarget.root.lexical.`+`.`type`.`safe spaced name`
      }.asTerm),
      3
    ).exists(_.names.map(_.decoded) == Vector("+", "type", "safe spaced name"))
    val wrappedTarget = Inlined(
      None,
      Nil,
      stripInlined('{ Q077StandardChainTarget.root.alpha.beta }.asTerm)
    )
    val wrapped = peelN(
      wrappedTarget,
      2
    ).exists(_.names.map(_.decoded) == Vector("alpha", "beta"))
    val applyRejected = peelN(
      stripInlined('{ Q077StandardChainTarget.root.alpha.beta.applied(1) }.asTerm),
      1
    ).isEmpty
    val nullRejected = peelN(null.asInstanceOf[Term], 1).isEmpty

    Expr.ofList(
      List(
        depth0,
        depth1,
        depth2,
        depth3,
        depth4,
        depth8,
        s"short:$short",
        s"long-arbitrary-prefix:$longArbitrary",
        s"long-fixed-root-rejected:$longFixedRejected",
        s"lexical:+,type,safe spaced name:$lexical",
        s"wrapped:$wrapped",
        s"apply:$applyRejected",
        s"null:$nullRejected"
      ).map(Expr(_))
    )

  private def flatModelObservationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    val plans = (1 to 4).map(depth =>
      Q077StandardFlatModel.plan(depth).fold(report.errorAndAbort(_), identity)
    ).toVector
    val targets = Vector(
      stripInlined('{ Q077StandardChainTarget.root.alpha }.asTerm),
      stripInlined('{ Q077StandardChainTarget.root.alpha.beta }.asTerm),
      stripInlined('{ Q077StandardChainTarget.root.alpha.beta.gamma }.asTerm),
      stripInlined('{ Q077StandardChainTarget.root.alpha.beta.gamma.delta }.asTerm)
    )

    val one: TermPatternProductExtractor[Term, (Term, SelectedMemberName)] =
      Q077StandardFlatModel.one(plans(0))
    val two: TermPatternProductExtractor[
      Term,
      (Term, SelectedMemberName, SelectedMemberName)
    ] = Q077StandardFlatModel.two(plans(1))
    val three: TermPatternProductExtractor[
      Term,
      (Term, SelectedMemberName, SelectedMemberName, SelectedMemberName)
    ] = Q077StandardFlatModel.three(plans(2))
    val four: TermPatternProductExtractor[
      Term,
      (Term, SelectedMemberName, SelectedMemberName, SelectedMemberName, SelectedMemberName)
    ] = Q077StandardFlatModel.four(plans(3))

    val typed = Vector(
      one.unapply(targets(0)).map(value => (value._1, Vector(value._2))),
      two.unapply(targets(1)).map(value => (value._1, Vector(value._2, value._3))),
      three.unapply(targets(2)).map(value => (value._1, Vector(value._2, value._3, value._4))),
      four.unapply(targets(3)).map(value =>
        (value._1, Vector(value._2, value._3, value._4, value._5))
      )
    )

    val expectedNames = Vector("alpha", "beta", "gamma", "delta")
    val modelLines = typed.zipWithIndex.map { case (value, index) =>
      val result = value.getOrElse(report.errorAndAbort(s"Q077 typed flat n${index + 1} failed"))
      val expectedReceiver = rawPeel(targets(index), index + 1).map(_._1).getOrElse(
        report.errorAndAbort(s"Q077 typed flat fixture n${index + 1} malformed")
      )
      s"n${index + 1}:${result._2.map(_.decoded).mkString(",")}:${result._1 eq expectedReceiver}"
    }

    val compositionLines = (2 to 4).map { depth =>
      val generic = Q077StandardFlatModel.matchTarget(plans(depth - 1), targets(depth - 1))
      val recursive = peelN(targets(depth - 1), depth)
      val equal = (generic, recursive) match
        case (Some(flat), Some(nested)) =>
          (flat.receiver eq nested.receiver) && flat.names == nested.names
        case _ => false
      s"composition-n$depth:$equal"
    }

    Expr.ofList((modelLines ++ compositionLines).toList.map(Expr(_)))

  private def rankedPublic(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(
      q.reflect.Term,
      SelectedMemberName,
      SelectedMemberName,
      Seq[q.reflect.Term],
      q.reflect.Term
  )] =
    import QuasiPattern.*
    target match
      case qq"$prefix.$second(..$arguments)" =>
        prefix match
          case qq"$receiver.$first" =>
            Some((receiver, first, second, arguments, prefix))
          case _ => None
      case _ => None

  private def rankedCompositionObservationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    val plan = Q076StandardMixedModel.plan().fold(report.errorAndAbort(_), identity)
    val fixtures = List(
      "zero" -> '{ Q076StandardMixedTarget.receiver.first.second() }.asTerm,
      "one" -> '{ Q076StandardMixedTarget.receiver.first.second(1) }.asTerm,
      "many" -> '{ Q076StandardMixedTarget.receiver.first.second(1, 2, 3) }.asTerm
    )
    val positive = fixtures.map { case (label, raw) =>
      val target = stripInlined(raw)
      val expected = target match
        case Apply(Select(prefix @ Select(receiver, _), _), arguments) =>
          (receiver, prefix, arguments)
        case other => report.errorAndAbort(s"Q077 ranked fixture malformed: ${other.show}")
      val nested = rankedPublic(target).getOrElse(
        report.errorAndAbort(s"Q077 public ranked composition failed for $label")
      )
      val flat = Q076StandardMixedModel.matchTarget(plan, target).getOrElse(
        report.errorAndAbort(s"Q077 Q076 comparator failed for $label")
      )
      val identity =
        (nested._1 eq expected._1) && (nested._5 eq expected._2) &&
          nested._4.zip(expected._3).forall { case (left, right) => left eq right }
      val equivalent =
        (nested._1 eq flat._1) && nested._2 == flat._2 && nested._3 == flat._3 &&
          nested._4.zip(flat._4).forall { case (left, right) => left eq right }
      s"$label:${nested._2.decoded}:${nested._3.decoded}:${nested._4.size}:$identity:$equivalent"
    }
    val nonApply = stripInlined('{ Q076StandardMixedTarget.receiver.first }.asTerm)
    val curried = stripInlined('{ Q076StandardMixedTarget.receiver.first.curried(1)(2) }.asTerm)
    val negatives = List(
      s"non-apply:${rankedPublic(nonApply).isEmpty && Q076StandardMixedModel.matchTarget(plan, nonApply).isEmpty}",
      s"curried:${rankedPublic(curried).isEmpty && Q076StandardMixedModel.matchTarget(plan, curried).isEmpty}"
    )
    Expr.ofList((positive ++ negatives).map(Expr(_)))

  private def negativeObservationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    val plan = Q077StandardFlatModel.plan(3).fold(report.errorAndAbort(_), identity)
    val badOrdinal = Q077StandardFlatModel
      .validate(
        plan.pattern,
        plan.receiverName,
        plan.selectedOccurrences.updated(0, plan.selectedOccurrences(0).copy(selectOrdinal = 0))
      )
      .isLeft
    val duplicateTransport = Q077StandardFlatModel
      .validate(
        plan.pattern,
        plan.receiverName,
        plan.selectedOccurrences.updated(
          1,
          plan.selectedOccurrences(1).copy(transport = plan.selectedOccurrences(0).transport)
        )
      )
      .isLeft
    val malformedTransport = Q077StandardFlatModel
      .validate(
        plan.pattern,
        plan.receiverName,
        plan.selectedOccurrences.updated(
          1,
          plan.selectedOccurrences(1).copy(transport = "bad.transport")
        )
      )
      .isLeft
    val categoryConflict = Q077StandardFlatModel
      .planSource("$receiver.$receiver.$second", List("", ".", ".", ""))
      .isLeft
    val missingSource = TwoSelectedMemberPatternSource
      .classify(List("", ".", ".", ""), null)
      .isLeft
    val sourceSpans =
      plan.sourceStarts == plan.sourceStarts.sorted && plan.sourceStarts.distinct.size == 4
    val unicodeSource = Q077StandardFlatModel
      .planSource("$receiver.$naïve", List("", ".", ""))
      .isLeft
    val repeated = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$same.$same" => ()
          case _ => ()
      }"""
    ).exists(_.message.contains("duplicate pattern variable: same"))
    val repeatMarker = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$same.#repeat" => ()
          case _ => ()
      }"""
    ).nonEmpty
    val rank3 = RankedPatternSource.classify(List("", "(...", ")")).isLeft
    val multipleRank2 = RankedPatternSource
      .classify(List("", "(..", ", ..", ")"))
      .isLeft
    val rightAssociative = peelN(stripInlined('{ 1 :: Nil }.asTerm), 1).isEmpty
    val typeApply = peelN(
      stripInlined('{ Q077StandardChainTarget.root.alpha.beta.generic[Int](1) }.asTerm),
      1
    ).isEmpty
    val namedApply = peelN(
      stripInlined('{ Q077StandardChainTarget.root.alpha.beta.applied(value = 1) }.asTerm),
      1
    ).isEmpty
    val contextualMulticlause = peelN(
      stripInlined('{
        Q077StandardChainTarget.root.alpha.beta.contextual(1)(using "context")
      }.asTerm),
      1
    ).isEmpty
    val extraScalarRank2 = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second($head, ..$tail)" => ()
          case _ => ()
      }"""
    ).nonEmpty
    val foreignQuotes = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.construct.SelectedMemberName
        import quasiquotes.matching.TermPatternProductExtractor
        def reject(using q1: Quotes)(
          extractor: TermPatternProductExtractor[q1.reflect.Term, (q1.reflect.Term, SelectedMemberName)]
        )(using q2: Quotes)(target: q2.reflect.Term) = extractor.unapply(target)
      }"""
    ).nonEmpty

    Expr.ofList(
      List(
        s"bad-ordinal:$badOrdinal",
        s"duplicate-transport:$duplicateTransport",
        s"malformed-transport:$malformedTransport",
        s"category-conflict:$categoryConflict",
        s"missing-source:$missingSource",
        s"source-spans:$sourceSpans",
        s"unicode-source:$unicodeSource",
        s"native-e005:$repeated",
        s"repeat-marker:$repeatMarker",
        s"rank3:$rank3",
        s"multiple-rank2:$multipleRank2",
        s"right-associative:$rightAssociative",
        s"typeapply:$typeApply",
        s"named-apply:$namedApply",
        s"contextual-multiclause:$contextualMulticlause",
        s"extra-scalar-rank2:$extraScalarRank2",
        s"foreign-quotes:$foreignQuotes"
      ).map(Expr(_))
    )

  private def stripInlined(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term match
      case Inlined(_, Nil, body) => stripInlined(body)
      case other => other
