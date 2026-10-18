package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*

import quasiquotes.construct.SelectedMemberName
import quasiquotes.scalameta.TermFrontend

/** Q077 test-only Scalameta-primary compiler projection of the shared generic
  * source-role algorithm.  Matching and tuple construction stay depth-
  * independent and reuse the same test-only recursive implementation.
  */
private[matching] object Q077ScalametaFlatModel:
  final case class Plan(
      delegate: Q077StandardFlatModel.Plan,
      engine: TermFrontend.Engine,
      primaryFailure: Option[TermFrontend.Failure]
  ):
    def pattern: TermPattern = delegate.pattern
    def receiverName: String = delegate.receiverName
    def selectedOccurrences: Vector[SelectedNamePatternOccurrence] =
      delegate.selectedOccurrences

  def plan(depth: Int): Either[String, Plan] =
    for
      standard <- Q077StandardFlatModel.plan(depth)
      compiled <- TermFrontend.compile(standard.effectiveSource).left.map(_.message)
      _ <- Either.cond(
        compiled.engine == TermFrontend.Engine.Scalameta && compiled.primaryFailure.isEmpty,
        (),
        s"Q077 flat N=$depth was not Scalameta-primary: ${compiled.engine} / ${compiled.primaryFailure}"
      )
      _ <- Q077StandardFlatModel.validate(
        compiled.pattern,
        standard.receiverName,
        standard.selectedOccurrences
      )
      delegate = standard.copy(pattern = compiled.pattern)
    yield Plan(delegate, compiled.engine, compiled.primaryFailure)

  def matchTarget(using q: Quotes)(
      plan: Plan,
      target: q.reflect.Term
  ): Option[Q077StandardFlatModel.Result[q.reflect.Term]] =
    Q077StandardFlatModel.matchTarget(plan.delegate, target)

  def one(using q: Quotes)(plan: Plan): TermPatternProductExtractor[
      q.reflect.Term,
      (q.reflect.Term, SelectedMemberName)
  ] = Q077StandardFlatModel.one(plan.delegate)

  def two(using q: Quotes)(plan: Plan): TermPatternProductExtractor[
      q.reflect.Term,
      (q.reflect.Term, SelectedMemberName, SelectedMemberName)
  ] = Q077StandardFlatModel.two(plan.delegate)

  def three(using q: Quotes)(plan: Plan): TermPatternProductExtractor[
      q.reflect.Term,
      (q.reflect.Term, SelectedMemberName, SelectedMemberName, SelectedMemberName)
  ] = Q077StandardFlatModel.three(plan.delegate)

  def four(using q: Quotes)(plan: Plan): TermPatternProductExtractor[
      q.reflect.Term,
      (
          q.reflect.Term,
          SelectedMemberName,
          SelectedMemberName,
          SelectedMemberName,
          SelectedMemberName
      )
  ] = Q077StandardFlatModel.four(plan.delegate)

object Q077ScalametaChainProbe:
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
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq
    target match
      case qq"$prefix.$second" =>
        prefix match
          case qq"$receiver.$first" => Some((receiver, first, second, prefix))
          case _ => None
      case _ => None

  private def flatPublic(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName, SelectedMemberName)] =
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq
    target match
      case qq"$receiver.$first.$second" => Some((receiver, first, second))
      case _ => None

  private def peel(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName)] =
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq
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
        case other => report.errorAndAbort(s"Q077 Scalameta composition failed: $label / $other")

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
        report.errorAndAbort(s"Q077 Scalameta raw fixture is shorter than $depth for $label")
      )
      val result = peelN(target, depth).getOrElse(
        report.errorAndAbort(s"Q077 Scalameta public peel failed for $label")
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
    ).getOrElse(report.errorAndAbort("Q077 Scalameta arbitrary-prefix long peel failed"))
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
    val wrapped = peelN(wrappedTarget, 2).exists(
      _.names.map(_.decoded) == Vector("alpha", "beta")
    )
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
      Q077ScalametaFlatModel.plan(depth).fold(report.errorAndAbort(_), identity)
    ).toVector
    val targets = Vector(
      stripInlined('{ Q077StandardChainTarget.root.alpha }.asTerm),
      stripInlined('{ Q077StandardChainTarget.root.alpha.beta }.asTerm),
      stripInlined('{ Q077StandardChainTarget.root.alpha.beta.gamma }.asTerm),
      stripInlined('{ Q077StandardChainTarget.root.alpha.beta.gamma.delta }.asTerm)
    )

    val one: TermPatternProductExtractor[Term, (Term, SelectedMemberName)] =
      Q077ScalametaFlatModel.one(plans(0))
    val two: TermPatternProductExtractor[
      Term,
      (Term, SelectedMemberName, SelectedMemberName)
    ] = Q077ScalametaFlatModel.two(plans(1))
    val three: TermPatternProductExtractor[
      Term,
      (Term, SelectedMemberName, SelectedMemberName, SelectedMemberName)
    ] = Q077ScalametaFlatModel.three(plans(2))
    val four: TermPatternProductExtractor[
      Term,
      (Term, SelectedMemberName, SelectedMemberName, SelectedMemberName, SelectedMemberName)
    ] = Q077ScalametaFlatModel.four(plans(3))

    val typed = Vector(
      one.unapply(targets(0)).map(value => (value._1, Vector(value._2))),
      two.unapply(targets(1)).map(value => (value._1, Vector(value._2, value._3))),
      three.unapply(targets(2)).map(value => (value._1, Vector(value._2, value._3, value._4))),
      four.unapply(targets(3)).map(value =>
        (value._1, Vector(value._2, value._3, value._4, value._5))
      )
    )

    val modelLines = typed.zipWithIndex.map { case (value, index) =>
      val result = value.getOrElse(
        report.errorAndAbort(s"Q077 Scalameta typed flat n${index + 1} failed")
      )
      val expectedReceiver = rawPeel(targets(index), index + 1).map(_._1).getOrElse(
        report.errorAndAbort(s"Q077 Scalameta typed fixture n${index + 1} malformed")
      )
      s"n${index + 1}:${result._2.map(_.decoded).mkString(",")}:${result._1 eq expectedReceiver}"
    }

    val compositionLines = (2 to 4).map { depth =>
      val generic = Q077ScalametaFlatModel.matchTarget(plans(depth - 1), targets(depth - 1))
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
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq
    target match
      case qq"$prefix.$second(..$arguments)" =>
        prefix match
          case qq"$receiver.$first" =>
            Some((receiver, first, second, arguments, prefix))
          case _ => None
      case _ => None

  private def rankedCompositionObservationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    val plan = Q076ScalametaMixedModel.plan().fold(report.errorAndAbort(_), identity)
    val fixtures = List(
      "zero" -> '{ Q076ScalametaMixedTarget.receiver.first.second() }.asTerm,
      "one" -> '{ Q076ScalametaMixedTarget.receiver.first.second(1) }.asTerm,
      "many" -> '{ Q076ScalametaMixedTarget.receiver.first.second(1, 2, 3) }.asTerm
    )
    val positive = fixtures.map { case (label, raw) =>
      val target = stripInlined(raw)
      val expected = target match
        case Apply(Select(prefix @ Select(receiver, _), _), arguments) =>
          (receiver, prefix, arguments)
        case other => report.errorAndAbort(s"Q077 Scalameta ranked fixture malformed: ${other.show}")
      val nested = rankedPublic(target).getOrElse(
        report.errorAndAbort(s"Q077 Scalameta public ranked composition failed for $label")
      )
      val flat = Q076ScalametaMixedModel.matchTarget(plan, target).getOrElse(
        report.errorAndAbort(s"Q077 Scalameta Q076 comparator failed for $label")
      )
      val identity =
        (nested._1 eq expected._1) && (nested._5 eq expected._2) &&
          nested._4.zip(expected._3).forall { case (left, right) => left eq right }
      val equivalent =
        (nested._1 eq flat._1) && nested._2 == flat._2 && nested._3 == flat._3 &&
          nested._4.zip(flat._4).forall { case (left, right) => left eq right }
      s"$label:${nested._2.decoded}:${nested._3.decoded}:${nested._4.size}:$identity:$equivalent"
    }
    val nonApply = stripInlined('{ Q076ScalametaMixedTarget.receiver.first }.asTerm)
    val curried = stripInlined('{ Q076ScalametaMixedTarget.receiver.first.curried(1)(2) }.asTerm)
    val negatives = List(
      s"non-apply:${rankedPublic(nonApply).isEmpty && Q076ScalametaMixedModel.matchTarget(plan, nonApply).isEmpty}",
      s"curried:${rankedPublic(curried).isEmpty && Q076ScalametaMixedModel.matchTarget(plan, curried).isEmpty}"
    )
    Expr.ofList((positive ++ negatives).map(Expr(_)))

  private def negativeObservationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    val plan = Q077ScalametaFlatModel.plan(3).fold(report.errorAndAbort(_), identity)
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
    val missingSource = ScalametaTwoSelectedMemberPatternSource
      .classify(List("", ".", ".", ""), null)
      .isLeft
    val sourceSpans =
      plan.delegate.sourceStarts == plan.delegate.sourceStarts.sorted &&
        plan.delegate.sourceStarts.distinct.size == 4
    val unicodeSource = Q077StandardFlatModel
      .planSource("$receiver.$naïve", List("", ".", ""))
      .isLeft
    val repeated = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$same.$same" => ()
          case _ => ()
      }"""
    ).exists(_.message.contains("duplicate pattern variable: same"))
    val repeatMarker = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.scalameta.ScalametaQuasiPattern.qq
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
        import quasiquotes.scalameta.ScalametaQuasiPattern.qq
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
