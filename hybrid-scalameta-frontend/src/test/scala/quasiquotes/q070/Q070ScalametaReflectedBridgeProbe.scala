package quasiquotes.matching

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName
import quasiquotes.scalameta.TermFrontend

object Q070ScalametaReflectedBridgeTarget:
  object unaryReceiver:
    object inner:
      def outer(value: Int): Int = value + 1

/** Test-only Q070 Scalameta-primary plan and reflected-target bridge. */
object Q070ScalametaReflectedBridgeProbe:
  private enum Layout:
    case Direct, Unary

  private final case class Plan(
      layout: Layout,
      pattern: TermPattern,
      selectedOccurrences: Vector[SelectedNamePatternOccurrence],
      engine: TermFrontend.Engine,
      primaryFailure: Option[TermFrontend.Failure]
  )

  private final case class ReflectedMatch[T](
      termBindings: Map[String, T],
      selectedBindings: Map[String, SelectedMemberName]
  )

  inline def unaryObservation: String = ${ unaryObservationImpl }

  def planObservation: String =
    val plans = Vector(
      scalametaPlan(
        "$receiver.$first.$second",
        StringContext("", ".", ".", "").parts.toList,
        Layout.Direct
      ),
      scalametaPlan(
        "$receiver.$first.$second($argument)",
        StringContext("", ".", ".", "(", ")").parts.toList,
        Layout.Unary
      ),
      scalametaPlan(
        "$receiver.$same.$same",
        StringContext("", ".", ".", "").parts.toList,
        Layout.Direct
      )
    ).map(_.fold(error => throw new AssertionError(error), identity))

    plans
      .map(plan => s"${plan.engine}:${plan.primaryFailure.isEmpty}")
      .mkString(":")

  private def unaryObservationImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    val plan = scalametaPlan(
      "$receiver.$first.$second($argument)",
      StringContext("", ".", ".", "(", ")").parts.toList,
      Layout.Unary
    ).fold(error => report.errorAndAbort(error), identity)
    if plan.engine != TermFrontend.Engine.Scalameta || plan.primaryFailure.nonEmpty then
      report.errorAndAbort("Q070 hybrid probe did not remain Scalameta-primary")

    val target = stripInlined(
      '{ Q070ScalametaReflectedBridgeTarget.unaryReceiver.inner.outer(41) }.asTerm
    )
    val (originalReceiver, originalArgument) = target match
      case Apply(Select(Select(receiver, _), _), argument :: Nil) => (receiver, argument)
      case other => report.errorAndAbort(s"Q070 hybrid fixture has the wrong shape: ${other.show}")

    val extractor: TermPatternProductExtractor[
      q.reflect.Term,
      (q.reflect.Term, SelectedMemberName, SelectedMemberName, q.reflect.Term)
    ] = TermPatternProductExtractorFactory.productExtractor { candidate =>
      matchTarget(plan, candidate).flatMap { result =>
        for
          receiver <- result.termBindings.get("receiver")
          first <- result.selectedBindings.get("first")
          second <- result.selectedBindings.get("second")
          argument <- result.termBindings.get("argument")
        yield (receiver, first, second, argument)
      }
    }

    val (receiver, first, second, argument) = extractor.unapply(target).getOrElse(
      report.errorAndAbort("Q070 hybrid reflected bridge did not match")
    )
    Expr(
      s"${first.decoded}:${second.decoded}:${receiver eq originalReceiver}:${argument eq originalArgument}"
    )

  private def scalametaPlan(
      source: String,
      parts: List[String],
      layout: Layout
  ): Either[String, Plan] =
    val expectedParts = layout match
      case Layout.Direct => List("", ".", ".", "")
      case Layout.Unary  => List("", ".", ".", "(", ")")
    if parts != expectedParts then Left("unsupported Q070 hybrid source parts")
    else
      for
        normalized <- SelectedNameSourceNormalizer
          .normalize(
            source,
            Vector(SelectedNameSourcePosition(1, 1), SelectedNameSourcePosition(2, 0))
          )
          .left
          .map(_.message)
        first = normalized.selectedOccurrences(0)
        second = normalized.selectedOccurrences(1)
        receiver <- normalized.occurrences.headOption.map(_.name).toRight("missing receiver")
        effectiveSource <- layout match
          case Layout.Direct =>
            Right(s"$$$receiver.${first.transport}.${second.transport}")
          case Layout.Unary =>
            normalized.occurrences
              .lift(3)
              .map(_.name)
              .toRight("missing unary argument")
              .map(argument =>
                s"$$$receiver.${first.transport}.${second.transport}($$$argument)"
              )
        compiled <- TermFrontend.compile(effectiveSource).left.map(_.message)
        _ <- ValidatedSelectedNamePattern
          .createTwo(compiled.pattern, first, second)
          .left
          .map(_.message)
      yield Plan(
        layout,
        compiled.pattern,
        normalized.selectedOccurrences,
        compiled.engine,
        compiled.primaryFailure
      )

  private def matchTarget(using q: Quotes)(
      plan: Plan,
      target: q.reflect.Term
  ): Option[ReflectedMatch[q.reflect.Term]] =
    import q.reflect.*

    if target == null then None
    else
      val prepared = stripInlined(target)
      val selectedNames = (plan.layout, prepared) match
        case (Layout.Direct, Select(Select(_, inner), outer)) => Some(Vector(outer, inner))
        case (Layout.Unary, Apply(Select(Select(_, inner), outer), _ :: Nil)) =>
          Some(Vector(outer, inner))
        case _ => None

      for
        names <- selectedNames
        selected <- selectedBindings(plan.selectedOccurrences, names)
        concrete <- replaceSelectedNames(plan.pattern, names)
        matched <- TermMatcher.matchTermRaw(concrete, prepared).toOption
      yield ReflectedMatch(matched.bindings, selected)

  private def selectedBindings(
      occurrences: Vector[SelectedNamePatternOccurrence],
      rootFirstNames: Vector[String]
  ): Option[Map[String, SelectedMemberName]] =
    occurrences.foldLeft(Option(Map.empty[String, SelectedMemberName])) {
      case (state, occurrence) =>
        for
          bindings <- state
          decoded <- rootFirstNames.lift(occurrence.selectOrdinal)
          selected <- SelectedMemberName.from(decoded).toOption
          updated <- bindings.get(occurrence.name) match
            case None => Some(bindings.updated(occurrence.name, selected))
            case Some(previous) if previous == selected => Some(bindings)
            case Some(_) => None
        yield updated
    }

  private def replaceSelectedNames(
      pattern: TermPattern,
      rootFirstNames: Vector[String]
  ): Option[TermPattern] =
    def loop(current: TermPattern, nextOrdinal: Int): Option[(TermPattern, Int)] =
      current match
        case TermPattern.Select(qualifier, _) =>
          for
            name <- rootFirstNames.lift(nextOrdinal)
            replaced <- loop(qualifier, nextOrdinal + 1)
          yield (TermPattern.Select(replaced._1, name), replaced._2)
        case TermPattern.Apply(function, arguments) =>
          loop(function, nextOrdinal).map { case (replaced, afterFunction) =>
            (TermPattern.Apply(replaced, arguments), afterFunction)
          }
        case other => Some(other -> nextOrdinal)

    loop(pattern, 0).collect {
      case (replaced, consumed) if consumed == rootFirstNames.size => replaced
    }

  private def stripInlined(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term match
      case Inlined(_, Nil, body) => stripInlined(body)
      case other => other
