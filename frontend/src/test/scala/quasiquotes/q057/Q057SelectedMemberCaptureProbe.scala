package quasiquotes.q057

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName
import quasiquotes.matching.{QuasiPattern, TargetTermView}
import quasiquotes.q057.Q057SelectedMemberPatternProbe.*

object Q057SelectedMemberTarget:
  val field: Int = 1
  def nullary(): Int = 2
  def ordinary(value: Int): Int = value + 1
  def +(value: Int): Int = value + 2
  def `type`(value: Int): Int = value + 3
  def `safe spaced name`(value: Int): Int = value + 4
  def overloaded(value: Int): Int = value
  def overloaded(value: String): Int = value.length
  def `naïve`(value: Int): Int = value
  def `x$internal`(value: Int): Int = value

object Q057SelectedMemberCaptureProbe:
  inline def observations: List[String] = ${ observationsImpl }

  private def observationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    def sameReference(left: Term, right: Term): Boolean =
      left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]

    def direct(target: Term): (Term, SelectedMemberName) =
      target match
        case q057qq"$receiver.$selectedName" =>
          val typedReceiver: Term = receiver
          val typedName: SelectedMemberName = selectedName
          (typedReceiver, typedName)
        case _ => report.errorAndAbort("Q057 direct selected-name probe did not match")

    def nullary(target: Term): (Term, SelectedMemberName) =
      target match
        case q057qq"$receiver.$selectedName()" =>
          val typedReceiver: Term = receiver
          val typedName: SelectedMemberName = selectedName
          (typedReceiver, typedName)
        case _ => report.errorAndAbort("Q057 nullary selected-name probe did not match")

    def unary(target: Term): (Term, SelectedMemberName, Term) =
      target match
        case q057qq"$receiver.$selectedName($argument)" =>
          val typedReceiver: Term = receiver
          val typedName: SelectedMemberName = selectedName
          val typedArgument: Term = argument
          (typedReceiver, typedName, typedArgument)
        case _ =>
          report.errorAndAbort(
            s"Q057 unary selected-name probe did not match: view=${TargetTermView.fromTerm(target)}; tree=${target.show(using Printer.TreeStructure)}"
          )

    def checkDirectIdentity(target: Term, captured: (Term, SelectedMemberName)): Unit =
      TargetTermView.fromTerm(target).toOption match
        case Some(TargetTermView.Select(qualifier, _, _)) =>
          if !sameReference(captured._1, qualifier.original) then
            report.errorAndAbort("Q057 direct receiver capture did not retain exact target identity")
        case other => report.errorAndAbort(s"Q057 direct target view was unexpected: $other")

    def checkUnaryIdentity(
        target: Term,
        captured: (Term, SelectedMemberName, Term)
    ): Unit =
      TargetTermView.fromTerm(target).toOption match
        case Some(TargetTermView.Apply(TargetTermView.Select(qualifier, _, _), argument :: Nil, _)) =>
          if !sameReference(captured._1, qualifier.original) then
            report.errorAndAbort("Q057 unary receiver capture did not retain exact target identity")
          if !sameReference(captured._3, argument.original) then
            report.errorAndAbort("Q057 unary argument capture did not retain exact target identity")
        case other => report.errorAndAbort(s"Q057 unary target view was unexpected: $other")

    def checkNullaryIdentity(target: Term, captured: (Term, SelectedMemberName)): Unit =
      TargetTermView.fromTerm(target).toOption match
        case Some(TargetTermView.Apply(TargetTermView.Select(qualifier, _, _), Nil, _)) =>
          if !sameReference(captured._1, qualifier.original) then
            report.errorAndAbort("Q057 nullary receiver capture did not retain exact target identity")
        case other => report.errorAndAbort(s"Q057 nullary target view was unexpected: $other")

    def optionalUnaryName(target: Term): Option[String] =
      target match
        case q057qq"$receiver.$selectedName($argument)" => Some(selectedName.decoded)
        case _ => None

    val directTarget = '{ Q057SelectedMemberTarget.field }.asTerm
    val directCapture = direct(directTarget)
    checkDirectIdentity(directTarget, directCapture)

    val nullaryTarget = '{ Q057SelectedMemberTarget.nullary() }.asTerm
    val nullaryCapture = nullary(nullaryTarget)
    checkNullaryIdentity(nullaryTarget, nullaryCapture)

    val ordinaryTarget = '{ Q057SelectedMemberTarget.ordinary(40) }.asTerm
    val ordinaryCapture = unary(ordinaryTarget)
    checkUnaryIdentity(ordinaryTarget, ordinaryCapture)

    val operatorCapture = unary('{ Q057SelectedMemberTarget.+(40) }.asTerm)
    val keywordCapture = unary('{ Q057SelectedMemberTarget.`type`(40) }.asTerm)
    val spacedCapture = unary('{ Q057SelectedMemberTarget.`safe spaced name`(40) }.asTerm)
    val overloadedCapture = unary('{ Q057SelectedMemberTarget.overloaded(40) }.asTerm)

    val directPattern = QuasiPattern
      .termOrThrow("$q057Receiver.__q057_selected_name_placeholder")
      .pattern
    val general = Q057SelectedMemberPatternProbeFactory.direct(
      directPattern,
      "q057Receiver"
    )
    val mechanicallyRanked =
      Q057SelectedMemberPatternProbeFactory.mechanicallyReuseRanked(general)
    val rankedMechanicalName = mechanicallyRanked
      .unapply(directTarget)
      .map(_._2.decoded)
      .getOrElse(report.errorAndAbort("Q057 mechanical Ranked extractor reuse failed"))

    val nullTarget = null.asInstanceOf[Term]
    if general.unapply(nullTarget).nonEmpty then
      report.errorAndAbort("Q057 null target did not fall through")
    if general.unapply('{ 1 }.asTerm).nonEmpty then
      report.errorAndAbort("Q057 non-Select target did not fall through")
    if optionalUnaryName('{ Q057SelectedMemberTarget.`naïve`(40) }.asTerm).nonEmpty then
      report.errorAndAbort("Q057 Unicode target name did not fall through")
    if optionalUnaryName('{ Q057SelectedMemberTarget.`x$internal`(40) }.asTerm).nonEmpty then
      report.errorAndAbort("Q057 dollar-bearing target name did not fall through")
    if general.unapply('{ new Object() }.asTerm).nonEmpty then
      report.errorAndAbort("Q057 compiler-special constructor target did not fall through")

    Expr.ofList(
      List(
        s"direct:${directCapture._2.decoded}",
        s"nullary:${nullaryCapture._2.decoded}",
        s"ordinary:${ordinaryCapture._2.decoded}",
        s"symbolic:${operatorCapture._2.decoded}",
        s"keyword:${keywordCapture._2.decoded}",
        s"spaced:${spacedCapture._2.decoded}",
        s"overloaded:${overloadedCapture._2.decoded}",
        s"ranked-mechanical:$rankedMechanicalName",
        "null:fallthrough",
        "non-select:fallthrough",
        "unicode:fallthrough",
        "dollar-internal:fallthrough",
        "compiler-special:fallthrough"
      ).map(Expr(_))
    )
