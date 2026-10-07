package quasiquotes.q060

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName
import quasiquotes.matching.{QuasiPattern, TargetTermView}

private object Q060SelectedMemberRankedTarget:
  def zero(): Int = 0
  def one(value: Int): Int = value
  def many(first: Int, second: Int, third: Int): Int = first + second + third
  def +(first: Int, second: Int, third: Int): Int = first + second + third
  def `type`(first: Int, second: Int, third: Int): Int = first + second + third
  def `safe spaced name`(first: Int, second: Int, third: Int, last: Int): Int =
    first + second + third + last
  def overloaded(first: Int, second: Int, third: Int): Int = first + second + third
  def overloaded(first: String, second: Int, third: Int): Int = first.length + second + third
  def `naïve`(first: Int, second: Int): Int = first + second
  def `x$internal`(first: Int, second: Int): Int = first + second

object Q060SelectedMemberRankedPatternProbe:
  inline def observations: List[String] = ${ observationsImpl }

  private def observationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    import QuasiPattern.*

    def sameReference(left: Term, right: Term): Boolean =
      left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]

    def all(target: Term): (Term, SelectedMemberName, Seq[Term]) =
      target match
        case qq"$receiver.$selectedName(..$arguments)" =>
          val typedReceiver: Term = receiver
          val typedName: SelectedMemberName = selectedName
          val typedArguments: Seq[Term] = arguments
          (typedReceiver, typedName, typedArguments)
        case _ => report.errorAndAbort("Q060 all-arguments pattern did not match")

    def tail(target: Term): (Term, SelectedMemberName, Term, Seq[Term]) =
      target match
        case qq"$receiver.$selectedName($head, ..$remaining)" =>
          val typedReceiver: Term = receiver
          val typedName: SelectedMemberName = selectedName
          val typedHead: Term = head
          val typedRemaining: Seq[Term] = remaining
          (typedReceiver, typedName, typedHead, typedRemaining)
        case _ => report.errorAndAbort("Q060 tail pattern did not match")

    def init(target: Term): (Term, SelectedMemberName, Seq[Term], Term) =
      target match
        case qq"$receiver.$selectedName(..$leading, $last)" =>
          val typedReceiver: Term = receiver
          val typedName: SelectedMemberName = selectedName
          val typedLeading: Seq[Term] = leading
          val typedLast: Term = last
          (typedReceiver, typedName, typedLeading, typedLast)
        case _ => report.errorAndAbort("Q060 init pattern did not match")

    def middle(target: Term): (Term, SelectedMemberName, Term, Seq[Term], Term) =
      target match
        case qq"$receiver.$selectedName($first, ..$between, $last)" =>
          val typedReceiver: Term = receiver
          val typedName: SelectedMemberName = selectedName
          val typedFirst: Term = first
          val typedBetween: Seq[Term] = between
          val typedLast: Term = last
          (typedReceiver, typedName, typedFirst, typedBetween, typedLast)
        case _ => report.errorAndAbort("Q060 middle pattern did not match")

    def check(
        label: String,
        target: Term,
        receiver: Term,
        selectedName: SelectedMemberName,
        capturedArguments: List[Term]
    ): Unit =
      TargetTermView.fromTerm(target).toOption match
        case Some(
              TargetTermView.Apply(
                TargetTermView.Select(targetReceiver, targetName, _),
                targetArguments,
                _
              )
            ) =>
          if !sameReference(receiver, targetReceiver.original) then
            report.errorAndAbort(s"Q060 $label receiver capture lost target identity")
          if selectedName.decoded != targetName then
            report.errorAndAbort(s"Q060 $label selected name differs from Select.name")
          if capturedArguments.size != targetArguments.size then
            report.errorAndAbort(s"Q060 $label argument capture changed target arity")
          capturedArguments.zip(targetArguments).zipWithIndex.foreach {
            case ((captured, expected), index) =>
              if !sameReference(captured, expected.original) then
                report.errorAndAbort(
                  s"Q060 $label argument $index lost target identity or source order"
                )
          }
        case other => report.errorAndAbort(s"Q060 $label target view was unexpected: $other")

    def optionalAllName(target: Term): Option[String] =
      target match
        case qq"$receiver.$selectedName(..$arguments)" => Some(selectedName.decoded)
        case _ => None

    def optionalMiddle(target: Term): Boolean =
      target match
        case qq"$receiver.$selectedName($first, ..$between, $last)" => true
        case _ => false

    val zeroTarget = '{ Q060SelectedMemberRankedTarget.zero() }.asTerm
    val zeroCapture = all(zeroTarget)
    check("zero", zeroTarget, zeroCapture._1, zeroCapture._2, zeroCapture._3.toList)

    val oneTarget = '{ Q060SelectedMemberRankedTarget.one(1) }.asTerm
    val oneCapture = tail(oneTarget)
    check("one", oneTarget, oneCapture._1, oneCapture._2, oneCapture._3 :: oneCapture._4.toList)
    if optionalMiddle(oneTarget) then
      report.errorAndAbort("Q060 insufficient middle target did not fall through atomically")

    val ordinaryTarget = '{ Q060SelectedMemberRankedTarget.many(1, 2, 3) }.asTerm
    val ordinaryCapture = all(ordinaryTarget)
    check(
      "ordinary",
      ordinaryTarget,
      ordinaryCapture._1,
      ordinaryCapture._2,
      ordinaryCapture._3.toList
    )

    val symbolicTarget = '{ Q060SelectedMemberRankedTarget.+(1, 2, 3) }.asTerm
    val symbolicCapture = tail(symbolicTarget)
    check(
      "symbolic",
      symbolicTarget,
      symbolicCapture._1,
      symbolicCapture._2,
      symbolicCapture._3 :: symbolicCapture._4.toList
    )

    val keywordTarget = '{ Q060SelectedMemberRankedTarget.`type`(1, 2, 3) }.asTerm
    val keywordCapture = init(keywordTarget)
    check(
      "keyword",
      keywordTarget,
      keywordCapture._1,
      keywordCapture._2,
      keywordCapture._3.toList :+ keywordCapture._4
    )

    val spacedTarget =
      '{ Q060SelectedMemberRankedTarget.`safe spaced name`(1, 2, 3, 4) }.asTerm
    val spacedCapture = middle(spacedTarget)
    check(
      "spaced",
      spacedTarget,
      spacedCapture._1,
      spacedCapture._2,
      spacedCapture._3 :: (spacedCapture._4.toList :+ spacedCapture._5)
    )

    val overloadedTarget = '{ Q060SelectedMemberRankedTarget.overloaded(1, 2, 3) }.asTerm
    val overloadedCapture = middle(overloadedTarget)
    check(
      "overloaded",
      overloadedTarget,
      overloadedCapture._1,
      overloadedCapture._2,
      overloadedCapture._3 :: (overloadedCapture._4.toList :+ overloadedCapture._5)
    )

    if optionalAllName('{ Q060SelectedMemberRankedTarget.`naïve`(1, 2) }.asTerm).nonEmpty then
      report.errorAndAbort("Q060 Unicode target name did not fall through")
    if optionalAllName('{ Q060SelectedMemberRankedTarget.`x$internal`(1, 2) }.asTerm).nonEmpty then
      report.errorAndAbort("Q060 dollar-bearing target name did not fall through")
    if optionalAllName('{ 1 }.asTerm).nonEmpty then
      report.errorAndAbort("Q060 non-Apply target did not fall through")

    Expr.ofList(
      List(
        s"zero:${zeroCapture._2.decoded}:${zeroCapture._3.size}",
        s"one:${oneCapture._2.decoded}:${oneCapture._4.size}",
        s"ordinary:${ordinaryCapture._2.decoded}:${ordinaryCapture._3.size}",
        s"symbolic:${symbolicCapture._2.decoded}:${symbolicCapture._4.size}",
        s"keyword:${keywordCapture._2.decoded}:${keywordCapture._3.size}",
        s"spaced:${spacedCapture._2.decoded}:${spacedCapture._4.size}",
        s"overloaded:${overloadedCapture._2.decoded}:${overloadedCapture._4.size}",
        "unicode:fallthrough",
        "dollar-internal:fallthrough",
        "non-apply:fallthrough",
        "insufficient:fallthrough"
      ).map(Expr(_))
    )
