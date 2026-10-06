package quasiquotes.q058

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName
import quasiquotes.matching.{QuasiPattern, TargetTermView}

private object Q058SelectedMemberTarget:
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

object Q058SelectedMemberPatternProbe:
  inline def observations: List[String] = ${ observationsImpl }

  private def observationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    import QuasiPattern.*

    def sameReference(left: Term, right: Term): Boolean =
      left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]

    def direct(target: Term): (Term, SelectedMemberName) =
      target match
        case qq"$receiver.$selectedName" =>
          val typedReceiver: Term = receiver
          val typedName: SelectedMemberName = selectedName
          (typedReceiver, typedName)
        case _ => report.errorAndAbort("Q058 direct selected-name pattern did not match")

    def nullary(target: Term): (Term, SelectedMemberName) =
      target match
        case qq"$receiver.$selectedName()" =>
          val typedReceiver: Term = receiver
          val typedName: SelectedMemberName = selectedName
          (typedReceiver, typedName)
        case _ => report.errorAndAbort("Q058 nullary selected-name pattern did not match")

    def unary(target: Term): (Term, SelectedMemberName, Term) =
      target match
        case qq"$receiver.$selectedName($argument)" =>
          val typedReceiver: Term = receiver
          val typedName: SelectedMemberName = selectedName
          val typedArgument: Term = argument
          (typedReceiver, typedName, typedArgument)
        case _ => report.errorAndAbort("Q058 unary selected-name pattern did not match")

    def checkDirect(
        target: Term,
        captured: (Term, SelectedMemberName)
    ): Unit =
      TargetTermView.fromTerm(target).toOption match
        case Some(TargetTermView.Select(qualifier, targetName, _)) =>
          if !sameReference(captured._1, qualifier.original) then
            report.errorAndAbort("Q058 direct receiver capture lost target identity")
          if captured._2.decoded != targetName then
            report.errorAndAbort("Q058 direct name capture differs from Select.name")
        case other => report.errorAndAbort(s"Q058 direct target view was unexpected: $other")

    def checkNullary(
        target: Term,
        captured: (Term, SelectedMemberName)
    ): Unit =
      TargetTermView.fromTerm(target).toOption match
        case Some(TargetTermView.Apply(TargetTermView.Select(qualifier, targetName, _), Nil, _)) =>
          if !sameReference(captured._1, qualifier.original) then
            report.errorAndAbort("Q058 nullary receiver capture lost target identity")
          if captured._2.decoded != targetName then
            report.errorAndAbort("Q058 nullary name capture differs from Select.name")
        case other => report.errorAndAbort(s"Q058 nullary target view was unexpected: $other")

    def checkUnary(
        target: Term,
        captured: (Term, SelectedMemberName, Term)
    ): Unit =
      TargetTermView.fromTerm(target).toOption match
        case Some(
              TargetTermView.Apply(
                TargetTermView.Select(qualifier, targetName, _),
                argument :: Nil,
                _
              )
            ) =>
          if !sameReference(captured._1, qualifier.original) then
            report.errorAndAbort("Q058 unary receiver capture lost target identity")
          if !sameReference(captured._3, argument.original) then
            report.errorAndAbort("Q058 unary argument capture lost target identity")
          if captured._2.decoded != targetName then
            report.errorAndAbort("Q058 unary name capture differs from Select.name")
        case other => report.errorAndAbort(s"Q058 unary target view was unexpected: $other")

    def optionalUnaryName(target: Term): Option[String] =
      target match
        case qq"$receiver.$selectedName($argument)" => Some(selectedName.decoded)
        case _ => None

    def optionalDirectName(target: Term): Option[String] =
      target match
        case qq"$receiver.$selectedName" => Some(selectedName.decoded)
        case _ => None

    val directTarget = '{ Q058SelectedMemberTarget.field }.asTerm
    val directCapture = direct(directTarget)
    checkDirect(directTarget, directCapture)

    val nullaryTarget = '{ Q058SelectedMemberTarget.nullary() }.asTerm
    val nullaryCapture = nullary(nullaryTarget)
    checkNullary(nullaryTarget, nullaryCapture)

    val ordinaryTarget = '{ Q058SelectedMemberTarget.ordinary(40) }.asTerm
    val ordinaryCapture = unary(ordinaryTarget)
    checkUnary(ordinaryTarget, ordinaryCapture)

    val operatorCapture = unary('{ Q058SelectedMemberTarget.+(40) }.asTerm)
    val keywordCapture = unary('{ Q058SelectedMemberTarget.`type`(40) }.asTerm)
    val spacedCapture = unary('{ Q058SelectedMemberTarget.`safe spaced name`(40) }.asTerm)
    val overloadedCapture = unary('{ Q058SelectedMemberTarget.overloaded(40) }.asTerm)

    if optionalDirectName(null.asInstanceOf[Term]).nonEmpty then
      report.errorAndAbort("Q058 null target did not fall through")
    if optionalDirectName('{ 1 }.asTerm).nonEmpty then
      report.errorAndAbort("Q058 non-Select target did not fall through")
    if optionalUnaryName('{ Q058SelectedMemberTarget.`naïve`(40) }.asTerm).nonEmpty then
      report.errorAndAbort("Q058 Unicode target name did not fall through")
    if optionalUnaryName('{ Q058SelectedMemberTarget.`x$internal`(40) }.asTerm).nonEmpty then
      report.errorAndAbort("Q058 dollar-bearing target name did not fall through")
    if optionalDirectName('{ new Object() }.asTerm).nonEmpty then
      report.errorAndAbort("Q058 compiler-special/non-Select target did not fall through")

    Expr.ofList(
      List(
        s"direct:${directCapture._2.decoded}",
        s"nullary:${nullaryCapture._2.decoded}",
        s"ordinary:${ordinaryCapture._2.decoded}",
        s"symbolic:${operatorCapture._2.decoded}",
        s"keyword:${keywordCapture._2.decoded}",
        s"spaced:${spacedCapture._2.decoded}",
        s"overloaded:${overloadedCapture._2.decoded}",
        "null:fallthrough",
        "non-select:fallthrough",
        "unicode:fallthrough",
        "dollar-internal:fallthrough",
        "compiler-special:fallthrough"
      ).map(Expr(_))
    )
