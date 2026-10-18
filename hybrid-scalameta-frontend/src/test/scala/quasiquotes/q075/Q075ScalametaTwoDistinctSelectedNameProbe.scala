package quasiquotes.q075

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName
import quasiquotes.matching.TargetTermView
import quasiquotes.scalameta.ScalametaQuasiPattern.qq

private object Q075ScalametaTwoDistinctSelectedNameTargets:
  object directReceiver:
    object inner:
      val outer: Int = 42

  object unaryReceiver:
    object inner:
      def outer(value: Int): Int = value + 1

  object equalReceiver:
    object same:
      val same: Int = 42

  object wrongArityReceiver:
    object inner:
      def outer(first: Int, second: Int): Int = first + second

  object invalidNameReceiver:
    object inner:
      val naïve: Int = 42

  object lexicalReceiver:
    object `type`:
      val `safe spaced name`: Int = 42

  object symbolicReceiver:
    object `+`:
      val `type`: Int = 42

  object generatedLookingReceiver:
    object inner:
      val `__qqhole_first__selected_1`: Int = 42

object Q075ScalametaTwoDistinctSelectedNameProbe:
  inline def observation: String = ${ observationImpl }
  inline def negativeObservation: String = ${ negativeObservationImpl }
  inline def lexicalObservation: String = ${ lexicalObservationImpl }

  private def direct(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName, SelectedMemberName)] =
    target match
      case qq"$receiver.$first.$second" => Some((receiver, first, second))
      case _ => None

  private def unary(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName, SelectedMemberName, q.reflect.Term)] =
    target match
      case qq"$receiver.$first.$second($argument)" =>
        Some((receiver, first, second, argument))
      case _ => None

  private def guardedEqual(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName, SelectedMemberName)] =
    target match
      case qq"$receiver.$first.$second" if first == second =>
        Some((receiver, first, second))
      case _ => None

  private def observationImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    def stripInlined(term: Term): Term = term match
      case Inlined(_, Nil, inner) => stripInlined(inner)
      case other => other

    def sameReference(left: Term, right: Term): Boolean =
      left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]

    val directTarget = stripInlined(
      '{ Q075ScalametaTwoDistinctSelectedNameTargets.directReceiver.inner.outer }.asTerm
    )
    val unaryTarget = stripInlined(
      '{ Q075ScalametaTwoDistinctSelectedNameTargets.unaryReceiver.inner.outer(41) }.asTerm
    )
    val equalTarget = stripInlined(
      '{ Q075ScalametaTwoDistinctSelectedNameTargets.equalReceiver.same.same }.asTerm
    )

    val directResult = direct(directTarget).getOrElse(
      report.errorAndAbort("Q075 direct public typed-Scalameta qq pattern did not match")
    )
    val unaryResult = unary(unaryTarget).getOrElse(
      report.errorAndAbort("Q075 unary public typed-Scalameta qq pattern did not match")
    )
    val equal = direct(equalTarget).getOrElse(
      report.errorAndAbort("Q075 equal target values did not match distinct source binders")
    )

    val directReceiverIdentity = TargetTermView.fromTerm(directTarget).toOption.exists {
      case TargetTermView.Select(TargetTermView.Select(receiver, _, _), _, _) =>
        sameReference(directResult._1, receiver.original)
      case _ => false
    }
    val unaryIdentity = TargetTermView.fromTerm(unaryTarget).toOption.exists {
      case TargetTermView.Apply(
            TargetTermView.Select(TargetTermView.Select(receiver, _, _), _, _),
            argument :: Nil,
            _
          ) =>
        sameReference(unaryResult._1, receiver.original) &&
          sameReference(unaryResult._4, argument.original)
      case _ => false
    }

    val equalPassedGuard = guardedEqual(equalTarget).nonEmpty
    val differentFailedGuard = guardedEqual(directTarget).isEmpty

    Expr(
      Vector(
        directResult._2.decoded,
        directResult._3.decoded,
        unaryResult._2.decoded,
        unaryResult._3.decoded,
        equal._2.decoded,
        equal._3.decoded,
        directReceiverIdentity.toString,
        unaryIdentity.toString,
        equalPassedGuard.toString,
        differentFailedGuard.toString
      ).mkString(":")
    )

  private def negativeObservationImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    def stripInlined(term: Term): Term = term match
      case Inlined(_, Nil, inner) => stripInlined(inner)
      case other => other

    val wrongTopology = stripInlined('{ 42 }.asTerm)
    val wrongArity = stripInlined(
      '{ Q075ScalametaTwoDistinctSelectedNameTargets.wrongArityReceiver.inner.outer(1, 2) }.asTerm
    )
    val invalidName = stripInlined(
      '{ Q075ScalametaTwoDistinctSelectedNameTargets.invalidNameReceiver.inner.naïve }.asTerm
    )
    val unaryTarget = stripInlined(
      '{ Q075ScalametaTwoDistinctSelectedNameTargets.unaryReceiver.inner.outer(41) }.asTerm
    )

    Expr(
      Vector(
        direct(wrongTopology).isEmpty,
        unary(wrongArity).isEmpty,
        direct(unaryTarget).isEmpty,
        direct(invalidName).isEmpty,
        direct(null.asInstanceOf[Term]).isEmpty,
        unary(null.asInstanceOf[Term]).isEmpty
      ).mkString(":")
    )

  private def lexicalObservationImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    def stripInlined(term: Term): Term = term match
      case Inlined(_, Nil, inner) => stripInlined(inner)
      case other => other

    val lexical = stripInlined(
      '{ Q075ScalametaTwoDistinctSelectedNameTargets.lexicalReceiver.`type`.`safe spaced name` }.asTerm
    )
    val generatedLooking = stripInlined(
      '{
        Q075ScalametaTwoDistinctSelectedNameTargets.generatedLookingReceiver.inner
          .`__qqhole_first__selected_1`
      }.asTerm
    )
    val symbolic = stripInlined(
      '{ Q075ScalametaTwoDistinctSelectedNameTargets.symbolicReceiver.`+`.`type` }.asTerm
    )
    val lexicalMatch = direct(lexical).getOrElse(
      report.errorAndAbort("Q075 lexical target names did not match")
    )
    val generatedMatch = direct(generatedLooking).getOrElse(
      report.errorAndAbort("Q075 generated-looking literal target name did not match")
    )
    val symbolicMatch = direct(symbolic).getOrElse(
      report.errorAndAbort("Q075 symbolic target name did not match")
    )

    Expr(
      Vector(
        lexicalMatch._2.decoded,
        lexicalMatch._3.decoded,
        generatedMatch._2.decoded,
        generatedMatch._3.decoded,
        symbolicMatch._2.decoded,
        symbolicMatch._3.decoded
      ).mkString(":")
    )
