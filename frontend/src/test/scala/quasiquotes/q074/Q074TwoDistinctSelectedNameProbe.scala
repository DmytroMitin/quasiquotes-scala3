package quasiquotes.q074

import scala.quoted.*

import quasiquotes.matching.TargetTermView

private object Q074TwoDistinctSelectedNameTargets:
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

object Q074TwoDistinctSelectedNameProbe:
  inline def observation: String = ${ observationImpl }
  inline def negativeObservation: String = ${ negativeObservationImpl }
  inline def lexicalObservation: String = ${ lexicalObservationImpl }

  private def observationImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    def stripInlined(term: Term): Term =
      term match
        case Inlined(_, Nil, inner) => stripInlined(inner)
        case other => other

    def sameReference(left: Term, right: Term): Boolean =
      left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]

    val directTarget = stripInlined(
      '{ Q074TwoDistinctSelectedNameTargets.directReceiver.inner.outer }.asTerm
    )
    val unaryTarget = stripInlined(
      '{ Q074TwoDistinctSelectedNameTargets.unaryReceiver.inner.outer(41) }.asTerm
    )
    val equalTarget = stripInlined(
      '{ Q074TwoDistinctSelectedNameTargets.equalReceiver.same.same }.asTerm
    )

    val direct = Q074TwoDistinctSelectedNameConsumers.direct(directTarget).getOrElse(
      report.errorAndAbort("Q074 direct public qq pattern did not match")
    )
    val unary = Q074TwoDistinctSelectedNameConsumers.unary(unaryTarget).getOrElse(
      report.errorAndAbort("Q074 unary public qq pattern did not match")
    )
    val equal = Q074TwoDistinctSelectedNameConsumers.direct(equalTarget).getOrElse(
      report.errorAndAbort("Q074 equal target values did not match distinct source binders")
    )

    val directReceiverIdentity = TargetTermView.fromTerm(directTarget).toOption.exists {
      case TargetTermView.Select(TargetTermView.Select(receiver, _, _), _, _) =>
        sameReference(direct._1, receiver.original)
      case _ => false
    }
    val unaryIdentity = TargetTermView.fromTerm(unaryTarget).toOption.exists {
      case TargetTermView.Apply(
            TargetTermView.Select(TargetTermView.Select(receiver, _, _), _, _),
            argument :: Nil,
            _
          ) =>
        sameReference(unary._1, receiver.original) && sameReference(unary._4, argument.original)
      case _ => false
    }

    val guardedEqual = Q074TwoDistinctSelectedNameConsumers.guardedEqual(equalTarget).nonEmpty
    val guardedDifferent = Q074TwoDistinctSelectedNameConsumers.guardedEqual(directTarget).isEmpty

    Expr(
      Vector(
        direct._2.decoded,
        direct._3.decoded,
        unary._2.decoded,
        unary._3.decoded,
        equal._2.decoded,
        equal._3.decoded,
        directReceiverIdentity.toString,
        unaryIdentity.toString,
        guardedEqual.toString,
        guardedDifferent.toString
      ).mkString(":")
    )

  private def negativeObservationImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    def stripInlined(term: Term): Term = term match
      case Inlined(_, Nil, inner) => stripInlined(inner)
      case other => other

    val wrongTopology = stripInlined('{ 42 }.asTerm)
    val wrongArity = stripInlined(
      '{ Q074TwoDistinctSelectedNameTargets.wrongArityReceiver.inner.outer(1, 2) }.asTerm
    )
    val invalidName = stripInlined(
      '{ Q074TwoDistinctSelectedNameTargets.invalidNameReceiver.inner.naïve }.asTerm
    )
    val unary = stripInlined(
      '{ Q074TwoDistinctSelectedNameTargets.unaryReceiver.inner.outer(41) }.asTerm
    )

    Expr(
      Vector(
        Q074TwoDistinctSelectedNameConsumers.direct(wrongTopology).isEmpty,
        Q074TwoDistinctSelectedNameConsumers.unary(wrongArity).isEmpty,
        Q074TwoDistinctSelectedNameConsumers.direct(unary).isEmpty,
        Q074TwoDistinctSelectedNameConsumers.direct(invalidName).isEmpty,
        Q074TwoDistinctSelectedNameConsumers.direct(null.asInstanceOf[Term]).isEmpty,
        Q074TwoDistinctSelectedNameConsumers.unary(null.asInstanceOf[Term]).isEmpty
      ).mkString(":")
    )

  private def lexicalObservationImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    def stripInlined(term: Term): Term = term match
      case Inlined(_, Nil, inner) => stripInlined(inner)
      case other => other

    val lexical = stripInlined(
      '{ Q074TwoDistinctSelectedNameTargets.lexicalReceiver.`type`.`safe spaced name` }.asTerm
    )
    val generatedLooking = stripInlined(
      '{
        Q074TwoDistinctSelectedNameTargets.generatedLookingReceiver.inner
          .`__qqhole_first__selected_1`
      }.asTerm
    )
    val symbolic = stripInlined(
      '{ Q074TwoDistinctSelectedNameTargets.symbolicReceiver.`+`.`type` }.asTerm
    )
    val lexicalMatch = Q074TwoDistinctSelectedNameConsumers.direct(lexical).getOrElse(
      report.errorAndAbort("Q074 lexical target names did not match")
    )
    val generatedMatch = Q074TwoDistinctSelectedNameConsumers.direct(generatedLooking).getOrElse(
      report.errorAndAbort("Q074 generated-looking literal target name did not match")
    )
    val symbolicMatch = Q074TwoDistinctSelectedNameConsumers.direct(symbolic).getOrElse(
      report.errorAndAbort("Q074 symbolic target name did not match")
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
