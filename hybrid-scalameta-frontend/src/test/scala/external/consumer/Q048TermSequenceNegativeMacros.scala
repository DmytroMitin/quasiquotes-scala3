package external.consumer

import scala.quoted.*

import quasiquotes.construct.{TermSequenceSplice, TermSequenceSplices}

object Q048TermSequenceNegativeTargets:
  def one(value: Int): Int = value
  def two(first: Int, second: Int): Int = first + second
  def curried(first: Int)(second: Int): Int = first + second
  def named(first: Int): Int = first
  def contextual(using value: Int): Int = value

object Q048TermSequenceNegativeMacros:
  inline def standardRejected0: Unit = ${ rejectedImpl(0, false) }
  inline def standardRejected1: Unit = ${ rejectedImpl(1, false) }
  inline def standardRejected2: Unit = ${ rejectedImpl(2, false) }
  inline def standardRejected3: Unit = ${ rejectedImpl(3, false) }
  inline def standardRejected4: Unit = ${ rejectedImpl(4, false) }
  inline def standardRejected5: Unit = ${ rejectedImpl(5, false) }
  inline def standardRejected6: Unit = ${ rejectedImpl(6, false) }
  inline def standardRejected7: Unit = ${ rejectedImpl(7, false) }
  inline def standardRejected8: Unit = ${ rejectedImpl(8, false) }
  inline def standardRejected9: Unit = ${ rejectedImpl(9, false) }
  inline def standardContextual: Unit = ${ rejectedImpl(10, false) }

  inline def optInRejected0: Unit = ${ rejectedImpl(0, true) }
  inline def optInRejected1: Unit = ${ rejectedImpl(1, true) }
  inline def optInRejected2: Unit = ${ rejectedImpl(2, true) }
  inline def optInRejected3: Unit = ${ rejectedImpl(3, true) }
  inline def optInRejected4: Unit = ${ rejectedImpl(4, true) }
  inline def optInRejected5: Unit = ${ rejectedImpl(5, true) }
  inline def optInRejected6: Unit = ${ rejectedImpl(6, true) }
  inline def optInRejected7: Unit = ${ rejectedImpl(7, true) }
  inline def optInRejected8: Unit = ${ rejectedImpl(8, true) }
  inline def optInRejected9: Unit = ${ rejectedImpl(9, true) }
  inline def optInContextual: Unit = ${ rejectedImpl(10, true) }

  private def rejectedImpl(scenario: Int, optIn: Boolean)(using q: Quotes): Expr[Unit] =
    import q.reflect.*

    val scalar = Expr(1).asTerm
    val sequence = TermSequenceSplices.termSplice(Seq(scalar))
    val nullCarrier = null.asInstanceOf[TermSequenceSplice[Term]]
    val nullElement = TermSequenceSplices.termSplice(Seq(null.asInstanceOf[Term]))

    def standard(): Term =
      import quasiquotes.construct.Quasiquotes.qr
      scenario match
        case 0 => qr"external.consumer.Q048TermSequenceNegativeTargets.one($sequence)"
        case 1 => qr"external.consumer.Q048TermSequenceNegativeTargets.one(..$scalar)"
        case 2 => qr"..$sequence"
        case 3 => qr"external.consumer.Q048TermSequenceNegativeTargets.two(..$sequence, ..$sequence)"
        case 4 => qr"external.consumer.Q048TermSequenceNegativeTargets.one(...$sequence)"
        case 5 => qr"external.consumer.Q048TermSequenceNegativeTargets.one(. .$sequence)"
        case 6 => qr"external.consumer.Q048TermSequenceNegativeTargets.curried(1)(..$sequence)"
        case 7 => qr"external.consumer.Q048TermSequenceNegativeTargets.one(..$nullCarrier)"
        case 8 => qr"external.consumer.Q048TermSequenceNegativeTargets.one(..$nullElement)"
        case 9 => qr"external.consumer.Q048TermSequenceNegativeTargets.named(first = ..$sequence)"
        case 10 => qr"external.consumer.Q048TermSequenceNegativeTargets.contextual(using ..$sequence)"
        case _ => report.errorAndAbort("Unknown Q048 standard negative scenario")

    def optInBuild(): Term =
      import quasiquotes.scalameta.ScalametaQuasiquotes.qr
      scenario match
        case 0 => qr"external.consumer.Q048TermSequenceNegativeTargets.one($sequence)"
        case 1 => qr"external.consumer.Q048TermSequenceNegativeTargets.one(..$scalar)"
        case 2 => qr"..$sequence"
        case 3 => qr"external.consumer.Q048TermSequenceNegativeTargets.two(..$sequence, ..$sequence)"
        case 4 => qr"external.consumer.Q048TermSequenceNegativeTargets.one(...$sequence)"
        case 5 => qr"external.consumer.Q048TermSequenceNegativeTargets.one(. .$sequence)"
        case 6 => qr"external.consumer.Q048TermSequenceNegativeTargets.curried(1)(..$sequence)"
        case 7 => qr"external.consumer.Q048TermSequenceNegativeTargets.one(..$nullCarrier)"
        case 8 => qr"external.consumer.Q048TermSequenceNegativeTargets.one(..$nullElement)"
        case 9 => qr"external.consumer.Q048TermSequenceNegativeTargets.named(first = ..$sequence)"
        case 10 => qr"external.consumer.Q048TermSequenceNegativeTargets.contextual(using ..$sequence)"
        case _ => report.errorAndAbort("Unknown Q048 opt-in negative scenario")

    if optIn then optInBuild() else standard()
    '{ () }
