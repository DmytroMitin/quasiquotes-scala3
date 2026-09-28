package external.consumer

import scala.quoted.*

import quasiquotes.construct.TermSequenceSplices.termSplice

object Q048TermSequenceTargets:
  def empty(): List[Int] = Nil
  def one(first: Int): List[Int] = List(first)
  def four(first: Int, second: Int, third: Int, fourth: Int): List[Int] =
    List(first, second, third, fourth)
  def five(first: Int, second: Int, third: Int, fourth: Int, fifth: Int): List[Int] =
    List(first, second, third, fourth, fifth)
  def six(first: Int, second: Int, third: Int, fourth: Int, fifth: Int, sixth: Int): List[Int] =
    List(first, second, third, fourth, fifth, sixth)

final class Q048EmptyBox():
  def ordered: List[Int] = Nil

final class Q048OneBox(first: Int):
  def ordered: List[Int] = List(first)

final class Q048FourBox(first: Int, second: Int, third: Int, fourth: Int):
  def ordered: List[Int] = List(first, second, third, fourth)

final class Q048SixBox(
    first: Int,
    second: Int,
    third: Int,
    fourth: Int,
    fifth: Int,
    sixth: Int
):
  def ordered: List[Int] = List(first, second, third, fourth, fifth, sixth)

object Q048TermSequenceParityMacros:
  inline def direct(inline callerLocal: Int): Boolean = ${ directImpl('callerLocal) }
  inline def umbrella(inline callerLocal: Int): Boolean = ${ umbrellaImpl('callerLocal) }

  private def directImpl(callerLocal: Expr[Int])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    val literal = Expr(1).asTerm
    val local = callerLocal.asTerm
    val earlierQr =
      import quasiquotes.construct.Quasiquotes.qr
      qr"40 + 2"
    val ownedDefinitionBlock =
      import quasiquotes.construct.Quasiquotes.qr
      qr"{ def keep(x: Int): Int = x; keep($local) }"
    val sequenceTerms = List(literal, local, earlierQr, ownedDefinitionBlock)
    val empty = termSplice(Seq.empty[Term])
    val one = termSplice(sequenceTerms.take(1))
    val many = termSplice(sequenceTerms)
    val fixedBefore = Expr(-1).asTerm
    val fixedAfter = Expr(99).asTerm
    val emptyConstructor = TypeRepr.of[Q048EmptyBox]
    val oneConstructor = TypeRepr.of[Q048OneBox]
    val fourConstructor = TypeRepr.of[Q048FourBox]
    val sixConstructor = TypeRepr.of[Q048SixBox]

    val standardApplyEmpty =
      import quasiquotes.construct.Quasiquotes.qr
      qr"external.consumer.Q048TermSequenceTargets.empty(..$empty)"
    val optInApplyEmpty =
      import quasiquotes.scalameta.ScalametaQuasiquotes.qr
      qr"external.consumer.Q048TermSequenceTargets.empty(..$empty)"
    val standardApplyOne =
      import quasiquotes.construct.Quasiquotes.qr
      qr"external.consumer.Q048TermSequenceTargets.one(..$one)"
    val optInApplyOne =
      import quasiquotes.scalameta.ScalametaQuasiquotes.qr
      qr"external.consumer.Q048TermSequenceTargets.one(..$one)"
    val standardApplyMany =
      import quasiquotes.construct.Quasiquotes.qr
      qr"external.consumer.Q048TermSequenceTargets.four(..$many)"
    val optInApplyMany =
      import quasiquotes.scalameta.ScalametaQuasiquotes.qr
      qr"external.consumer.Q048TermSequenceTargets.four(..$many)"
    val standardApplyBefore =
      import quasiquotes.construct.Quasiquotes.qr
      qr"external.consumer.Q048TermSequenceTargets.five($fixedBefore, ..$many)"
    val optInApplyBefore =
      import quasiquotes.scalameta.ScalametaQuasiquotes.qr
      qr"external.consumer.Q048TermSequenceTargets.five($fixedBefore, ..$many)"
    val standardApplyAfter =
      import quasiquotes.construct.Quasiquotes.qr
      qr"external.consumer.Q048TermSequenceTargets.five(..$many, $fixedAfter)"
    val optInApplyAfter =
      import quasiquotes.scalameta.ScalametaQuasiquotes.qr
      qr"external.consumer.Q048TermSequenceTargets.five(..$many, $fixedAfter)"
    val standardApplyAround =
      import quasiquotes.construct.Quasiquotes.qr
      qr"external.consumer.Q048TermSequenceTargets.six($fixedBefore, ..$many, $fixedAfter)"
    val optInApplyAround =
      import quasiquotes.scalameta.ScalametaQuasiquotes.qr
      qr"external.consumer.Q048TermSequenceTargets.six($fixedBefore, ..$many, $fixedAfter)"

    val standardNewEmpty =
      import quasiquotes.construct.Quasiquotes.qr
      qr"new $emptyConstructor(..$empty)"
    val optInNewEmpty =
      import quasiquotes.scalameta.ScalametaQuasiquotes.qr
      qr"new $emptyConstructor(..$empty)"
    val standardNewOne =
      import quasiquotes.construct.Quasiquotes.qr
      qr"new $oneConstructor(..$one)"
    val optInNewOne =
      import quasiquotes.scalameta.ScalametaQuasiquotes.qr
      qr"new $oneConstructor(..$one)"
    val standardNewMany =
      import quasiquotes.construct.Quasiquotes.qr
      qr"new $fourConstructor(..$many)"
    val optInNewMany =
      import quasiquotes.scalameta.ScalametaQuasiquotes.qr
      qr"new $fourConstructor(..$many)"
    val standardNewAround =
      import quasiquotes.construct.Quasiquotes.qr
      qr"new $sixConstructor($fixedBefore, ..$many, $fixedAfter)"
    val optInNewAround =
      import quasiquotes.scalameta.ScalametaQuasiquotes.qr
      qr"new $sixConstructor($fixedBefore, ..$many, $fixedAfter)"

    def directArguments(term: Term): List[Term] = term match
      case Apply(_, arguments) => arguments
      case other => report.errorAndAbort(s"Q048_EXPECTED_APPLY: ${other.show(using Printer.TreeStructure)}")

    def sameObjects(actual: List[Term], expected: List[Term]): Boolean =
      actual.size == expected.size && actual.zip(expected).forall { (left, right) =>
        left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]
      }

    def sameTopology(left: Term, right: Term): Boolean =
      left.show(using Printer.TreeStructure) == right.show(using Printer.TreeStructure)

    def ordered(term: Term): Expr[List[Int]] =
      Select.unique(term, "ordered").asExprOf[List[Int]]

    val expectedMany = sequenceTerms
    val expectedBefore = fixedBefore :: sequenceTerms
    val expectedAfter = sequenceTerms :+ fixedAfter
    val expectedAround = (fixedBefore :: sequenceTerms) :+ fixedAfter
    val pairs = List(
      standardApplyEmpty -> optInApplyEmpty,
      standardApplyOne -> optInApplyOne,
      standardApplyMany -> optInApplyMany,
      standardApplyBefore -> optInApplyBefore,
      standardApplyAfter -> optInApplyAfter,
      standardApplyAround -> optInApplyAround,
      standardNewEmpty -> optInNewEmpty,
      standardNewOne -> optInNewOne,
      standardNewMany -> optInNewMany,
      standardNewAround -> optInNewAround
    )
    val identityChecks = List(
      sameObjects(directArguments(standardApplyMany), expectedMany),
      sameObjects(directArguments(optInApplyMany), expectedMany),
      sameObjects(directArguments(standardApplyBefore), expectedBefore),
      sameObjects(directArguments(optInApplyBefore), expectedBefore),
      sameObjects(directArguments(standardApplyAfter), expectedAfter),
      sameObjects(directArguments(optInApplyAfter), expectedAfter),
      sameObjects(directArguments(standardApplyAround), expectedAround),
      sameObjects(directArguments(optInApplyAround), expectedAround),
      sameObjects(directArguments(standardNewMany), expectedMany),
      sameObjects(directArguments(optInNewMany), expectedMany),
      sameObjects(directArguments(standardNewAround), expectedAround),
      sameObjects(directArguments(optInNewAround), expectedAround)
    )
    val compileTimeParity = pairs.forall((left, right) => sameTopology(left, right)) &&
      identityChecks.forall(identity)

    '{
      ${ Expr(compileTimeParity) } &&
      List(
        ${ standardApplyEmpty.asExprOf[List[Int]] },
        ${ standardApplyOne.asExprOf[List[Int]] },
        ${ standardApplyMany.asExprOf[List[Int]] },
        ${ standardApplyBefore.asExprOf[List[Int]] },
        ${ standardApplyAfter.asExprOf[List[Int]] },
        ${ standardApplyAround.asExprOf[List[Int]] },
        ${ ordered(standardNewEmpty) },
        ${ ordered(standardNewOne) },
        ${ ordered(standardNewMany) },
        ${ ordered(standardNewAround) }
      ) == List(
        ${ optInApplyEmpty.asExprOf[List[Int]] },
        ${ optInApplyOne.asExprOf[List[Int]] },
        ${ optInApplyMany.asExprOf[List[Int]] },
        ${ optInApplyBefore.asExprOf[List[Int]] },
        ${ optInApplyAfter.asExprOf[List[Int]] },
        ${ optInApplyAround.asExprOf[List[Int]] },
        ${ ordered(optInNewEmpty) },
        ${ ordered(optInNewOne) },
        ${ ordered(optInNewMany) },
        ${ ordered(optInNewAround) }
      )
    }

  private def umbrellaImpl(callerLocal: Expr[Int])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.Quasiquotes.qr

    val arguments = List(Expr(1).asTerm, callerLocal.asTerm)
    val splice = termSplice(arguments)
    val result: Term = qr"external.consumer.Q048TermSequenceTargets.four(${Expr(-1).asTerm}, ..$splice, ${Expr(99).asTerm})"
    result match
      case Apply(_, actual) =>
        val expected = (Expr(-1).asTerm :: arguments) :+ Expr(99).asTerm
        val identities = actual.size == expected.size && actual.zip(expected).forall { (left, right) =>
          left.show == right.show
        }
        '{ ${ result.asExprOf[List[Int]] } == List(-1, 1, $callerLocal, 99) && ${ Expr(identities) } }
      case other => report.errorAndAbort(s"Q048_EXPECTED_UMBRELLA_APPLY: ${other.show}")
