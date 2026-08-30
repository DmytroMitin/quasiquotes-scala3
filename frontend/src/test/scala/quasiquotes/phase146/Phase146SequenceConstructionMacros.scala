package quasiquotes.phase146

import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*
import quasiquotes.construct.TermSequenceSplices.termSplice

private object Phase146FixedApplyTargets:
  def empty(): List[Int] = Nil
  def one(first: Int): List[Int] = List(first)
  def four(first: Int, second: Int, third: Int, fourth: Int): List[Int] =
    List(first, second, third, fourth)
  def six(first: Int, second: Int, third: Int, fourth: Int, fifth: Int, sixth: Int): List[Int] =
    List(first, second, third, fourth, fifth, sixth)

final class Phase146EmptyBox():
  def ordered: List[Int] = Nil

final class Phase146OneBox(first: Int):
  def ordered: List[Int] = List(first)

final class Phase146FourBox(first: Int, second: Int, third: Int, fourth: Int):
  def ordered: List[Int] = List(first, second, third, fourth)

final class Phase146SixBox(
    first: Int,
    second: Int,
    third: Int,
    fourth: Int,
    fifth: Int,
    sixth: Int
):
  def ordered: List[Int] = List(first, second, third, fourth, fifth, sixth)

final case class Phase146SequenceConstructionEvidence(
    applyEmpty: List[Int],
    applyOne: List[Int],
    applyMany: List[Int],
    applyFixedAround: List[Int],
    newEmpty: List[Int],
    newOne: List[Int],
    newMany: List[Int],
    newFixedAround: List[Int],
    staticNewOne: List[Int],
    applyChildrenRetainObjects: Boolean,
    newChildrenRetainObjects: Boolean,
    callerLocalRetained: Boolean,
    reusedQrTermRetained: Boolean,
    ownedDefinitionBlockRetained: Boolean,
    ordinarySingleTermUnchanged: Int
)

object Phase146SequenceConstructionMacros:
  inline def evidence(inline callerLocal: Int): Phase146SequenceConstructionEvidence =
    ${ evidenceImpl('callerLocal) }

  private def evidenceImpl(
      callerLocal: Expr[Int]
  )(using q: Quotes): Expr[Phase146SequenceConstructionEvidence] =
    import q.reflect.*

    val literal = Expr(1).asTerm
    val local = callerLocal.asTerm
    val reusedQr = qr"40 + 2"
    val ownedDefinitionBlock = qr"{ def keep(x: Int): Int = x; keep($local) }"
    val sequence = List(literal, local, reusedQr, ownedDefinitionBlock)

    val empty = termSplice(Seq.empty[Term])
    val one = termSplice(sequence.take(1))
    val many = termSplice(sequence)
    val emptyConstructorType = TypeRepr.of[Phase146EmptyBox]
    val oneConstructorType = TypeRepr.of[Phase146OneBox]
    val fourConstructorType = TypeRepr.of[Phase146FourBox]
    val sixConstructorType = TypeRepr.of[Phase146SixBox]

    val applyEmptyTree = qr"quasiquotes.phase146.Phase146FixedApplyTargets.empty(..$empty)"
    val applyOneTree = qr"quasiquotes.phase146.Phase146FixedApplyTargets.one(..$one)"
    val applyManyTree = qr"quasiquotes.phase146.Phase146FixedApplyTargets.four(..$many)"
    val applyFixedAroundTree =
      qr"quasiquotes.phase146.Phase146FixedApplyTargets.six(${Expr(-1).asTerm}, ..$many, ${Expr(99).asTerm})"
    val newEmptyTree = qr"new $emptyConstructorType(..$empty)"
    val newOneTree = qr"new $oneConstructorType(..$one)"
    val newManyTree = qr"new $fourConstructorType(..$many)"
    val newFixedAroundTree =
      qr"new $sixConstructorType(${Expr(-1).asTerm}, ..$many, ${Expr(99).asTerm})"
    val staticNewOneTree = qr"new quasiquotes.phase146.Phase146OneBox(..$one)"
    val ordinarySingleTermTree =
      qr"quasiquotes.phase146.Phase146FixedApplyTargets.one($literal)"

    def directArguments(tree: Term): List[Term] = tree match
      case Apply(_, arguments) => arguments
      case other => report.errorAndAbort(s"PHASE146_EXPECTED_APPLY: ${other.show(using Printer.TreeStructure)}")

    def sameObjects(actual: List[Term], expected: List[Term]): Boolean =
      actual.size == expected.size && actual.zip(expected).forall { (left, right) =>
        left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]
      }

    def ordered(tree: Term): Expr[List[Int]] =
      Select.unique(tree, "ordered").asExprOf[List[Int]]

    val applyActual = directArguments(applyManyTree)
    val newActual = directArguments(newManyTree)

    '{
      Phase146SequenceConstructionEvidence(
        ${ applyEmptyTree.asExprOf[List[Int]] },
        ${ applyOneTree.asExprOf[List[Int]] },
        ${ applyManyTree.asExprOf[List[Int]] },
        ${ applyFixedAroundTree.asExprOf[List[Int]] },
        ${ ordered(newEmptyTree) },
        ${ ordered(newOneTree) },
        ${ ordered(newManyTree) },
        ${ ordered(newFixedAroundTree) },
        ${ ordered(staticNewOneTree) },
        ${ Expr(sameObjects(applyActual, sequence)) },
        ${ Expr(sameObjects(newActual, sequence)) },
        ${ Expr(applyActual.exists(term => term.asInstanceOf[AnyRef] eq local.asInstanceOf[AnyRef])) },
        ${ Expr(applyActual.exists(term => term.asInstanceOf[AnyRef] eq reusedQr.asInstanceOf[AnyRef])) },
        ${ Expr(applyActual.exists(term => term.asInstanceOf[AnyRef] eq ownedDefinitionBlock.asInstanceOf[AnyRef])) },
        ${ ordinarySingleTermTree.asExprOf[List[Int]] }.head
      )
    }
