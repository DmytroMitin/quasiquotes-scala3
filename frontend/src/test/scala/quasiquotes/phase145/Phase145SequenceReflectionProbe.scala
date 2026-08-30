package quasiquotes.phase145

import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*

private object Phase145FixedApplyTargets:
  def empty(): List[Int] = Nil
  def one(first: Int): List[Int] = List(first)
  def four(first: Int, second: Int, third: Int, fourth: Int): List[Int] =
    List(first, second, third, fourth)
  def six(first: Int, second: Int, third: Int, fourth: Int, fifth: Int, sixth: Int): List[Int] =
    List(first, second, third, fourth, fifth, sixth)

final class Phase145EmptyBox():
  def ordered: List[Int] = Nil

final class Phase145OneBox(first: Int):
  def ordered: List[Int] = List(first)

final class Phase145FourBox(first: Int, second: Int, third: Int, fourth: Int):
  def ordered: List[Int] = List(first, second, third, fourth)

final case class Phase145SequenceReflectionEvidence(
    applyEmpty: List[Int],
    applyOne: List[Int],
    applyMany: List[Int],
    fixedAround: List[Int],
    newEmpty: List[Int],
    newOne: List[Int],
    newMany: List[Int],
    applyChildrenRetainObjects: Boolean,
    newChildrenRetainObjects: Boolean,
    callerLocalRetained: Boolean,
    reusedQrTermRetained: Boolean,
    ownedDefinitionBlockRetained: Boolean
)

object Phase145SequenceReflectionProbe:
  inline def evidence(inline callerLocal: Int): Phase145SequenceReflectionEvidence =
    ${ evidenceImpl('callerLocal) }

  private def evidenceImpl(
      callerLocal: Expr[Int]
  )(using q: Quotes): Expr[Phase145SequenceReflectionEvidence] =
    import q.reflect.*

    val literal = Expr(1).asTerm
    val local = callerLocal.asTerm
    val reusedQr = qr"40 + 2"
    val ownedDefinitionBlock = qr"{ def keep(x: Int): Int = x; keep($local) }"
    val sequence = List(literal, local, reusedQr, ownedDefinitionBlock)

    def sameObjects(actual: List[Term], expected: List[Term]): Boolean =
      actual.size == expected.size && actual.zip(expected).forall { (left, right) =>
        left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]
      }

    def fixedApply(method: String, arguments: List[Term]): Term =
      Apply(
        Select.unique(Ref(Symbol.requiredModule("quasiquotes.phase145.Phase145FixedApplyTargets")), method),
        arguments
      )

    def newBox(constructorType: TypeRepr, arguments: List[Term]): Term =
      Select.overloaded(
        New(Inferred(constructorType)),
        "<init>",
        Nil,
        arguments
      )

    def directArguments(tree: Term): List[Term] = tree match
      case Apply(_, arguments) => arguments
      case other => report.errorAndAbort(s"PHASE145_EXPECTED_APPLY: ${other.show(using Printer.TreeStructure)}")

    def ordered(box: Term): Expr[List[Int]] =
      Select.unique(box, "ordered").asExprOf[List[Int]]

    val applyEmptyTree = fixedApply("empty", Nil)
    val applyOneTree = fixedApply("one", sequence.take(1))
    val applyManyTree = fixedApply("four", sequence)
    val fixedAroundTree = fixedApply("six", Expr(-1).asTerm :: sequence ::: List(Expr(99).asTerm))
    val newEmptyTree = newBox(TypeRepr.of[Phase145EmptyBox], Nil)
    val newOneTree = newBox(TypeRepr.of[Phase145OneBox], sequence.take(1))
    val newManyTree = newBox(TypeRepr.of[Phase145FourBox], sequence)
    val applyActual = directArguments(applyManyTree)
    val newActual = directArguments(newManyTree)

    val callerLocalRetained =
      applyActual.exists(term => term.asInstanceOf[AnyRef] eq local.asInstanceOf[AnyRef])
    val reusedQrTermRetained =
      applyActual.exists(term => term.asInstanceOf[AnyRef] eq reusedQr.asInstanceOf[AnyRef])
    val ownedDefinitionBlockRetained =
      applyActual.exists(term => term.asInstanceOf[AnyRef] eq ownedDefinitionBlock.asInstanceOf[AnyRef])

    '{
      Phase145SequenceReflectionEvidence(
        ${ applyEmptyTree.asExprOf[List[Int]] },
        ${ applyOneTree.asExprOf[List[Int]] },
        ${ applyManyTree.asExprOf[List[Int]] },
        ${ fixedAroundTree.asExprOf[List[Int]] },
        ${ ordered(newEmptyTree) },
        ${ ordered(newOneTree) },
        ${ ordered(newManyTree) },
        ${ Expr(sameObjects(applyActual, sequence)) },
        ${ Expr(sameObjects(newActual, sequence)) },
        ${ Expr(callerLocalRetained) },
        ${ Expr(reusedQrTermRetained) },
        ${ Expr(ownedDefinitionBlockRetained) }
      )
    }
