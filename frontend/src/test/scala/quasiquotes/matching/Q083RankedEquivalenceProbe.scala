package quasiquotes.matching

import scala.quoted.*
import quasiquotes.construct.SelectedMemberName
import quasiquotes.matching.QuasiPattern.*

/** Test-only comparators never appear in the independently usable consumer. */
object Q083RankedEquivalenceProbe:
  inline def observations: List[String] = ${ observationsImpl }

  private def publicMixed(using q: Quotes)(target: q.reflect.Term): Option[
    (q.reflect.Term, SelectedMemberName, SelectedMemberName, Seq[q.reflect.Term])
  ] = target match
    case qq"$receiver.$first.$second(..$arguments)" => Some((receiver, first, second, arguments))
    case _ => None

  private def nested(using q: Quotes)(target: q.reflect.Term): Option[
    (q.reflect.Term, SelectedMemberName, SelectedMemberName, Seq[q.reflect.Term])
  ] = target match
    case qq"$prefix.$second(..$arguments)" => prefix match
      case qq"$receiver.$first" => Some((receiver, first, second, arguments))
      case _ => None
    case _ => None

  private def observationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    def strip(term: Term): Term = term match
      case Inlined(_, Nil, body) => strip(body)
      case other => other
    val plan = Q076StandardMixedModel.plan().fold(report.errorAndAbort(_), identity)
    val result = List(
      "zero" -> '{ Q076StandardMixedTarget.receiver.first.second() }.asTerm,
      "one" -> '{ Q076StandardMixedTarget.receiver.first.second(1) }.asTerm,
      "many" -> '{ Q076StandardMixedTarget.receiver.first.second(1, 2, 3) }.asTerm
    ).map { (label, raw) =>
      val target = strip(raw)
      val actual = publicMixed(target).getOrElse(report.errorAndAbort(s"Q083 missing $label"))
      val recursive = nested(target).getOrElse(report.errorAndAbort(s"nested public missing $label"))
      val model = Q076StandardMixedModel.matchTarget(plan, target).getOrElse(report.errorAndAbort(s"Q076 missing $label"))
      def same(other: (Term, SelectedMemberName, SelectedMemberName, Seq[Term])): Boolean =
        (actual._1 eq other._1) && actual._2 == other._2 && actual._3 == other._3 &&
          actual._4.size == other._4.size && actual._4.zip(other._4).forall((a,b) => a eq b)
      s"$label:nested=${same(recursive)}:model=${same(model)}"
    }
    Expr.ofList(result.map(Expr(_)))
