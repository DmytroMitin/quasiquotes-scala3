package quasiquotes.matching

import scala.quoted.*
import quasiquotes.construct.SelectedMemberName

object Q079StandardProbe:
  inline def observations: List[String] = ${ observationsImpl }

  private def observationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    import Q078StandardInterpolator.*
    def strip(term: Term): Term = term match
      case Inlined(_, Nil, body) => strip(body)
      case other => other
    def original(term: Term, count: Int): Term =
      if count == 0 then term
      else term match
        case Select(prefix, _) => original(prefix, count - 1)
        case _ => report.errorAndAbort("Q079 expected nested Select target")
    def publicPeel(value: Term, count: Int): Option[(Term, List[SelectedMemberName])] =
      import quasiquotes.matching.QuasiPattern.*
      if count == 0 then Some(value -> Nil)
      else value match
        case qq"$prefix.$name" =>
          publicPeel(prefix, count - 1).map((base, names) => base -> (names :+ name))
        case _ => None
    def check(receiver: Term, names: List[SelectedMemberName], target: Term,
        expectedNames: List[String], whole: Tuple, snapshot: List[(Term, Symbol, Symbol)]): String =
      val count = expectedNames.size
      val base = original(target, count)
      val stable = (receiver eq base) && receiver.symbol == base.symbol &&
        receiver.symbol.owner == base.symbol.owner && receiver.symbol != Symbol.noSymbol
      val equivalent = publicPeel(target, count).exists((root, selected) =>
        (root eq receiver) && selected == names)
      // Snapshot every intermediate Select before and after both matchers.
      val unchanged = snapshot.zipWithIndex.forall { case ((t, symbol, owner), i) =>
        val after = original(target, i)
        (t eq after) && symbol == after.symbol && owner == after.symbol.owner
      }
      val valid = stable && equivalent && unchanged && names.map(_.decoded) == expectedNames &&
        whole.productArity == count + 1
      if !valid then report.errorAndAbort(s"Q079 n$count check failed: $names / $expectedNames")
      s"n$count:arity=${whole.productArity}:identity=$stable:public=$equivalent:names=true:owners=$unchanged"
    val target8 = strip('{ Q079ChainTarget.root.alpha.beta.gamma.`+`.`type`.`safe spaced name`.alpha.beta }.asTerm)
    val snapshot8 = (0 until 8).map(i => original(target8, i)).map(t => (t, t.symbol, t.symbol.owner)).toList
    val n8 = target8 match
      case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8" =>
        val typedReceiver: Term = receiver
        val typed1: SelectedMemberName = slot1
        val typed4: SelectedMemberName = slot4
        val typed8: SelectedMemberName = slot8
        val whole: Term *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: EmptyTuple = (receiver, slot1, slot2, slot3, slot4, slot5, slot6, slot7, slot8)
        check(typedReceiver, List(slot1, slot2, slot3, slot4, slot5, slot6, slot7, slot8), target8, List("alpha", "beta", "gamma", "+", "type", "safe spaced name", "alpha", "beta"), whole, snapshot8)
      case _ => report.errorAndAbort("Q079 n8 extractor failed")
    val target12 = strip('{ Q079ChainTarget.root.alpha.beta.gamma.`+`.`type`.`safe spaced name`.alpha.beta.gamma.`+`.`type`.`safe spaced name` }.asTerm)
    val snapshot12 = (0 until 12).map(i => original(target12, i)).map(t => (t, t.symbol, t.symbol.owner)).toList
    val n12 = target12 match
      case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12" =>
        val typedReceiver: Term = receiver
        val typed1: SelectedMemberName = slot1
        val typed6: SelectedMemberName = slot6
        val typed12: SelectedMemberName = slot12
        val whole: Term *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: EmptyTuple = (receiver, slot1, slot2, slot3, slot4, slot5, slot6, slot7, slot8, slot9, slot10, slot11, slot12)
        check(typedReceiver, List(slot1, slot2, slot3, slot4, slot5, slot6, slot7, slot8, slot9, slot10, slot11, slot12), target12, List("alpha", "beta", "gamma", "+", "type", "safe spaced name", "alpha", "beta", "gamma", "+", "type", "safe spaced name"), whole, snapshot12)
      case _ => report.errorAndAbort("Q079 n12 extractor failed")
    val target16 = strip('{ Q079ChainTarget.root.alpha.beta.gamma.`+`.`type`.`safe spaced name`.alpha.beta.gamma.`+`.`type`.`safe spaced name`.alpha.beta.gamma.`+` }.asTerm)
    val snapshot16 = (0 until 16).map(i => original(target16, i)).map(t => (t, t.symbol, t.symbol.owner)).toList
    val n16 = target16 match
      case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12.$slot13.$slot14.$slot15.$slot16" =>
        val typedReceiver: Term = receiver
        val typed1: SelectedMemberName = slot1
        val typed8: SelectedMemberName = slot8
        val typed16: SelectedMemberName = slot16
        val whole: Term *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: EmptyTuple = (receiver, slot1, slot2, slot3, slot4, slot5, slot6, slot7, slot8, slot9, slot10, slot11, slot12, slot13, slot14, slot15, slot16)
        check(typedReceiver, List(slot1, slot2, slot3, slot4, slot5, slot6, slot7, slot8, slot9, slot10, slot11, slot12, slot13, slot14, slot15, slot16), target16, List("alpha", "beta", "gamma", "+", "type", "safe spaced name", "alpha", "beta", "gamma", "+", "type", "safe spaced name", "alpha", "beta", "gamma", "+"), whole, snapshot16)
      case _ => report.errorAndAbort("Q079 n16 extractor failed")
    val target21 = strip('{ Q079ChainTarget.root.alpha.beta.gamma.`+`.`type`.`safe spaced name`.alpha.beta.gamma.`+`.`type`.`safe spaced name`.alpha.beta.gamma.`+`.`type`.`safe spaced name`.alpha.beta.gamma }.asTerm)
    val snapshot21 = (0 until 21).map(i => original(target21, i)).map(t => (t, t.symbol, t.symbol.owner)).toList
    val n21 = target21 match
      case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12.$slot13.$slot14.$slot15.$slot16.$slot17.$slot18.$slot19.$slot20.$slot21" =>
        val typedReceiver: Term = receiver
        val typed1: SelectedMemberName = slot1
        val typed11: SelectedMemberName = slot11
        val typed21: SelectedMemberName = slot21
        val whole: Term *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: EmptyTuple = (receiver, slot1, slot2, slot3, slot4, slot5, slot6, slot7, slot8, slot9, slot10, slot11, slot12, slot13, slot14, slot15, slot16, slot17, slot18, slot19, slot20, slot21)
        check(typedReceiver, List(slot1, slot2, slot3, slot4, slot5, slot6, slot7, slot8, slot9, slot10, slot11, slot12, slot13, slot14, slot15, slot16, slot17, slot18, slot19, slot20, slot21), target21, List("alpha", "beta", "gamma", "+", "type", "safe spaced name", "alpha", "beta", "gamma", "+", "type", "safe spaced name", "alpha", "beta", "gamma", "+", "type", "safe spaced name", "alpha", "beta", "gamma"), whole, snapshot21)
      case _ => report.errorAndAbort("Q079 n21 extractor failed")
    Expr.ofList(List(n8,n12,n16,n21).map(Expr(_)))
