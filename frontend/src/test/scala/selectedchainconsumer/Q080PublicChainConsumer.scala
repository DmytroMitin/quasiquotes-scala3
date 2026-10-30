package selectedchainconsumer

import scala.quoted.*
import quasiquotes.construct.SelectedMemberName
import quasiquotes.matching.QuasiPattern.*

/** Foreign-package public source and emitted-TASTy consumer. */
object Q080PublicChainConsumer:
  object root:
    object alpha:
      object beta:
        object gamma:
          object delta:
            object epsilon:
              object zeta:
                object eta:
                  object theta

  inline def observations: List[String] = ${ observationsImpl }

  private def strip(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term match
      case Inlined(_, Nil, body) => strip(body)
      case other => other

  private def recursive(using q: Quotes)(target: q.reflect.Term, count: Int): Option[(q.reflect.Term, List[SelectedMemberName])] =
    if count == 0 then Some(target -> Nil)
    else target match
      case qq"$prefix.$name" =>
        import q.reflect.*
        strip(target) match
          case Select(originalPrefix, _) if (prefix eq originalPrefix) &&
              prefix.symbol == originalPrefix.symbol && prefix.symbol.owner == originalPrefix.symbol.owner =>
            recursive(prefix, count - 1).map((receiver, names) => receiver -> (names :+ name))
          case _ => None
      case _ => None

  private def observationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    def prefixes(term: Term, count: Int): List[Term] =
      if count == 0 then Nil
      else term match
        case Select(prefix, _) => prefix :: prefixes(prefix, count - 1)
        case _ => report.errorAndAbort("malformed chain fixture")
    def check(receiver: Term, names: List[SelectedMemberName], target: Term, count: Int): String =
      val before = prefixes(target, count)
      val symbols = before.map(t => t.symbol -> t.symbol.owner)
      val nested = recursive(target, count)
      val after = prefixes(target, count)
      val original = before.last
      val valid = (receiver eq original) && receiver.symbol == original.symbol &&
        receiver.symbol.owner == original.symbol.owner &&
        nested.exists((root, selected) => (root eq receiver) && selected == names) &&
        before.zip(after).forall((a,b) => a eq b) && symbols == after.map(t => t.symbol -> t.symbol.owner)
      if !valid then report.errorAndAbort(s"chain n$count identity mismatch")
      s"n$count:${names.map(_.decoded).mkString(",")}:identity=true:recursive=true"
    val target1 = strip('{ root.alpha }.asTerm)
    val n1 = target1 match
      case qq"$receiver.$a" =>
        val typedReceiver: Term = receiver
        val typed_a: SelectedMemberName = a
        check(typedReceiver, List(a), target1, 1)
      case _ => report.errorAndAbort("public chain n1 did not match")
    val target2 = strip('{ root.alpha.beta }.asTerm)
    val n2 = target2 match
      case qq"$receiver.$a.$b" =>
        val typedReceiver: Term = receiver
        val typed_a: SelectedMemberName = a
        val typed_b: SelectedMemberName = b
        check(typedReceiver, List(a, b), target2, 2)
      case _ => report.errorAndAbort("public chain n2 did not match")
    val target3 = strip('{ root.alpha.beta.gamma }.asTerm)
    val n3 = target3 match
      case qq"$receiver.$a.$b.$c" =>
        val typedReceiver: Term = receiver
        val typed_a: SelectedMemberName = a
        val typed_b: SelectedMemberName = b
        val typed_c: SelectedMemberName = c
        check(typedReceiver, List(a, b, c), target3, 3)
      case _ => report.errorAndAbort("public chain n3 did not match")
    val target4 = strip('{ root.alpha.beta.gamma.delta }.asTerm)
    val n4 = target4 match
      case qq"$receiver.$a.$b.$c.$d" =>
        val typedReceiver: Term = receiver
        val typed_a: SelectedMemberName = a
        val typed_b: SelectedMemberName = b
        val typed_c: SelectedMemberName = c
        val typed_d: SelectedMemberName = d
        check(typedReceiver, List(a, b, c, d), target4, 4)
      case _ => report.errorAndAbort("public chain n4 did not match")
    val target5 = strip('{ root.alpha.beta.gamma.delta.epsilon }.asTerm)
    val n5 = target5 match
      case qq"$receiver.$a.$b.$c.$d.$e" =>
        val typedReceiver: Term = receiver
        val typed_a: SelectedMemberName = a
        val typed_b: SelectedMemberName = b
        val typed_c: SelectedMemberName = c
        val typed_d: SelectedMemberName = d
        val typed_e: SelectedMemberName = e
        check(typedReceiver, List(a, b, c, d, e), target5, 5)
      case _ => report.errorAndAbort("public chain n5 did not match")
    val target6 = strip('{ root.alpha.beta.gamma.delta.epsilon.zeta }.asTerm)
    val n6 = target6 match
      case qq"$receiver.$a.$b.$c.$d.$e.$f" =>
        val typedReceiver: Term = receiver
        val typed_a: SelectedMemberName = a
        val typed_b: SelectedMemberName = b
        val typed_c: SelectedMemberName = c
        val typed_d: SelectedMemberName = d
        val typed_e: SelectedMemberName = e
        val typed_f: SelectedMemberName = f
        check(typedReceiver, List(a, b, c, d, e, f), target6, 6)
      case _ => report.errorAndAbort("public chain n6 did not match")
    val target7 = strip('{ root.alpha.beta.gamma.delta.epsilon.zeta.eta }.asTerm)
    val n7 = target7 match
      case qq"$receiver.$a.$b.$c.$d.$e.$f.$g" =>
        val typedReceiver: Term = receiver
        val typed_a: SelectedMemberName = a
        val typed_b: SelectedMemberName = b
        val typed_c: SelectedMemberName = c
        val typed_d: SelectedMemberName = d
        val typed_e: SelectedMemberName = e
        val typed_f: SelectedMemberName = f
        val typed_g: SelectedMemberName = g
        check(typedReceiver, List(a, b, c, d, e, f, g), target7, 7)
      case _ => report.errorAndAbort("public chain n7 did not match")
    val target8 = strip('{ root.alpha.beta.gamma.delta.epsilon.zeta.eta.theta }.asTerm)
    val n8 = target8 match
      case qq"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" =>
        val typedReceiver: Term = receiver
        val typed_a: SelectedMemberName = a
        val typed_b: SelectedMemberName = b
        val typed_c: SelectedMemberName = c
        val typed_d: SelectedMemberName = d
        val typed_e: SelectedMemberName = e
        val typed_f: SelectedMemberName = f
        val typed_g: SelectedMemberName = g
        val typed_h: SelectedMemberName = h
        check(typedReceiver, List(a, b, c, d, e, f, g, h), target8, 8)
      case _ => report.errorAndAbort("public chain n8 did not match")
    val prototype1 =
      import quasiquotes.matching.Q078StandardInterpolator.*
      target1 match
        case q078"$receiver.$a" => check(receiver, List(a), target1, 1)
        case _ => report.errorAndAbort("prototype n1 failed")
    if n1 != prototype1 then report.errorAndAbort("public/prototype n1 mismatch")
    val prototype2 =
      import quasiquotes.matching.Q078StandardInterpolator.*
      target2 match
        case q078"$receiver.$a.$b" => check(receiver, List(a, b), target2, 2)
        case _ => report.errorAndAbort("prototype n2 failed")
    if n2 != prototype2 then report.errorAndAbort("public/prototype n2 mismatch")
    val prototype3 =
      import quasiquotes.matching.Q078StandardInterpolator.*
      target3 match
        case q078"$receiver.$a.$b.$c" => check(receiver, List(a, b, c), target3, 3)
        case _ => report.errorAndAbort("prototype n3 failed")
    if n3 != prototype3 then report.errorAndAbort("public/prototype n3 mismatch")
    val prototype4 =
      import quasiquotes.matching.Q078StandardInterpolator.*
      target4 match
        case q078"$receiver.$a.$b.$c.$d" => check(receiver, List(a, b, c, d), target4, 4)
        case _ => report.errorAndAbort("prototype n4 failed")
    if n4 != prototype4 then report.errorAndAbort("public/prototype n4 mismatch")
    val prototype5 =
      import quasiquotes.matching.Q078StandardInterpolator.*
      target5 match
        case q078"$receiver.$a.$b.$c.$d.$e" => check(receiver, List(a, b, c, d, e), target5, 5)
        case _ => report.errorAndAbort("prototype n5 failed")
    if n5 != prototype5 then report.errorAndAbort("public/prototype n5 mismatch")
    val prototype6 =
      import quasiquotes.matching.Q078StandardInterpolator.*
      target6 match
        case q078"$receiver.$a.$b.$c.$d.$e.$f" => check(receiver, List(a, b, c, d, e, f), target6, 6)
        case _ => report.errorAndAbort("prototype n6 failed")
    if n6 != prototype6 then report.errorAndAbort("public/prototype n6 mismatch")
    val prototype7 =
      import quasiquotes.matching.Q078StandardInterpolator.*
      target7 match
        case q078"$receiver.$a.$b.$c.$d.$e.$f.$g" => check(receiver, List(a, b, c, d, e, f, g), target7, 7)
        case _ => report.errorAndAbort("prototype n7 failed")
    if n7 != prototype7 then report.errorAndAbort("public/prototype n7 mismatch")
    val prototype8 =
      import quasiquotes.matching.Q078StandardInterpolator.*
      target8 match
        case q078"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" => check(receiver, List(a, b, c, d, e, f, g, h), target8, 8)
        case _ => report.errorAndAbort("prototype n8 failed")
    if n8 != prototype8 then report.errorAndAbort("public/prototype n8 mismatch")
    Expr.ofList(List(n1,n2,n3,n4,n5,n6,n7,n8).map(Expr(_)))
