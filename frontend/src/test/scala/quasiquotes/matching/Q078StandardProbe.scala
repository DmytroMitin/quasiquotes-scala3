package quasiquotes.matching

import scala.quoted.*
import quasiquotes.construct.SelectedMemberName

object Q078StandardProbe:
  inline def boundaries: List[String] = ${ boundariesImpl }
  inline def observations: List[String] = ${ observationsImpl }

  private def observationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    import Q078StandardInterpolator.*
    def strip(term: Term): Term = term match
      case Inlined(_, Nil, body) => strip(body)
      case other => other
    def expected(term: Term, count: Int): Term =
      if count == 0 then term
      else term match
        case Select(prefix, _) => expected(prefix, count - 1)
        case _ => report.errorAndAbort("Q078 malformed target")
    def publicPeel(target: Term, count: Int): Option[(Term, List[SelectedMemberName])] =
      import quasiquotes.matching.QuasiPattern.*
      if count == 0 then Some(target -> Nil)
      else target match
        case qq"$prefix.$name" =>
          publicPeel(prefix, count - 1).map((receiver, names) => receiver -> (names :+ name))
        case _ => None
    def check(receiver: Term, names: List[SelectedMemberName], target: Term, count: Int): String =
      val original = expected(target, count)
      val valid = (receiver eq original) && receiver.symbol == original.symbol &&
        receiver.symbol.owner == original.symbol.owner && names.size == count &&
        publicPeel(target, count).exists((root, selected) => (root eq receiver) && selected == names)
      s"n$count:$valid"
    val target1 = strip('{ Q077StandardChainTarget.root.alpha }.asTerm)
    val n1 = target1 match
      case q078"$receiver.$a" =>
        val typedReceiver: Term = receiver
        val typed_a: SelectedMemberName = a
        check(typedReceiver, List(a), target1, 1)
      case _ => report.errorAndAbort("Q078 n1 failed")
    val target2 = strip('{ Q077StandardChainTarget.root.alpha.beta }.asTerm)
    val n2 = target2 match
      case q078"$receiver.$a.$b" =>
        val typedReceiver: Term = receiver
        val typed_a: SelectedMemberName = a
        val typed_b: SelectedMemberName = b
        check(typedReceiver, List(a, b), target2, 2)
      case _ => report.errorAndAbort("Q078 n2 failed")
    val target3 = strip('{ Q077StandardChainTarget.root.alpha.beta.gamma }.asTerm)
    val n3 = target3 match
      case q078"$receiver.$a.$b.$c" =>
        val typedReceiver: Term = receiver
        val typed_a: SelectedMemberName = a
        val typed_b: SelectedMemberName = b
        val typed_c: SelectedMemberName = c
        check(typedReceiver, List(a, b, c), target3, 3)
      case _ => report.errorAndAbort("Q078 n3 failed")
    val target4 = strip('{ Q077StandardChainTarget.root.alpha.beta.gamma.delta }.asTerm)
    val n4 = target4 match
      case q078"$receiver.$a.$b.$c.$d" =>
        val typedReceiver: Term = receiver
        val typed_a: SelectedMemberName = a
        val typed_b: SelectedMemberName = b
        val typed_c: SelectedMemberName = c
        val typed_d: SelectedMemberName = d
        check(typedReceiver, List(a, b, c, d), target4, 4)
      case _ => report.errorAndAbort("Q078 n4 failed")
    val target8 = strip('{ Q077StandardChainTarget.root.alpha.beta.gamma.delta.epsilon.zeta.eta.theta }.asTerm)
    val n8 = target8 match
      case q078"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" =>
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
      case _ => report.errorAndAbort("Q078 n8 failed")
    Expr.ofList(List(n1,n2,n3,n4,n8).map(Expr(_)))

  private def capture(using q: Quotes)(target: q.reflect.Term): Option[(q.reflect.Term, List[SelectedMemberName])] =
    import Q078StandardInterpolator.*
    target match
      case q078"$receiver.$first.$second.$third" => Some(receiver -> List(first, second, third))
      case _ => None

  private def boundariesImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    def strip(term: Term): Term = term match
      case Inlined(_, Nil, body) => strip(body)
      case other => other
    val target = strip('{ Q077StandardChainTarget.root.alpha.beta.gamma }.asTerm)
    val direct = capture(target).getOrElse(report.errorAndAbort("Q078 boundary baseline"))
    val wrapped = capture(Inlined(None, Nil, target)).exists((receiver, names) =>
      (receiver eq direct._1) && names == direct._2)
    val long = capture(strip('{ Q077StandardChainTarget.root.alpha.beta.gamma.delta }.asTerm))
    val longerPrefix = long.exists((receiver, names) => receiver.symbol.name == "alpha" &&
      names.map(_.decoded) == List("beta", "gamma", "delta"))
    // Fixed-root anchoring is an explicit predicate over the receiver-hole result.
    val rootSymbol = strip('{ Q077StandardChainTarget.root }.asTerm).symbol
    def anchored(value: Term): Option[(Term, List[SelectedMemberName])] =
      capture(value).filter(_._1.symbol == rootSymbol)
    val exactRootAccepted = anchored(target).exists((receiver, names) =>
      (receiver eq direct._1) && names == direct._2)
    val exactRootRejected = anchored(strip('{ Q077StandardChainTarget.root.alpha.beta.gamma.delta }.asTerm)).isEmpty
    val equal = capture(strip('{ Q078ChainTarget.root.same.same.same }.asTerm))
      .exists(_._2.map(_.decoded) == List.fill(3)("same"))
    val lexical = capture(strip('{ Q077StandardChainTarget.root.lexical.`+`.`type`.`safe spaced name` }.asTerm))
      .exists(_._2.map(_.decoded) == List("+", "type", "safe spaced name"))
    val unicode = capture(strip('{ Q078ChainTarget.root.alpha.beta.naïve }.asTerm)).isEmpty
    val short = capture(strip('{ Q077StandardChainTarget.root.alpha }.asTerm)).isEmpty
    val applied = capture(strip('{ Q077StandardChainTarget.root.alpha.beta.applied(1) }.asTerm)).isEmpty
    val genericCall = strip('{ Q077StandardChainTarget.root.alpha.beta.generic[Int](1) }.asTerm)
    val typeApplyNode = genericCall match
      case Apply(node @ TypeApply(_, _), _) => node
      case other => report.errorAndAbort(s"Q078 expected actual TypeApply child: ${other.show(using Printer.TreeStructure)}")
    val typeApplied = capture(typeApplyNode).isEmpty
    // The compiler may erase named-argument syntax from an applied quote.
    // Exercise a shape-checked caller-Quotes NamedArg independently of Apply.
    val namedNode = NamedArg("value", Literal(IntConstant(1)))
    val named = (namedNode: Term) match
      case NamedArg("value", Literal(IntConstant(1))) => capture(namedNode).isEmpty
      case _ => false
    val namedApply = capture(strip('{ Q077StandardChainTarget.root.alpha.beta.applied(value = 1) }.asTerm)).isEmpty
    val contextual = capture(strip('{ Q077StandardChainTarget.root.alpha.beta.contextual(1)(using "ctx") }.asTerm)).isEmpty
    val rightAssociated = capture(strip('{ 1 :: List(2) }.asTerm)).isEmpty
    val nullRejected = capture(null.asInstanceOf[Term]).isEmpty
    Expr.ofList(List(
      "wrapped" -> wrapped, "long-suffix" -> longerPrefix, "anchored-positive" -> exactRootAccepted, "anchored-root" -> exactRootRejected,
      "equal-targets" -> equal, "lexical" -> lexical, "unicode" -> unicode, "short" -> short,
      "apply" -> applied, "typeapply" -> typeApplied, "named" -> named, "named-apply" -> namedApply, "contextual" -> contextual,
      "right-associative" -> rightAssociated, "null" -> nullRejected
    ).map((label, valid) => Expr(s"$label:$valid")))
