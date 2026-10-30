package quasiquotes.matching

import scala.quoted.*
import quasiquotes.construct.SelectedMemberName

object Q081ShellTarget:
  object root:
    object first:
      object second:
        def third(): Int = 1
        def curried(a: Int)(b: Int): Int = a + b

object Q081PublicChainBoundaryProbe:
  inline def boundaries: List[String] = ${ boundariesImpl }
  private def capture(using q: Quotes)(target: q.reflect.Term): Option[(q.reflect.Term, List[SelectedMemberName])] =
    import quasiquotes.scalameta.ScalametaQuasiPattern.*
    target match
      case qq"$receiver.$first.$second.$third" => Some(receiver -> List(first, second, third))
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
    val nullary = capture(strip('{ Q081ShellTarget.root.first.second.third() }.asTerm)).isEmpty
    val curried = capture(strip('{ Q081ShellTarget.root.first.second.curried(1)(2) }.asTerm)).isEmpty
    val nullRejected = capture(null.asInstanceOf[Term]).isEmpty
    Expr.ofList(List(
      "wrapped" -> wrapped, "long-suffix" -> longerPrefix, "anchored-positive" -> exactRootAccepted, "anchored-root" -> exactRootRejected,
      "equal-targets" -> equal, "lexical" -> lexical, "unicode" -> unicode, "short" -> short,
      "apply" -> applied, "typeapply" -> typeApplied, "named" -> named, "named-apply" -> namedApply, "contextual" -> contextual,
      "right-associative" -> rightAssociated, "nullary" -> nullary, "curried" -> curried, "null" -> nullRejected
    ).map((label, valid) => Expr(s"$label:$valid")))
