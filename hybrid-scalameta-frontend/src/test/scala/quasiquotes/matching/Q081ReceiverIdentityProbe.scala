package quasiquotes.matching

import scala.quoted.*
import quasiquotes.construct.SelectedMemberName

object Q081ReceiverIdentityProbe:
  inline def observations: List[String] = ${ observationsImpl }

  private def capture(using q: Quotes)(target: q.reflect.Term): Option[(q.reflect.Term, List[SelectedMemberName])] =
    import quasiquotes.scalameta.ScalametaQuasiPattern.*
    target match
      case qq"$receiver.$a.$b.$c" => Some(receiver -> List(a,b,c))
      case _ => None

  private def prototype(using q: Quotes)(target: q.reflect.Term): Option[(q.reflect.Term, List[SelectedMemberName])] =
    import Q078StandardInterpolator.*
    target match
      case q078"$receiver.$a.$b.$c" => Some(receiver -> List(a,b,c))
      case _ => None

  private def observationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    def strip(term: Term): Term = term match
      case Inlined(_, Nil, body) => strip(body)
      case other => other
    val target = strip('{ Q077StandardChainTarget.root.alpha.beta.gamma }.asTerm)
    def wrapped(label: String, wrap: Term => Term): String = target match
      case outer @ Select(middle @ Select(inner @ Select(root, a), b), c) =>
        val receiver = wrap(root)
        val chain = Select.copy(outer)(Select.copy(middle)(Select.copy(inner)(receiver, a), b), c)
        val result = capture(chain)
        val valid = result.exists((actual, names) => (actual eq receiver) &&
          actual.symbol == receiver.symbol &&
          (!receiver.symbol.exists || actual.symbol.owner == receiver.symbol.owner) &&
          names.map(_.decoded) == List("alpha","beta","gamma"))
        val equivalent = for
          actual <- result
          model <- prototype(chain)
        yield (actual._1 eq model._1) && actual._2 == model._2
        s"$label:${valid && equivalent.contains(true)}"
      case _ => report.errorAndAbort("invalid wrapped receiver fixture")
    val localBody = strip('{
      val stable = Q077StandardChainTarget.root
      stable.alpha.beta.gamma
    }.asTerm)
    val local = localBody match
      case Block(List(_: ValDef), chain @ Select(Select(Select(receiver: Ident, _), _), _)) =>
        val expected = capture(chain)
        expected.exists((actual, names) => (actual eq receiver) &&
          actual.symbol == receiver.symbol &&
          (!receiver.symbol.exists || actual.symbol.owner == receiver.symbol.owner) &&
          names.map(_.decoded) == List("alpha","beta","gamma")) &&
          prototype(chain).exists((actual, names) => (actual eq receiver) &&
            expected.exists((publicReceiver, publicNames) => (publicReceiver eq actual) && publicNames == names))
      case other => report.errorAndAbort(s"invalid local receiver fixture: ${other.show(using Printer.TreeStructure)}")
    Expr.ofList(List(wrapped("inner-inlined", receiver => Inlined(None, Nil, receiver)),
      wrapped("inner-empty-block", receiver => Block(Nil, receiver)), s"local-stable:$local").map(Expr(_)))
