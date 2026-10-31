package quasiquotes.matching

import scala.quoted.*
import scala.quoted.staging
import quasiquotes.matching.QuasiPattern.*
import quasiquotes.source.ReflectedPositionProvenance

object Q083GeneratedTargetFixture:
  object root:
    object inner:
      def outer(value: Int): Int = value

object Q083GeneratedTargetProbe:
  def observation: String =
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
    staging.withQuotes:
      observationImpl

  private def observationImpl(using q: Quotes): String =
    import q.reflect.*
    def strip(term: Term): Term = term match
      case Inlined(_, Nil, body) => strip(body)
      case other => other
    val original = strip('{ Q083GeneratedTargetFixture.root.inner.outer(1) }.asTerm)
    val (receiver, originalInner, originalMethod, arguments) = original match
      case Apply(method @ Select(inner @ Select(receiver, _), _), arguments) =>
        (receiver, inner, method, arguments)
      case _ => report.errorAndAbort("invalid Q083 generated target fixture")
    val freshInner = Select(receiver, originalInner.symbol)
    val freshMethod = Select(freshInner, originalMethod.symbol)
    val freshApply = Apply(freshMethod, arguments)
    val noSpan = List(freshInner, freshMethod, freshApply).forall(term =>
      ReflectedPositionProvenance.sourceCode(term.pos).isEmpty &&
        ReflectedPositionProvenance.usableBounds(term.pos).isEmpty)
    val result = freshApply match
      case qq"$capturedReceiver.$first.$second(..$capturedArguments)" =>
        val identity = (capturedReceiver eq receiver) &&
          capturedReceiver.symbol == receiver.symbol && capturedReceiver.symbol.owner == receiver.symbol.owner &&
          capturedArguments.size == arguments.size && capturedArguments.zip(arguments).forall((a,b) =>
            (a eq b) && a.symbol == b.symbol && (!a.symbol.exists || a.symbol.owner == b.symbol.owner))
        s"noSpan=$noSpan:${first.decoded}:${second.decoded}:${capturedArguments.size}:identity=$identity"
      case _ => s"noSpan=$noSpan:fallthrough"
    result
