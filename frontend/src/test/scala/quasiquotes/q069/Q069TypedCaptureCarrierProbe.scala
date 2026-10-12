package quasiquotes.matching

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName

object Q069CaptureCarrierTarget:
  object inner:
    def outer(value: Int): Int = value + 1

/** Test-only carrier experiment; this is not a qq macro or a public matcher. */
object Q069TypedCaptureCarrierProbe:
  inline def observation: String = ${ observe }

  private def observe(using q: Quotes): Expr[String] =
    import q.reflect.*

    val target = '{ Q069CaptureCarrierTarget.inner.outer(41) }.asTerm match
      case Inlined(_, Nil, body) => body
      case other => other
    val extractor: TermPatternProductExtractor[
      q.reflect.Term,
      (q.reflect.Term, SelectedMemberName, SelectedMemberName, q.reflect.Term)
    ] = TermPatternProductExtractorFactory.productExtractor { term =>
      term match
        case Apply(Select(Select(receiver, first), second), argument :: Nil) =>
          for
            firstName <- SelectedMemberName.from(first).toOption
            secondName <- SelectedMemberName.from(second).toOption
          yield (receiver, firstName, secondName, argument)
        case _ => None
    }

    val original = target match
      case Apply(Select(Select(receiver, _), _), argument :: Nil) => (receiver, argument)
      case _ => report.errorAndAbort("Q069 test fixture is not a two-Select unary Apply")
    val (receiver, first, second, argument) = extractor.unapply(target).getOrElse(
      report.errorAndAbort("Q069 test-only heterogeneous carrier failed")
    )
    if extractor.unapply(null.asInstanceOf[q.reflect.Term]).nonEmpty then
      report.errorAndAbort("Q069 test-only carrier accepted a null target")
    val receiverIdentity = receiver eq original._1
    val argumentIdentity = argument eq original._2
    Expr(s"${first.decoded}:${second.decoded}:$receiverIdentity:$argumentIdentity")
