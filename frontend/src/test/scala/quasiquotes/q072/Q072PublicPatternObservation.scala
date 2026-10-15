package quasiquotes.q072

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName

/** Third compilation layer: runs the compiled public-pattern-shaped consumers
  * against real reflected terms.
  */
object Q072PublicPatternObservation:
  inline def value: String = ${ valueImpl }

  private def valueImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    def target(expression: Expr[Any]): Term =
      expression.asTerm match
        case Inlined(_, _, inner) => inner
        case other => other

    val distinct = target('{ Q072ProbeTargets.distinctReceiver.inner.outer })
    val repeated = target('{ Q072ProbeTargets.repeatedReceiver.same.same })
    val unary = target('{ Q072ProbeTargets.unaryReceiver.same.same(41) })
    val same = SelectedMemberName.from("same").toOption.get
    val inner = SelectedMemberName.from("inner").toOption.get

    val distinctResult = Q072PublicPatternConsumers.distinctDirect(distinct)
    val unaryResult = Q072PublicPatternConsumers.distinctUnary(unary)
    val guardedSame = Q072PublicPatternConsumers.guardedEquality(repeated)
    val guardedMismatch = Q072PublicPatternConsumers.guardedEquality(distinct)
    val wildcardMismatch = Q072PublicPatternConsumers.wildcardSecond(distinct)
    val constantSame = Q072PublicPatternConsumers.stableConstants(repeated, same)
    val constantMismatch = Q072PublicPatternConsumers.stableConstants(distinct, inner)
    val markerSame = Q072PublicPatternConsumers.repeatedMarker(repeated)
    val markerMismatch = Q072PublicPatternConsumers.repeatedMarker(distinct)
    val markerUnary = Q072PublicPatternConsumers.repeatedMarkerUnary(unary)
    val nullResult = Q072PublicPatternConsumers.distinctDirect(null.asInstanceOf[Term])

    val observations = Vector(
      distinctResult.exists(result => result._2.decoded == "inner" && result._3.decoded == "outer"),
      unaryResult.exists(result => result._2.decoded == "same" && result._3.decoded == "same"),
      guardedSame.nonEmpty,
      guardedMismatch.isEmpty,
      wildcardMismatch.nonEmpty,
      constantSame.nonEmpty,
      constantMismatch.isEmpty,
      markerSame.exists(_._2.decoded == "same"),
      markerMismatch.isEmpty,
      markerUnary.exists(result => result._2.decoded == "same"),
      nullResult.isEmpty
    )
    Expr(observations.mkString(":"))
