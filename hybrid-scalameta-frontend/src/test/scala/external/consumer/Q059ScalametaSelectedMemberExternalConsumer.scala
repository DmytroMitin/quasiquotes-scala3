package external.consumer

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName

private object Q059ExternalTarget:
  val field: Int = 1
  def nullary(): Int = 2
  def ordinary(value: Int): Int = value + 1

object Q059ScalametaSelectedMemberExternalConsumer:
  inline def directObservations: List[String] = ${ directObservationsImpl }
  inline def umbrellaObservations: List[String] = ${ umbrellaObservationsImpl }

  private def directObservationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq

    val target = '{ Q059ExternalTarget.ordinary(1) }.asTerm
    target match
      case qq"$receiver.$selectedName($argument)" =>
        val typedReceiver: Term = receiver
        val typedName: SelectedMemberName = selectedName
        val typedArgument: Term = argument
        Expr(List(typedName.decoded, typedArgument.show))
      case _ => report.errorAndAbort("external direct Q059 pattern did not match")

  private def umbrellaObservationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    import quasiquotes.scalameta.Quasiquotes.qq

    val directTarget = '{ Q059ExternalTarget.field }.asTerm
    val nullaryTarget = '{ Q059ExternalTarget.nullary() }.asTerm
    val directName = directTarget match
      case qq"$receiver.$selectedName" =>
        val typedReceiver: Term = receiver
        val typedName: SelectedMemberName = selectedName
        typedName.decoded
      case _ => report.errorAndAbort("external umbrella direct Q059 pattern did not match")
    val nullaryName = nullaryTarget match
      case qq"$receiver.$selectedName()" =>
        val typedReceiver: Term = receiver
        val typedName: SelectedMemberName = selectedName
        typedName.decoded
      case _ => report.errorAndAbort("external umbrella nullary Q059 pattern did not match")
    Expr(List(directName, nullaryName))
