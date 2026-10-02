package external.consumer

import scala.quoted.*

private object Q050ScalametaQqExternalTargets:
  def three(first: Int, second: Int, third: Int): List[Int] = List(first, second, third)

object Q050ScalametaQqExternalConsumerMacros:
  inline def direct(inline callerLocal: Int): List[Int] =
    ${ directImpl('callerLocal) }

  inline def umbrella(inline callerLocal: Int): List[Int] =
    ${ umbrellaImpl('callerLocal) }

  private def directImpl(callerLocal: Expr[Int])(using q: Quotes): Expr[List[Int]] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq

    '{ Q050ScalametaQqExternalTargets.three(1, $callerLocal, 3) }.asTerm match
      case qq"$function(..$arguments)" =>
        val _: q.reflect.Term = function
        val _: Seq[q.reflect.Term] = arguments
        Expr.ofList(arguments.toList.map(_.asExprOf[Int]))
      case _ => '{ Nil }

  private def umbrellaImpl(callerLocal: Expr[Int])(using q: Quotes): Expr[List[Int]] =
    import q.reflect.*
    import quasiquotes.scalameta.Quasiquotes.qq

    '{ Q050ScalametaQqExternalTargets.three(1, $callerLocal, 3) }.asTerm match
      case qq"$function(..$arguments)" =>
        val _: q.reflect.Term = function
        val _: Seq[q.reflect.Term] = arguments
        Expr.ofList(arguments.toList.map(_.asExprOf[Int]))
      case _ => '{ Nil }
