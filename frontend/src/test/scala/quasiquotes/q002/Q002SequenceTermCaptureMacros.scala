package quasiquotes.q002

import scala.quoted.*

object Q002SequenceTermCaptureMacros:
  inline def captureArguments(inline expression: Any): List[Int] =
    ${ captureArgumentsImpl('expression) }

  private def captureArgumentsImpl(
      expression: Expr[Any]
  )(using q: Quotes): Expr[List[Int]] =
    import q.reflect.*
    import quasiquotes.matching.QuasiPattern.*

    expression.asTerm match
      case qq"$function(..$arguments)" =>
        val _: q.reflect.Term = function
        val _: Seq[q.reflect.Term] = arguments
        Expr.ofList(arguments.toList.map(_.asExprOf[Int]))
      case _ => '{ Nil }

  inline def captureTail(inline expression: Any): (Int, List[Int]) =
    ${ captureTailImpl('expression) }

  inline def captureInit(inline expression: Any): (List[Int], Int) =
    ${ captureInitImpl('expression) }

  inline def captureMiddle(inline expression: Any): Option[(Int, List[Int], Int)] =
    ${ captureMiddleImpl('expression) }

  inline def fixedEndsMatch(inline expression: Any): Boolean =
    ${ fixedEndsMatchImpl('expression) }

  inline def scalarCapture(inline expression: Int): (Int, Int) =
    ${ scalarCaptureImpl('expression) }

  private def captureTailImpl(
      expression: Expr[Any]
  )(using q: Quotes): Expr[(Int, List[Int])] =
    import q.reflect.*
    import quasiquotes.matching.QuasiPattern.*

    expression.asTerm match
      case qq"$function($head, ..$tail)" =>
        val _: q.reflect.Term = function
        val _: q.reflect.Term = head
        val _: Seq[q.reflect.Term] = tail
        '{ (${ head.asExprOf[Int] }, ${ Expr.ofList(tail.toList.map(_.asExprOf[Int])) }) }
      case _ => '{ (-1, Nil) }

  private def captureInitImpl(
      expression: Expr[Any]
  )(using q: Quotes): Expr[(List[Int], Int)] =
    import q.reflect.*
    import quasiquotes.matching.QuasiPattern.*

    expression.asTerm match
      case qq"$function(..$init, $last)" =>
        val _: q.reflect.Term = function
        val _: Seq[q.reflect.Term] = init
        val _: q.reflect.Term = last
        '{ (${ Expr.ofList(init.toList.map(_.asExprOf[Int])) }, ${ last.asExprOf[Int] }) }
      case _ => '{ (Nil, -1) }

  private def captureMiddleImpl(
      expression: Expr[Any]
  )(using q: Quotes): Expr[Option[(Int, List[Int], Int)]] =
    import q.reflect.*
    import quasiquotes.matching.QuasiPattern.*

    expression.asTerm match
      case qq"$function($first, ..$middle, $last)" =>
        val _: q.reflect.Term = function
        val _: q.reflect.Term = first
        val _: Seq[q.reflect.Term] = middle
        val _: q.reflect.Term = last
        '{
          Some(
            (
              ${ first.asExprOf[Int] },
              ${ Expr.ofList(middle.toList.map(_.asExprOf[Int])) },
              ${ last.asExprOf[Int] }
            )
          )
        }
      case _ => '{ None }

  private def fixedEndsMatchImpl(
      expression: Expr[Any]
  )(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.matching.QuasiPattern.*

    expression.asTerm match
      case qq"$function(1, ..$middle, 9)" =>
        val _: q.reflect.Term = function
        val _: Seq[q.reflect.Term] = middle
        Expr(true)
      case _ => Expr(false)

  private def scalarCaptureImpl(
      expression: Expr[Int]
  )(using q: Quotes): Expr[(Int, Int)] =
    import q.reflect.*
    import quasiquotes.matching.QuasiPattern.*

    expression.asTerm match
      case qq"$left + $right" =>
        val _: q.reflect.Term = left
        val _: q.reflect.Term = right
        '{ (${ left.asExprOf[Int] }, ${ right.asExprOf[Int] }) }
      case _ => '{ (-1, -1) }
