package quasiquotes.q001

import scala.quoted.*

object Q001SequenceExtractorTypingProbe:
  inline def captureTypesAndValues(expression: Int): (Int, List[Int]) =
    ${ captureTypesAndValuesImpl('expression) }

  private def captureTypesAndValuesImpl(
      expression: Expr[Int]
  )(using q: Quotes): Expr[(Int, List[Int])] =
    import q.reflect.*
    import Q001RankAwareProbe.*

    expression.asTerm match
      case qq"$fun(..$args)" =>
        val _: q.reflect.Term = fun
        val _: Seq[q.reflect.Term] = args
        val argumentExpressions = args.map(_.asExprOf[Int]).toList
        '{
          (
            ${ Expr(fun.show.length) },
            List(${ Varargs(argumentExpressions) }*)
          )
        }
      case _ => '{ (-1, Nil) }

  inline def scalarCaptureTypesAndValues(expression: Int): (Int, Int) =
    ${ scalarCaptureTypesAndValuesImpl('expression) }

  private def scalarCaptureTypesAndValuesImpl(
      expression: Expr[Int]
  )(using q: Quotes): Expr[(Int, Int)] =
    import q.reflect.*
    import Q001RankAwareProbe.*

    expression.asTerm match
      case qq"$lhs + $rhs" =>
        val _: q.reflect.Term = lhs
        val _: q.reflect.Term = rhs
        '{ (${ lhs.asExprOf[Int] }, ${ rhs.asExprOf[Int] }) }
      case _ => '{ (-1, -1) }
