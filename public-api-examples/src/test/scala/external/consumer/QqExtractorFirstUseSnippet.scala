package external.consumer

// snippet:qq-extractor-first-use:start
import scala.quoted.*

import quasiquotes.construct.SelectedMemberName
import quasiquotes.matching.QuasiPattern.*

object QqSelectedMemberFirstUseTarget:
  def many(first: Int, second: Int, third: Int): Int = first + second + third

  object direct:
    object inner:
      val outer: Int = 42

  object unary:
    object inner:
      def outer(value: Int): Int = value + 1

object QqExtractorFirstUseSnippet:
  inline def splitAddition(expression: Int): (Int, Int) =
    ${ splitAdditionImpl('expression) }

  inline def isAddition(expression: Int): Boolean =
    ${ isAdditionImpl('expression) }

  inline def nestedMiddle(expression: Int): Int =
    ${ nestedMiddleImpl('expression) }

  inline def literalAndCapture(expression: Int): Int =
    ${ literalAndCaptureImpl('expression) }

  inline def selectedCallSummary(inline expression: Int): String =
    ${ selectedCallSummaryImpl('expression) }

  inline def twoSelectedDirectSummary: String =
    ${ twoSelectedDirectSummaryImpl }

  inline def twoSelectedUnarySummary: String =
    ${ twoSelectedUnarySummaryImpl }

  inline def malformedTemplate: Unit =
    ${ malformedTemplateImpl }

  private def splitAdditionImpl(
      expression: Expr[Int]
  )(using q: Quotes): Expr[(Int, Int)] =
    import q.reflect.*

    expression.asTerm match
      case qq"$left + $right" =>
        '{ (${ left.asExprOf[Int] }, ${ right.asExprOf[Int] }) }
      case _ => '{ (-1, -1) }

  private def isAdditionImpl(expression: Expr[Int])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    expression.asTerm match
      case qq"$left + $right" => Expr(true)
      case _ => Expr(false)

  private def nestedMiddleImpl(expression: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*

    expression.asTerm match
      case qq"($left + $middle) + $right" => middle.asExprOf[Int]
      case _ => Expr(-1)

  private def literalAndCaptureImpl(expression: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*

    expression.asTerm match
      case qq"qqCapture0 + $value" => value.asExprOf[Int]
      case _ => Expr(-1)

  private def selectedCallSummaryImpl(
      expression: Expr[Int]
  )(using q: Quotes): Expr[String] =
    import q.reflect.*

    expression.asTerm match
      case qq"$receiver.$selectedName($first, ..$middle, $last)" =>
        val _: q.reflect.Term = receiver
        val _: SelectedMemberName = selectedName
        val _: q.reflect.Term = first
        val _: Seq[q.reflect.Term] = middle
        val _: q.reflect.Term = last
        Expr(s"${selectedName.decoded}:${middle.size + 2}")
      case _ => Expr("no-match")

  private def twoSelectedDirectSummaryImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    def stripInlined(term: Term): Term = term match
      case Inlined(_, Nil, inner) => stripInlined(inner)
      case other => other

    val target = stripInlined('{ QqSelectedMemberFirstUseTarget.direct.inner.outer }.asTerm)
    target match
      case qq"$receiver.$first.$second" =>
        val _: q.reflect.Term = receiver
        val _: SelectedMemberName = first
        val _: SelectedMemberName = second
        Expr(s"${first.decoded}:${second.decoded}")
      case _ => Expr("no-match")

  private def twoSelectedUnarySummaryImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    def stripInlined(term: Term): Term = term match
      case Inlined(_, Nil, inner) => stripInlined(inner)
      case other => other

    val target = stripInlined('{ QqSelectedMemberFirstUseTarget.unary.inner.outer(41) }.asTerm)
    target match
      case qq"$receiver.$first.$second($argument)" =>
        val _: q.reflect.Term = receiver
        val _: SelectedMemberName = first
        val _: SelectedMemberName = second
        val _: q.reflect.Term = argument
        Expr(s"${first.decoded}:${second.decoded}:${argument.show}")
      case _ => Expr("no-match")

  private def malformedTemplateImpl(using q: Quotes): Expr[Unit] =
    import q.reflect.*

    Expr(1).asTerm match
      case qq"$value +" => '{ () }
      case _ => '{ () }
// snippet:qq-extractor-first-use:end
