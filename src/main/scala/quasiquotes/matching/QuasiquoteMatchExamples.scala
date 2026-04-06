package quasiquotes.matching

import scala.quoted.*

object QuasiquoteMatchExamples:
  final case class MatchDemo(
      pattern: String,
      target: String,
      success: Boolean,
      bindings: List[String],
      detail: String
  )

  inline def summarizeMatch[A](inline pattern: String, expr: A): MatchDemo =
    ${ summarizeMatchImpl('pattern, 'expr) }

  inline def classifyInfix(expr: Int): String =
    ${ classifyInfixImpl('expr) }

  inline def classifyNested(expr: Int): String =
    ${ classifyNestedImpl('expr) }

  private def summarizeMatchImpl[A: Type](patternExpr: Expr[String], expr: Expr[A])(using Quotes): Expr[MatchDemo] =
    import quotes.reflect.*

    val patternText = patternExpr.valueOrAbort
    val target = expr.asTerm
    val targetText = target.show(using Printer.TreeStructure)

    QuasiPattern.term(patternText) match
      case Left(error) =>
        '{ MatchDemo(${ Expr(patternText) }, ${ Expr(targetText) }, false, Nil, ${ Expr(error.message) }) }
      case Right(pattern) =>
        pattern.matchTerm(target) match
          case Left(failure) =>
            '{ MatchDemo(${ Expr(patternText) }, ${ Expr(targetText) }, false, Nil, ${ Expr(failure.message) }) }
          case Right(result) =>
            val bindingStrings = result.bindings.toList.sortBy(_._1).map { (name, term) =>
              s"$$$name = ${term.show(using Printer.TreeStructure)}"
            }
            '{
              MatchDemo(
                pattern = ${ Expr(patternText) },
                target = ${ Expr(targetText) },
                success = true,
                bindings = ${ Expr.ofList(bindingStrings.map(Expr(_))) },
                detail = "matched"
              )
            }

  private def classifyInfixImpl(expr: Expr[Int])(using Quotes): Expr[String] =
    import quotes.reflect.*
    QuasiPattern.termOrThrow("$x + $y").matchTerm(expr.asTerm) match
      case Right(result) =>
        val x = result.bindings("x").show(using Printer.TreeStructure)
        val y = result.bindings("y").show(using Printer.TreeStructure)
        Expr(s"infix-match(x=$x, y=$y)")
      case Left(failure) =>
        Expr(s"no-infix-match(${failure.message})")

  private def classifyNestedImpl(expr: Expr[Int])(using Quotes): Expr[String] =
    import quotes.reflect.*
    QuasiPattern.termOrThrow("f(g($x))").matchTerm(expr.asTerm) match
      case Right(result) =>
        val x = result.bindings("x").show(using Printer.TreeStructure)
        Expr(s"nested-match(x=$x)")
      case Left(failure) =>
        Expr(s"no-nested-match(${failure.message})")
