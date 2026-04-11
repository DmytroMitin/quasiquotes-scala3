package quasiquotes.matching

import scala.quoted.*

object QuasiquoteMatchExamples:
  final case class MatchDemo(
      mode: String,
      pattern: String,
      target: String,
      success: Boolean,
      bindings: List[String],
      detail: String
  )

  inline def summarizeMatch[A](inline pattern: String, expr: A): MatchDemo =
    ${ summarizeMatchImpl('pattern, 'expr, useNormalization = true) }

  inline def summarizeMatchRaw[A](inline pattern: String, expr: A): MatchDemo =
    ${ summarizeMatchImpl('pattern, 'expr, useNormalization = false) }

  inline def summarizeMatchNormalized[A](inline pattern: String, expr: A): MatchDemo =
    ${ summarizeMatchImpl('pattern, 'expr, useNormalization = true) }

  inline def summarizeNormalization[A](inline pattern: String, expr: A): NormalizationDemo =
    ${ summarizeNormalizationImpl('pattern, 'expr) }

  inline def classifyInfix(expr: Int): String =
    ${ classifyInfixImpl('expr) }

  inline def classifyInfixRaw(expr: Int): String =
    ${ classifyInfixRawImpl('expr) }

  inline def classifyNested(expr: Int): String =
    ${ classifyNestedImpl('expr) }

  inline def classifyRepeatedOperand(expr: Int): String =
    ${ classifyRepeatedOperandImpl('expr) }

  final case class NormalizationDemo(
      pattern: String,
      target: String,
      before: MatchDemo,
      after: MatchDemo
  )

  private def summarizeMatchImpl[A: Type](patternExpr: Expr[String], expr: Expr[A], useNormalization: Boolean)(using Quotes): Expr[MatchDemo] =
    import quotes.reflect.*

    val patternText = patternExpr.valueOrAbort
    val target = expr.asTerm
    val targetText = target.show(using Printer.TreeStructure)
    val mode = if useNormalization then "after normalization" else "before normalization"

    QuasiPattern.term(patternText) match
      case Left(error) =>
        '{ MatchDemo(${ Expr(mode) }, ${ Expr(patternText) }, ${ Expr(targetText) }, false, Nil, ${ Expr(error.message) }) }
      case Right(pattern) =>
        val result =
          if useNormalization then pattern.matchTerm(target)
          else pattern.matchTermRaw(target)
        result match
          case Left(failure) =>
            '{ MatchDemo(${ Expr(mode) }, ${ Expr(patternText) }, ${ Expr(targetText) }, false, Nil, ${ Expr(failure.message) }) }
          case Right(result) =>
            val bindingStrings = result.bindings.toList.sortBy(_._1).map { (name, term) =>
              s"$$$name = ${term.show(using Printer.TreeStructure)}"
            }
            '{
              MatchDemo(
                mode = ${ Expr(mode) },
                pattern = ${ Expr(patternText) },
                target = ${ Expr(targetText) },
                success = true,
                bindings = ${ Expr.ofList(bindingStrings.map(Expr(_))) },
                detail = "matched"
              )
            }

  private def summarizeNormalizationImpl[A: Type](patternExpr: Expr[String], expr: Expr[A])(using Quotes): Expr[NormalizationDemo] =
    import quotes.reflect.*
    '{
      NormalizationDemo(
        pattern = $patternExpr,
        target = ${ Expr(expr.asTerm.show(using Printer.TreeStructure)) },
        before = ${ summarizeMatchImpl(patternExpr, expr, useNormalization = false) },
        after = ${ summarizeMatchImpl(patternExpr, expr, useNormalization = true) }
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

  private def classifyInfixRawImpl(expr: Expr[Int])(using Quotes): Expr[String] =
    import quotes.reflect.*
    QuasiPattern.termOrThrow("$x + $y").matchTermRaw(expr.asTerm) match
      case Right(result) =>
        val x = result.bindings("x").show(using Printer.TreeStructure)
        val y = result.bindings("y").show(using Printer.TreeStructure)
        Expr(s"raw-infix-match(x=$x, y=$y)")
      case Left(failure) =>
        Expr(s"raw-no-infix-match(${failure.message})")

  private def classifyNestedImpl(expr: Expr[Int])(using Quotes): Expr[String] =
    import quotes.reflect.*
    QuasiPattern.termOrThrow("f(g($x))").matchTerm(expr.asTerm) match
      case Right(result) =>
        val x = result.bindings("x").show(using Printer.TreeStructure)
        Expr(s"nested-match(x=$x)")
      case Left(failure) =>
        Expr(s"no-nested-match(${failure.message})")

  private def classifyRepeatedOperandImpl(expr: Expr[Int])(using Quotes): Expr[String] =
    import quotes.reflect.*
    QuasiPattern.termOrThrow("$x + $x").matchTerm(expr.asTerm) match
      case Right(result) =>
        val x = result.bindings("x").show(using Printer.TreeStructure)
        Expr(s"duplicated-operand(x=$x)")
      case Left(failure) =>
        Expr(s"not-duplicated(${failure.message})")
