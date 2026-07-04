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

  final case class CanonicalDemo(
      target: String,
      success: Boolean,
      canonical: String,
      detail: String
  )

  final case class EqualityComparisonDemo(
      normalizedEqual: Boolean,
      canonicalEqual: Boolean,
      leftNormalized: String,
      rightNormalized: String,
      leftCanonical: String,
      rightCanonical: String,
      detail: String
  )

  inline def summarizeCanonical[A](expr: A): CanonicalDemo =
    ${ summarizeCanonicalImpl('expr) }

  inline def compareEquality[A, B](left: A, right: B): EqualityComparisonDemo =
    ${ compareEqualityImpl('left, 'right) }

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

  private def summarizeCanonicalImpl[A: Type](expr: Expr[A])(using Quotes): Expr[CanonicalDemo] =
    import quotes.reflect.*
    val target = expr.asTerm
    val targetText = target.show(using Printer.TreeStructure)
    TermCanonicalizer.canonicalize(target) match
      case Right(canonical) =>
        '{
          CanonicalDemo(
            target = ${ Expr(targetText) },
            success = true,
            canonical = ${ Expr(canonical.render) },
            detail = "canonicalized"
          )
        }
      case Left(failure) =>
        '{
          CanonicalDemo(
            target = ${ Expr(targetText) },
            success = false,
            canonical = "",
            detail = ${ Expr(failure.message) }
          )
        }

  private def compareEqualityImpl[A: Type, B: Type](left: Expr[A], right: Expr[B])(using Quotes): Expr[EqualityComparisonDemo] =
    import quotes.reflect.*
    val leftTerm = left.asTerm
    val rightTerm = right.asTerm

    val leftNormalized = normalizedStructure(leftTerm)
    val rightNormalized = normalizedStructure(rightTerm)
    val leftCanonical = canonicalStructure(leftTerm)
    val rightCanonical = canonicalStructure(rightTerm)

    val normalizedEqual = leftNormalized.toOption == rightNormalized.toOption && leftNormalized.isRight
    val canonicalEqual = leftCanonical.toOption == rightCanonical.toOption && leftCanonical.isRight
    val detail =
      List(leftNormalized.left.toOption, rightNormalized.left.toOption, leftCanonical.left.toOption, rightCanonical.left.toOption).flatten match
        case Nil => "compared"
        case failures => failures.mkString("; ")

    '{
      EqualityComparisonDemo(
        normalizedEqual = ${ Expr(normalizedEqual) },
        canonicalEqual = ${ Expr(canonicalEqual) },
        leftNormalized = ${ Expr(leftNormalized.fold(identity, identity)) },
        rightNormalized = ${ Expr(rightNormalized.fold(identity, identity)) },
        leftCanonical = ${ Expr(leftCanonical.fold(identity, identity)) },
        rightCanonical = ${ Expr(rightCanonical.fold(identity, identity)) },
        detail = ${ Expr(detail) }
      )
    }

  private def normalizedStructure(using Quotes)(term: quotes.reflect.Term): Either[String, String] =
    MatchNormalizer.normalizedView(term).map(_.render).left.map(_.message)

  private def canonicalStructure(using Quotes)(term: quotes.reflect.Term): Either[String, String] =
    TermCanonicalizer.canonicalize(term).map(_.render).left.map(_.message)

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
