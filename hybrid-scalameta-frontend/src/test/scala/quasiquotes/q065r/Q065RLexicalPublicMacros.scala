package quasiquotes.q065r

import scala.quoted.*

import _root_.quasiquotes.scalameta.ScalametaQuasiPattern.qq

object Q065RLexicalPublicMacros:
  inline def underscorePatternMatches(inline expression: Int): Boolean =
    ${ underscorePatternMatchesImpl('expression) }

  inline def ordinaryPatternMatchesUnicodeTarget(inline expression: Int): Boolean =
    ${ ordinaryPatternMatchesUnicodeTargetImpl('expression) }

  private def underscorePatternMatchesImpl(expression: Expr[Int])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    expression.asTerm match
      case qq"""{ def _id(_value: Int): Int = _value; _id($argument) }""" => Expr(true)
      case _ => Expr(false)

  private def ordinaryPatternMatchesUnicodeTargetImpl(expression: Expr[Int])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    expression.asTerm match
      case qq"""{ def id(value: Int): Int = value; id($argument) }""" => Expr(true)
      case _ => Expr(false)
