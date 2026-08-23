package external.consumer

// snippet:p2-local-val-first-use:start
import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*
import quasiquotes.matching.QuasiPattern.*

object P2LocalValFirstUseSnippet:
  inline def bind(inline initializer: Int): Int =
    ${ bindImpl('initializer) }

  inline def alphaEquivalent(inline expression: Int): Boolean =
    ${ alphaEquivalentImpl('expression) }

  inline def captureInitializer(inline expression: Int): Int =
    ${ captureInitializerImpl('expression) }

  inline def preserveExternal(inline x: Int): Int =
    ${ preserveExternalImpl('x) }

  private def bindImpl(initializer: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    qr"{ val x: Int = ${initializer.asTerm}; x }".asExprOf[Int]

  private def alphaEquivalentImpl(expression: Expr[Int])(using Quotes): Expr[Boolean] =
    import quotes.reflect.*
    expression.asTerm match
      case qq"{ val x: Int = $initializer; x }" => Expr(true)
      case _ => Expr(false)

  private def captureInitializerImpl(expression: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    expression.asTerm match
      case qq"{ val x: Int = $initializer; x }" => initializer.asExprOf[Int]
      case _ => Expr(-1)

  private def preserveExternalImpl(external: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    qr"{ val x: Int = 1; ${external.asTerm} }".asExprOf[Int]
// snippet:p2-local-val-first-use:end
