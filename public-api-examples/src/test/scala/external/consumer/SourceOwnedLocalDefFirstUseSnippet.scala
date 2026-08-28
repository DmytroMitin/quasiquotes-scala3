package external.consumer

// snippet:source-owned-local-def-first-use:start
import scala.quoted.*

import quasiquotes.construct.Quasiquotes.{dqr, qr}

object SourceOwnedLocalDefFirstUseSnippet:
  inline def manual(inline value: Int): Int = ${ manualImpl('value) }

  inline def sourceOwned(inline value: Int): Int = ${ sourceOwnedImpl('value) }

  private def manualImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    val parameterType = TypeRepr.of[Int]
    val resultType = TypeRepr.of[Int]
    val definition: DefDef =
      dqr"def boundedIdentity(value: $parameterType): $resultType = value"

    Block(
      List(definition),
      Apply(Ref(definition.symbol), List(value.asTerm))
    ).asExprOf[Int]

  private def sourceOwnedImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    val parameterType = TypeRepr.of[Int]
    val resultType = TypeRepr.of[Int]

    qr"""{
      def boundedIdentity(value: $parameterType): $resultType = value
      boundedIdentity(${value.asTerm})
    }""".asExprOf[Int]
// snippet:source-owned-local-def-first-use:end
