package quasiquotes.matching

import scala.quoted.*

private[quasiquotes] final case class InterpolationTypedTargetEvidence(
    sourceCode: Option[String],
    treeStructure: String,
    normalizedView: String
)

private[quasiquotes] object InterpolationTypedTargetProbe:
  inline def inspect[A](inline expression: A): InterpolationTypedTargetEvidence =
    ${ inspectImpl('expression) }

  private def inspectImpl[A: Type](
      expression: Expr[A]
  )(using Quotes): Expr[InterpolationTypedTargetEvidence] =
    import quotes.reflect.*
    val term = expression.asTerm
    val source = term.pos.sourceCode
    val structure = term.show(using Printer.TreeStructure)
    val normalized = MatchNormalizer.normalizedView(term).fold(_.message, _.render)
    '{
      InterpolationTypedTargetEvidence(
        ${ Expr(source) },
        ${ Expr(structure) },
        ${ Expr(normalized) }
      )
    }
