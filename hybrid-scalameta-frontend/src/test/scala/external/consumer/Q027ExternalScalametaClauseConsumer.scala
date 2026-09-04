package external.consumer

import quasiquotes.hybrid.q027.{
  Q027ScalametaDefinitionClauseSyntax,
  Q027ScalametaDefinitionSummary
}

object Q027ExternalScalametaClauseConsumer:
  val namedUsing: Either[String, Q027ScalametaDefinitionSummary] =
    Q027ScalametaDefinitionClauseSyntax.inspect(
      "def named(using ordering: Ordering[Int]): Int = 1"
    )
