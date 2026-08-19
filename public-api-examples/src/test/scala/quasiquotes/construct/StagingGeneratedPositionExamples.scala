package quasiquotes.construct

import scala.quoted.{Expr, staging, quotes}

object StagingGeneratedPositionExamples:
  def generatedArgumentPositionFallsBack(value: Int): Boolean =
    given staging.Compiler =
      staging.Compiler.make(getClass.getClassLoader)

    staging.withQuotes:
      import quotes.reflect.*

      MacroArgumentPositionResolver.resolve(
        index = 0,
        arguments = Seq(Expr(value).asTerm)
      )
      true
