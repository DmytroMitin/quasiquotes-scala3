package external.consumer

// snippet:definition-pattern-first-use:start
import scala.quoted.*

import quasiquotes.matching.{
  DefinitionPattern,
  SingleParameterDefinitionMatch
}

object DefinitionPatternFirstUseSnippet:
  val configured = DefinitionPattern.singleParameter(
    "def boundedIdentity(value: Int): Int = $body"
  )

  def dqrIdentity(value: Int): Int =
    DefinitionPatternFirstUseMacros.matchDqr(value)

  def independent(value: Int): Int =
    DefinitionPatternFirstUseMacros.matchIndependent(value)

  def inspect(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[
    SingleParameterDefinitionMatch[q.reflect.TypeRepr, q.reflect.Term]
  ] =
    val pattern = DefinitionPattern
      .singleParameter("def boundedIdentity(value: Int): Int = $body")
      .toOption
      .get
    val matched = pattern.matchDefinition(target)

    matched.foreach { result =>
      val methodName: String = result.methodName
      val parameterName: String = result.parameterName
      val parameterType: q.reflect.TypeRepr = result.parameterType
      val resultType: q.reflect.TypeRepr = result.resultType
      val body: q.reflect.Term = result.body
      assert(
        methodName.nonEmpty && parameterName.nonEmpty &&
          parameterType != null && resultType != null && body != null
      )
    }
    matched
// snippet:definition-pattern-first-use:end
