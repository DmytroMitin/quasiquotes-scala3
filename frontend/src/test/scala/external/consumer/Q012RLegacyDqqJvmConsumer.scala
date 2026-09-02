package external.consumer

import java.lang.invoke.{MethodHandles, MethodType}

import scala.quoted.Quotes

import quasiquotes.matching.{DefinitionPattern, SingleParameterDefinitionPattern}

object Q012RLegacyDqqJvmConsumer:
  def build(
      context: StringContext,
      quotes: Quotes
  ): SingleParameterDefinitionPattern =
    val descriptor = MethodType.methodType(
      classOf[SingleParameterDefinitionPattern],
      classOf[StringContext],
      classOf[Quotes]
    )
    MethodHandles.publicLookup()
      .findVirtual(DefinitionPattern.getClass, "dqq", descriptor)
      .invoke(DefinitionPattern, context, quotes)
      .asInstanceOf[SingleParameterDefinitionPattern]
