package external.consumer

import java.lang.invoke.{MethodHandles, MethodType}

import scala.quoted.Quotes

import quasiquotes.matching.SingleParameterDefinitionPattern
import quasiquotes.scalameta.ScalametaQuasiPattern

object Q014LegacyScalametaDqqJvmConsumer:
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
      .findVirtual(ScalametaQuasiPattern.getClass, "dqq", descriptor)
      .invoke(ScalametaQuasiPattern, context, quotes)
      .asInstanceOf[SingleParameterDefinitionPattern]
