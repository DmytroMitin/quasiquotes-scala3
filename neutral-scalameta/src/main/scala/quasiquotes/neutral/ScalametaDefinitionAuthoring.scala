package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionShape

import scala.annotation.nowarn
import scala.meta.Defn

/** Internal authoring boundary over the accepted reusable Definition families. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaDefinitionAuthoring:
  sealed trait Error derives CanEqual

  object Error:
    case object Missing extends Error
    final case class SimpleTypeAlias(
        problem: ScalametaSimpleTypeAliasAuthoring.Error
    ) extends Error
    final case class ImmutableVal(
        problem: ScalametaTypedImmutableValAuthoring.Error
    ) extends Error
    final case class ParameterlessDef(
        problem: ScalametaTypedParameterlessDefAuthoring.Error
    ) extends Error
    final case class SingleParameterDef(
        problem: ScalametaTypedSingleParameterDefAuthoring.Error
    ) extends Error
    final case class TwoParameterDef(
        problem: ScalametaTypedTwoParameterDefAuthoring.Error
    ) extends Error

  def author(shape: DefinitionShape): Either[Error, Defn] =
    Option(shape)
      .toRight(Error.Missing)
      .flatMap {
        case alias: DefinitionShape.SimpleTypeAlias =>
          ScalametaSimpleTypeAliasAuthoring
            .author(alias)
            .left
            .map(Error.SimpleTypeAlias.apply)
        case value: DefinitionShape.ImmutableVal =>
          ScalametaTypedImmutableValAuthoring
            .author(value)
            .left
            .map(Error.ImmutableVal.apply)
        case method: DefinitionShape.ParameterlessDef =>
          ScalametaTypedParameterlessDefAuthoring
            .author(method)
            .left
            .map(Error.ParameterlessDef.apply)
        case method: DefinitionShape.SingleParameterDef =>
          ScalametaTypedSingleParameterDefAuthoring
            .author(method)
            .left
            .map(Error.SingleParameterDef.apply)
        case method: DefinitionShape.TwoParameterDef =>
          ScalametaTypedTwoParameterDefAuthoring
            .author(method)
            .left
            .map(Error.TwoParameterDef.apply)
      }
