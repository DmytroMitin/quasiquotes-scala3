package quasiquotes.construct

import scala.quoted.*
import scala.util.control.NonFatal

import quasiquotes.definitions.DefinitionName
import quasiquotes.publicapi.{CompletedTerm, DefinitionConstruction}
import quasiquotes.types.TargetTypeReprInspector

private[quasiquotes] object TypedSingleParameterDefinitionLowerer:
  def lower(using q: Quotes)(
      methodName: DefinitionName,
      parameterName: DefinitionName,
      parameterType: q.reflect.TypeRepr,
      resultType: q.reflect.TypeRepr
  ): Either[String, q.reflect.DefDef] =
    import q.reflect.*

    try
      for
        inspectedParameterType <- TargetTypeReprInspector
          .inspect(parameterType)
          .left
          .map(error => s"unsupported TypeRepr splice at ordinal 0: ${error.message}")
        inspectedResultType <- TargetTypeReprInspector
          .inspect(resultType)
          .left
          .map(error => s"unsupported TypeRepr splice at ordinal 1: ${error.message}")
        _ <- Either.cond(
          inspectedParameterType == inspectedResultType,
          (),
          "the parameter and result TypeRepr splices must have equal normalized types."
        )
        completedBody <- CompletedTerm
          .definitionParameterReference(parameterName.decoded)
          .left
          .map(_.message)
        _ <- DefinitionConstruction
          .constructSingleParameterMethodFromNormalForms(
            methodName.decoded,
            parameterName.decoded,
            inspectedParameterType,
            inspectedResultType,
            completedBody
          )
          .left
          .map(_.message)
        definition <- construct(
          methodName,
          parameterName,
          parameterType,
          resultType
        )
      yield definition
    catch
      case NonFatal(_) => Left("typed single-parameter Definition lowering failed.")

  private def construct(using q: Quotes)(
      methodName: DefinitionName,
      parameterName: DefinitionName,
      parameterType: q.reflect.TypeRepr,
      resultType: q.reflect.TypeRepr
  ): Either[String, q.reflect.DefDef] =
    import q.reflect.*

    val methodType = MethodType(List(parameterName.decoded))(
      _ => List(parameterType),
      _ => resultType
    )
    val methodSymbol = Symbol.newMethod(
      Symbol.spliceOwner,
      methodName.decoded,
      methodType
    )
    var parameterFailure = false
    val definition = DefDef(methodSymbol, parameterClauses =>
      parameterClauses match
        case List(List(parameter)) => Some(Ref(parameter.symbol))
        case _ =>
          parameterFailure = true
          None
    )
    Either.cond(
      !parameterFailure,
      definition,
      "generated method parameters violated the single-parameter contract."
    )
