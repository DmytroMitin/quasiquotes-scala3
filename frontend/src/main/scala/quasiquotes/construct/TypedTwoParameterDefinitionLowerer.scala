package quasiquotes.construct

import scala.quoted.*
import scala.util.control.NonFatal

import quasiquotes.definitions.DefinitionName
import quasiquotes.publicapi.{CompletedTerm, CompletedType, DefinitionConstruction}
import quasiquotes.types.{TargetTypeReprInspector, TypeNormalForm}

private[quasiquotes] object TypedTwoParameterDefinitionLowerer:
  private val AdmittedTypes = Set("Int", "String", "Boolean")

  def lower(using q: Quotes)(
      methodName: DefinitionName,
      firstParameterName: DefinitionName,
      firstParameterType: q.reflect.TypeRepr,
      secondParameterName: DefinitionName,
      secondParameterType: q.reflect.TypeRepr,
      resultType: q.reflect.TypeRepr,
      selectedParameterName: DefinitionName
  ): Either[String, q.reflect.DefDef] =
    try
      for
        firstNormal <- inspect(firstParameterType, 0)
        secondNormal <- inspect(secondParameterType, 1)
        resultNormal <- inspect(resultType, 2)
        completedFirst <- standalone(firstNormal, 0)
        completedSecond <- standalone(secondNormal, 1)
        completedResult <- standalone(resultNormal, 2)
        body <- CompletedTerm
          .definitionParameterReference(selectedParameterName.decoded)
          .left
          .map(_.message)
        validated <- DefinitionConstruction
          .twoParameterMethod(
            methodName.decoded,
            firstParameterName.decoded,
            completedFirst,
            secondParameterName.decoded,
            completedSecond,
            completedResult,
            body
          )
          .left
          .map(_.message)
        definition <- construct(
          methodName,
          firstParameterName,
          firstParameterType,
          secondParameterName,
          secondParameterType,
          resultType,
          validated.body.referenceName
        )
      yield definition
    catch
      case NonFatal(_) => Left("typed exact-two Definition lowering failed.")

  private def inspect(using q: Quotes)(
      value: q.reflect.TypeRepr,
      ordinal: Int
  ): Either[String, TypeNormalForm] =
    if value == null then Left(s"TypeRepr splice at ordinal $ordinal must not be null.")
    else
      TargetTypeReprInspector
        .inspect(value)
        .left
        .map(error => s"unsupported TypeRepr splice at ordinal $ordinal: ${error.message}")

  private def standalone(
      normalForm: TypeNormalForm,
      ordinal: Int
  ): Either[String, CompletedType] =
    normalForm match
      case TypeNormalForm.STypeIdent(name) if AdmittedTypes(name) =>
        CompletedType.named(name).left.map(_.message)
      case _ =>
        Left(
          s"TypeRepr splice at ordinal $ordinal must be standalone Int, String, or Boolean."
        )

  private def construct(using q: Quotes)(
      methodName: DefinitionName,
      firstParameterName: DefinitionName,
      firstParameterType: q.reflect.TypeRepr,
      secondParameterName: DefinitionName,
      secondParameterType: q.reflect.TypeRepr,
      resultType: q.reflect.TypeRepr,
      selectedParameterName: String
  ): Either[String, q.reflect.DefDef] =
    import q.reflect.*

    val methodType = MethodType(
      List(firstParameterName.decoded, secondParameterName.decoded)
    )(
      _ => List(firstParameterType, secondParameterType),
      _ => resultType
    )
    val methodSymbol = Symbol.newMethod(Symbol.spliceOwner, methodName.decoded, methodType)
    var parameterFailure = false
    val definition = DefDef(methodSymbol, parameterClauses =>
      parameterClauses match
        case List(List(first, second)) =>
          if selectedParameterName == first.symbol.name then Some(Ref(first.symbol))
          else if selectedParameterName == second.symbol.name then Some(Ref(second.symbol))
          else
            parameterFailure = true
            None
        case _ =>
          parameterFailure = true
          None
    )
    Either.cond(
      !parameterFailure,
      definition,
      "generated method parameters violated the exact-two contract."
    )
