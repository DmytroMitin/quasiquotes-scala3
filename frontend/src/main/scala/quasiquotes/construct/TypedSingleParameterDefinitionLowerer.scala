package quasiquotes.construct

import scala.quoted.*
import scala.util.control.NonFatal

import quasiquotes.definitions.DefinitionName
import quasiquotes.publicapi.{CompletedTerm, CompletedType, DefinitionConstruction}
import quasiquotes.types.{TargetTypeReprInspector, TypeNormalForm}

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
        completedType <- toCompletedType(inspectedParameterType)
        completedBody <- CompletedTerm
          .definitionParameterReference(parameterName.decoded)
          .left
          .map(_.message)
        _ <- DefinitionConstruction
          .singleParameterMethod(
            methodName.decoded,
            parameterName.decoded,
            completedType,
            completedType,
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

  private def toCompletedType(normalForm: TypeNormalForm): Either[String, CompletedType] =
    normalForm match
      case TypeNormalForm.STypeIdent(name) => named(name)
      case TypeNormalForm.STypeResolved(id) =>
        Left(s"Resolved selected Type `${id.canonicalSource}` is not admitted by the public Definition construction surface.")
      case TypeNormalForm.STypeApply(constructor, arguments) =>
        for
          completedConstructor <- toCompletedType(constructor)
          completedArguments <- collect(arguments.map(toCompletedType))
          result <- CompletedType
            .applied(completedConstructor, completedArguments.toVector)
            .left
            .map(_.message)
        yield result
      case TypeNormalForm.STypeTuple(elements) =>
        applied(s"Tuple${elements.size}", elements)
      case TypeNormalForm.STypeFunction(arguments, result) =>
        applied(s"Function${arguments.size}", arguments :+ result)

  private def named(name: String): Either[String, CompletedType] =
    CompletedType.named(name).left.map(_.message)

  private def applied(
      constructor: String,
      arguments: List[TypeNormalForm]
  ): Either[String, CompletedType] =
    for
      completedConstructor <- named(constructor)
      completedArguments <- collect(arguments.map(toCompletedType))
      result <- CompletedType
        .applied(completedConstructor, completedArguments.toVector)
        .left
        .map(_.message)
    yield result

  private def collect[A](values: List[Either[String, A]]): Either[String, List[A]] =
    values.foldRight[Either[String, List[A]]](Right(Nil)) { (value, accumulated) =>
      for
        head <- value
        tail <- accumulated
      yield head :: tail
    }
