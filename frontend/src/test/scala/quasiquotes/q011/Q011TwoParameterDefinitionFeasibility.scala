package quasiquotes.q011

import scala.quoted.*
import scala.util.control.NonFatal

import quasiquotes.definitions.DefinitionName
import quasiquotes.matching.{DefinitionPattern, SingleParameterDefinitionPattern}
import quasiquotes.publicapi.{CompletedTerm, CompletedType, DefinitionConstruction}
import quasiquotes.types.{TargetTypeReprInspector, TypeNormalForm}

/** Q011-only exact-two matcher prototype. It deliberately is not a production
  * subtype, alias, or widening of `SingleParameterDefinitionPattern`.
  */
final class Q011TwoParameterDefinitionPattern private[q011] (
    private val expectedMethodName: String,
    private val expectedFirstParameterName: String,
    private val expectedFirstParameterType: TypeNormalForm,
    private val expectedSecondParameterName: String,
    private val expectedSecondParameterType: TypeNormalForm,
    private val expectedResultType: TypeNormalForm
):
  def unapply(using q: Quotes)(target: q.reflect.DefDef): Option[q.reflect.Term] =
    import q.reflect.*

    if target == null ||
        target.name != expectedMethodName ||
        target.symbol == Symbol.noSymbol ||
        !target.symbol.isDefDef ||
        target.symbol.isClassConstructor ||
        target.symbol.flags.is(Flags.ExtensionMethod) ||
        target.symbol.flags.is(Flags.FieldAccessor) ||
        target.symbol.flags.is(Flags.ParamAccessor) ||
        target.symbol.flags.is(Flags.CaseAccessor) ||
        target.symbol.flags.is(Flags.Given)
    then None
    else
      target.paramss match
        case List(clause: TermParamClause)
            if !clause.isImplicit && !clause.isGiven && !clause.isErased =>
          clause.params match
            case List(first, second)
                if admittedParameters(target, first, second) &&
                  first.name == expectedFirstParameterName &&
                  second.name == expectedSecondParameterName =>
              for
                body <- target.rhs
                firstType <- TargetTypeReprInspector.inspect(first.tpt.tpe).toOption
                if firstType == expectedFirstParameterType
                secondType <- TargetTypeReprInspector.inspect(second.tpt.tpe).toOption
                if secondType == expectedSecondParameterType
                resultType <- TargetTypeReprInspector.inspect(target.returnTpt.tpe).toOption
                if resultType == expectedResultType
              yield body
            case _ => None
        case _ => None

  private def admittedParameters(using q: Quotes)(
      target: q.reflect.DefDef,
      first: q.reflect.ValDef,
      second: q.reflect.ValDef
  ): Boolean =
    import q.reflect.*

    target.symbol.paramSymss match
      case List(List(firstSymbol, secondSymbol)) =>
        first.symbol != Symbol.noSymbol &&
          second.symbol != Symbol.noSymbol &&
          first.symbol != second.symbol &&
          !first.symbol.flags.is(Flags.HasDefault) &&
          !second.symbol.flags.is(Flags.HasDefault) &&
          firstSymbol == first.symbol &&
          secondSymbol == second.symbol &&
          first.symbol.owner == target.symbol &&
          second.symbol.owner == target.symbol
      case _ => false

object Q011TwoParameterDefinitionPattern:
  def exact(
      methodName: String,
      firstParameterName: String,
      firstParameterType: TypeNormalForm,
      secondParameterName: String,
      secondParameterType: TypeNormalForm,
      resultType: TypeNormalForm
  ): Q011TwoParameterDefinitionPattern =
    new Q011TwoParameterDefinitionPattern(
      methodName,
      firstParameterName,
      firstParameterType,
      secondParameterName,
      secondParameterType,
      resultType
    )

  def fromParts(parts: Seq[String]): Either[String, Q011TwoParameterDefinitionPattern] =
    if parts == List("def first(left: Int, right: String): Int = ", "") then
      Right(exactFirst)
    else if parts == List("def second(left: Int, right: String): String = ", "") then
      Right(exactSecond)
    else Left("Q011 probe admits only its two exact static templates.")

  val exactFirst: Q011TwoParameterDefinitionPattern =
    exact(
      "first",
      "left",
      TypeNormalForm.STypeIdent("Int"),
      "right",
      TypeNormalForm.STypeIdent("String"),
      TypeNormalForm.STypeIdent("Int")
    )

  val exactSecond: Q011TwoParameterDefinitionPattern =
    exact(
      "second",
      "left",
      TypeNormalForm.STypeIdent("Int"),
      "right",
      TypeNormalForm.STypeIdent("String"),
      TypeNormalForm.STypeIdent("String")
    )

/** Q011-only structured reflection lowerer. It proves the future frontend seam
  * can reuse Core's current exact-two semantic authority.
  */
object Q011TwoParameterDefinitionLowerer:
  def lower(using q: Quotes)(
      methodName: DefinitionName,
      firstParameterName: DefinitionName,
      firstParameterType: q.reflect.TypeRepr,
      secondParameterName: DefinitionName,
      secondParameterType: q.reflect.TypeRepr,
      resultType: q.reflect.TypeRepr,
      selectedParameterName: DefinitionName
  ): Either[String, q.reflect.DefDef] =
    import q.reflect.*

    try
      for
        firstNormal <- inspect(firstParameterType, 0)
        secondNormal <- inspect(secondParameterType, 1)
        resultNormal <- inspect(resultType, 2)
        completedFirst <- standaloneNamed(firstNormal, 0)
        completedSecond <- standaloneNamed(secondNormal, 1)
        completedResult <- standaloneNamed(resultNormal, 2)
        body <- CompletedTerm
          .definitionParameterReference(selectedParameterName.decoded)
          .left
          .map(_.message)
        core <- DefinitionConstruction
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
          core.body.referenceName
        )
      yield definition
    catch
      case NonFatal(_) => Left("typed exact-two Definition lowering failed.")

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
    val methodSymbol = Symbol.newMethod(
      Symbol.spliceOwner,
      methodName.decoded,
      methodType
    )
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

  private def inspect(using q: Quotes)(
      value: q.reflect.TypeRepr,
      ordinal: Int
  ): Either[String, TypeNormalForm] =
    TargetTypeReprInspector
      .inspect(value)
      .left
      .map(error => s"unsupported TypeRepr splice at ordinal $ordinal: ${error.message}")

  private def standaloneNamed(
      normalForm: TypeNormalForm,
      ordinal: Int
  ): Either[String, CompletedType] =
    normalForm match
      case TypeNormalForm.STypeIdent(name) =>
        CompletedType.named(name).left.map(_.message)
      case _ =>
        Left(s"TypeRepr splice at ordinal $ordinal is outside Q011's standalone-named bound.")

/** Strategy A: same spelling with transparent-inline static specialization. */
object Q011SpecializedDqqProbe:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q011DefinitionPatternSpecializationMacro.extractor('context, 'q) }

/** Strategy B: additive syntax; the production `dqq` signature stays intact. */
object Q011AdditiveDqqProbe:
  extension (context: StringContext)
    def dqq2(using q: Quotes): Q011TwoParameterDefinitionPattern =
      Q011TwoParameterDefinitionPattern.fromParts(context.parts) match
        case Right(pattern) => pattern
        case Left(detail) => q.reflect.report.errorAndAbort(detail)

/** Test-only umbrella demonstrating direct exports for the additive strategy. */
object Q011AdditiveUmbrellaProbe:
  export DefinitionPattern.dqq
  export Q011AdditiveDqqProbe.dqq2

/** Strategy C's truthful minimum interface. Returning it from today's `dqq`
  * would still replace the concrete public Scala/TASTy result type and lose
  * `matchDefinition` from explicitly typed single-parameter callers.
  */
trait Q011CommonDefinitionPattern:
  def unapply(using q: Quotes)(target: q.reflect.DefDef): Option[q.reflect.Term]

final class Q011CommonSinglePattern(
    delegate: SingleParameterDefinitionPattern
) extends Q011CommonDefinitionPattern:
  def unapply(using q: Quotes)(target: q.reflect.DefDef): Option[q.reflect.Term] =
    delegate.unapply(target)

final class Q011CommonTwoPattern(
    delegate: Q011TwoParameterDefinitionPattern
) extends Q011CommonDefinitionPattern:
  def unapply(using q: Quotes)(target: q.reflect.DefDef): Option[q.reflect.Term] =
    delegate.unapply(target)
