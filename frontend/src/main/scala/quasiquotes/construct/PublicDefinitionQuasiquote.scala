package quasiquotes.construct

import scala.quoted.*
import scala.util.matching.Regex

import quasiquotes.definitions.DefinitionName

private[quasiquotes] object PublicDefinitionQuasiquote:
  private val DiagnosticPrefix = "Invalid dqr definition template:"
  private val Prefix: Regex =
    raw"""\s*def\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(\s*([A-Za-z_][A-Za-z0-9_]*)\s*:\s*""".r
  private val BetweenTypes: Regex = raw"""\s*\)\s*:\s*""".r
  private val BetweenParameters: Regex =
    raw"""\s*,\s*([A-Za-z_][A-Za-z0-9_]*)\s*:\s*""".r
  private val Suffix: Regex =
    raw"""\s*=\s*([A-Za-z_][A-Za-z0-9_]*)\s*""".r

  def build(using q: Quotes)(
      sc: StringContext,
      args: Seq[q.reflect.TypeRepr]
  ): q.reflect.DefDef =
    import q.reflect.*

    def abort(detail: String): Nothing =
      report.errorAndAbort(s"$DiagnosticPrefix $detail")

    if sc == null then abort("StringContext must not be null.")
    val parts = sc.parts
    if parts == null || parts.isEmpty then
      abort("StringContext must contain exactly three literal parts.")
    if parts.exists(_ == null) then
      abort("StringContext literal parts must not be null.")
    if args == null then abort("TypeRepr splices must not be null.")

    (parts.size, args.size) match
      case (3, 2) => buildSingle(parts, args, abort)
      case (4, 3) => buildTwo(parts, args, abort)
      case (3, received) =>
        abort(s"expected exactly two TypeRepr splices, but received $received.")
      case (4, received) =>
        abort(s"expected exactly three TypeRepr splices, but received $received.")
      case _ =>
        abort("expected exactly two or three TypeRepr splice positions.")

  private def buildSingle(using q: Quotes)(
      parts: Seq[String],
      args: Seq[q.reflect.TypeRepr],
      abort: String => Nothing
  ): q.reflect.DefDef =

    val (methodNameSource, parameterNameSource) = parts(0) match
      case Prefix(methodName, parameterName) => (methodName, parameterName)
      case _ =>
        abort("expected `def method(parameter: $parameterType): $resultType = parameter`.")

    parts(1) match
      case BetweenTypes() => ()
      case _ =>
        abort("expected one ordinary parameter clause followed by the result type splice.")

    val bodyNameSource = parts(2) match
      case Suffix(bodyName) => bodyName
      case _ => abort("the body must be the literal declared parameter reference.")

    val methodName = DefinitionName.plain(methodNameSource).fold(
      _ => abort("the method name must be a valid ordinary Scala identifier."),
      identity
    )
    val parameterName = DefinitionName.plain(parameterNameSource).fold(
      _ => abort("the parameter name must be a valid ordinary Scala identifier."),
      identity
    )
    if bodyNameSource != parameterName.source then
      abort("the body must reference the declared parameter name exactly.")

    TypedSingleParameterDefinitionLowerer
      .lower(methodName, parameterName, args.head, args(1))
      .fold(abort, identity)

  private def buildTwo(using q: Quotes)(
      parts: Seq[String],
      args: Seq[q.reflect.TypeRepr],
      abort: String => Nothing
  ): q.reflect.DefDef =
    val (methodNameSource, firstParameterNameSource) = parts(0) match
      case Prefix(methodName, parameterName) => (methodName, parameterName)
      case _ =>
        abort(
          "expected `def method(first: $firstType, second: $secondType): $resultType = firstOrSecond`."
        )

    val secondParameterNameSource = parts(1) match
      case BetweenParameters(parameterName) => parameterName
      case _ => abort("expected a second ordinary parameter after the first TypeRepr splice.")

    parts(2) match
      case BetweenTypes() => ()
      case _ => abort("expected one ordinary exact-two parameter clause followed by the result type splice.")

    val bodyNameSource = parts(3) match
      case Suffix(bodyName) => bodyName
      case _ => abort("the body must be a literal reference to one declared parameter.")

    val methodName = plainName(methodNameSource, "method", abort)
    val firstParameterName = plainName(firstParameterNameSource, "first parameter", abort)
    val secondParameterName = plainName(secondParameterNameSource, "second parameter", abort)
    if firstParameterName == secondParameterName then
      abort("the two parameter names must be distinct.")
    val selectedParameterName =
      if bodyNameSource == firstParameterName.source then firstParameterName
      else if bodyNameSource == secondParameterName.source then secondParameterName
      else abort("the body must reference exactly one declared parameter name.")

    TypedTwoParameterDefinitionLowerer
      .lower(
        methodName,
        firstParameterName,
        args(0),
        secondParameterName,
        args(1),
        args(2),
        selectedParameterName
      )
      .fold(abort, identity)

  private def plainName(
      source: String,
      role: String,
      abort: String => Nothing
  ): DefinitionName =
    DefinitionName.plain(source).fold(
      _ => abort(s"the $role name must be a valid ordinary Scala identifier."),
      identity
    )
