package quasiquotes.construct

import scala.quoted.*
import scala.util.matching.Regex

import quasiquotes.definitions.DefinitionName

private[quasiquotes] object PublicDefinitionQuasiquote:
  private val DiagnosticPrefix = "Invalid dqr definition template:"
  private val Prefix: Regex =
    raw"""\s*def\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(\s*([A-Za-z_][A-Za-z0-9_]*)\s*:\s*""".r
  private val BetweenTypes: Regex = raw"""\s*\)\s*:\s*""".r
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
    if parts.size != 3 then
      abort("expected exactly two TypeRepr splice positions.")
    if args == null || args.size != 2 then
      abort(s"expected exactly two TypeRepr splices, but received ${Option(args).fold(0)(_.size)}.")

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
