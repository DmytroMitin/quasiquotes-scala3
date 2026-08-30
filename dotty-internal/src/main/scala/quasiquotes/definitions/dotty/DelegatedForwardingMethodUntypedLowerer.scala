package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.definitions.*
import quasiquotes.definitions.DelegatedForwardingMethodPlan.Plan
import quasiquotes.definitions.ScopedType.*

private[quasiquotes] final case class DelegatedForwardingMethodRawError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"

/** Parser-independent source-free lowering for the exact AUXify-043 plan. */
private[quasiquotes] object DelegatedForwardingMethodUntypedLowerer:
  def lower(
      plan: Plan
  )(using Context): Either[DelegatedForwardingMethodRawError, untpd.DefDef] =
    given SourceFile = NoSource

    for
      present <- Option(plan).toRight(
        error("INTERNAL_INVARIANT_FAILED", "the validated 043 plan was null.")
      )
      methodName <- decodedTermName(present.methodIdentity.sourceName, "method")
      typeParameterName <- decodedTypeName(
        present.typeParameter.displayName,
        "Type parameter"
      )
      ordinaryName <- decodedTermName(
        present.ordinaryParameter.displayName,
        "ordinary parameter"
      )
      contextualName <- decodedTermName(
        present.contextualParameter.displayName,
        "contextual parameter"
      )
      contextualConstructor <- present.contextualParameter.parameterType match
        case Applied(SourceName(value), Vector(_: TypeParameterReference)) =>
          decodedTypeName(value, "contextual Type constructor")
        case _ =>
          Left(
            error(
              "INTERNAL_INVARIANT_FAILED",
              "the validated contextual Type lost its unary source-name shape."
            )
          )
      resultTypeName <- decodedTypeName(present.resultType.value, "result Type")
      raw =
        val typeParameter = untpd
          .TypeDef(
            typeParameterName,
            untpd.TypeBoundsTree(untpd.EmptyTree, untpd.EmptyTree)
          )
          .withMods(untpd.Modifiers(Flags.Param))
        val ordinary = untpd
          .ValDef(
            ordinaryName,
            untpd.Ident(typeParameterName),
            untpd.EmptyTree
          )
          .withMods(untpd.Modifiers(Flags.Param))
        val contextual = untpd
          .ValDef(
            contextualName,
            untpd.AppliedTypeTree(
              untpd.Ident(contextualConstructor),
              untpd.Ident(typeParameterName) :: Nil
            ),
            untpd.EmptyTree
          )
          .withMods(untpd.Modifiers(Flags.Param | Flags.Given))
        untpd
          .DefDef(
            methodName,
            List(typeParameter :: Nil, ordinary :: Nil, contextual :: Nil),
            untpd.Ident(resultTypeName),
            untpd.Apply(
              untpd.Select(untpd.Ident(contextualName), methodName),
              untpd.Ident(ordinaryName) :: Nil
            )
          )
          .withMods(untpd.Modifiers(Flags.Method))
      _ <- validateRaw(raw, present)
    yield raw

  private def decodedTermName(
      value: String,
      role: String
  ) =
    decoded(value, role).map(termName)

  private def decodedTypeName(
      value: String,
      role: String
  ) =
    decoded(value, role).map(typeName)

  private def decoded(
      value: String,
      role: String
  ): Either[DelegatedForwardingMethodRawError, String] =
    DefinitionName
      .fromSource(value)
      .left
      .map(problem =>
        error(
          "RAW_LOWERING_UNSUPPORTED",
          s"$role name lowering failed: ${problem.message}"
        )
      )
      .map(_.decoded)

  private def validateRaw(
      raw: untpd.DefDef,
      plan: Plan
  )(using Context): Either[DelegatedForwardingMethodRawError, Unit] =
    val errors = Vector.newBuilder[String]
    if raw.name.toString != decodedUnsafe(plan.methodIdentity.sourceName) ||
        raw.mods.flags != Flags.Method
    then errors += "method name or flags diverged"

    raw.leadingTypeParams match
      case List(parameter: untpd.TypeDef) =>
        if parameter.name.toString != decodedUnsafe(plan.typeParameter.displayName) ||
            parameter.mods.flags != Flags.Param
        then errors += "Type-parameter name or flags diverged"
        parameter.rhs match
          case untpd.TypeBoundsTree(lo, hi, alias)
              if lo.isEmpty && hi.isEmpty && alias.isEmpty => ()
          case _ => errors += "Type parameter is not one unbounded TypeBoundsTree"
      case _ => errors += "method does not have exactly one Type parameter"

    raw.trailingParamss match
      case List(List(ordinary: untpd.ValDef), List(contextual: untpd.ValDef)) =>
        if ordinary.name.toString != decodedUnsafe(plan.ordinaryParameter.displayName) ||
            ordinary.mods.flags != Flags.Param
        then errors += "ordinary parameter name or flags diverged"
        if contextual.name.toString != decodedUnsafe(
            plan.contextualParameter.displayName
          ) || contextual.mods.flags != (Flags.Param | Flags.Given)
        then errors += "contextual parameter name or flags diverged"
      case _ => errors += "value-clause topology diverged"

    val trees = allTrees(raw)
    if trees.size != 14 then
      errors += s"raw tree has ${trees.size} nonempty nodes instead of 14"
    trees.foreach { tree =>
      if tree.source.exists || tree.span.exists || tree.symbol != NoSymbol then
        errors += s"${tree.getClass.getSimpleName} is not source/span/symbol free"
      if tree.isInstanceOf[untpd.TypedSplice] then
        errors += "raw tree contains TypedSplice"
    }

    val result = errors.result()
    Either.cond(
      result.isEmpty,
      (),
      error("RAW_LOWERING_UNSUPPORTED", result.mkString("; "))
    )

  private def decodedUnsafe(value: String): String =
    DefinitionName.fromSource(value).fold(_ => value, _.decoded)

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs)
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def error(
      code: String,
      detail: String
  ): DelegatedForwardingMethodRawError =
    DelegatedForwardingMethodRawError(code, detail)
