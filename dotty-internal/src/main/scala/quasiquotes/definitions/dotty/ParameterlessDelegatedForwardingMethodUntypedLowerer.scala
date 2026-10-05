package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.definitions.DefinitionName
import quasiquotes.definitions.ParameterlessDelegatedForwardingPlan
import quasiquotes.definitions.ParameterlessDelegatedForwardingPlan.Plan
import quasiquotes.definitions.ScopedType.*

/** Parser-independent source-free lowering for the exact AUXify-083 plan. */
private[quasiquotes] object ParameterlessDelegatedForwardingMethodUntypedLowerer:
  def lower(
      plan: Plan
  )(using Context): Either[
    ParameterlessDelegatedForwardingMethodUntypedLoweringError,
    untpd.DefDef
  ] =
    given SourceFile = NoSource

    for
      present <- Option(plan).toRight(
        error("PLAN_REQUIRED", "the parameterless delegated-forwarding plan must be present.")
      )
      _ <- ParameterlessDelegatedForwardingPlan
        .validate(present)
        .left
        .map(problem => error("PLAN_INVALID", problem.message))
      methodName <- decodedTermName(present.methodIdentity.sourceName, "method")
      typeParameterName <- decodedTypeName(
        present.typeParameter.displayName,
        "Type parameter"
      )
      contextualName <- decodedTermName(
        present.contextualParameter.displayName,
        "contextual parameter"
      )
      contextualConstructor <- present.contextualParameter.parameterType match
        case Applied(SourceName(value), Vector(reference: TypeParameterReference))
            if reference.eq(present.evidenceTypeArgument) =>
          decodedTypeName(value, "contextual Type constructor")
        case _ =>
          Left(
            error(
              "PLAN_INVALID",
              "the validated contextual Type lost its exact unary evidence edge."
            )
          )
      raw =
        val typeParameter = untpd
          .TypeDef(
            typeParameterName,
            untpd.TypeBoundsTree(untpd.EmptyTree, untpd.EmptyTree)
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
            List(typeParameter :: Nil, contextual :: Nil),
            untpd.Ident(typeParameterName),
            untpd.Select(untpd.Ident(contextualName), methodName)
          )
          .withMods(untpd.Modifiers(Flags.Method))
      _ <- validateCandidate(present, raw)
    yield raw

  private[dotty] def validateCandidate(
      plan: Plan,
      raw: untpd.DefDef
  )(using Context): Either[
    ParameterlessDelegatedForwardingMethodUntypedLoweringError,
    Unit
  ] =
    for
      present <- Option(plan).toRight(
        error("PLAN_REQUIRED", "the parameterless delegated-forwarding plan must be present.")
      )
      _ <- ParameterlessDelegatedForwardingPlan
        .validate(present)
        .left
        .map(problem => error("PLAN_INVALID", problem.message))
      candidate <- Option(raw).toRight(
        error("EXACT_RAW_INVARIANT_FAILED", "the raw candidate must be present.")
      )
      methodName <- decoded(present.methodIdentity.sourceName, "method")
      typeParameterName <- decoded(present.typeParameter.displayName, "Type parameter")
      contextualName <- decoded(
        present.contextualParameter.displayName,
        "contextual parameter"
      )
      constructorName <- present.contextualParameter.parameterType match
        case Applied(SourceName(value), Vector(reference: TypeParameterReference))
            if reference.eq(present.evidenceTypeArgument) =>
          decoded(value, "contextual Type constructor")
        case _ =>
          Left(
            error(
              "PLAN_INVALID",
              "the contextual Type must retain the exact unary evidence edge."
            )
          )
      _ <- validateExact(
        candidate,
        methodName,
        typeParameterName,
        contextualName,
        constructorName
      )
    yield ()

  private def validateExact(
      raw: untpd.DefDef,
      methodName: String,
      typeParameterName: String,
      contextualName: String,
      constructorName: String
  )(using Context): Either[
    ParameterlessDelegatedForwardingMethodUntypedLoweringError,
    Unit
  ] =
    val errors = Vector.newBuilder[String]

    if raw.name.toString != methodName || raw.mods.flags != Flags.Method then
      errors += "method name or flags diverged"

    raw.paramss match
      case List(List(typeParameter: untpd.TypeDef), List(contextual: untpd.ValDef)) =>
        if typeParameter.name.toString != typeParameterName ||
            typeParameter.mods.flags != Flags.Param
        then errors += "Type-parameter role or flags diverged"
        typeParameter.rhs match
          case untpd.TypeBoundsTree(lo, hi, alias)
              if lo.isEmpty && hi.isEmpty && alias.isEmpty => ()
          case _ => errors += "Type parameter is not one exact empty TypeBoundsTree"

        if contextual.name.toString != contextualName ||
            contextual.mods.flags != (Flags.Param | Flags.Given) ||
            !contextual.rhs.isEmpty
        then errors += "contextual-parameter role flags or body diverged"
        contextual.tpt match
          case untpd.AppliedTypeTree(
                untpd.Ident(constructor),
                List(untpd.Ident(argument))
              ) =>
            if constructor.toString != constructorName then
              errors += "contextual evidence-constructor role diverged"
            if argument.toString != typeParameterName then
              errors += "contextual Type-argument role diverged"
          case _ => errors += "contextual Type is not one exact unary application"
      case _ => errors += "clause topology is not one Type clause plus one contextual clause"

    raw.tpt match
      case untpd.Ident(name) if name.toString == typeParameterName => ()
      case _ => errors += "result Type role diverged"

    raw.rhs match
      case untpd.Select(untpd.Ident(receiver), selected) =>
        if receiver.toString != contextualName then errors += "body receiver role diverged"
        if selected.toString != methodName then errors += "selected MethodIdentity role diverged"
      case _: untpd.Apply => errors += "body must not contain Apply"
      case _ => errors += "body is not one direct stable Select"

    val trees = allTrees(raw)
    if trees.size != 10 then
      errors += s"raw tree has ${trees.size} nonempty nodes instead of 10"
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
      error("EXACT_RAW_INVARIANT_FAILED", result.mkString("; "))
    )

  private def decodedTermName(value: String, role: String) =
    decoded(value, role).map(termName)

  private def decodedTypeName(value: String, role: String) =
    decoded(value, role).map(typeName)

  private def decoded(
      value: String,
      role: String
  ): Either[ParameterlessDelegatedForwardingMethodUntypedLoweringError, String] =
    DefinitionName
      .fromSource(value)
      .left
      .map(problem =>
        error(
          "NAME_REPRESENTATION_UNSUPPORTED",
          s"$role name lowering failed: ${problem.message}"
        )
      )
      .map(_.decoded)

  private[dotty] def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private[dotty] def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeDef => Vector(value.rhs).filterNot(_.isEmpty)
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
  ): ParameterlessDelegatedForwardingMethodUntypedLoweringError =
    ParameterlessDelegatedForwardingMethodUntypedLoweringError(code, detail)
