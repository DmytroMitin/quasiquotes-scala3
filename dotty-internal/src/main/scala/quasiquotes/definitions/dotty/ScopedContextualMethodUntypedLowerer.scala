package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{TermName, TypeName, termName, typeName}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.definitions.*
import quasiquotes.parser.BinderId

private[quasiquotes] final case class ScopedContextualMethodRawError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"

/** Parser-free raw lowering for the validated Phase-134 internal plan. */
private[quasiquotes] object ScopedContextualMethodUntypedLowerer:
  import ScopedType.*

  def lower(
      plan: ScopedContextualMethodPlan
  )(using Context): Either[ScopedContextualMethodRawError, untpd.DefDef] =
    given SourceFile = NoSource

    Option(plan)
      .toRight(error("the scoped contextual-method plan was null."))
      .flatMap { validated =>
        for
          methodName <- lowerTermName(validated.methodDisplayName, "method")
          typeParameters <- lowerTypeParameters(validated.typeParameters)
          contextualName <- lowerTermName(
            validated.contextualDisplayName,
            "contextual parameter"
          )
          contextualType <- lowerApplied(validated.contextualType, validated)
          resultType <- lowerRefinement(validated.resultType, validated)
          bodyName <- contextualNameFor(
            validated.bodyTermBinderId,
            validated,
            "method body"
          )
          contextualParameter = untpd
            .ValDef(contextualName, contextualType, untpd.EmptyTree)
            .withMods(untpd.Modifiers(Flags.Param | Flags.Given))
          raw = untpd
            .DefDef(
              methodName,
              List(typeParameters.toList, contextualParameter :: Nil),
              resultType,
              untpd.Ident(bodyName)
            )
            .withMods(untpd.Modifiers(Flags.Method))
          _ <- validateRaw(raw, validated)
        yield raw
      }

  private def lowerTypeParameters(
      parameters: Vector[ScopedTypeParameter]
  )(using SourceFile): Either[ScopedContextualMethodRawError, Vector[untpd.TypeDef]] =
    parameters.foldLeft(
      Right(Vector.empty): Either[
        ScopedContextualMethodRawError,
        Vector[untpd.TypeDef]
      ]
    ) { (accumulated, parameter) =>
      for
        values <- accumulated
        parameterName <- lowerTypeName(parameter.displayName, "Type parameter")
        upperName <- parameter.upperBound match
          case SourceName(value) => lowerTypeName(value, "Type-parameter upper bound")
          case _ => Left(error("a validated Type parameter lost its source-named upper bound."))
        raw = untpd
          .TypeDef(
            parameterName,
            untpd.TypeBoundsTree(
              untpd.EmptyTree,
              untpd.Ident(upperName)
            )
          )
          .withMods(untpd.Modifiers(Flags.Param))
      yield values :+ raw
    }

  private def lowerApplied(
      applied: Applied,
      plan: ScopedContextualMethodPlan
  )(using SourceFile): Either[ScopedContextualMethodRawError, untpd.AppliedTypeTree] =
    for
      constructor <- applied.constructor match
        case SourceName(value) => lowerTypeName(value, "applied Type constructor")
        case _ => Left(error("a validated applied Type lost its source-name constructor."))
      arguments <- applied.arguments.foldLeft(
        Right(List.empty): Either[ScopedContextualMethodRawError, List[untpd.Tree]]
      ) { (accumulated, argument) =>
        for
          values <- accumulated
          argumentName <- argument match
            case TypeParameterReference(binderId, _) =>
              typeParameterNameFor(binderId, plan)
            case _ => Left(error("a validated applied Type lost a scoped binder reference."))
        yield values :+ untpd.Ident(argumentName)
      }
    yield untpd.AppliedTypeTree(untpd.Ident(constructor), arguments)

  private def lowerRefinement(
      refinement: Refinement,
      plan: ScopedContextualMethodPlan
  )(using SourceFile): Either[ScopedContextualMethodRawError, untpd.RefinedTypeTree] =
    for
      base <- refinement.base match
        case value: Applied => lowerApplied(value, plan)
        case _ => Left(error("a validated result refinement lost its applied base."))
      memberName <- lowerTypeName(plan.refinementMember.memberName, "refinement member")
      selected <- plan.refinementMember.rhs match
        case DirectStableSelected(prefixBinderId, memberExpectation) =>
          for
            prefixName <- contextualNameFor(prefixBinderId, plan, "selected Type prefix")
            selectedName <- lowerTypeName(memberExpectation, "selected Type member")
          yield untpd.Select(untpd.Ident(prefixName), selectedName)
        case _ => Left(error("a validated refinement member lost its direct selected-Type RHS."))
      member = untpd.TypeDef(memberName, selected)
    yield untpd.RefinedTypeTree(base, member :: Nil)

  private def typeParameterNameFor(
      binderId: BinderId,
      plan: ScopedContextualMethodPlan
  ): Either[ScopedContextualMethodRawError, TypeName] =
    plan.typeParameters
      .find(_.binderId == binderId)
      .toRight(error(s"Type binder ${String.valueOf(binderId)} detached before raw lowering."))
      .flatMap(parameter => lowerTypeName(parameter.displayName, "Type-parameter reference"))

  private def contextualNameFor(
      binderId: BinderId,
      plan: ScopedContextualMethodPlan,
      role: String
  ): Either[ScopedContextualMethodRawError, TermName] =
    Either
      .cond(
        binderId == plan.contextualTermBinderId,
        plan.contextualDisplayName,
        error(s"$role detached from the contextual Term binder before raw lowering.")
      )
      .flatMap(value => lowerTermName(value, role))

  private def lowerTermName(
      value: String,
      role: String
  ): Either[ScopedContextualMethodRawError, TermName] =
    decodedName(value, role).map(termName)

  private def lowerTypeName(
      value: String,
      role: String
  ): Either[ScopedContextualMethodRawError, TypeName] =
    decodedName(value, role).map(typeName)

  private def decodedName(
      value: String,
      role: String
  ): Either[ScopedContextualMethodRawError, String] =
    DefinitionName
      .fromSource(value)
      .left
      .map(problem => error(s"$role name lowering failed: ${problem.message}"))
      .map(_.decoded)

  private def validateRaw(
      raw: untpd.DefDef,
      plan: ScopedContextualMethodPlan
  )(using Context): Either[ScopedContextualMethodRawError, Unit] =
    val errors = Vector.newBuilder[String]
    if raw.name.toString != decoded(plan.methodDisplayName) ||
        raw.mods.flags != Flags.Method
    then errors += "method name or flags diverged"
    if raw.paramss.map(_.size) != List(2, 1) then
      errors += "parameter clauses diverged from the exact two-plus-one shape"
    if raw.leadingTypeParams.map(_.name.toString) !=
        plan.typeParameters.map(parameter => decoded(parameter.displayName)).toList
    then errors += "Type-parameter order or names diverged"
    raw.leadingTypeParams.zip(plan.typeParameters).foreach { case (parameter, expected) =>
      if parameter.mods.flags != Flags.Param then errors += "Type-parameter flags diverged"
      parameter.rhs match
        case untpd.TypeBoundsTree(lo, untpd.Ident(hi), alias) =>
          val expectedBound = expected.upperBound.asInstanceOf[SourceName].value
          if !lo.isEmpty || !alias.isEmpty || hi.toString != decoded(expectedBound) then
            errors += "upper-only Type bounds diverged"
        case _ => errors += "Type parameter does not contain one upper-only TypeBoundsTree"
    }
    raw.trailingParamss match
      case List(List(parameter: untpd.ValDef)) =>
        if parameter.name.toString != decoded(plan.contextualDisplayName) ||
            parameter.mods.flags != (Flags.Param | Flags.Given) ||
            !parameter.rhs.isEmpty
        then errors += "contextual parameter diverged"
      case _ => errors += "contextual clause diverged"
    allTrees(raw).foreach { tree =>
      if tree.source.exists || tree.span.exists || tree.symbol != NoSymbol then
        errors += s"${tree.getClass.getSimpleName} is not source/span/symbol free"
      if tree.isInstanceOf[untpd.TypedSplice] then errors += "raw tree contains TypedSplice"
    }
    val result = errors.result()
    Either.cond(result.isEmpty, (), error(result.mkString("; ")))

  private def decoded(value: String): String =
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
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def error(detail: String): ScopedContextualMethodRawError =
    ScopedContextualMethodRawError("RAW_LOWERING_FAILED", detail)
