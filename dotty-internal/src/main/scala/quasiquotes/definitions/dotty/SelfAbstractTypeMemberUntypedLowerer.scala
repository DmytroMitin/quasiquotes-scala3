package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.definitions.*

private[quasiquotes] final case class SelfAbstractTypeMemberRawError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"

/** Source-free parser-independent lowering for the validated AUXify-046/086 plan. */
private[quasiquotes] object SelfAbstractTypeMemberUntypedLowerer:
  def lower(
      plan: SelfAbstractTypeMemberPlan
  )(using Context): Either[SelfAbstractTypeMemberRawError, untpd.TypeDef] =
    given SourceFile = NoSource

    for
      validated <- Option(plan).toRight(
        error("the self abstract-Type-member plan was null.")
      )
      _ <- SelfAbstractTypeMemberPlan
        .validate(validated)
        .left
        .map(problem => error(problem.message))
      member <- decodedTypeName(validated.memberName, "outer member")
      selfAlias <- decodedTermName(validated.selfAlias.source, "external self alias")
      base <- decodedTypeName(validated.upperBaseName, "upper base")
      lower <- validated.lowerBoundOption match
        case Some(value) =>
          decodedTermName(value.alias.source, "singleton lower-bound alias")
            .map(name => untpd.SingletonTypeTree(untpd.Ident(name)))
        case None => Right(untpd.EmptyTree)
      upper <- validated.fBoundRefinementOption match
        case Some(value) =>
          for
            refinementAlias <- decodedTypeName(value.aliasName, "refinement alias")
            selectedMember <- decodedTypeName(value.rhs.memberName, "selected member")
            selectedAlias <- decodedTermName(value.rhs.alias.source, "selected prefix")
          yield
            val selected = untpd.Select(untpd.Ident(selectedAlias), selectedMember)
            val refinementMember = untpd.TypeDef(refinementAlias, selected)
            untpd.RefinedTypeTree(untpd.Ident(base), refinementMember :: Nil)
        case None => Right(untpd.Ident(base))
      raw = untpd.TypeDef(member, untpd.TypeBoundsTree(lower, upper))
      _ <- validateCandidate(validated, raw)
    yield raw

  private def decodedTypeName(
      value: String,
      role: String
  ) =
    decodedDefinitionName(value, role).map(typeName)

  private def decodedTermName(
      value: String,
      role: String
  ) =
    Option(value)
      .filter(_.nonEmpty)
      .map(termName)
      .toRight(error(s"$role name was absent before raw lowering."))

  private def decodedDefinitionName(
      value: String,
      role: String
  ): Either[SelfAbstractTypeMemberRawError, String] =
    DefinitionName
      .fromSource(value)
      .left
      .map(problem => error(s"$role name lowering failed: ${problem.message}"))
      .map(_.decoded)

  private[dotty] def validateCandidate(
      plan: SelfAbstractTypeMemberPlan,
      raw: untpd.TypeDef
  )(using Context): Either[SelfAbstractTypeMemberRawError, Unit] =
    for
      validated <- Option(plan).toRight(
        error("the self abstract-Type-member plan was null.")
      )
      _ <- SelfAbstractTypeMemberPlan
        .validate(validated)
        .left
        .map(problem => error(problem.message))
      candidate <- Option(raw).toRight(error("the raw candidate was null."))
      _ <- validateRaw(candidate, validated)
    yield ()

  private def validateRaw(
      raw: untpd.TypeDef,
      plan: SelfAbstractTypeMemberPlan
  )(using Context): Either[SelfAbstractTypeMemberRawError, Unit] =
    val errors = Vector.newBuilder[String]
    if raw.name.toString != decoded(plan.memberName) || raw.mods.hasFlags then
      errors += "outer TypeDef name or flags diverged"

    raw.rhs match
      case bounds: untpd.TypeBoundsTree =>
        if !bounds.alias.isEmpty then errors += "TypeBoundsTree alias slot is nonempty"

        plan.lowerBoundOption match
          case Some(value) =>
            bounds.lo match
              case untpd.SingletonTypeTree(untpd.Ident(lowerAlias)) =>
                if lowerAlias.toString != value.alias.source then
                  errors += "singleton lower-bound alias diverged"
              case _ => errors += "present lower edge is not one singleton alias"
          case None =>
            if !bounds.lo.isEmpty then
              errors += "absent lower edge is not exact EmptyTree"

        plan.fBoundRefinementOption match
          case Some(value) =>
            bounds.hi match
              case untpd.RefinedTypeTree(
                    untpd.Ident(base),
                    List(member: untpd.TypeDef)
                  ) =>
                if base.toString != decoded(value.baseName) then
                  errors += "upper refinement base diverged"
                if member.name.toString != decoded(value.aliasName) || member.mods.hasFlags
                then errors += "refinement TypeDef name or flags diverged"
                member.rhs match
                  case untpd.Select(untpd.Ident(prefix), selected) =>
                    if prefix.toString != value.rhs.alias.source then
                      errors += "selected prefix diverged"
                    if selected.toString != decoded(value.rhs.memberName) then
                      errors += "selected member diverged"
                  case _ => errors += "refinement RHS is not one direct selected Type"
              case _ => errors += "present F-bound is not one exact alias refinement"
          case None =>
            bounds.hi match
              case untpd.Ident(base) if base.toString == decoded(plan.upperBaseName) => ()
              case _ => errors += "absent F-bound is not one direct upper-base Ident"
      case _ => errors += "raw tree RHS is not one TypeBoundsTree"

    val trees = allTrees(raw)
    val expectedCount =
      3 + Option.when(plan.lowerBoundOption.nonEmpty)(2).getOrElse(0) +
        Option.when(plan.fBoundRefinementOption.nonEmpty)(4).getOrElse(0)
    if trees.size != expectedCount then
      errors += s"raw tree has ${trees.size} nonempty nodes instead of $expectedCount"
    trees.foreach { tree =>
      if tree.source.exists || tree.span.exists || tree.symbol != NoSymbol then
        errors += s"${tree.getClass.getSimpleName} is not source/span/symbol free"
      if tree.isInstanceOf[untpd.TypedSplice] then
        errors += "raw tree contains TypedSplice"
    }

    val result = errors.result()
    Either.cond(result.isEmpty, (), error(result.mkString("; ")))

  private def decoded(value: String): String =
    DefinitionName.fromSource(value).fold(_ => value, _.decoded)

  private[dotty] def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private[dotty] def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.SingletonTypeTree => Vector(value.ref)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def error(detail: String): SelfAbstractTypeMemberRawError =
    SelfAbstractTypeMemberRawError("RAW_LOWERING_FAILED", detail)
