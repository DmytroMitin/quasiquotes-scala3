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

/** Source-free parser-independent lowering for the validated AUXify-046 plan. */
private[quasiquotes] object SelfAbstractTypeMemberUntypedLowerer:
  def lower(
      plan: SelfAbstractTypeMemberPlan
  )(using Context): Either[SelfAbstractTypeMemberRawError, untpd.TypeDef] =
    given SourceFile = NoSource

    Option(plan)
      .toRight(error("the self abstract-Type-member plan was null."))
      .flatMap { validated =>
        for
          member <- decodedTypeName(validated.memberName, "outer member")
          alias <- decodedTermName(validated.selfAlias.source, "external self alias")
          base <- decodedTypeName(validated.upperBound.baseName, "upper base")
          refinementAlias <- decodedTypeName(
            validated.upperBound.aliasName,
            "refinement alias"
          )
          selectedMember <- decodedTypeName(
            validated.upperBound.rhs.memberName,
            "selected member"
          )
          selectedAlias <- decodedTermName(
            validated.upperBound.rhs.alias.source,
            "selected prefix"
          )
          lowerAlias <- decodedTermName(
            validated.lowerBound.alias.source,
            "singleton lower-bound alias"
          )
          raw =
            val lower = untpd.SingletonTypeTree(untpd.Ident(lowerAlias))
            val selected = untpd.Select(untpd.Ident(selectedAlias), selectedMember)
            val refinementMember = untpd.TypeDef(refinementAlias, selected)
            val upper = untpd.RefinedTypeTree(
              untpd.Ident(base),
              refinementMember :: Nil
            )
            untpd.TypeDef(
              member,
              untpd.TypeBoundsTree(lower, upper)
            )
          _ <- validateRaw(raw, validated)
        yield raw
      }

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

  private def validateRaw(
      raw: untpd.TypeDef,
      plan: SelfAbstractTypeMemberPlan
  )(using Context): Either[SelfAbstractTypeMemberRawError, Unit] =
    val errors = Vector.newBuilder[String]
    raw.rhs match
      case untpd.TypeBoundsTree(
            untpd.SingletonTypeTree(untpd.Ident(lowerAlias)),
            untpd.RefinedTypeTree(
              untpd.Ident(base),
              List(member: untpd.TypeDef)
            ),
            alias
          ) =>
        if raw.name.toString != decoded(plan.memberName) || raw.mods.hasFlags then
          errors += "outer TypeDef name or flags diverged"
        if lowerAlias.toString != plan.selfAlias.source then
          errors += "singleton lower-bound alias diverged"
        if base.toString != decoded(plan.upperBound.baseName) then
          errors += "upper refinement base diverged"
        if !alias.isEmpty then errors += "TypeBoundsTree alias slot is nonempty"
        if member.name.toString != decoded(plan.memberName) || member.mods.hasFlags then
          errors += "refinement TypeDef name or flags diverged"
        member.rhs match
          case untpd.Select(untpd.Ident(prefix), selected) =>
            if prefix.toString != plan.selfAlias.source then
              errors += "selected prefix diverged"
            if selected.toString != decoded(plan.memberName) then
              errors += "selected member diverged"
          case _ => errors += "refinement RHS is not one direct selected Type"
      case _ => errors += "raw tree is not the exact two-bound one-refinement TypeDef"

    val trees = allTrees(raw)
    if trees.size != 9 then errors += s"raw tree has ${trees.size} nonempty nodes instead of 9"
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

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
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
