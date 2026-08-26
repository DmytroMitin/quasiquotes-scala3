package quasiquotes.definitions

import quasiquotes.parser.BinderId
import quasiquotes.types.ResolvedTypeNameId

private[quasiquotes] final case class DefinitionScopedSelectedTypeError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"

/** A selected Type value owned by exactly one enclosing definition scope.
  *
  * Equality is intentionally not defined on this detached value. The owning
  * [[DefinitionScopedSelectedTypePlan]] performs scope-aware comparison.
  */
private[quasiquotes] final class ScopedSelectedType private[definitions] (
    private[definitions] val scopeToken: AnyRef,
    private[quasiquotes] val prefixBinder: BinderId,
    private[quasiquotes] val member: ResolvedTypeNameId
)

/** Enclosing carrier for one direct project-binder selected Type.
  *
  * Binder values remain local to this carrier. Cross-definition comparison is
  * alpha-structural by binder position and never compares detached BinderIds.
  */
private[quasiquotes] final class DefinitionScopedSelectedTypePlan private (
    private val scopeToken: AnyRef,
    private[quasiquotes] val binders: Vector[BinderId],
    val selectedType: ScopedSelectedType
):
  val prefixBinderPosition: Int =
    binders.indexOf(selectedType.prefixBinder)

  private[quasiquotes] def memberIdentity: ResolvedTypeNameId =
    selectedType.member

  private[quasiquotes] def binderCount: Int = binders.size

  private[quasiquotes] def accepts(
      prefixPosition: Int,
      member: ResolvedTypeNameId
  ): Boolean =
    prefixBinderPosition == prefixPosition && memberIdentity == member

  def owns(value: ScopedSelectedType): Boolean =
    value != null && (value.scopeToken eq scopeToken)

  def alphaEquivalentTo(
      other: DefinitionScopedSelectedTypePlan
  ): Boolean =
    other != null &&
      binderCount == other.binderCount &&
      prefixBinderPosition == other.prefixBinderPosition &&
      memberIdentity == other.memberIdentity

private[quasiquotes] object DefinitionScopedSelectedTypePlan:
  def fromSingleParameterDefinition(
      definition: ConstructedDefinition.SingleParameterDef,
      member: ResolvedTypeNameId
  ): Either[DefinitionScopedSelectedTypeError, DefinitionScopedSelectedTypePlan] =
    create(
      Vector(definition.parameterBinderId),
      definition.parameterBinderId,
      member
    )

  def fromTwoParameterDefinition(
      definition: ConstructedDefinition.TwoParameterDef,
      prefixBinder: BinderId,
      member: ResolvedTypeNameId
  ): Either[DefinitionScopedSelectedTypeError, DefinitionScopedSelectedTypePlan] =
    create(
      Vector(
        definition.firstParameterBinderId,
        definition.secondParameterBinderId
      ),
      prefixBinder,
      member
    )

  def create(
      binders: Vector[BinderId],
      prefixBinder: BinderId,
      member: ResolvedTypeNameId
  ): Either[DefinitionScopedSelectedTypeError, DefinitionScopedSelectedTypePlan] =
    if binders.isEmpty then
      Left(
        error(
          "STABLE_SELECTED_TYPE_SCOPE_INVALID",
          "a definition-scoped selected Type requires at least one enclosing binder."
        )
      )
    else if binders.distinct.size != binders.size then
      Left(
        error(
          "STABLE_SELECTED_TYPE_SCOPE_INVALID",
          "enclosing definition binder identities must be distinct."
        )
      )
    else if !binders.contains(prefixBinder) then
      Left(
        error(
          "STABLE_SELECTED_TYPE_PREFIX_UNBOUND",
          "the selected Type prefix must reference a binder declared by the enclosing definition."
        )
      )
    else if member == null then
      Left(
        error(
          "STABLE_SELECTED_TYPE_MEMBER_INVALID",
          "the selected Type member declaration identity must be present."
        )
      )
    else
      val token = new Object
      val selected = new ScopedSelectedType(token, prefixBinder, member)
      Right(new DefinitionScopedSelectedTypePlan(token, binders, selected))

  private def error(
      code: String,
      detail: String
  ): DefinitionScopedSelectedTypeError =
    DefinitionScopedSelectedTypeError(code, detail)
