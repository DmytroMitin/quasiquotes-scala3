package quasiquotes.phase74

private[phase74] object BinderSemanticPrototype:
  final case class BinderId(value: Int) derives CanEqual
  final case class FreeId(value: String) derives CanEqual

  enum Surface derives CanEqual:
    case Name(displayName: String, freeId: FreeId)
    case Number(value: Int)
    case Add(left: Surface, right: Surface)
    case Lambda(displayName: String, parameterType: String, body: Surface)
    case External(term: Semantic)

  enum Semantic derives CanEqual:
    case Bound(id: BinderId, displayName: String)
    case Free(id: FreeId, displayName: String)
    case Number(value: Int)
    case Add(left: Semantic, right: Semantic)
    case Lambda(id: BinderId, displayName: String, parameterType: String, body: Semantic)

  private enum AlphaKey derives CanEqual:
    case Bound(distanceFromInnermost: Int)
    case Free(id: FreeId)
    case Number(value: Int)
    case Add(left: AlphaKey, right: AlphaKey)
    case Lambda(parameterType: String, body: AlphaKey)
    case ScopeMismatch(id: BinderId)

  def resolve(surface: Surface): Semantic =
    val (semantic, _) = resolveFrom(surface, Nil, 0)
    semantic

  def alphaEquivalent(left: Semantic, right: Semantic): Boolean =
    alphaEquivalentUnder(left, Nil, right, Nil)

  def alphaEquivalentUnder(
      left: Semantic,
      leftAmbientScope: List[BinderId],
      right: Semantic,
      rightAmbientScope: List[BinderId]
  ): Boolean =
    canonical(left, leftAmbientScope) == canonical(right, rightAmbientScope)

  private def resolveFrom(
      surface: Surface,
      scope: List[(String, BinderId)],
      nextId: Int
  ): (Semantic, Int) =
    surface match
      case Surface.Name(displayName, freeId) =>
        val reference = scope.reverseIterator
          .collectFirst { case (`displayName`, id) => Semantic.Bound(id, displayName) }
          .getOrElse(Semantic.Free(freeId, displayName))
        (reference, nextId)
      case Surface.Number(value) => (Semantic.Number(value), nextId)
      case Surface.Add(left, right) =>
        val (resolvedLeft, afterLeft) = resolveFrom(left, scope, nextId)
        val (resolvedRight, afterRight) = resolveFrom(right, scope, afterLeft)
        (Semantic.Add(resolvedLeft, resolvedRight), afterRight)
      case Surface.Lambda(displayName, parameterType, body) =>
        val id = BinderId(nextId)
        val (resolvedBody, afterBody) =
          resolveFrom(body, scope :+ (displayName -> id), nextId + 1)
        (Semantic.Lambda(id, displayName, parameterType, resolvedBody), afterBody)
      case Surface.External(term) =>
        // External terms are already resolved in their source scope. They are not
        // re-resolved by display name under newly introduced binders.
        (term, nextId)

  private def canonical(term: Semantic, scope: List[BinderId]): AlphaKey =
    term match
      case Semantic.Bound(id, _) =>
        val reverseIndex = scope.reverseIterator.indexOf(id)
        if reverseIndex >= 0 then AlphaKey.Bound(reverseIndex)
        else AlphaKey.ScopeMismatch(id)
      case Semantic.Free(id, _) => AlphaKey.Free(id)
      case Semantic.Number(value) => AlphaKey.Number(value)
      case Semantic.Add(left, right) =>
        AlphaKey.Add(canonical(left, scope), canonical(right, scope))
      case Semantic.Lambda(id, _, parameterType, body) =>
        AlphaKey.Lambda(parameterType, canonical(body, scope :+ id))
