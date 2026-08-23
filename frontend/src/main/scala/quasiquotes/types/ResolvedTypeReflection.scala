package quasiquotes.types

import scala.quoted.*

private[quasiquotes] object ResolvedTypeReflection:
  def bindingFromWitness(using q: Quotes)(
      witness: q.reflect.TypeRepr
  ): Either[TypeQuasiquoteError, GlobalSelectedTypeEnvironment.Binding[q.reflect.TypeRepr]] =
    import q.reflect.*

    witness match
      case AppliedType(constructor: TypeRef, arguments) =>
        deriveTypeRef(constructor).map { id =>
          GlobalSelectedTypeEnvironment.Binding(
            id,
            constructor,
            GlobalSelectedTypeEnvironment.WitnessRole.Constructor(arguments.size)
          )
        }
      case typeRef: TypeRef =>
        deriveTypeRef(typeRef).map { id =>
          GlobalSelectedTypeEnvironment.Binding(
            id,
            witness,
            GlobalSelectedTypeEnvironment.WitnessRole.Terminal
          )
        }
      case _ =>
        Left(
          TypeQuasiquoteError(
            TypeNameResolutionDiagnostics.compilerShapeUnsupported(
              "typed witness is not a terminal TypeRef or an applied constructor witness"
            )
          )
        )

  def deriveTypeRef(using q: Quotes)(
      typeRef: q.reflect.TypeRef
  ): Either[TypeQuasiquoteError, ResolvedTypeNameId] =
    for
      _ <- validatePrefix(typeRef.qualifier, typeRef.name)
      id <- deriveFromOwner(typeRef.typeSymbol.owner, typeRef.name)
    yield id

  private[quasiquotes] def deriveFromOwner(using q: Quotes)(
      owner: q.reflect.Symbol,
      terminalName: String
  ): Either[TypeQuasiquoteError, ResolvedTypeNameId] =
    for
      owners <- deriveOwners(owner)
      id <- safeIdentity(owners, terminalName)
    yield id

  private def validatePrefix(using q: Quotes)(
      prefix: q.reflect.TypeRepr,
      terminalName: String
  ): Either[TypeQuasiquoteError, Unit] =
    import q.reflect.*

    prefix match
      case term: TermRef if !term.termSymbol.flags.is(Flags.Module) =>
        Left(TypeQuasiquoteError(TypeNameResolutionDiagnostics.unstableTermPrefix(terminalName)))
      case term: TermRef =>
        validatePrefix(term.qualifier, terminalName)
      case ref: TypeRef =>
        validatePrefix(ref.qualifier, terminalName)
      case thisType: ThisType =>
        val owner = thisType.tref.typeSymbol
        if owner.flags.is(Flags.Package) || owner.flags.is(Flags.Module) then Right(())
        else Left(TypeQuasiquoteError(TypeNameResolutionDiagnostics.unstableTermPrefix(terminalName)))
      case _: NoPrefix => Right(())
      case _ =>
        Left(
          TypeQuasiquoteError(
            TypeNameResolutionDiagnostics.compilerShapeUnsupported(
              s"prefix category for `$terminalName` is outside the global package/type/module tranche"
            )
          )
        )

  private def deriveOwners(using q: Quotes)(
      initial: q.reflect.Symbol
  ): Either[TypeQuasiquoteError, Vector[ResolvedTypeOwnerSegment]] =
    import q.reflect.*

    def loop(
        current: Symbol,
        accumulated: List[ResolvedTypeOwnerSegment]
    ): Either[TypeQuasiquoteError, List[ResolvedTypeOwnerSegment]] =
      if current == Symbol.noSymbol || current.owner == Symbol.noSymbol then Right(accumulated)
      else ownerSegment(current).flatMap(segment => loop(current.owner, segment :: accumulated))

    loop(initial, Nil).map(_.toVector)

  private def ownerSegment(using q: Quotes)(
      symbol: q.reflect.Symbol
  ): Either[TypeQuasiquoteError, ResolvedTypeOwnerSegment] =
    import q.reflect.*

    val classified =
      if symbol.flags.is(Flags.Package) then
        Some(ResolvedTypeOwnerKind.Package -> symbol.name)
      else if symbol.flags.is(Flags.Module) then
        val module = symbol.companionModule
        val sourceName = if module == Symbol.noSymbol then symbol.name else module.name
        Some(ResolvedTypeOwnerKind.Module -> sourceName)
      else if symbol.isType then
        Some(ResolvedTypeOwnerKind.Type -> symbol.name)
      else None

    classified match
      case Some((kind, name)) => safeOwner(kind, name)
      case None =>
        Left(
          TypeQuasiquoteError(
            TypeNameResolutionDiagnostics.resolvedFamilyUnsupported("local or non-static owner")
          )
        )

  private def safeOwner(
      kind: ResolvedTypeOwnerKind,
      name: String
  ): Either[TypeQuasiquoteError, ResolvedTypeOwnerSegment] =
    try Right(ResolvedTypeOwnerSegment(kind, name))
    catch
      case _: IllegalArgumentException =>
        Left(TypeQuasiquoteError(TypeNameResolutionDiagnostics.compilerShapeUnsupported("owner name requires unsupported compiler encoding")))

  private def safeIdentity(
      owners: Vector[ResolvedTypeOwnerSegment],
      terminalName: String
  ): Either[TypeQuasiquoteError, ResolvedTypeNameId] =
    try Right(ResolvedTypeNameId(owners, terminalName))
    catch
      case _: IllegalArgumentException =>
        Left(TypeQuasiquoteError(TypeNameResolutionDiagnostics.compilerShapeUnsupported("terminal name or owner path requires unsupported compiler encoding")))
