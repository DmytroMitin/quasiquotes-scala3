package quasiquotes.construct

import scala.util.control.NonFatal

import scala.quoted.Quotes

object IdentifierResolver:
  def resolve(name: String)(using q: Quotes): Either[QuasiquoteError, q.reflect.Term] =
    import q.reflect.*

    def asResolvedTerm(symbol: Symbol): Term =
      normalizeTerm(Ref(symbol))

    def ownerCandidate(owner: Symbol): Option[Symbol] =
      val fromParams = owner.paramSymss.flatten.find(sym => sym.isTerm && sym.name == name)
      val fromDecls = owner.declarations.find(sym => sym.isTerm && sym.name == name)
      val fromField =
        Option.when(owner.isClassDef || owner.isPackageDef) {
          owner.fieldMember(name)
        }.filter(_.exists)
      val fromMethod =
        Option.when(owner.isClassDef || owner.isPackageDef) {
          owner.methodMember(name).headOption.getOrElse(Symbol.noSymbol)
        }.filter(_.exists)
      fromParams.orElse(fromDecls).orElse(fromField).orElse(fromMethod)

    def loop(owner: Symbol): Option[Symbol] =
      if !owner.exists then None
      else
        ownerCandidate(owner).orElse(loop(owner.owner))

    loop(Symbol.spliceOwner) match
      case Some(symbol) => Right(asResolvedTerm(symbol))
      case None =>
        Left(QuasiquoteError.UnresolvedIdentifier(name))

  private def normalizeTerm(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term.tpe.widen match
      case mt: MethodType if mt.paramNames.isEmpty => term.appliedToNone
      case _ => term
