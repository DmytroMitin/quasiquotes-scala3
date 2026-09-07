package quasiquotes.types

import scala.quoted.*
import scala.util.control.NonFatal
import io.github.dmytromitin.allowexperimental.allowExperimental

/** A private class-application validator, with no normalization or Type authoring. */
private[types] object ReflectedTypeApplication:
  private final case class Kind(parameters: List[Kind])
  private val proper = Kind(Nil)

  // Permission ends at this ordinary, non-inline leaf. Public callers need no marker.
  @allowExperimental
  private def parameterInfo(using q: Quotes)(symbol: q.reflect.Symbol): q.reflect.TypeRepr =
    symbol.info

  def build(using q: Quotes)(
      constructor: q.reflect.TypeRepr,
      arguments: Seq[q.reflect.TypeRepr]
  ): Either[String, q.reflect.TypeRepr] =
    import q.reflect.*

    def invalidConstructor = Left("INVALID_TYPE_CONSTRUCTOR_KIND: expected an unapplied class Type constructor with supported parameter kinds.")
    def wrongKind = Left("WRONG_TYPE_ARGUMENT_KIND: a Type argument does not have the required supported kind.")

    def boundKind(bounds: TypeBounds): Option[Kind] =
      if !(bounds.low =:= TypeRepr.of[Nothing]) then None
      else upperKind(bounds.hi)

    def upperKind(upper: TypeRepr): Option[Kind] = upper match
      case lambda: TypeLambda =>
        if !(lambda.resType =:= TypeRepr.of[Any]) then None
        else sequenceKinds(lambda.paramBounds.map(boundKind)).map(Kind.apply)
      case t if t =:= TypeRepr.of[Any] => Some(proper)
      case _ => None

    def sequenceKinds(kinds: List[Option[Kind]]): Option[List[Kind]] =
      kinds.foldRight(Option(List.empty[Kind]))((next, rest) => for k <- next; ks <- rest yield k :: ks)

    def staticOwner(symbol: Symbol): Boolean =
      symbol == Symbol.noSymbol || symbol.flags.is(Flags.Package) ||
        (symbol.flags.is(Flags.Module) && staticOwner(symbol.owner))

    def supportedPrefix(prefix: TypeRepr): Boolean = prefix match
      case term: TermRef =>
        term.termSymbol.flags.is(Flags.Module) && supportedPrefix(term.qualifier)
      case ref: TypeRef =>
        (ref.typeSymbol.flags.is(Flags.Package) || ref.typeSymbol.flags.is(Flags.Module)) &&
          supportedPrefix(ref.qualifier)
      case self: ThisType =>
        staticOwner(self.tref.typeSymbol)
      case _: NoPrefix => true
      case _ => false

    def classKind(tpe: TypeRepr): Option[Kind] = tpe match
      case ref: TypeRef if ref.typeSymbol.isClassDef && supportedPrefix(ref.qualifier) =>
        val parameters = ref.typeSymbol.primaryConstructor.paramSymss.flatten.filter(_.isTypeParam)
        sequenceKinds(parameters.map { parameter =>
          parameterInfo(parameter) match
            case bounds: TypeBounds => boundKind(bounds)
            case _ => None
        }).map(Kind.apply)
      case _ => None

    def argumentKind(tpe: TypeRepr): Option[Kind] = tpe match
      case AppliedType(c, args) =>
        classKind(c).filter(k => k.parameters.nonEmpty && k.parameters.size == args.size).flatMap { kind =>
          Option.when(args.zip(kind.parameters).forall((arg, expected) => argumentKind(arg).contains(expected)))(proper)
        }
      case _: TypeRef => classKind(tpe)
      case _ => None

    if constructor == null then invalidConstructor
    else if arguments == null then
      Left("TYPE_SEQUENCE_RANK_MISMATCH: the Type argument sequence must not be null.")
    else
      // Snapshot the collection once; never copy, dealias or reconstruct a TypeRepr.
      val preflight = try
        val snapshot = arguments.toList
        if snapshot.exists(_ == null) then wrongKind
        else classKind(constructor) match
          case None => invalidConstructor
          case Some(kind) if kind.parameters.isEmpty => invalidConstructor
          case Some(kind) if kind.parameters.size != snapshot.size =>
            Left(s"WRONG_TYPE_ARGUMENT_COUNT: constructor expects ${kind.parameters.size} Type argument(s), received ${snapshot.size}.")
          case Some(kind) =>
            if snapshot.zip(kind.parameters).forall((arg, expected) => argumentKind(arg).contains(expected)) then Right(snapshot)
            else wrongKind
      catch
        case NonFatal(_) => invalidConstructor

      preflight.flatMap { snapshot =>
        try
          verifyResult(using q)(constructor, snapshot, AppliedType(constructor, snapshot))
        catch
          case NonFatal(_) => Left(invariantDiagnostic)
      }

  private val invariantDiagnostic =
    "TYPE_REFLECTION_APPLICATION_INVARIANT: validated reflection application did not preserve its inputs."

  /** The postcondition shared by every successful reflection application. */
  def verifyResult(using q: Quotes)(
      constructor: q.reflect.TypeRepr,
      arguments: List[q.reflect.TypeRepr],
      result: q.reflect.TypeRepr
  ): Either[String, q.reflect.TypeRepr] =
    import q.reflect.*
    def sameObject(left: TypeRepr, right: TypeRepr): Boolean = (left, right) match
      case (a: AnyRef, b: AnyRef) => a eq b
      case _ => false
    result match
      case AppliedType(found, elements) if sameObject(found, constructor) &&
          elements.size == arguments.size && elements.zip(arguments).forall((a, b) => sameObject(a, b)) => Right(result)
      case _ => Left(invariantDiagnostic)
