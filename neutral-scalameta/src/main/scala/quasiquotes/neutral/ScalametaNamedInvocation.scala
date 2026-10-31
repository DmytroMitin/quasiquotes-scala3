package quasiquotes.neutral

import quasiquotes.definitions.DefinitionName
import quasiquotes.parser.TermShape
import scala.annotation.nowarn
import scala.meta.*
import scala.util.control.NonFatal

/** Private source-role staging; deliberately unrelated to positional TermShape roots. */
@nowarn("cat=deprecation")
private[neutral] object ScalametaNamedInvocation:
  final case class Error(code: String, detail: String) derives CanEqual
  private type Result[A] = Either[Error,A]
  enum Kind:
    case Apply, New

  final class Named private[ScalametaNamedInvocation] (val label: DefinitionName, val value: TermShape):
    require(validLabel(label) && fragment(value).isRight, "invalid private named argument")
    override def equals(other: Any): Boolean = other match
      case n: Named => label == n.label && value == n.value
      case _ => false
    override def hashCode(): Int = (label,value).hashCode

  final class Invocation private[ScalametaNamedInvocation] (val kind: Kind, val target: DefinitionName, val arguments: List[Named]):
    require(validate(kind,target,arguments).isRight, "invalid private named invocation")
    override def equals(other: Any): Boolean = other match
      case i: Invocation => kind == i.kind && target == i.target && arguments == i.arguments
      case _ => false
    override def hashCode(): Int = (kind,target,arguments).hashCode

  final case class Origin(tokens: String, span: Option[(Int,Int)])
  final case class Projected(invocation: Invocation, source: Origin, arguments: List[Origin])
  private def error(code: String): Error = Error(code,"outside the private ordinary named-only invocation family")
  private def safe[A](code: String)(f: => Result[A]): Result[A] =
    try f catch case NonFatal(_) => Left(error(code))
  private def name(s: String, code: String): Result[DefinitionName] =
    DefinitionName.fromSource(s).left.map(_ => error(code)).flatMap(n => Either.cond(validLabel(n),n,error(code)))
  private def validLabel(n: DefinitionName): Boolean =
    n != null && n.source != null && n.source == n.decoded && DefinitionName.fromSource(n.source).toOption.contains(n)
  private def sourceName(n: Name, code: String): Result[DefinitionName] =
    if n == null || n.tokens.map(_.text).mkString != n.value then Left(error(code)) else name(n.value,code)
  private def origin(t: Tree): Origin = Origin(t.tokens.map(_.text).mkString,
    if t.pos == Position.None then None else Some((t.pos.start,t.pos.end)))
  private def collect[A,B](xs: List[A], code: String)(f: A => Result[B]): Result[List[B]] =
    Option(xs).toRight(error(code)).flatMap(_.foldLeft[Result[List[B]]](Right(Nil))((a,x) => for { acc <- a; b <- f(x) } yield b :: acc).map(_.reverse))

  // This is only a binder filter; ordinary syntax/name/Type admission belongs to the existing authorer.
  private def binderFree(s: TermShape): Boolean =
    def all(xs: List[TermShape]): Boolean = xs != null && xs.forall(binderFree)
    s match
      case TermShape.Identifier(_,false) | TermShape.Literal(_) => true
      case TermShape.Select(q,_) => binderFree(q)
      case TermShape.Apply(f,args) => binderFree(f) && all(args)
      case TermShape.New(_,args) => all(args)
      case TermShape.Infix(l,_,r) => binderFree(l) && binderFree(r)
      case TermShape.Unary(_,v) => binderFree(v)
      case TermShape.Typed(v,_) => binderFree(v)
      case TermShape.Tuple(xs) => all(xs)
      case TermShape.If(c,t,e) => binderFree(c) && binderFree(t) && binderFree(e)
      case TermShape.InterpolatedString(_,_,xs) => all(xs)
      case TermShape.Parenthesized(v) => binderFree(v)
      case _ => false
  private def fragment(s: TermShape): Result[Term] = safe("RHS_UNSUPPORTED") {
    if !binderFree(s) then Left(error("RHS_UNSUPPORTED"))
    else ScalametaTermShapeAuthoring.author(s).left.map(_ => error("RHS_UNSUPPORTED"))
  }
  def named(label: String, value: TermShape): Result[Named] = safe("RHS_UNSUPPORTED") {
    for { n <- name(label,"LABEL_UNSUPPORTED"); _ <- fragment(value) } yield new Named(n,value)
  }
  private def validate(k: Kind, target: DefinitionName, args: List[Named]): Result[Unit] = safe("RHS_UNSUPPORTED") {
    for
      _ <- Either.cond(((k eq Kind.Apply) || (k eq Kind.New)) && validLabel(target),(),error("TARGET_UNSUPPORTED"))
      xs <- Option(args).toRight(error("NAMED_ONLY_REQUIRED"))
      _ <- Either.cond(xs.nonEmpty,(),error("NAMED_ONLY_REQUIRED"))
      _ <- collect(xs,"NAMED_ONLY_REQUIRED") { a =>
        if a == null then Left(error("LABEL_UNSUPPORTED"))
        else for { _ <- Either.cond(validLabel(a.label),(),error("LABEL_UNSUPPORTED")); _ <- fragment(a.value) } yield ()
      }
      _ <- Either.cond(xs.map(_.label).distinct.size == xs.size,(),error("DUPLICATE_LABEL"))
    yield ()
  }
  def invocation(k: Kind, target: String, args: List[Named]): Result[Invocation] =
    for { n <- name(target,"TARGET_UNSUPPORTED"); _ <- validate(k,n,args) } yield new Invocation(k,n,args)

  def project(source: Term): Result[Projected] = safe("INVOCATION_CONTEXT_REQUIRED") {
    def clause(k: Kind, n: Name, c: Term.ArgClause): Result[Projected] =
      for
        target <- sourceName(n,"TARGET_UNSUPPORTED")
        present <- Option(c).toRight(error("NAMED_ONLY_REQUIRED"))
        _ <- Either.cond(present.mod.isEmpty,(),error("CONTEXTUAL_UNSUPPORTED"))
        _ <- Either.cond(present.values.nonEmpty,(),error("NAMED_ONLY_REQUIRED"))
        args <- collect(present.values,"NAMED_ONLY_REQUIRED") {
          case a: Term.Assign => a.lhs match
            case label: Term.Name =>
              for
                n <- sourceName(label,"LABEL_UNSUPPORTED")
                rhs <- ScalametaTermProjection.project(a.rhs).left.map(_ => error("RHS_UNSUPPORTED"))
                value <- named(n.source,rhs.shape)
              yield value
            case _ => Left(error("LABEL_UNSUPPORTED"))
          case _ => Left(error("NAMED_ONLY_REQUIRED"))
        }
        _ <- validate(k,target,args)
      yield Projected(new Invocation(k,target,args),origin(source),present.values.map(origin))
    source match
      case null => Left(error("MISSING_INVOCATION"))
      case a: Term.Apply => a.fun match
        case _: Term.Apply => Left(error("CLAUSE_COUNT_UNSUPPORTED"))
        case n: Term.Name => clause(Kind.Apply,n,a.argClause)
        case _ => Left(error("TARGET_UNSUPPORTED"))
      case n: Term.New =>
        if n.init == null || n.init.argClauses == null || n.init.argClauses.size != 1 then Left(error("CLAUSE_COUNT_UNSUPPORTED"))
        else n.init.tpe match
          case t: Type.Name if n.init.name.isInstanceOf[Name.Anonymous] => clause(Kind.New,t,n.init.argClauses.head)
          case _ => Left(error("TARGET_UNSUPPORTED"))
      case _ => Left(error("INVOCATION_CONTEXT_REQUIRED"))
  }
  def author(value: Invocation): Result[Term] = safe("INVOCATION_INVALID") {
    for
      v <- Option(value).toRight(error("MISSING_INVOCATION"))
      _ <- validate(v.kind,v.target,v.arguments)
      args <- collect(v.arguments,"NAMED_ONLY_REQUIRED")(a => fragment(a.value).map(t => Term.Assign(Term.Name(a.label.decoded),t)))
      clause = Term.ArgClause(args)
      fresh: Term = if v.kind eq Kind.Apply then Term.Apply(Term.Name(v.target.decoded),clause)
        else Term.New(Init(Type.Name(v.target.decoded),Name.Anonymous(),List(clause)))
      p <- project(fresh)
      _ <- Either.cond(p.invocation == v,(),error("ROUNDTRIP_FAILED"))
    yield fresh
  }
  def toLegacy(value: Invocation): Result[TermShape] = Left(error("NAMED_INVOCATION_NOT_LEGACY_TERMSHAPE"))
