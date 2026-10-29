package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3
import scala.util.control.NonFatal

/** Syntax-only closed research oracle, not public refinement Type support. */
@nowarn("cat=deprecation")
final class N080StructuralRefinementTypeMemberModelFitTest extends munit.FunSuite:
  private enum T derives CanEqual:
    case Name(value: String)
    case Apply(constructor: String, args: List[T])
  private enum Member derives CanEqual:
    case Alias(name: String, rhs: T)
    case Abstract(name: String, lo: Option[T], hi: Option[T])
  private case class Refinement(parent: Option[T], members: List[Member]) derives CanEqual
  private case class Projection(model: Refinement, tokens: String, span: Option[(Int,Int)])
  private type R[A] = Either[String,A]
  private def parse(s: String): Type = Input.String(s).parse[Type].get


  test("exact aliases and abstract bounds preserve ordered distinct member roles"):
    assertEquals(project(parse("Parent { type Out = String; type Item >: Nothing <: AnyVal }")).map(_.model),
      Right(Refinement(Some(T.Name("Parent")),List(
        Member.Alias("Out",T.Name("String")),
        Member.Abstract("Item",Some(T.Name("Nothing")),Some(T.Name("AnyVal")))
      ))))

  test("observe actual optional-parent member and nested syntax"):
    List("AnyRef { type Out = String }", "Parent { type Out <: AnyVal }",
      "Parent { type Out >: Nothing <: AnyVal }", "{ type Out = String }",
      "Parent { type Out = String; type Item <: AnyVal }",
      "Parent { type Out = AnyRef { type Item = Int } }").foreach { s =>
      val t = parse(s).asInstanceOf[Type.Refine]
      println(s + " => " + t.structure)
      assertEquals(t.productPrefix,"Type.Refine")
      assertEquals(t.tokens.map(_.text).mkString,s)
      assertEquals((t.pos.start,t.pos.end),(0,s.length))
      assertEquals(t.stats.head.productPrefix,if s.contains("Out =") then "Defn.Type" else "Decl.Type")
    }

  test("base erasure conflates aliases declarations bounds names and order"):
    val sources=List("Parent { type Out = String }","Parent { type Out <: String }",
      "Parent { type Item = String }","Parent { type Out >: Nothing <: String }")
    val parsed=sources.map(s => parse(s).asInstanceOf[Type.Refine])
    assertEquals(parsed.map(_.tpe.get.structure).distinct,List("Type.Name(\"Parent\")"))
    assertEquals(sources.map(s=>fit(s).model).distinct.size,4)
    assertNotEquals(fit("Parent { type Out = String; type Item = Int }").model,
      fit("Parent { type Item = Int; type Out = String }").model)
    val unsupported = _root_.quasiquotes.parser.TypeShape.Unsupported("Type.Refine","erased")
    assert(_root_.quasiquotes.types.TypeNormalForm.fromShape(unsupported).isLeft)
    assert(_root_.quasiquotes.types.TypeTemplate.fromShape(unsupported).isLeft)

  test("one two three and seven independent members fresh-author exact AST structure"):
    List(1,2,3,7).foreach { n =>
      val s="Parent { "+(1 to n).map(i=>s"type Member$i = String").mkString("; ")+" }"
      val p=fit(s)
      assertEquals(p.model.members.size,n)
      roundtrip(s)
    }
    List("{ type Out = String }","Parent { type Out }",
      "Parent { type Out >: Nothing }","Parent { type Out <: AnyVal }",
      "List[Int] { type Out = Option[String] }",
      "Parent { type Out = String; type Item >: Nothing <: AnyVal }").foreach(roundtrip)

  test("zero-member refinement has honest AST support without invented members"):
    val p=fit("Parent {}")
    assertEquals(p.model,Refinement(Some(T.Name("Parent")),Nil))
    roundtrip("Parent {}")
    val empty=Refinement(None,Nil)
    assertEquals(author(empty).flatMap(project).map(_.model),Right(empty))

  test("source provenance differs from fresh AST synthetic tokens and redundant grouping"):
    val p=fit("Parent { /* note */ type Out = String }")
    val t=author(p.model).toOption.get
    assertEquals(t.pos,Position.None)
    assert(all(t).forall(_.pos==Position.None))
    assert(t.tokens.nonEmpty)
    assertEquals(project(t).toOption.get.span,None)
    assertNotEquals(project(t).toOption.get.tokens,p.tokens)
    assertEquals(fit("(Parent { type Out = String })").model,p.model)

  test("generic public projection continues rejecting every selected refinement precisely"):
    List("AnyRef { type Out = String }","{ type Out = String }","Parent {}",
      "Parent { type Out <: AnyVal }","List[Int] { type Out = String }").foreach { s=>
      assertEquals(ScalametaTypeNormalFormProjection.project(parse(s)),Left(NeutralProjectionError(
        "NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED","unsupported Scalameta type node: Type.Refine.")))
    }
    import _root_.quasiquotes.types.TypeNormalForm.*
    List("Int"->STypeIdent("Int"),"List[Int]"->STypeApply(STypeIdent("List"),List(STypeIdent("Int"))),
      "Option[String]"->STypeApply(STypeIdent("Option"),List(STypeIdent("String"))),
      "Either[Int,String]"->STypeApply(STypeIdent("Either"),List(STypeIdent("Int"),STypeIdent("String"))),
      "(Int,String)"->STypeTuple(List(STypeIdent("Int"),STypeIdent("String"))),
      "Int => String"->STypeFunction(List(STypeIdent("Int")),STypeIdent("String"))).foreach { (s,w)=>
      assertEquals(ScalametaTypeNormalFormProjection.project(parse(s)).map(_.normalForm),Right(w))
      assertEquals(ScalametaTypeNormalFormProjection.project(ScalametaTypeNormalFormAuthoring.author(w).toOption.get).map(_.normalForm),Right(w))
    }
    val alias=Input.String("type Out = String").parse[Stat].get.asInstanceOf[Defn.Type]
    assert(ScalametaSimpleTypeAliasProjection.project(alias).isRight)
    val refined=Input.String("type R = AnyRef { type Out = String }").parse[Stat].get.asInstanceOf[Defn.Type]
    assertEquals(ScalametaSimpleTypeAliasProjection.project(refined).left.toOption.map(_.code),Some("NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"))

  test("all member declarations are collected before projecting dependencies in either order"):
    List("Parent { type Out = Item; type Item = String }",
      "Parent { type Item = String; type Out = Item }",
      "Parent { type Out <: Out }",
      "Parent { type Out = this.Out }").foreach { s=>
      val expected=if s.contains("this.") then "N080_TYPE_NODE_UNSUPPORTED" else "N080_MEMBER_REFERENCE_AUTHORITY_GAP"
      assertEquals(project(parse(s)).left.toOption,Some(expected),clues(s))
    }
    List("List","Option","Either").foreach { c=>
      val arg=if c=="Either" then "Int,String" else "Int"
      List(s"Parent { type $c = String; type Out = $c[$arg] }",
        s"$c[$arg] { type $c = String }").foreach { s=>
        assertEquals(project(parse(s)).left.toOption,Some("N080_CONSTRUCTOR_SHADOWED"))
      }
      val bad=Refinement(Some(T.Name("Parent")),List(Member.Alias(c,T.Name("String")),Member.Alias("Out",T.Apply(c,List.fill(if c=="Either" then 2 else 1)(T.Name("Int"))))))
      assertEquals(author(bad).left.toOption,Some("N080_CONSTRUCTOR_SHADOWED"))
    }
    assertEquals(project(parse("Parent { type String = Int }")).left.toOption,Some("N080_RESERVED_MEMBER_NAME"))

  test("unsupported members parameters quoting duplicates and nested result Types fail closed"):
    val cases=List(
      "Parent { type Out = String; type Out = Int }"->"N080_DUPLICATE_MEMBER",
      "Parent { val out: Int }"->"N080_MEMBER_KIND_UNSUPPORTED",
      "Parent { def out: Int }"->"N080_MEMBER_KIND_UNSUPPORTED",
      "Parent { type Out[A] = String }"->"N080_MEMBER_HEADER_UNSUPPORTED",
      "Parent { type `Out` = String }"->"N080_NAME_SPELLING_UNSUPPORTED",
      "Parent { type Out = Box[Int] }"->"N080_CONSTRUCTOR_UNSUPPORTED",
      "Parent { type Out = types.String }"->"N080_TYPE_NODE_UNSUPPORTED",
      "Parent { type Out = AnyRef { type Item = String } }"->"N080_NESTED_REFINEMENT_UNSUPPORTED",
      "Parent { type Out = [A] =>> A }"->"N080_TYPE_NODE_UNSUPPORTED",
      "Parent { type Out = X match { case t => t } }"->"N080_TYPE_NODE_UNSUPPORTED")
    cases.foreach { (s,w)=> assertEquals(project(parse(s)).left.toOption,Some(w),clues(s)) }
    val bounded=Defn.Type(Nil,Type.Name("Out"),Type.ParamClause(Nil),Type.Name("String"),Type.Bounds(None,Some(Type.Name("AnyVal")),Nil,Nil))
    assertEquals(project(Type.Refine(Some(Type.Name("Parent")),Stat.Block(List(bounded)))).left.toOption,Some("N080_ALIAS_BOUNDS_UNSUPPORTED"))
    val nestedParent=Type.Refine(Some(parse("Parent { type Item = Int }")),Stat.Block(Nil))
    assertEquals(project(nestedParent).left.toOption,Some("N080_NESTED_REFINEMENT_UNSUPPORTED"))

  test("parser rejection is independent of candidate and public errors"):
    List("Parent { type Out = }","Parent { type Out <: }","Parent { type Out[A <: ] }").foreach { s=>
      assert(parseResult(s).isLeft)
    }
    assertEquals(project(parse("Parent { type Out <: AnyVal = Int }")).left.toOption,Some("N080_ALIAS_BOUNDS_UNSUPPORTED"))
    assertEquals(fit("{}").model,Refinement(None,Nil))

  test("malformed candidate nulls and invalid arities reject without constructor exceptions"):
    val cases=List(
      (null, "N080_CANDIDATE_MISSING"),
      Refinement(null,Nil)->"N080_PARENT_OPTION_MISSING",
      Refinement(None,null)->"N080_MEMBERS_MISSING",
      Refinement(None,List(null))->"N080_MEMBER_MISSING",
      Refinement(None,List(Member.Alias("Out",null)))->"N080_TYPE_MISSING",
      Refinement(None,List(Member.Abstract("Out",null,None)))->"N080_BOUND_OPTION_MISSING",
      Refinement(Some(T.Apply("List",null)),Nil)->"N080_ARGUMENTS_MISSING",
      Refinement(Some(T.Apply("Either",List(T.Name("Int")))),Nil)->"N080_CONSTRUCTOR_ARITY")
    cases.foreach { (m,w)=>assertEquals(author(m).left.toOption,Some(w)) }
    assertEquals(project(null).left.toOption,Some("N080_SOURCE_MISSING"))
    assertEquals(author(Refinement(Some(null),Nil)).left.toOption,Some("N080_TYPE_MISSING"))

  private def fit(s: String): Projection = project(parse(s)).fold(e=>fail(s+": "+e),identity)
  private def roundtrip(s: String): Unit =
    val p=fit(s)
    val t=author(p.model).fold(e=>fail(e),identity)
    assertEquals(t.structure,parse(s).structure)
    assertEquals(project(t).map(_.model),Right(p.model))
  private def all(t: Tree): List[Tree] = t :: t.children.flatMap(all)
  private def parseResult(s: String): R[Type] =
    try Input.String(s).parse[Type] match
      case Parsed.Success(t)=>Right(t)
      case _: Parsed.Error=>Left("N080_PARSE_ERROR")
    catch case NonFatal(_)=>Left("N080_PARSER_THROWN")

  private val fixed = Map("List"->1,"Option"->1,"Either"->2)
  private val external = Set("Parent","AnyRef","Int","String","Boolean","AnyVal","Any","Nothing")
  private def spelling(s: String): Boolean = s!=null && s.matches("[A-Z][A-Za-z0-9]*")
  private def plain(n: Type.Name): R[String] =
    Either.cond(spelling(n.value) && n.tokens.map(_.text).mkString==n.value,n.value,"N080_NAME_SPELLING_UNSUPPORTED")
  private def collect[A,B](xs: List[A])(f: A=>R[B]): R[List[B]] =
    xs.foldLeft[R[List[B]]](Right(Nil))((a,x)=>for { acc<-a; b<-f(x) } yield b::acc).map(_.reverse)
  private def namesValid(ns: List[String]): R[Unit] =
    for
      _<-Either.cond(ns.forall(spelling),(),"N080_NAME_SPELLING_UNSUPPORTED")
      _<-Either.cond(ns.distinct.size==ns.size,(),"N080_DUPLICATE_MEMBER")
      _<-Either.cond(!ns.exists(fixed.contains),(),"N080_CONSTRUCTOR_SHADOWED")
      _<-Either.cond(!ns.exists(external.contains),(),"N080_RESERVED_MEMBER_NAME")
    yield ()
  private def project(source: Type): R[Projection] =
    def tpe(t: Type, names: Set[String]): R[T] = t match
      case n: Type.Name => plain(n).flatMap { s=>
        if names(s) then Left("N080_MEMBER_REFERENCE_AUTHORITY_GAP")
        else Either.cond(external(s),T.Name(s),"N080_TYPE_NAME_UNSUPPORTED")
      }
      case a: Type.Apply => a.tpe match
        case n: Type.Name =>
          for
            s<-plain(n)
            _<-Either.cond(!names(s),(),"N080_CONSTRUCTOR_SHADOWED")
            arity<-fixed.get(s).toRight("N080_CONSTRUCTOR_UNSUPPORTED")
            _<-Either.cond(a.args.size==arity,(),"N080_CONSTRUCTOR_ARITY")
            args<-collect(a.args)(tpe(_,names))
          yield T.Apply(s,args)
        case _=>Left("N080_CONSTRUCTOR_UNSUPPORTED")
      case _: Type.Refine=>Left("N080_NESTED_REFINEMENT_UNSUPPORTED")
      case _=>Left("N080_TYPE_NODE_UNSUPPORTED")
    def header(s: Stat): R[String] = s match
      case d: Defn.Type =>
        for
          n<-plain(d.name)
          _<-Either.cond(d.mods.isEmpty && d.tparamClause.values.isEmpty,(),"N080_MEMBER_HEADER_UNSUPPORTED")
          _<-Either.cond(d.bounds.lo.isEmpty && d.bounds.hi.isEmpty && d.bounds.context.isEmpty && d.bounds.view.isEmpty,(),"N080_ALIAS_BOUNDS_UNSUPPORTED")
        yield n
      case d: Decl.Type =>
        for
          n<-plain(d.name)
          _<-Either.cond(d.mods.isEmpty && d.tparamClause.values.isEmpty && d.bounds.context.isEmpty && d.bounds.view.isEmpty,(),"N080_MEMBER_HEADER_UNSUPPORTED")
        yield n
      case _=>Left("N080_MEMBER_KIND_UNSUPPORTED")
    Option(source).toRight("N080_SOURCE_MISSING").flatMap {
      case r: Type.Refine =>
        for
          ns<-collect(r.stats)(header)
          _<-namesValid(ns)
          parent<-r.tpe.map(tpe(_,ns.toSet).map(Some(_))).getOrElse(Right(None))
          members<-collect(r.stats.zip(ns)) { (s,n)=>s match
            case d: Defn.Type=>tpe(d.body,ns.toSet).map(Member.Alias(n,_))
            case d: Decl.Type=>
              for
                lo<-d.bounds.lo.map(tpe(_,ns.toSet).map(Some(_))).getOrElse(Right(None))
                hi<-d.bounds.hi.map(tpe(_,ns.toSet).map(Some(_))).getOrElse(Right(None))
              yield Member.Abstract(n,lo,hi)
            case _=>Left("N080_MEMBER_KIND_UNSUPPORTED")
          }
        yield Projection(Refinement(parent,members),r.tokens.map(_.text).mkString,
          if r.pos==Position.None then None else Some(r.pos.start->r.pos.end))
      case _=>Left("N080_ROOT_UNSUPPORTED")
    }

  private def author(model: Refinement): R[Type] =
    def tpe(t: T): R[Type] = Option(t).toRight("N080_TYPE_MISSING").flatMap {
      case T.Name(n)=>Either.cond(spelling(n),(),"N080_NAME_SPELLING_UNSUPPORTED").map(_=>Type.Name(n))
      case T.Apply(n,args)=>
        for
          arity<-fixed.get(n).toRight("N080_CONSTRUCTOR_UNSUPPORTED")
          xs<-Option(args).toRight("N080_ARGUMENTS_MISSING")
          _<-Either.cond(xs.size==arity,(),"N080_CONSTRUCTOR_ARITY")
          a<-collect(xs)(tpe)
        yield Type.Apply(Type.Name(n),a)
    }
    def opt(o: Option[T],code: String): R[Option[Type]] =
      Option(o).toRight(code).flatMap(_.map(tpe(_).map(Some(_))).getOrElse(Right(None)))
    for
      m<-Option(model).toRight("N080_CANDIDATE_MISSING")
      ms<-Option(m.members).toRight("N080_MEMBERS_MISSING")
      ns<-collect(ms)(x=>Option(x).toRight("N080_MEMBER_MISSING").map {
        case Member.Alias(n,_)=>n
        case Member.Abstract(n,_,_)=>n
      })
      _<-namesValid(ns)
      p<-opt(m.parent,"N080_PARENT_OPTION_MISSING")
      stats<-collect(ms) {
        case Member.Alias(n,r)=>tpe(r).map(t=>Defn.Type(Nil,Type.Name(n),Type.ParamClause(Nil),t,Type.Bounds.empty))
        case Member.Abstract(n,l,h)=>
          for
            lo<-opt(l,"N080_BOUND_OPTION_MISSING")
            hi<-opt(h,"N080_BOUND_OPTION_MISSING")
          yield Decl.Type(Nil,Type.Name(n),Type.ParamClause(Nil),Type.Bounds(lo,hi,Nil,Nil))
      }
      tree=Type.Refine(p,Stat.Block(stats))
      reprojected<-project(tree)
      _<-Either.cond(reprojected.model==m,(),"N080_AUTHOR_ROUNDTRIP_FAILED")
    yield tree
