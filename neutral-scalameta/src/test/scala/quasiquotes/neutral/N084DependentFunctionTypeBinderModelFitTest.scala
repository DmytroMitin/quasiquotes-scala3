package quasiquotes.neutral

import scala.annotation.nowarn
import scala.compiletime.testing.{typeChecks,typeCheckErrors}
import scala.meta.*
import scala.meta.dialects.Scala3

/** Closed source-scope convention only, never compiler Symbol authority. */
@nowarn("cat=deprecation")
final class N084DependentFunctionTypeBinderModelFitTest extends munit.FunSuite:
  private def parse(s: String): Type = Input.String(s).parse[Type].get
  private val fixtures = List("(x: Int) => x.type", "(x: Int, y: String) => x.type",
    "(x: Int) => List[x.type]", "(x: Int) ?=> x.type", "(x: Int) => (y: String) ?=> x.type",
    "(x: Int) => (x: String) => x.type", "(x: Int, y: x.type) => y.type", "() => Int",
    "(x: y.type, y: Int) => x.type", "(x: Int) => x.Out", "(x: Int) => outer.x.type")
  test("observe real dependent function parser before choosing a carrier"):
    fixtures.foreach { s =>
      Input.String(s).parse[Type] match
        case Parsed.Success(t) =>
          println(s"N084_AST $s => ${t.structure}")
          println(s"N084_PUBLIC $s => ${ScalametaTypeNormalFormProjection.project(t)}")
          assertEquals(t.tokens.map(_.text).mkString,s)
        case e => println(s"N084_PARSE $s => $e")
    }
    val f = parse(fixtures.head).asInstanceOf[Type.Function]
    assertEquals(f.params.head.productPrefix,"Type.TypedParam")
    assertEquals(f.res.productPrefix,"Type.Singleton")

  private enum T derives CanEqual:
    case Atom(name: String)
    case Apply(name: String, args: List[T])
    case Bound(depth: Int, index: Int)
    case Free(name: String)
    case Fn(context: Boolean, named: Boolean, inputs: List[T], result: T)
  private case class Projection(model: T, names: List[List[String]], tokens: String, span: Option[(Int,Int)])
  private type R[A] = Either[String,A]
  private val int = T.Atom("Int")
  private def fit(s: String): Projection = project(parse(s)).fold(fail(_),identity)
  private def nodes(t: Tree): List[Tree] = t :: t.children.flatMap(nodes)

  test("lexical result scopes preserve bound depth index and unresolved singleton labels"):
    assertEquals(fit("(x: Int) => x.type").model,T.Fn(false,true,List(int),T.Bound(0,0)))
    assertEquals(fit("(x: Int, y: String) => y.type").model,T.Fn(false,true,List(int,T.Atom("String")),T.Bound(0,1)))
    assertEquals(fit("(x: Int) => (y: String) ?=> x.type").model,
      T.Fn(false,true,List(int),T.Fn(true,true,List(T.Atom("String")),T.Bound(1,0))))
    assertEquals(fit("(x: Int) => (x: String) => x.type").model,
      T.Fn(false,true,List(int),T.Fn(false,true,List(T.Atom("String")),T.Bound(0,0))))
    assertEquals(fit("(x: Int) => free.type").model,T.Fn(false,true,List(int),T.Free("free")))
    assertNotEquals(fit("(x: Int) => x.type").model,fit("Int => Int").model)
    assertNotEquals(fit("(x: Int) => x.type").model,fit("(x: Int) ?=> x.type").model)

  test("fresh AST authoring preserves scoped structure and separates source names from alpha-shaped equality"):
    val sources = fixtures.take(6) ++ List("Int => String", "Int ?=> String", "Int => (x: Int) => x.type", "(x: Int) => Int => x.type", "List[(x: Int) => x.type]", "Either[(x: Int) => x.type, (y: String) ?=> y.type]")
    sources.foreach { s =>
      val original = parse(s); val p = project(original).fold(fail(_),identity); val t = author(p.model,p.names).fold(fail(_),identity)
      assertEquals(t.structure,parse(s).structure,clues(s))
      assertEquals(project(t).map(_.model),Right(p.model))
      assert(nodes(t).forall(_.pos == Position.None))
      assert(!nodes(t).exists(n => nodes(original).exists(_ eq n)))
      assertEquals(project(t).toOption.get.span,None)
      assertEquals(parse(t.syntax).structure,t.structure)
    }
    val a = fit("(x: Int) => x.type"); val b = fit("(renamed: Int) => renamed.type")
    assertEquals(a.model,b.model); assertNotEquals(a.names,b.names); assertNotEquals(a.tokens,b.tokens)
    val free = fit("(x: Int) => free.type")
    assert(author(free.model,List(List("free"))).isLeft)
    val outer = fit("(x: Int) => (y: String) => x.type")
    assert(author(outer.model,List(List("x"),List("x"))).isLeft)
    val comment = fit("((x: Int) => /* note */ x.type)")
    assertEquals(comment.model,a.model)
    assert(!author(comment.model,comment.names).toOption.get.syntax.contains("note"))

  test("zero anonymous and one two three seven named inputs retain order without declared arity semantics"):
    List(1,2,3,7).foreach { n =>
      val s = (1 to n).map(i => s"p$i: Int").mkString("(",", ",")") + s" => p$n.type"
      val p = fit(s)
      assertEquals(p.model,T.Fn(false,true,List.fill(n)(int),T.Bound(0,n-1)))
      assertEquals(author(p.model,p.names).toOption.get.structure,parse(s).structure)
    }
    val empty = fit("() => Int")
    assertEquals(empty.model,T.Fn(false,false,Nil,int))
    assertEquals(author(empty.model,empty.names).toOption.get.structure,parse("() => Int").structure)

  test("public Type shape template alias and method errors remain exact and ordinary positives survive"):
    fixtures.take(6).foreach { s =>
      val kind = if s.startsWith("(x: Int) ?=>") then "Type.ContextFunction" else "Type.TypedParam"
      assertEquals(ScalametaTypeNormalFormProjection.project(parse(s)),Left(NeutralProjectionError(
        "NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED",s"unsupported Scalameta type node: $kind.")))
    }
    import _root_.quasiquotes.parser.TypeShape
    import _root_.quasiquotes.types.{TypeNormalForm,TypeTemplate}
    val unsupported = TypeShape.Unsupported("Type.TypedParam","dependent parameter")
    assertEquals(TypeNormalForm.fromShape(unsupported).left.toOption.map(_.message),Some(
      "Unsupported type syntax for structural normal-form conversion; supported applied constructors are List/1, Option/1, Either/2."))
    assertEquals(TypeTemplate.fromShape(unsupported).left.toOption.map(_.message),Some(
      "Unsupported type syntax for type-template construction; supported applied constructors are List/1, Option/1, Either/2."))
    val alias = Input.String("type D = (x: Int) => x.type").parse[Stat].get.asInstanceOf[Defn.Type]
    assertEquals(ScalametaSimpleTypeAliasProjection.project(alias).left.toOption.map(_.code),Some("NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"))
    val method = Input.String("def f(x: Int): (y: Int) => y.type = ???").parse[Stat].get.asInstanceOf[Defn.Def]
    assertEquals(ScalametaTypedSingleParameterDefProjection.project(method).left.toOption.map(_.code),Some("NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"))
    List("Int","List[Int]","Option[String]","Either[Int,String]","(Int,String)","Int => String","(Int,String) => Boolean").foreach { s =>
      val nf = ScalametaTypeNormalFormProjection.project(parse(s)).toOption.get.normalForm
      assertEquals(ScalametaTypeNormalFormAuthoring.author(nf).toOption.get.structure,parse(s).structure)
    }
    val value = Input.String("val f: (x: Int) => x.type = ???").parse[Stat].get.asInstanceOf[Defn.Val]
    assertEquals(fit("(x: Int) => x.type").model, project(value.decltpe.get).toOption.get.model)
    assert(typeChecks("type D = (x: Int) => x.type"))
    assert(typeChecks("type D = (x: Int, y: String) => x.type"))
    assert(typeCheckErrors("type D = (x: Int) => missing.type").nonEmpty)

  test("scope-sensitive and neighboring grammar fails closed rather than inventing owner or parameter scope"):
    val bad = List("(x: Int, x: String) => x.type", "(x: Int, y: x.type) => y.type", "(x: y.type, y: Int) => x.type",
      "(x: Int) => x.Out", "(x: Int) => outer.x.type", "(`x`: Int) => x.type", "(x: F[Int]) => x.type",
      "(x: List[?]) => x.type", "(x: Int) => (AnyRef { type Out = Int })", "(x: Int) => ([A] =>> A)",
      "(x: Int) => (Int match { case Int => String })", "(x: => Int) => x.type", "(x: Int*) => x.type", "(using x: Int) => x.type")
    bad.foreach { s =>
      val observed = try Right(Input.String(s).parse[Type])
        catch case scala.util.control.NonFatal(e) => Left(e.getClass.getName + ": " + e.getMessage)
      observed match
        case Right(Parsed.Success(t)) => assert(project(t).isLeft,clues(s))
        case e => println(s"N084_NEGATIVE_PARSE $s => $e")
    }
    List("(x: ) => x.type","(x Int) => x.type").foreach(s => assert(Input.String(s).parse[Type].isInstanceOf[Parsed.Error]))
    assertEquals(project(null),Left("N084_SOURCE_MISSING"))
    val badModels = List[T](null,T.Atom(null),T.Apply("List",null),T.Apply("F",List(int)),T.Bound(-1,0),T.Bound(0,0),
      T.Fn(false,true,null,int),T.Fn(false,true,List(null),int),T.Fn(false,true,List(int),null),T.Fn(false,true,Nil,int))
    badModels.foreach(t => assert(author(t,List(List("x"))).isLeft,clues(t)))
    val p = fit("(x: Int) => x.type")
    List[List[List[String]]](null,Nil,List(null),List(Nil),List(List(null)),List(List("x","y")),List(List("type"))).foreach(ns => assert(author(p.model,ns).isLeft))

  private val atoms = Set("Int","String","Boolean","AnyVal")
  private val fixed = Map("List" -> 1,"Option" -> 1,"Either" -> 2)
  private def legal(s: String): Boolean = s != null && s.matches("[a-z][A-Za-z0-9]*")
  private def collect[A,B](xs: List[A])(f: A => R[B]): R[List[B]] =
    Option(xs).toRight("N084_LIST_MISSING").flatMap(_.foldLeft[R[List[B]]](Right(Nil))((acc,x) => for { a <- acc; b <- f(x) } yield b :: a).map(_.reverse))
  private def lookup(s: String, env: List[List[String]]): T =
    env.zipWithIndex.collectFirst { case (ns,d) if ns.contains(s) => T.Bound(d,ns.indexOf(s)) }.getOrElse(T.Free(s))
  private def namesValid(ns: List[String]): R[Unit] =
    Either.cond(ns != null && ns.nonEmpty && ns.forall(legal) && ns.distinct.size == ns.size,(),"N084_NAMES_INVALID")
  private def plain(n: Name): Boolean = n.tokens.map(_.text).mkString == n.value

  private def project(source: Type): R[Projection] =
    var frames = List.empty[List[String]]
    def function(ctx: Boolean, xs: List[Type], result: Type, env: List[List[String]]): R[T] =
      val named = xs.exists(_.isInstanceOf[Type.TypedParam])
      val declarations = xs.collect { case p: Type.TypedParam => p.name.value }
      for
        _ <- Either.cond(!named || xs.forall(_.isInstanceOf[Type.TypedParam]),(),"N084_MIXED_PARAMETERS")
        _ <- if named then namesValid(declarations) else Right(())
        _ = if named then frames = frames :+ declarations
        inputs <- collect(xs) {
          case p: Type.TypedParam =>
            if p.mods.nonEmpty || !plain(p.name) then Left("N084_PARAMETER_HEADER") else tpe(p.typ,env,true)
          case other => tpe(other,env,true)
        }
        res <- tpe(result,if named then declarations :: env else env,false)
      yield T.Fn(ctx,named,inputs,res)
    def tpe(t: Type, env: List[List[String]], input: Boolean): R[T] = t match
      case n: Type.Name if atoms(n.value) && plain(n) => Right(T.Atom(n.value))
      case a: Type.Apply => a.tpe match
        case n: Type.Name if fixed.get(n.value).contains(a.args.size) && plain(n) => collect(a.args)(tpe(_,env,input)).map(T.Apply(n.value,_))
        case _ => Left("N084_CONSTRUCTOR_UNSUPPORTED")
      case Type.Singleton(n: Term.Name) if !input && legal(n.value) && plain(n) => Right(lookup(n.value,env))
      case f: Type.Function if !input => function(false,f.params,f.res,env)
      case f: Type.ContextFunction if !input => function(true,f.params,f.res,env)
      case _ => Left(if input then "N084_INPUT_TYPE_UNSUPPORTED" else "N084_TYPE_UNSUPPORTED")
    Option(source).toRight("N084_SOURCE_MISSING").flatMap { t =>
      tpe(t,Nil,false).map(m => Projection(m,frames,t.tokens.map(_.text).mkString,
        if t.pos == Position.None then None else Some((t.pos.start,t.pos.end))))
    }

  private def author(model: T, frames: List[List[String]]): R[Type] =
    var cursor = 0
    def tpe(t: T, env: List[List[String]], input: Boolean): R[Type] = Option(t).toRight("N084_TYPE_MISSING").flatMap {
      case T.Atom(n) => Either.cond(n != null && atoms(n),(),"N084_ATOM_UNSUPPORTED").map(_ => Type.Name(n))
      case T.Apply(n,args) =>
        for
          xs <- Option(args).toRight("N084_LIST_MISSING")
          _ <- Either.cond(fixed.get(n).contains(xs.size),(),"N084_CONSTRUCTOR_UNSUPPORTED")
          ts <- collect(xs)(tpe(_,env,input))
        yield Type.Apply(Type.Name(n),ts)
      case T.Free(n) if !input =>
        Either.cond(legal(n) && lookup(n,env) == T.Free(n),(),"N084_CAPTURE_OR_NAME_INVALID").map(_ => Type.Singleton(Term.Name(n)))
      case T.Bound(d,i) if !input =>
        for
          _ <- Either.cond(d >= 0 && d < env.size && i >= 0 && i < env(d).size,(),"N084_BOUND_SLOT_INVALID")
          name = env(d)(i)
          _ <- Either.cond(lookup(name,env) == T.Bound(d,i),(),"N084_CAPTURE_OR_NAME_INVALID")
        yield Type.Singleton(Term.Name(name))
      case T.Fn(ctx,named,inputs,result) if !input =>
        for
          xs <- Option(inputs).toRight("N084_LIST_MISSING")
          ns <- if named then
            if cursor >= frames.size then Left("N084_NAMES_MISSING")
            else { val ns = frames(cursor); cursor += 1; namesValid(ns).map(_ => ns) }
            else Right(Nil)
          _ <- Either.cond(!named || ns.size == xs.size,(),"N084_NAME_ARITY")
          ts <- collect(xs)(tpe(_,env,true))
          params = if named then ts.zip(ns).map((t,n) => Type.TypedParam(Type.Name(n),t,Nil)) else ts
          res <- tpe(result,if named then ns :: env else env,false)
        yield if ctx then Type.ContextFunction(params,res) else Type.Function(params,res)
      case _ => Left("N084_INPUT_TYPE_UNSUPPORTED")
    }
    for
      _ <- Option(frames).toRight("N084_NAMES_MISSING")
      fresh <- tpe(model,Nil,false)
      _ <- Either.cond(cursor == frames.size,(),"N084_NAMES_UNUSED")
      again <- project(fresh)
      _ <- Either.cond(again.model == model && again.names == frames,(),"N084_ROUNDTRIP_OR_CAPTURE_FAILED")
    yield fresh

  test("erasing declared reference slots creates a real collision and source occurrences retain offsets"):
    val a = parse("(x: Int, y: Int) => x.type").asInstanceOf[Type.Function]
    val b = parse("(x: Int, y: Int) => y.type").asInstanceOf[Type.Function]
    def erased(f: Type.Function): (List[String],String) =
      (f.params.map(_.asInstanceOf[Type.TypedParam].typ.structure),f.res.productPrefix)
    assertEquals(erased(a),erased(b))
    assertNotEquals(project(a).map(_.model),project(b).map(_.model))
    val s = "(x: Int) => List[x.type]"
    nodes(parse(s)).filter(t => t.isInstanceOf[Type.TypedParam] || t.isInstanceOf[Type.Singleton]).foreach { t =>
      assertEquals(s.substring(t.pos.start,t.pos.end),t.tokens.map(_.text).mkString)
      println(s"N084_OCCURRENCE ${t.productPrefix} ${t.pos.start}:${t.pos.end} ${t.tokens.map(_.text).mkString}")
    }
    val m = Input.String("def f(x: Int): x.type = x").parse[Stat].get.asInstanceOf[Defn.Def]
    assertEquals(m.decltpe.get.productPrefix,"Type.Singleton")
    assertEquals(ScalametaTypedSingleParameterDefProjection.project(m).left.toOption.map(_.code),Some("NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"))
    assert(typeChecks("def f(x: Int): x.type = x"))
    println("N084_COMPILER_CONTEXT " + typeChecks("type D = (x: Int) ?=> x.type"))
    println("N084_COMPILER_NESTED " + typeChecks("type D = (x: Int) => (y: String) => x.type"))
    assert(typeChecks("object Scope { val free = 1; type D = (x: Int) => free.type }"))
