package quasiquotes.neutral

import scala.annotation.nowarn
import scala.compiletime.testing.{typeChecks, typeCheckErrors}
import scala.meta.*
import scala.meta.dialects.Scala3
import scala.util.control.NonFatal

/** A closed all-direct-members syntactic convention, not Scala member resolution. */
@nowarn("cat=deprecation")
final class N081RefinementMemberReferenceScopeModelFitTest extends munit.FunSuite:
  private enum T derives CanEqual:
    case Free(name: String)
    case BoundMember(index: Int)
    case Apply(constructor: String, args: List[T])
  private enum M derives CanEqual:
    case Alias(rhs: T)
    case Abstract(lo: Option[T], hi: Option[T])
  private case class R(parent: Option[T], members: List[M]) derives CanEqual
  private case class Projection(model: R, names: List[String], tokens: String, span: Option[(Int, Int)])
  private type Result[A] = Either[String, A]
  private def parse(s: String): Type = Input.String(s).parse[Type].get
  private def fit(s: String): Projection = project(parse(s)).fold(e => fail(s + ": " + e), identity)
  private def nodes(t: Tree): List[Tree] = t :: t.children.flatMap(nodes)
  private def roundtrip(s: String): Unit =
    val p = fit(s)
    val fresh = author(p.model, p.names).fold(fail(_), identity)
    assertEquals(fresh.structure, parse(s).structure)
    assertEquals(project(fresh).map(_.model), Right(p.model))
    assert(nodes(fresh).forall(_.pos == Position.None))
    assertEquals(project(fresh).toOption.get.span, None)

  test("direct member syntax preserves declaration slots and backward reference provenance"):
    val source = "Parent { type Out = String; type Item = Out }"
    val p = fit(source)
    assertEquals(p.model, R(Some(T.Free("Parent")), List(M.Alias(T.Free("String")), M.Alias(T.BoundMember(0)))))
    assertEquals(p.names, List("Out", "Item"))
    assertEquals(p.tokens, source)
    assertEquals(p.span, Some((0, source.length)))

  test("forward self and abstract lower upper references use all direct member slots"):
    assertEquals(fit("Parent { type Item = Out; type Out = String }").model,
      R(Some(T.Free("Parent")), List(M.Alias(T.BoundMember(1)), M.Alias(T.Free("String")))))
    assertEquals(fit("Parent { type Out >: Out <: Out }").model,
      R(Some(T.Free("Parent")), List(M.Abstract(Some(T.BoundMember(0)), Some(T.BoundMember(0))))))
    roundtrip("Parent { type Out = Out }")
    roundtrip("Parent { type Out <: Out }")

  test("actual Scala compiler resolves direct members but inherited identity needs environment"):
    assert(typeChecks("""object Scope {
      type Out = Int
      type R = AnyRef { type Out = String; type Item = Out }
      def witness(r: R): Unit = { summon[r.Item =:= String]; () }
    }"""))
    val wrong = typeCheckErrors("""object Scope {
      type Out = Int
      type R = AnyRef { type Out = String; type Item = Out }
      def witness(r: R): Unit = { summon[r.Item =:= Int]; () }
    }""")
    assert(wrong.exists(_.message.contains("Cannot prove")))
    assert(typeChecks("""object Scope {
      trait Parent { type Out = Int }
      type R = Parent { type Item = Out }
      def witness(r: R): Unit = { summon[r.Item =:= Int]; () }
    }"""))
    assert(typeChecks("""object Scope {
      trait Parent { type Out = String }
      type R = Parent { type Item = Out }
      def witness(r: R): Unit = { summon[r.Item =:= String]; () }
    }"""))
    // The same isolated AST cannot distinguish the two inherited declarations.
    assertEquals(fit("Parent { type Item = Out }").model,
      R(Some(T.Free("Parent")), List(M.Alias(T.Free("Out")))))

  test("zero one two three and seven ordered members preserve alias and abstract roles"):
    List(0, 1, 2, 3, 7).foreach { n =>
      val s = "Parent { " + (1 to n).map(i => s"type Member$i = String").mkString("; ") + " }"
      assertEquals(fit(s).model.members.size, n)
      roundtrip(s)
    }
    List("{}", "{ type Out = String }", "Parent { type Out }",
      "Parent { type Out >: Nothing }", "Parent { type Out <: AnyVal }",
      "Parent { type Out = String; type Item >: Nothing <: Out }").foreach(roundtrip)
    assertNotEquals(fit("Parent { type Out = String; type Item = Int }").model,
      fit("Parent { type Item = Int; type Out = String }").model)

  test("parent is outside member frame while RHSs and bounds share it"):
    val p = fit("Out { type Out = String; type Item <: Out }")
    assertEquals(p.model, R(Some(T.Free("Out")), List(M.Alias(T.Free("String")), M.Abstract(None, Some(T.BoundMember(0))))))
    roundtrip("Out { type Out = String; type Item <: Out }")

  test("alpha source changes preserve scoped model and ordered declaration provenance"):
    val a = fit("Parent { type Out = String; type Item = List[Out] }")
    val b = fit("Parent { type Result = String; type Next = List[Result] }")
    assertEquals(a.model, b.model)
    assertNotEquals(a.names, b.names)
    assertNotEquals(a.tokens, b.tokens)
    assertEquals(author(a.model, b.names).map(_.structure), Right(parse("Parent { type Result = String; type Next = List[Result] }").structure))
    val free = fit("Parent { type Out = X; type Item = Out }")
    assertEquals(author(free.model, List("X", "Item")), Left("N081_CAPTURE_OR_SCOPE_DRIFT"))
    // Renaming a declaration alone does not rewrite an unresolved RHS.
    assertNotEquals(a.model, fit("Parent { type Result = String; type Item = List[Out] }").model)

  test("independent free names are unresolved labels and fixed constructors are shadow guarded"):
    roundtrip("Parent { type Out = X; type Item = Either[Out,Option[X]] }")
    List("List", "Option", "Either").foreach { name =>
      val arity = if name == "Either" then "Int,String" else "Int"
      assertEquals(project(parse(s"Parent { type $name = String; type Item = $name[$arity] }")).left.toOption,
        Some("N081_CONSTRUCTOR_SHADOWED"))
      assertEquals(author(R(None, List(M.Alias(T.Free("String")))), List(name)).left.toOption,
        Some("N081_CONSTRUCTOR_SHADOWED"))
    }

  test("this selected inherited and qualified constructors never become local binding authority"):
    List("Parent { type Out = this.Out }", "Parent { type Out = outer.Out }",
      "Parent { type Out = scala.List[Int] }").foreach { s =>
      assert(parse(s).isInstanceOf[Type.Refine])
      assertEquals(project(parse(s)).left.toOption, Some("N081_TYPE_NODE_UNSUPPORTED"))
    }
    val rhs = parse("Parent { type Out = this.Out }").asInstanceOf[Type.Refine].stats.head.asInstanceOf[Defn.Type].body
    val selected = rhs.asInstanceOf[Type.Select]
    assertEquals(selected.name.value, "Out")
    assert(selected.qual.isInstanceOf[Term.This])
    assertEquals(selected.qual.asInstanceOf[Term.This].qual.structure, "Name.Anonymous()")
    assertNotEquals(fit("Parent { type Item = Out }").model,
      fit("Parent { type Out = String; type Item = Out }").model)

  test("parser AST classes tokens and occurrence offsets are exact source observations"):
    val fixtures = List("Parent { type Out = String; type Item = Out }",
      "Parent { type Out <: Out }", "Parent { type Out = this.Out }",
      "Parent { type Item = Out; type Out = String }",
      "Parent { type Out = String; type Inner = AnyRef { type Item = Out } }",
      "Parent { type Out = String; type Out = Int }",
      "Parent { type `Out` = String }", "Parent { type Out[A] = A }",
      "Parent { type Out = [A] =>> A }", "Parent { type Out = X match { case t => t } }")
    fixtures.foreach { s =>
      val tree = parse(s)
      assertEquals(tree.productPrefix, "Type.Refine")
      assertEquals(tree.tokens.map(_.text).mkString, s)
      assertEquals((tree.pos.start, tree.pos.end), (0, s.length))
      println("N081_AST " + s + " => " + tree.structure)
      nodes(tree).collect { case n: Type.Name => n }.foreach { n =>
        val text = n.tokens.map(_.text).mkString
        assertEquals(s.substring(n.pos.start, n.pos.end), text)
        println(s"N081_NAME ${n.value} [$text] @${n.pos.start}:${n.pos.end}")
      }
    }

  test("nested lambda match duplicate quoted higher kinded and non-Type members fail closed"):
    List(
      "Parent { type Out = AnyRef { type Item = Out } }" -> "N081_NESTED_REFINEMENT_UNSUPPORTED",
      "Parent { type Out = [A] =>> A }" -> "N081_TYPE_NODE_UNSUPPORTED",
      "Parent { type Out = X match { case t => t } }" -> "N081_TYPE_NODE_UNSUPPORTED",
      "Parent { type Out = String; type Out = Int }" -> "N081_DUPLICATE_MEMBER",
      "Parent { type `Out` = String }" -> "N081_NAME_UNSUPPORTED",
      "Parent { type Out[A] = A }" -> "N081_HEADER_UNSUPPORTED",
      "Parent { type Out <: AnyVal = Int }" -> "N081_ALIAS_BOUNDS_UNSUPPORTED",
      "Parent { val out: Int }" -> "N081_MEMBER_KIND_UNSUPPORTED",
      "Parent { def out: Int }" -> "N081_MEMBER_KIND_UNSUPPORTED"
    ).foreach { (s, code) => assertEquals(project(parse(s)).left.toOption, Some(code), clues(s)) }
    val nested = Type.Refine(Some(parse("Parent { type Out = Int }")), Stat.Block(Nil))
    assertEquals(project(nested).left.toOption, Some("N081_NESTED_REFINEMENT_UNSUPPORTED"))

  test("parser failures and Scalameta constructor invariants are separate from unsupported scope"):
    List("Parent { type Out = }", "Parent { type Out <: }", "Parent { type Out[A <: ] }").foreach { s =>
      Input.String(s).parse[Type] match
        case e: Parsed.Error => println("N081_PARSE_ERROR " + s + " => " + e.toString)
        case _ => fail("expected ordinary parse error: " + s)
    }
    val thrown = intercept[Exception] { Type.Refine(None, Stat.Block(List(Term.Name("notAMember")))) }
    println("N081_INVARIANT " + thrown.getClass.getName + " " + thrown.getMessage)

  test("fresh authored reference trees preserve structure while losing source provenance"):
    val a = fit("Parent { /* source */ type Out = String; type Item = Out }")
    val t = author(a.model, a.names).toOption.get
    assertEquals(t.structure, parse("Parent { type Out = String; type Item = Out }").structure)
    assert(nodes(t).forall(_.pos == Position.None))
    assert(t.tokens.nonEmpty)
    assertNotEquals(project(t).toOption.get.tokens, a.tokens)
    assertEquals(fit("(Parent { type Out = String; type Item = Out })").model, a.model)

  test("malformed scoped models and authoring names fail before AST constructors"):
    val one = R(None, List(M.Alias(T.BoundMember(0))))
    val cases: List[(R, List[String], String)] = List(
      (null, Nil, "N081_MODEL_MISSING"), (R(null, Nil), Nil, "N081_OPTION_MISSING"),
      (R(None, null), Nil, "N081_MEMBERS_MISSING"), (R(None, List(null)), List("Out"), "N081_MEMBER_MISSING"),
      (one, null, "N081_NAMES_MISSING"), (one, Nil, "N081_NAMES_ARITY"),
      (one, List(null), "N081_NAME_UNSUPPORTED"), (one, List("`Out`"), "N081_NAME_UNSUPPORTED"),
      (R(None, List(M.Alias(null))), List("Out"), "N081_TYPE_MISSING"),
      (R(None, List(M.Abstract(null, None))), List("Out"), "N081_OPTION_MISSING"),
      (R(Some(null), Nil), Nil, "N081_TYPE_MISSING"),
      (R(Some(T.BoundMember(0)), List(M.Alias(T.Free("Int")))), List("Out"), "N081_MEMBER_SLOT_INVALID"),
      (R(None, List(M.Alias(T.BoundMember(-1)))), List("Out"), "N081_MEMBER_SLOT_INVALID"),
      (R(None, List(M.Alias(T.BoundMember(1)))), List("Out"), "N081_MEMBER_SLOT_INVALID"),
      (R(None, List(M.Alias(T.Apply("List", null)))), List("Out"), "N081_ARGUMENTS_MISSING"),
      (R(None, List(M.Alias(T.Apply("Either", List(T.Free("Int")))))), List("Out"), "N081_CONSTRUCTOR_UNSUPPORTED"))
    cases.foreach { (r, ns, code) => assertEquals(author(r, ns).left.toOption, Some(code)) }
    assertEquals(project(null).left.toOption, Some("N081_SOURCE_MISSING"))
    assertEquals(author(R(None, List(M.Alias(T.Free("Int")), M.Alias(T.Free("String")))), List("Out", "Out")).left.toOption,
      Some("N081_DUPLICATE_MEMBER"))

  test("public refinement rejection and specialized simple alias contract stay bounded"):
    List("Parent {}", "{}", "Parent { type Out = String; type Item = Out }",
      "Parent { type Out <: Out }", "Parent { type Out = this.Out }").foreach { s =>
      assertEquals(ScalametaTypeNormalFormProjection.project(parse(s)), Left(NeutralProjectionError(
        "NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED", "unsupported Scalameta type node: Type.Refine.")))
    }
    val alias = Input.String("type Out = String").parse[Stat].get.asInstanceOf[Defn.Type]
    assert(ScalametaSimpleTypeAliasProjection.project(alias).isRight)
    val refined = Input.String("type R = Parent { type Item = Out }").parse[Stat].get.asInstanceOf[Defn.Type]
    assertEquals(ScalametaSimpleTypeAliasProjection.project(refined).left.toOption.map(_.code), Some("NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"))

  private val constructors = Map("List" -> 1, "Option" -> 1, "Either" -> 2)
  private val reserved = Set("Int", "String", "Boolean", "Any", "AnyRef", "AnyVal", "Nothing")
  private def plain(s: String): Boolean = s != null && s.matches("[A-Z][A-Za-z0-9]*")
  private def name(n: Type.Name): Result[String] =
    Either.cond(plain(n.value) && n.tokens.map(_.text).mkString == n.value, n.value, "N081_NAME_UNSUPPORTED")
  private def collect[A, B](xs: List[A])(f: A => Result[B]): Result[List[B]] =
    xs.foldLeft[Result[List[B]]](Right(Nil))((acc, x) => for { a <- acc; b <- f(x) } yield b :: a).map(_.reverse)
  private def validateNames(ns: List[String]): Result[Unit] =
    for
      present <- Option(ns).toRight("N081_NAMES_MISSING")
      _ <- Either.cond(present.forall(plain), (), "N081_NAME_UNSUPPORTED")
      _ <- Either.cond(present.distinct.size == present.size, (), "N081_DUPLICATE_MEMBER")
      _ <- Either.cond(!present.exists(constructors.contains), (), "N081_CONSTRUCTOR_SHADOWED")
      _ <- Either.cond(!present.exists(reserved), (), "N081_RESERVED_MEMBER_NAME")
    yield ()

  private def project(t: Type): Result[Projection] =
    // This policy is deliberately syntax-only: all direct declaration slots are in
    // scope for every member RHS/bound. Parent syntax is outside that frame.
    // Free is an unresolved label, including a possible inherited member.
    def tpe(t: Type, ns: List[String]): Result[T] = t match
      case n: Type.Name => name(n).map { s =>
        val i = ns.indexOf(s)
        if i >= 0 then T.BoundMember(i) else T.Free(s)
      }
      case a: Type.Apply => a.tpe match
        case n: Type.Name =>
          for
            c <- name(n)
            _ <- Either.cond(!ns.contains(c), (), "N081_CONSTRUCTOR_SHADOWED")
            _ <- Either.cond(constructors.get(c).contains(a.args.size), (), "N081_CONSTRUCTOR_UNSUPPORTED")
            args <- collect(a.args)(tpe(_, ns))
          yield T.Apply(c, args)
        case _ => Left("N081_TYPE_NODE_UNSUPPORTED")
      case _: Type.Refine => Left("N081_NESTED_REFINEMENT_UNSUPPORTED")
      case _ => Left("N081_TYPE_NODE_UNSUPPORTED")
    def header(s: Stat): Result[String] = s match
      case d: Defn.Type =>
        for
          n <- name(d.name)
          _ <- Either.cond(d.mods.isEmpty && d.tparamClause.values.isEmpty, (), "N081_HEADER_UNSUPPORTED")
          _ <- Either.cond(d.bounds.lo.isEmpty && d.bounds.hi.isEmpty && d.bounds.context.isEmpty && d.bounds.view.isEmpty,
            (), "N081_ALIAS_BOUNDS_UNSUPPORTED")
        yield n
      case d: Decl.Type =>
        for
          n <- name(d.name)
          _ <- Either.cond(d.mods.isEmpty && d.tparamClause.values.isEmpty && d.bounds.context.isEmpty && d.bounds.view.isEmpty,
            (), "N081_HEADER_UNSUPPORTED")
        yield n
      case _ => Left("N081_MEMBER_KIND_UNSUPPORTED")
    Option(t).toRight("N081_SOURCE_MISSING").flatMap {
      case r: Type.Refine =>
        for
          ns <- collect(r.stats)(header)
          _ <- validateNames(ns)
          parent <- r.tpe.map(tpe(_, Nil).map(Some(_))).getOrElse(Right(None))
          ms <- collect(r.stats) {
            case d: Defn.Type => tpe(d.body, ns).map(M.Alias(_))
            case d: Decl.Type =>
              for
                lo <- d.bounds.lo.map(tpe(_, ns).map(Some(_))).getOrElse(Right(None))
                hi <- d.bounds.hi.map(tpe(_, ns).map(Some(_))).getOrElse(Right(None))
              yield M.Abstract(lo, hi)
            case _ => Left("N081_MEMBER_KIND_UNSUPPORTED")
          }
        yield Projection(R(parent, ms), ns, r.tokens.map(_.text).mkString,
          if r.pos == Position.None then None else Some((r.pos.start, r.pos.end)))
      case _ => Left("N081_ROOT_UNSUPPORTED")
    }

  private def author(r: R, names: List[String]): Result[Type] =
    def tpe(t: T, ns: List[String]): Result[Type] = Option(t).toRight("N081_TYPE_MISSING").flatMap {
      case T.Free(n) => Either.cond(plain(n), (), "N081_NAME_UNSUPPORTED").map(_ => Type.Name(n))
      case T.BoundMember(i) =>
        Either.cond(i >= 0 && i < ns.size, (), "N081_MEMBER_SLOT_INVALID").map(_ => Type.Name(ns(i)))
      case T.Apply(c, args) =>
        for
          xs <- Option(args).toRight("N081_ARGUMENTS_MISSING")
          _ <- Either.cond(constructors.get(c).contains(xs.size), (), "N081_CONSTRUCTOR_UNSUPPORTED")
          _ <- Either.cond(!ns.contains(c), (), "N081_CONSTRUCTOR_SHADOWED")
          as <- collect(xs)(tpe(_, ns))
        yield Type.Apply(Type.Name(c), as)
    }
    def optional(o: Option[T], ns: List[String]): Result[Option[Type]] =
      Option(o).toRight("N081_OPTION_MISSING").flatMap(_.map(tpe(_, ns).map(Some(_))).getOrElse(Right(None)))
    for
      model <- Option(r).toRight("N081_MODEL_MISSING")
      ms <- Option(model.members).toRight("N081_MEMBERS_MISSING")
      _ <- validateNames(names)
      _ <- Either.cond(names.size == ms.size, (), "N081_NAMES_ARITY")
      parent <- optional(model.parent, Nil)
      stats <- collect(ms.zip(names)) { (m, n) =>
        Option(m).toRight("N081_MEMBER_MISSING").flatMap {
          case M.Alias(rhs) => tpe(rhs, names).map(t => Defn.Type(Nil, Type.Name(n), Type.ParamClause(Nil), t, Type.Bounds.empty))
          case M.Abstract(lo, hi) =>
            for
              l <- optional(lo, names)
              h <- optional(hi, names)
            yield Decl.Type(Nil, Type.Name(n), Type.ParamClause(Nil), Type.Bounds(l, h, Nil, Nil))
        }
      }
      tree = Type.Refine(parent, Stat.Block(stats))
      again <- project(tree)
      _ <- Either.cond(again.model == model && again.names == names, (), "N081_CAPTURE_OR_SCOPE_DRIFT")
    yield tree
