package quasiquotes.neutral

import scala.annotation.nowarn
import scala.compiletime.testing.{typeChecks, typeCheckErrors}
import scala.meta.*
import scala.meta.dialects.Scala3

/** Closed syntax research only; no public wildcard Type or compiler binder support. */
@nowarn("cat=deprecation")
final class N082WildcardTypeArgumentBoundsModelFitTest extends munit.FunSuite:
  private enum T derives CanEqual:
    case Atom(name: String)
    case Apply(constructor: String, args: List[T])
    case Wildcard(lower: Option[T], upper: Option[T])
  private case class Occurrence(path: List[String], spelling: String, tokens: String, span: Option[(Int, Int)])
  private case class Projection(model: T, occurrences: List[Occurrence], tokens: String, span: Option[(Int, Int)])
  private type Result[A] = Either[String, A]
  private val wild = T.Wildcard(None, None)
  private val int = T.Atom("Int")
  private val fixtures = List("List[?]", "Option[?]", "Either[?, Int]", "Either[Int, ?]", "Either[?, ?]",
    "List[? <: AnyVal]", "List[? >: Nothing]", "List[? >: Nothing <: AnyVal]",
    "Option[List[? <: AnyVal]]", "List[? <: List[?]]", "List[_]", "List[_ <: AnyVal]")
  private def parse(s: String): Type = Input.String(s).parse[Type].get
  private def nodes(t: Tree): List[Tree] = t :: t.children.flatMap(nodes)
  private def fit(s: String): Projection = project(parse(s)).fold(e => fail(s + ": " + e), identity)

  test("Scala3 parser exposes Type.Wildcard with optional lo hi and separate context view lists"):
    List("?", "_").foreach { s =>
      val w = parse(s).asInstanceOf[Type.Wildcard]
      assertEquals(w.productPrefix, "Type.Wildcard")
      assertEquals(w.productIterator.toList, List(w.bounds))
      assertEquals(w.bounds.lo, None)
      assertEquals(w.bounds.hi, None)
      assertEquals(w.bounds.context, Nil)
      assertEquals(w.bounds.view, Nil)
    }
    fixtures.foreach { s =>
      val t = parse(s)
      assertEquals(t.productPrefix, "Type.Apply")
      assertEquals(t.tokens.map(_.text).mkString, s)
      assertEquals((t.pos.start, t.pos.end), (0, s.length))
      println(s"N082_AST $s => ${t.structure}")
    }
    val both = parse("List[? >: Nothing <: AnyVal]").asInstanceOf[Type.Apply].args.head.asInstanceOf[Type.Wildcard]
    assertEquals(both.bounds.lo.get.structure, "Type.Name(\"Nothing\")")
    assertEquals(both.bounds.hi.get.structure, "Type.Name(\"AnyVal\")")

  test("public projection rejects real wildcard roots and nested argument occurrences precisely"):
    (fixtures ++ List("?", "_", "List[?, ?, ?]", "F[?]", "scala.List[?]")).foreach { s =>
      assertEquals(ScalametaTypeNormalFormProjection.project(parse(s)), Left(NeutralProjectionError(
        "NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED", "unsupported Scalameta type node: Type.Wildcard.")), clues(s))
    }
    val shape = _root_.quasiquotes.parser.TypeShape.Unsupported("Type.Wildcard", "bounds cannot be erased")
    assertEquals(_root_.quasiquotes.types.TypeNormalForm.fromShape(shape).left.toOption.map(_.message),
      Some("Unsupported type syntax for structural normal-form conversion; supported applied constructors are List/1, Option/1, Either/2."))
    assertEquals(_root_.quasiquotes.types.TypeTemplate.fromShape(shape).left.toOption.map(_.message),
      Some("Unsupported type syntax for type-template construction; supported applied constructors are List/1, Option/1, Either/2."))

  test("public authoring cannot turn punctuation identifiers or resolved identity into wildcard roles"):
    import _root_.quasiquotes.types.TypeNormalForm.*
    List("?", "_").foreach { name =>
      val detail = s"Unsupported type identifier `$name`; supported identifiers are Int, String, Boolean, AnyVal."
      val form = STypeApply(STypeIdent("List"), List(STypeIdent(name)))
      assertEquals(ScalametaTypeNormalFormAuthoring.author(form), Left(ScalametaTypeNormalFormAuthoring.Error(
        "NEUTRAL_TYPE_AUTHORING_NORMAL_FORM_REJECTED", detail)))
      assertEquals(_root_.quasiquotes.types.TypeNormalForm.fromShape(
        _root_.quasiquotes.parser.TypeShape.Identifier(name)).left.toOption.map(_.message), Some(detail))
    }
    assertEquals(ScalametaTypeNormalFormAuthoring.author(STypeResolved(
      _root_.quasiquotes.types.StandardResolvedTypeNames.OptionId)).left.toOption.map(_.code),
      Some("NEUTRAL_TYPE_AUTHORING_RESOLVED_UNSUPPORTED"))

  test("admitted primitive fixed tuple and ordinary Function1 Function2 positives stay intact"):
    List("Int", "String", "Boolean", "AnyVal", "List[Int]", "Option[String]", "Either[Int,String]",
      "(Int,String)", "(Int,String,Boolean)", "Int => String", "(Int,String) => Boolean").foreach { s =>
      val p = ScalametaTypeNormalFormProjection.project(parse(s)).toOption.get.normalForm
      val t = ScalametaTypeNormalFormAuthoring.author(p).toOption.get
      assertEquals(t.structure, parse(s).structure)
    }
    val alias = Input.String("type Out = List[?]").parse[Stat].get.asInstanceOf[Defn.Type]
    assertEquals(ScalametaSimpleTypeAliasProjection.project(alias), Left(NeutralProjectionError(
      "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED", "the alias right-hand side is outside the existing N002/Core normal-form family.")))

  test("closed model distinguishes absent lower upper both bounds and ordered argument roles"):
    List(
      "List[?]" -> T.Apply("List", List(wild)),
      "List[? <: AnyVal]" -> T.Apply("List", List(T.Wildcard(None, Some(T.Atom("AnyVal"))))),
      "List[? >: Nothing]" -> T.Apply("List", List(T.Wildcard(Some(T.Atom("Nothing")), None))),
      "List[? >: Nothing <: AnyVal]" -> T.Apply("List", List(T.Wildcard(Some(T.Atom("Nothing")), Some(T.Atom("AnyVal"))))),
      "Either[?, Int]" -> T.Apply("Either", List(wild, int)),
      "Either[Int, ?]" -> T.Apply("Either", List(int, wild)),
      "Option[List[? <: AnyVal]]" -> T.Apply("Option", List(T.Apply("List", List(T.Wildcard(None, Some(T.Atom("AnyVal")))))))
    ).foreach { (s, expected) => assertEquals(project(parse(s)).map(_.model), Right(expected), clues(s)) }

  test("erasing wildcard bounds or replacing wildcard by an ordinary named bound creates collisions"):
    val sources = List("List[?]", "List[? <: AnyVal]", "List[? >: Nothing]", "List[? >: Nothing <: AnyVal]")
    def erase(t: Type): String = t match
      case _: Type.Wildcard => "unknown"
      case n: Type.Name => n.value
      case a: Type.Apply => a.tpe.syntax + a.args.map(erase).mkString("[", ",", "]")
      case _ => fail("unselected erasure fixture")
    assertEquals(sources.map(s => erase(parse(s))).distinct, List("List[unknown]"))
    assertEquals(sources.map(s => fit(s).model).distinct.size, 4)
    assertNotEquals(fit("List[? <: AnyVal]").model, fit("List[AnyVal]").model)
    assertNotEquals(fit("Either[?, Int]").model, fit("Either[Int, ?]").model)
    assertNotEquals(parse("List[?]").structure, parse("List[`?`]").structure)
    assertEquals(project(parse("List[`?`]")).left.toOption, Some("N082_ATOM_UNSUPPORTED"))

  test("fresh genuine wildcard bounds authoring exactly reprojects without original node reuse"):
    fixtures.foreach { s =>
      val original = parse(s)
      val p = project(original).toOption.get
      val t = author(p.model).fold(fail(_), identity)
      assertEquals(t.structure, original.structure, clues(s))
      assertEquals(project(t).map(_.model), Right(p.model))
      assert(nodes(t).forall(_.pos == Position.None))
      assert(!nodes(t).exists(n => nodes(original).exists(_ eq n)))
      assert(t.tokens.nonEmpty)
      assertEquals(project(t).toOption.get.span, None)
      assert(project(t).toOption.get.occurrences.forall(_.span.isEmpty))
      println(s"N082_FRESH $s => ${t.syntax}; tokens=${t.tokens.map(_.text).mkString}")
    }

  test("duplicate wildcard slots have distinct structural paths and source occurrences without binder IDs"):
    val p = fit("Either[?, ?]")
    assertEquals(p.model, T.Apply("Either", List(wild, wild)))
    assertEquals(p.occurrences.map(_.path), List(List("arg[0]"), List("arg[1]")))
    assertEquals(p.occurrences.map(_.span), List(Some((7, 8)), Some((10, 11))))
    assertEquals(p.occurrences.map(_.spelling), List("?", "?"))
    val nested = fit("List[? <: List[?]]")
    assertEquals(nested.occurrences.map(_.path), List(List("arg[0]"), List("arg[0]", "upper", "arg[0]")))
    assertEquals(nested.model, T.Apply("List", List(T.Wildcard(None, Some(T.Apply("List", List(wild)))))))

  test("original spelling comments and grouping remain provenance not fresh source replay"):
    val a = fit("List[?]")
    val b = fit("List[_]")
    assertEquals(a.model, b.model)
    assertEquals(a.occurrences.map(_.spelling), List("?"))
    assertEquals(b.occurrences.map(_.spelling), List("_"))
    val s = "(List[(/* note */ ? <: AnyVal)])"
    val p = fit(s)
    assertEquals(p.model, fit("List[? <: AnyVal]").model)
    assertEquals(p.tokens, s)
    assertEquals(p.span, Some((0, s.length)))
    p.occurrences.foreach { w =>
      val (start, end) = w.span.get
      assertEquals(s.substring(start, end), w.tokens)
    }
    val fresh = author(p.model).toOption.get
    assertNotEquals(project(fresh).toOption.get.tokens, p.tokens)
    assert(!fresh.syntax.contains("note"))
    // Ordinary quoting of a constructor/atom is a separate spelling question.
    List("`List`[?]", "List[? <: `AnyVal`]").foreach { s =>
      assertEquals(project(parse(s)).left.toOption, Some("N082_NAME_SPELLING_UNSUPPORTED"))
    }

  test("zero one two three and runtime N wildcard occurrences compose through fixed applications"):
    def many(n: Int): String = (1 to n).foldRight("Int")((_, rest) => s"Either[?, $rest]")
    List(0, 1, 2, 3, 7).foreach { n =>
      val s = many(n)
      val p = fit(s)
      assertEquals(p.occurrences.size, n)
      val t = author(p.model).toOption.get
      assertEquals(t.structure, parse(s).structure)
      assertEquals(project(t).map(_.model), Right(p.model))
    }
    assertEquals(fit("Either[?, ?]").occurrences.size, 2)
    List(1, 2, 3, 7).foreach { n =>
      val s = "List[" + List.fill(n)("?").mkString(",") + "]"
      val ast = parse(s).asInstanceOf[Type.Apply]
      assertEquals(ast.args.size, n) // Parser cardinality is not declared constructor arity.
      if n == 1 then assert(project(ast).isRight)
      else assertEquals(project(ast).left.toOption, Some("N082_CONSTRUCTOR_ARITY"))
      assertEquals(author(T.Apply("List", List.fill(n)(wild))).isRight, n == 1)
    }
    assert(Input.String("List[]").parse[Type].isInstanceOf[Parsed.Error])
    intercept[org.scalameta.invariants.InvariantFailedException] { Type.Apply(Type.Name("List"), Nil) }
    assertEquals(author(T.Apply("List", Nil)), Left("N082_CONSTRUCTOR_ARITY"))

  test("unknown selected path dependent and wrong arity constructors never acquire declaration authority"):
    List("F[?]", "scala.List[?]", "outer.F[?]", "Outer#F[?]", "Either[?]").foreach { s =>
      assertEquals(project(parse(s)).left.toOption,
        Some(if s == "Either[?]" then "N082_CONSTRUCTOR_ARITY" else "N082_CONSTRUCTOR_UNSUPPORTED"), clues(s))
    }
    assertEquals(author(T.Apply("F", List(wild))), Left("N082_CONSTRUCTOR_UNSUPPORTED"))

  test("wildcards outside argument positions and unsupported bound structures fail closed"):
    List("?", "_", "? <: AnyVal").foreach { s =>
      assertEquals(project(parse(s)).left.toOption, Some("N082_WILDCARD_POSITION"))
    }
    List("List[? <: pkg.T]", "List[? <: (Int | String)]", "List[? <: ([A] =>> A)]",
      "List[? <: (AnyRef { type Out = Int })]", "List[? <: (Int match { case Int => String })]",
      "[A] =>> List[?]", "AnyRef { type Out = List[?] }", "Int match { case List[t] => List[?] }").foreach { s =>
      assertEquals(project(parse(s)).left.toOption, Some("N082_TYPE_NODE_UNSUPPORTED"), clues(s))
    }
    List("List[? : Int]", "List[? <% Int]").foreach { s =>
      val w = parse(s).asInstanceOf[Type.Apply].args.head.asInstanceOf[Type.Wildcard]
      assert(w.bounds.context.nonEmpty || w.bounds.view.nonEmpty)
      assertEquals(project(parse(s)).left.toOption, Some("N082_BOUNDS_KIND_UNSUPPORTED"))
    }
    assertEquals(parse("List[+?]").asInstanceOf[Type.Apply].args.head.productPrefix, "Type.Name")
    assertEquals(project(parse("List[+?]")).left.toOption, Some("N082_ATOM_UNSUPPORTED"))

  test("parser failures authoring validity and actual compiler typing are separate gates"):
    List("List[? <: ]", "List[? >: ]", "List[? >: Nothing <: ]", "List[? <: AnyVal >: Nothing]",
      "List[+ ?]", "List[- ?]").foreach { s =>
      assert(Input.String(s).parse[Type].isInstanceOf[Parsed.Error], clues(s))
    }
    assert(typeChecks("type A = List[?]; type B = List[? <: AnyVal]; type C = List[? >: Nothing]; type D = Either[?, ?]"))
    assert(typeCheckErrors("type A = List[?, ?, ?]").nonEmpty)
    assert(typeCheckErrors("type A = ?").nonEmpty)
    // Observed acceptance is not a proof of inhabitation or bound consistency.
    assert(typeChecks("type A = List[? >: String <: AnyVal]"))
    assert(project(parse("List[? >: String <: AnyVal]")).isRight)

  test("malformed null recursive candidates options lists and misplaced wildcard bounds reject before constructors"):
    val bad: List[(T, String)] = List(
      (null, "N082_TYPE_MISSING"), T.Atom(null) -> "N082_ATOM_UNSUPPORTED", T.Atom("?") -> "N082_ATOM_UNSUPPORTED",
      T.Apply(null, List(wild)) -> "N082_CONSTRUCTOR_UNSUPPORTED", T.Apply("List", null) -> "N082_ARGUMENTS_MISSING",
      T.Apply("List", List(null)) -> "N082_TYPE_MISSING", T.Apply("Either", List(wild)) -> "N082_CONSTRUCTOR_ARITY",
      wild -> "N082_WILDCARD_POSITION",
      T.Apply("List", List(T.Wildcard(null, None))) -> "N082_BOUND_OPTION_MISSING",
      T.Apply("List", List(T.Wildcard(None, null))) -> "N082_BOUND_OPTION_MISSING",
      T.Apply("List", List(T.Wildcard(Some(null), None))) -> "N082_TYPE_MISSING",
      T.Apply("List", List(T.Wildcard(Some(wild), None))) -> "N082_WILDCARD_POSITION",
      T.Apply("List", List(T.Wildcard(None, Some(T.Atom("Double"))))) -> "N082_ATOM_UNSUPPORTED")
    bad.foreach { (t, code) => assertEquals(author(t), Left(code), clues(t)) }
    assertEquals(project(null), Left("N082_SOURCE_MISSING"))

  private val fixed = Map("List" -> 1, "Option" -> 1, "Either" -> 2)
  private val atoms = Set("Int", "String", "Boolean", "AnyVal", "Nothing", "Any", "AnyRef")
  private def atom(s: String): Result[String] =
    Either.cond(s != null && atoms(s), s, "N082_ATOM_UNSUPPORTED")
  private def plain(n: Type.Name): Result[String] =
    Either.cond(n.tokens.map(_.text).mkString == n.value, n.value, "N082_NAME_SPELLING_UNSUPPORTED")
  private def span(t: Tree): Option[(Int, Int)] =
    if t.pos == Position.None then None else Some((t.pos.start, t.pos.end))
  private def collect[A, B](xs: List[A])(f: A => Result[B]): Result[List[B]] =
    xs.foldLeft[Result[List[B]]](Right(Nil))((acc, x) => for { a <- acc; b <- f(x) } yield b :: a).map(_.reverse)

  private def project(source: Type): Result[Projection] =
    def tpe(t: Type, argument: Boolean, path: List[String]): Result[(T, List[Occurrence])] = t match
      case n: Type.Name =>
        for { a <- atom(n.value); _ <- plain(n) } yield (T.Atom(a), Nil)
      case a: Type.Apply =>
        for
          c <- a.tpe match
            case n: Type.Name =>
              fixed.get(n.value).toRight("N082_CONSTRUCTOR_UNSUPPORTED").flatMap(_ => plain(n))
            case _ => Left("N082_CONSTRUCTOR_UNSUPPORTED")
          _ <- Either.cond(fixed(c) == a.args.size, (), "N082_CONSTRUCTOR_ARITY")
          args <- collect(a.args.zipWithIndex) { (t, i) => tpe(t, true, path :+ s"arg[$i]") }
        yield (T.Apply(c, args.map(_._1)), args.flatMap(_._2))
      case w: Type.Wildcard =>
        def bound(o: Option[Type], label: String): Result[(Option[T], List[Occurrence])] = o match
          case None => Right((None, Nil))
          case Some(t) => tpe(t, false, path :+ label).map { (m, ws) => (Some(m), ws) }
        for
          _ <- Either.cond(argument, (), "N082_WILDCARD_POSITION")
          _ <- Either.cond(w.bounds.context.isEmpty && w.bounds.view.isEmpty, (), "N082_BOUNDS_KIND_UNSUPPORTED")
          lower <- bound(w.bounds.lo, "lower")
          upper <- bound(w.bounds.hi, "upper")
          spelling <- w.tokens.iterator.map(_.text).find(s => s == "?" || s == "_")
            .toRight("N082_WILDCARD_SPELLING_UNSUPPORTED")
          occurrence = Occurrence(path, spelling, w.tokens.map(_.text).mkString, span(w))
        yield (T.Wildcard(lower._1, upper._1), occurrence :: (lower._2 ++ upper._2))
      case _ => Left("N082_TYPE_NODE_UNSUPPORTED")
    Option(source).toRight("N082_SOURCE_MISSING").flatMap { t =>
      tpe(t, false, Nil).map { (model, ws) => Projection(model, ws, t.tokens.map(_.text).mkString, span(t)) }
    }

  private def author(model: T): Result[Type] =
    def tpe(t: T, argument: Boolean): Result[Type] = Option(t).toRight("N082_TYPE_MISSING").flatMap {
      case T.Atom(n) => atom(n).map(Type.Name(_))
      case T.Apply(c, args) =>
        for
          arity <- fixed.get(c).toRight("N082_CONSTRUCTOR_UNSUPPORTED")
          xs <- Option(args).toRight("N082_ARGUMENTS_MISSING")
          _ <- Either.cond(xs.size == arity, (), "N082_CONSTRUCTOR_ARITY")
          ts <- collect(xs)(tpe(_, true))
        yield Type.Apply(Type.Name(c), ts)
      case T.Wildcard(lo, hi) =>
        def bound(o: Option[T]): Result[Option[Type]] =
          Option(o).toRight("N082_BOUND_OPTION_MISSING").flatMap {
            case None => Right(None)
            case Some(t) => tpe(t, false).map(Some(_))
          }
        for
          _ <- Either.cond(argument, (), "N082_WILDCARD_POSITION")
          lower <- bound(lo)
          upper <- bound(hi)
        yield Type.Wildcard(Type.Bounds(lower, upper, Nil, Nil))
    }
    for
      fresh <- tpe(model, false)
      again <- project(fresh)
      _ <- Either.cond(again.model == model, (), "N082_AUTHOR_ROUNDTRIP_FAILED")
    yield fresh
