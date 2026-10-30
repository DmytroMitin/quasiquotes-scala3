package quasiquotes.neutral

import scala.annotation.nowarn
import scala.compiletime.testing.{typeChecks, typeCheckErrors}
import scala.meta.*
import scala.meta.dialects.Scala3
import _root_.quasiquotes.types.TypeNormalForm
import _root_.quasiquotes.types.TypeNormalForm.*

/** Closed parameter-site syntax study, not public calling-mode support. */
@nowarn("cat=deprecation")
final class N083ParameterTypeRoleModelFitTest extends munit.FunSuite:
  private enum Role derives CanEqual:
    case Ordinary, ByName, Repeated
  private case class P(name: String, role: Role, underlying: TypeNormalForm) derives CanEqual
  private case class M(clauses: List[List[P]]) derives CanEqual
  private case class Origin(clause: Int, slot: Int, tokens: String, span: Option[(Int, Int)])
  private case class Projection(model: M, origins: List[Origin], tokens: String)
  private type R[A] = Either[String, A]
  private def parse(s: String): Defn.Def = Input.String(s).parse[Stat].get.asInstanceOf[Defn.Def]
  private def source(ps: String): String = s"def f($ps): Int = 0"
  private val int = STypeIdent("Int")
  private val string = STypeIdent("String")
  private val fixtures = List("def f: Int = 0", source(""), source("x: Int"), source("x: => Int"),
    source("xs: String*"), source("x: => List[Int], xs: String*"),
    "def f(x: Int)(y: => Option[String])(zs: Boolean*): Int = 0", source("fn: Int => String"), source("fn: => (Int => String)"), source("fs: (Int => String)*"))
  private def nodes(t: Tree): List[Tree] = t :: t.children.flatMap(nodes)

  test("observed parameter wrappers have distinct AST roles and truthful spans"):
    List("x: Int" -> "Type.Name", "x: => Int" -> "Type.ByName", "xs: String*" -> "Type.Repeated",
      "fn: Int => String" -> "Type.Function", "fn: Int ?=> String" -> "Type.ContextFunction").foreach { (p, kind) =>
      val s = source(p)
      val t = parse(s).paramClauseGroups.head.paramClauses.head.values.head.decltpe.get
      assertEquals(t.productPrefix, kind)
      assertEquals(s.substring(t.pos.start, t.pos.end), t.tokens.map(_.text).mkString)
      println(s"N083_AST $s => ${t.structure}; tokens=${t.tokens.map(_.text).mkString}; span=${t.pos}")
    }
    List("=> Int", "Int*", "=> Int*", "(=> Int)").foreach { s =>
      val t = Input.String(s).parse[Type].get
      assert(t.isInstanceOf[Type.ByName] || t.isInstanceOf[Type.Repeated])
      println(s"N083_ROOT $s => ${t.structure}")
    }

  test("public Type and ordinary typed method errors stay distinct and exact"):
    List("x: => Int" -> "Type.ByName", "xs: Int*" -> "Type.Repeated").foreach { (p, kind) =>
      val d = parse(source(p)); val t = d.paramClauseGroups.head.paramClauses.head.values.head.decltpe.get
      assertEquals(ScalametaTypeNormalFormProjection.project(t), Left(NeutralProjectionError(
        "NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED", s"unsupported Scalameta type node: $kind.")))
      assertEquals(ScalametaTypedSingleParameterDefProjection.project(d), Left(NeutralProjectionError(
        "NEUTRAL_SINGLE_PARAMETER_DEF_TOPOLOGY_UNSUPPORTED", "by-name and repeated parameter Types are outside the ordinary parameter topology.")))
      assertEquals(ScalametaDefinitionProjection.project(d).left.toOption, ScalametaTypedSingleParameterDefProjection.project(d).left.toOption)
    }
    List("Int", "List[Int]", "Option[String]", "Either[Int,String]", "(Int,String)",
      "Int => String", "(Int,String) => Boolean").foreach { s =>
      val t = Input.String(s).parse[Type].get
      val nf = ScalametaTypeNormalFormProjection.project(t).toOption.get.normalForm
      assertEquals(ScalametaTypeNormalFormAuthoring.author(nf).toOption.get.structure, t.structure)
    }
    val ordinary = ScalametaTypedSingleParameterDefProjection.project(parse(source("x: Int"))).toOption.get.shape
      .asInstanceOf[_root_.quasiquotes.definitions.DefinitionShape.SingleParameterDef]
    assert(ScalametaTypedSingleParameterDefAuthoring.author(ordinary).isRight)
    val invalid = _root_.quasiquotes.definitions.DefinitionShape.singleParameterDef(ordinary.name, ordinary.parameterBinderId, ordinary.parameterName,
      _root_.quasiquotes.parser.TypeShape.Unsupported("Type.ByName", "=> Int"), ordinary.resultType, ordinary.body)
    assert(invalid.isLeft)

  test("role erasure collides while underlying function syntax stays separate"):
    val expected = List(Role.Ordinary, Role.ByName, Role.Repeated)
    val results = List("x: Int", "x: => Int", "x: Int*").map(p => project(parse(source(p))).toOption.get.model.clauses.head.head)
    assertEquals(results.map(_.underlying).distinct, List(int))
    assertEquals(results.map(_.role), expected)
    assertEquals(results.distinct.size, 3)
    assertEquals(project(parse(source("x: => Int"))).map(_.model), Right(M(List(List(P("x", Role.ByName, int))))))

  test("fresh method ASTs reproject roles order clauses and omit original provenance"):
    fixtures.foreach { s =>
      val d = parse(s); val p = project(d).fold(fail(_), identity)
      val fresh = author(p.model).fold(fail(_), identity)
      assertEquals(fresh.structure, d.structure, clues(s))
      assertEquals(project(fresh).map(_.model), Right(p.model))
      assert(nodes(fresh).forall(_.pos == Position.None))
      assert(!nodes(fresh).exists(n => nodes(d).exists(_ eq n)))
      assert(project(fresh).toOption.get.origins.forall(_.span.isEmpty))
      assertEquals(parse(fresh.syntax).structure, fresh.structure)
    }
    val p = project(parse("def f(/* note */ x: => Int): Int = 0")).toOption.get
    assert(p.tokens.contains("note"))
    assert(!author(p.model).toOption.get.syntax.contains("note"))
    assertEquals(p.origins.map(o => (o.clause, o.slot)), List((0,0)))

  test("zero one two three and seven ordered parameters and clauses have no arity-named API"):
    List(0,1,2,3,7).foreach { n =>
      val args = (1 to n).map(i => s"p$i: => Int").mkString(", ")
      val p = project(parse(source(args))).toOption.get
      assertEquals(p.model.clauses.head.size, n)
      assertEquals(author(p.model).toOption.get.structure, parse(source(args)).structure)
      val s = "def f" + (1 to n).map(i => s"(p$i: => Int)").mkString + ": Int = 0"
      val m = project(parse(s)).toOption.get.model
      assertEquals(m.clauses.size, n)
      assertEquals(author(m).toOption.get.structure, parse(s).structure)
    }
    assertNotEquals(project(parse("def f: Int = 0")).map(_.model), project(parse(source(""))).map(_.model))

  test("unsupported syntax and calling-context choices fail closed"):
    List(source("x: => F[Int]"), source("x: => List[?]"), source("x: => pkg.T"),
      source("x: => (AnyRef { type Out = Int })"), source("x: => ([A] =>> A)"),
      source("x: => (Int ?=> String)"), source("`x`: => Int"), source("x: => Int = 1"),
      "def f(using x: Int): Int = 0", "def f(using x: => Int): Int = 0", "def f(using xs: Int*): Int = 0", source("x: => Int*"), source("x"), "def f[A](x: => Int): Int = 0",
      source("xs: Int*, y: Int"), source("xs: Int*, ys: Int*"),
      "def f(xs: Int*)(y: Int): Int = 0", "def f(x: Int): Int* = 0", "def f(x: Int): => Int = 0").foreach { s =>
      Input.String(s).parse[Stat] match
        case Parsed.Success(d: Defn.Def) => assert(project(d).isLeft, clues(s))
        case e => println(s"N083_PARSE_REJECTION $s => $e")
    }
    assert(typeChecks("def f(x: => Int, xs: String*): Int = 0"))
    assert(typeCheckErrors("def f(xs: Int*, y: Int): Int = 0").nonEmpty)
    assert(typeCheckErrors("def f(xs: Int*, ys: Int*): Int = 0").nonEmpty)

  test("malformed role models fail before AST construction"):
    def one(p: P): M = M(List(List(p)))
    val bad = List[M](null, M(null), M(List(null)), one(null),
      one(P(null, Role.Ordinary, int)), one(P("", Role.Ordinary, int)),
      one(P("x", null, int)), one(P("x", Role.ByName, null)),
      one(P("x", Role.ByName, STypeIdent("?"))),
      one(P("x", Role.ByName, STypeApply(STypeIdent("List"), null))),
      one(P("x", Role.ByName, STypeTuple(null))), one(P("x", Role.ByName, STypeFunction(null, int))),
      one(P("x", Role.ByName, STypeApply(null, List(int)))), one(P("x", Role.ByName, STypeFunction(List(int), null))),
      M(List(List(P("x", Role.Repeated, int), P("y", Role.Ordinary, int)))),
      M(List(List(P("x", Role.Ordinary, int), P("x", Role.ByName, int)))))
    bad.foreach { m => assert(author(m).isLeft, clues(m)) }
    assertEquals(project(null), Left("N083_SOURCE_MISSING"))

  private def collect[A,B](xs: List[A])(f: A => R[B]): R[List[B]] =
    xs.foldLeft[R[List[B]]](Right(Nil))((acc,x) => for { a <- acc; b <- f(x) } yield b :: a).map(_.reverse)
  private def validName(s: String): Boolean = s != null && s.matches("[a-z][A-Za-z0-9]*")
  private def validate(m: M): R[M] =
    for
      model <- Option(m).toRight("N083_MODEL_MISSING")
      clauses <- Option(model.clauses).toRight("N083_CLAUSES_MISSING")
      _ <- collect(clauses.zipWithIndex) { (clause, ci) =>
        for
          ps <- Option(clause).toRight("N083_CLAUSE_MISSING")
          _ <- collect(ps.zipWithIndex) { (p, pi) =>
            for
              value <- Option(p).toRight("N083_PARAMETER_MISSING")
              _ <- Either.cond(validName(value.name), (), "N083_NAME_UNSUPPORTED")
              role <- Option(value.role).toRight("N083_ROLE_MISSING")
              _ <- Either.cond(role != Role.Repeated || (ci == clauses.size - 1 && pi == ps.size - 1), (), "N083_REPEATED_POSITION")
              _ <- ScalametaTypeNormalFormAuthoring.author(value.underlying).left.map(_ => "N083_UNDERLYING_UNSUPPORTED")
            yield ()
          }
        yield ()
      }
      names = clauses.flatten.map(_.name)
      _ <- Either.cond(names.distinct.size == names.size, (), "N083_DUPLICATE_NAME")
    yield model

  private def project(d: Defn.Def): R[Projection] =
    for
      source <- Option(d).toRight("N083_SOURCE_MISSING")
      _ <- Either.cond(source.mods.isEmpty && source.name.value == "f" && source.name.tokens.map(_.text).mkString == "f" &&
        source.decltpe.exists(_.structure == Type.Name("Int").structure) && source.body.structure == Lit.Int(0).structure,
        (), "N083_METHOD_ENVELOPE")
      clauses <- source.paramClauseGroups match
        case Nil => Right(Nil)
        case group :: Nil if group.tparamClause.values.isEmpty => Right(group.paramClauses)
        case _ => Left("N083_GENERIC_OR_GROUP_UNSUPPORTED")
      projected <- collect(clauses.zipWithIndex) { (c, ci) =>
        for
          _ <- Either.cond(c.mod.isEmpty, (), "N083_CONTEXTUAL_UNSUPPORTED")
          ps <- collect(c.values.zipWithIndex) { (p, pi) =>
            for
              _ <- Either.cond(p.mods.isEmpty && p.default.isEmpty, (), "N083_PARAMETER_HEADER")
              name <- p.name match
                case n: Term.Name if validName(n.value) && n.tokens.map(_.text).mkString == n.value => Right(n.value)
                case _ => Left("N083_NAME_UNSUPPORTED")
              t <- p.decltpe.toRight("N083_TYPE_MISSING")
              pair = t match
                case Type.ByName(inner) => Role.ByName -> inner
                case Type.Repeated(inner) => Role.Repeated -> inner
                case other => Role.Ordinary -> other
              nf <- ScalametaTypeNormalFormProjection.project(pair._2).left.map(_ => "N083_UNDERLYING_UNSUPPORTED")
            yield (P(name, pair._1, nf.normalForm), Origin(ci, pi, t.tokens.map(_.text).mkString,
              if t.pos == Position.None then None else Some((t.pos.start,t.pos.end))))
          }
        yield ps
      }
      model <- validate(M(projected.map(_.map(_._1))))
    yield Projection(model, projected.flatten.map(_._2), source.tokens.map(_.text).mkString)

  private def author(m: M): R[Defn.Def] =
    for
      model <- validate(m)
      clauses <- collect(model.clauses) { ps =>
        collect(ps) { p =>
          ScalametaTypeNormalFormAuthoring.author(p.underlying).left.map(_ => "N083_UNDERLYING_UNSUPPORTED").map { t =>
            val wrapped = p.role match
              case Role.Ordinary => t
              case Role.ByName => Type.ByName(t)
              case Role.Repeated => Type.Repeated(t)
            Term.Param(Nil, Term.Name(p.name), Some(wrapped), None)
          }
        }.map(Term.ParamClause(_))
      }
      groups = if clauses.isEmpty then Nil else List(Member.ParamClauseGroup(Type.ParamClause(Nil), clauses))
      fresh = Defn.Def(Nil, Term.Name("f"), groups, Some(Type.Name("Int")), Lit.Int(0))
      again <- project(fresh)
      _ <- Either.cond(again.model == model, (), "N083_ROUNDTRIP_FAILED")
    yield fresh
