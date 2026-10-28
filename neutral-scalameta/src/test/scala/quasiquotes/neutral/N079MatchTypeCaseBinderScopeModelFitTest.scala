package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

/** Closed syntax-only research oracle; no production Type admission or compiler identity. */
@nowarn("cat=deprecation")
final class N079MatchTypeCaseBinderScopeModelFitTest extends munit.FunSuite:
  private enum T derives CanEqual:
    case Atom(name: String)
    case Free(name: String)
    case BoundCase(depth: Int, index: Int)
    case Apply(constructor: String, arguments: List[T])
    case Match(scrutinee: T, cases: List[C])
  private enum P derives CanEqual:
    case Wildcard
    case Atom(name: String)
    case PatternBinder(index: Int)
    case Apply(constructor: String, arguments: List[P])
  private final case class C(pattern: P, binderCount: Int, result: T) derives CanEqual
  private final case class Provenance(names: Vector[List[String]], tokens: String, span: Option[(Int, Int)])
  private final case class Projection(model: T, source: Provenance)
  private type Result[A] = Either[String, A]

  test("case pattern declaration and result reference have distinct typed scoped roles"):
    val source = parse("X match { case List[t] => Either[t, X] }")
    assertEquals(
      project(source).map(_.model),
      Right(T.Match(T.Free("X"), List(C(
        P.Apply("List", List(P.PatternBinder(0))), 1,
        T.Apply("Either", List(T.BoundCase(0, 0), T.Free("X")))
      ))))
    )

  private def parse(source: String): Type = Input.String(source).parse[Type].get
  private def project(source: Type): Result[Projection] = projectWithin(source, Set.empty)

  test("actual Scala3 AST uses Type.Name for simple two-binder repeated and nested patterns"):
    val fixtures = List(
      "X match { case t => t }" -> "Type.Name",
      "X match { case Either[a,b] => Either[b,a] }" -> "Type.Apply",
      "X match { case Either[t,t] => t }" -> "Type.Apply",
      "X match { case List[Option[t]] => t }" -> "Type.Apply",
      "X match { case _ => X }" -> "Type.PatWildcard"
    )
    fixtures.foreach { (source, patternClass) =>
      val matched = parse(source).asInstanceOf[Type.Match]
      assertEquals(matched.productPrefix, "Type.Match")
      assertEquals(matched.cases.head.productPrefix, "TypeCase")
      assertEquals(matched.cases.head.pat.productPrefix, patternClass)
      assertEquals(matched.tokens.map(_.text).mkString, source)
    }
    val clause = parse("X match { case List[t] => Either[t,X] }")
      .asInstanceOf[Type.Match].cases.head
    val declaration = clause.pat.asInstanceOf[Type.Apply].args.head
    val use = clause.body.asInstanceOf[Type.Apply].args.head
    assertEquals(declaration.structure, "Type.Name(\"t\")")
    assertEquals(use.structure, declaration.structure)
    assertNotEquals(declaration.pos, use.pos)
    val quoted = parse("X match { case List[`t`] => X }").asInstanceOf[Type.Match]
    val quotedName = quoted.cases.head.pat.asInstanceOf[Type.Apply].args.head
    assertEquals(quotedName.structure, declaration.structure)
    assertEquals(quotedName.tokens.map(_.text).mkString, "`t`")

  test("alpha renaming preserves indexed meaning and keeps names tokens and source spans separate"):
    val first = fit("X match { case List[t] => Option[t] }")
    val second = fit("X match { case List[u] => Option[u] }")
    assertEquals(first.model, second.model)
    assertEquals(first.source.names, Vector(List("t")))
    assertEquals(second.source.names, Vector(List("u")))
    assertNotEquals(first.source.tokens, second.source.tokens)
    assert(first.source.span.nonEmpty)
    assertEquals(fit("X match { case t => X }").model,
      T.Match(T.Free("X"), List(C(P.PatternBinder(0), 1, T.Free("X")))))
    assertNotEquals(fit("X match { case t => t }").model,
      fit("X match { case t => X }").model)

  test("two binders use declaration order and preserve result permutations"):
    val first = fit("X match { case Either[a,b] => Either[b,a] }")
    assertEquals(first.model, T.Match(T.Free("X"), List(C(
      P.Apply("Either", List(P.PatternBinder(0), P.PatternBinder(1))), 2,
      T.Apply("Either", List(T.BoundCase(0,1), T.BoundCase(0,0)))
    ))))
    assertEquals(first.model, fit("X match { case Either[t,u] => Either[u,t] }").model)
    assertNotEquals(first.model, fit("X match { case Either[a,b] => Either[a,b] }").model)
    roundtrip(first)

  test("sibling binders reset their frames and cannot leak into a different case or scrutinee"):
    val two = fit("X match { case List[t] => t\n case Option[t] => t }")
    val cases = two.model.asInstanceOf[T.Match].cases
    assertEquals(cases.map(_.result), List(T.BoundCase(0,0), T.BoundCase(0,0)))
    assertEquals(two.source.names, Vector(List("t"), List("t")))
    assertEquals(project(parse("X match { case List[t] => t\n case _ => t }")), Left("N079_UNBOUND_NAME"))
    assertEquals(project(parse("X match { case List[t] => u\n case Option[u] => u }")), Left("N079_UNBOUND_NAME"))
    assertEquals(project(parse("t match { case t => t }")), Left("N079_UNBOUND_NAME"))
    roundtrip(two)

  test("nested result match shadows nearest binders and captures outer binders at explicit depth"):
    val capture = fit("X match { case List[t] => Y match { case Option[u] => Either[t,u] } }")
    val inner = capture.model.asInstanceOf[T.Match].cases.head.result.asInstanceOf[T.Match]
    assertEquals(inner.scrutinee, T.Free("Y"))
    assertEquals(inner.cases.head.result,
      T.Apply("Either", List(T.BoundCase(1,0), T.BoundCase(0,0))))
    val shadow = fit("X match { case List[t] => t match { case Option[t] => t } }")
    val nested = shadow.model.asInstanceOf[T.Match].cases.head.result.asInstanceOf[T.Match]
    assertEquals(nested.scrutinee, T.BoundCase(0,0))
    assertEquals(nested.cases.head.result, T.BoundCase(0,0))
    assertNotEquals(capture.model, shadow.model)
    roundtrip(capture)
    roundtrip(shadow)
    val throughEmpty = fit("X match { case t => Y match { case _ => t } }")
    assertEquals(throughEmpty.model.asInstanceOf[T.Match].cases.head.result.asInstanceOf[T.Match]
      .cases.head.result, T.BoundCase(1,0))
    roundtrip(throughEmpty)

  test("inner declarations shadow pattern names but wildcard cases preserve outer captures"):
    val p = fit("X match { case t => Y match { case t => t\n case _ => t } }")
    val cases = p.model.asInstanceOf[T.Match].cases.head.result.asInstanceOf[T.Match].cases
    assertEquals(cases.map(_.result), List(T.BoundCase(0,0), T.BoundCase(1,0)))
    roundtrip(p)

  test("nested constructor patterns and nested scrutinees author and reproject"):
    val sources = List(
      "X match { case List[Option[t]] => Either[t,X] }",
      "(X match { case t => Option[t] }) match { case List[u] => u }",
      "List[X match { case Either[a,b] => Either[b,a] }]"
    )
    sources.foreach(source => roundtrip(fit(source)))

  test("0 cases rejected while 1 2 3 and N preserve order and duplicates"):
    List(1, 2, 3, 7).foreach { count =>
      val source = s"X match { ${List.fill(count)("case List[t] => t").mkString("\n")} }"
      val p = fit(source)
      assertEquals(p.model.asInstanceOf[T.Match].cases.size, count)
      assertEquals(p.source.names.size, count)
      roundtrip(p)
    }
    val ordered = fit("X match { case t => t\n case _ => X }")
    val reversed = fit("X match { case _ => X\n case t => t }")
    assertNotEquals(ordered.model, reversed.model)
    assertEquals(author(T.Match(T.Free("X"), Nil), Vector.empty), Left("N079_CASES_EMPTY"))

  test("fresh authoring has synthetic tokens without source ownership"):
    val p = fit("X match { case List[t] => Option[t] }")
    val authored = author(p.model, p.source.names).toOption.get
    val fresh = project(authored).toOption.get
    assertEquals(authored.pos, Position.None)
    assert(authored.tokens.nonEmpty)
    assertEquals(fresh.source.span, None)
    assertEquals(fresh.source.names, p.source.names)
    assertEquals(fresh.model, p.model)
    assertNotEquals(fresh.source, p.source)
    val renamed = author(p.model, Vector(List("u"))).toOption.get
    assertEquals(project(renamed).map(_.model), Right(p.model))
    assertEquals(parse(renamed.syntax).structure, renamed.structure)

  test("backticks cannot turn identical decoded Type.Name into guessed binder authority"):
    List(
      "X match { case List[`t`] => X }",
      "X match { case List[t] => `t` }",
      "X match { case `t` => X }",
      "X match { case `List`[t] => t }"
    ).foreach { source => assertEquals(project(parse(source)), Left("N079_LEXICAL_NAME_UNSUPPORTED"), clues(source)) }
    assertEquals(project(parse("X match { case List[unknown] => X }")), Left("N079_PATTERN_NAME_UNSUPPORTED"))
    assertEquals(project(parse("X match { case X => X }")), Left("N079_PATTERN_NAME_UNSUPPORTED"))

  test("duplicate binder declarations fail closed instead of claiming equality constraints"):
    List("Either[t,t]", "Either[List[t],Option[t]]").foreach { pattern =>
      assertEquals(project(parse(s"X match { case $pattern => t }")), Left("N079_DUPLICATE_BINDER"))
    }
    val duplicate = T.Match(T.Free("X"), List(C(P.Apply("Either",
      List(P.PatternBinder(0), P.PatternBinder(0))), 2, T.BoundCase(0,0))))
    assertEquals(author(duplicate, Vector(List("t","u"))), Left("N079_BINDER_DECLARATION_ORDER"))
    assertEquals(author(fit("X match { case Either[a,b] => a }").model,
      Vector(List("t","t"))), Left("N079_BINDER_NAMES_INVALID"))

  test("reserved constructor and free names cannot be supplied as binder authoring provenance"):
    val model = fit("X match { case t => List[t] }").model
    List("List", "Option", "Either", "X", "Int", "`t`", "unknown", "").foreach { name =>
      assertEquals(author(model, Vector(List(name))), Left("N079_BINDER_NAMES_INVALID"), clues(name))
    }
    List("List", "Option", "Either").foreach { constructor =>
      val args = if constructor == "Either" then "Int,String" else "Int"
      val pattern = parse(s"X match { case $constructor[$args] => Int }")
      val result = parse(s"X match { case _ => $constructor[$args] }")
      assertEquals(projectWithin(pattern, Set(constructor)), Left("N079_CONSTRUCTOR_SHADOWED"))
      assertEquals(projectWithin(result, Set(constructor)), Left("N079_CONSTRUCTOR_SHADOWED"))
      val model = fit(s"X match { case _ => $constructor[$args] }")
      assertEquals(author(model.model, model.source.names, Set(constructor)), Left("N079_CONSTRUCTOR_SHADOWED"))
    }
    val lambda = parse("[List] =>> X match { case List[t] => t }").asInstanceOf[Type.Lambda]
    assertEquals(project(lambda), Left("N079_TYPE_NODE_UNSUPPORTED"))
    assertEquals(projectWithin(lambda.tpe, Set("List")), Left("N079_CONSTRUCTOR_SHADOWED"))

  test("author rejects renaming that would capture an outer reference"):
    val p = fit("X match { case t => Y match { case u => Either[t,u] } }")
    assertEquals(author(p.model, Vector(List("t"), List("t"))), Left("N079_AUTHOR_CAPTURE"))
    assert(author(p.model, Vector(List("u"), List("t"))).isRight)
    assertEquals(author(T.BoundCase(0,0), Vector.empty), Left("N079_BOUND_REFERENCE_INVALID"))
    val bad = T.Match(T.Free("X"), List(C(P.PatternBinder(0),1,T.BoundCase(0,1))))
    assertEquals(author(bad,Vector(List("t"))),Left("N079_BOUND_REFERENCE_INVALID"))
    val negative = T.Match(T.Free("X"), List(C(P.PatternBinder(0),1,T.BoundCase(-1,0))))
    assertEquals(author(negative,Vector(List("t"))),Left("N079_BOUND_REFERENCE_INVALID"))

  test("parser errors invariants and candidate unsupported nodes stay independent"):
    List(
      "X match { case t: Int => t }",
      "X match { case t <: Any => t }",
      "X match { case List[t] => }"
    ).foreach(source => assertEquals(parseResult(source), Left("N079_PARSE_ERROR"), clues(source)))
    assertEquals(parseResult("X match {}"), Left("N079_PARSER_THROWN"))
    assertEquals(parseResult("X match { case t => [A] =>> t }"), Left("N079_PARSER_THROWN"))
    List("Int | String", "Int & AnyVal", "1", "(Int, String)").foreach { pattern =>
      assertEquals(project(parse(s"X match { case $pattern => Int }")), Left("N079_PATTERN_NODE_UNSUPPORTED"))
    }
    List("types.List[t]", "Box[t]", "t[Int]").foreach { pattern =>
      assertEquals(project(parse(s"X match { case $pattern => Int }")), Left("N079_PATTERN_CONSTRUCTOR_UNSUPPORTED"))
    }
    assertEquals(project(parse("X match { case t => Box[t] }")), Left("N079_CONSTRUCTOR_UNSUPPORTED"))
    assertEquals(project(parse("X match { case t => Int | String }")), Left("N079_TYPE_NODE_UNSUPPORTED"))
    val nestedPattern = Type.Match(Type.Name("X"), List(TypeCase(
      Type.Match(Type.Name("Y"), List(TypeCase(Type.PatWildcard(),Type.Name("Int")))), Type.Name("Int"))))
    assertEquals(project(nestedPattern), Left("N079_PATTERN_NODE_UNSUPPORTED"))

  test("null malformed carrier lists arities provenance and dangling declarations fail deterministically"):
    val valid = fit("X match { case t => t }")
    val badModels = List(
      (null, Vector.empty[List[String]], "N079_TYPE_MISSING"),
      (T.Match(null,Nil), Vector.empty[List[String]], "N079_TYPE_MISSING"),
      (T.Match(T.Free("X"),null), Vector.empty[List[String]], "N079_CASES_MISSING"),
      (T.Match(T.Free("X"),List(null)), Vector.empty[List[String]], "N079_CASE_MISSING"),
      (T.Match(T.Free("X"),List(C(null,0,T.Free("X")))), Vector(Nil), "N079_PATTERN_MISSING"),
      (T.Match(T.Free("X"),List(C(P.Wildcard,0,null))), Vector(Nil), "N079_TYPE_MISSING"),
      (T.Match(T.Free("X"),List(C(P.Wildcard,1,T.Free("X")))), Vector(List("t")), "N079_BINDER_COUNT_MISMATCH"),
      (T.Apply("List",null), Vector.empty[List[String]], "N079_ARGUMENTS_MISSING"),
      (T.Apply("Either",List(T.Atom("Int"))), Vector.empty[List[String]], "N079_CONSTRUCTOR_ARITY"),
      (T.Apply("Box",List(T.Atom("Int"))), Vector.empty[List[String]], "N079_CONSTRUCTOR_UNSUPPORTED"),
      (T.Free("t"), Vector.empty[List[String]], "N079_TYPE_NAME_UNSUPPORTED")
    )
    badModels.foreach { (model,names,code) => assertEquals(author(model,names),Left(code),clues(model)) }
    assertEquals(project(null), Left("N079_TYPE_MISSING"))
    assertEquals(author(valid.model,null), Left("N079_PROVENANCE_MISSING"))
    assertEquals(author(valid.model,Vector(null)), Left("N079_BINDER_NAMES_INVALID"))
    assertEquals(author(valid.model,Vector.empty), Left("N079_PROVENANCE_MISSING"))
    assertEquals(author(valid.model,valid.source.names :+ Nil), Left("N079_PROVENANCE_UNUSED"))
    assertEquals(author(valid.model,Vector(Nil)), Left("N079_BINDER_COUNT_MISMATCH"))

  test("public projection and existing carriers stay closed while ordinary Type positives survive"):
    import _root_.quasiquotes.types.TypeNormalForm.*
    List(
      "X match { case t => t }",
      "X match { case List[t] => Either[t,X] }",
      "X match { case Either[a,b] => b }",
      "List[X match { case t => t }]",
      "X match { case t => Y match { case u => Either[t,u] } }"
    ).foreach { source =>
      assertEquals(ScalametaTypeNormalFormProjection.project(parse(source)).left.toOption.map(_.code),
        Some("NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED"))
    }
    val positives = List(
      "Int" -> STypeIdent("Int"),
      "List[Int]" -> STypeApply(STypeIdent("List"),List(STypeIdent("Int"))),
      "Option[String]" -> STypeApply(STypeIdent("Option"),List(STypeIdent("String"))),
      "Either[Int,String]" -> STypeApply(STypeIdent("Either"),List(STypeIdent("Int"),STypeIdent("String"))),
      "(Int,String)" -> STypeTuple(List(STypeIdent("Int"),STypeIdent("String"))),
      "Int => String" -> STypeFunction(List(STypeIdent("Int")),STypeIdent("String"))
    )
    positives.foreach { (source, expected) =>
      assertEquals(ScalametaTypeNormalFormProjection.project(parse(source)).map(_.normalForm),Right(expected))
      assertEquals(ScalametaTypeNormalFormProjection.project(
        ScalametaTypeNormalFormAuthoring.author(expected).toOption.get).map(_.normalForm),Right(expected))
    }
    val unsupported = _root_.quasiquotes.parser.TypeShape.Unsupported("Type.Match","case binder")
    assert(_root_.quasiquotes.types.TypeNormalForm.fromShape(unsupported).isLeft)
    assert(_root_.quasiquotes.types.TypeTemplate.fromShape(unsupported).isLeft)
    val alias = Input.String("type R = X match { case t => t }").parse[Stat].get.asInstanceOf[Defn.Type]
    assertEquals(ScalametaSimpleTypeAliasProjection.project(alias).left.toOption.map(_.code),
      Some("NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"))

  // Read the unchanged N078 private oracle without exporting a new production/test API.
  test("unchanged N078 oracle still rejects the selected named-binder cases"):
    val oracle = new N078Scala3MatchTypeCaseModelFitTest
    val method = oracle.getClass.getDeclaredMethods.find(_.getName == "candidateProject").get
    method.setAccessible(true)
    List("t", "List[t]", "Either[a,b]", "List[Option[t]]").foreach { pattern =>
      val result = method.invoke(oracle, parse(s"X match { case $pattern => X }"))
        .asInstanceOf[Either[Product, Any]]
      assertEquals(result.left.toOption.map(_.productElement(0)), Some("N078_PATTERN_BINDER_UNSUPPORTED"))
    }

  test("nested scrutinee AST roundtrip is separate from Scalameta printer text reparsing"):
    val p = fit("(X match { case t => Option[t] }) match { case List[u] => u }")
    val tree = author(p.model,p.source.names).toOption.get
    assertEquals(project(tree).map(_.model),Right(p.model))
    // Scalameta 4.17.3 fresh printer omits the necessary scrutinee parentheses.
    assertEquals(parseResult(tree.syntax),Left("N079_PARSE_ERROR"))
    assertEquals(parseResult("X match { case t => t; case _ => X }"),Left("N079_PARSE_ERROR"))

  test("nested patterns retain 3 and 7 declaration slots without arity-specific carriers"):
    val three = fit("X match { case Either[a,Either[b,c]] => Either[c,a] }")
    val seven = fit("X match { case Either[a,Either[b,Either[c,Either[t,Either[u,Either[v,w]]]]]] => Either[w,a] }")
    val c3 = three.model.asInstanceOf[T.Match].cases.head
    val c7 = seven.model.asInstanceOf[T.Match].cases.head
    assertEquals(c3.binderCount,3)
    assertEquals(c7.binderCount,7)
    assertEquals(c3.result,T.Apply("Either",List(T.BoundCase(0,2),T.BoundCase(0,0))))
    assertEquals(c7.result,T.Apply("Either",List(T.BoundCase(0,6),T.BoundCase(0,0))))
    roundtrip(three)
    roundtrip(seven)

  test("outer lambda capture bounds and malformed Scalameta construction are separate rejected layers"):
    val lambda = parse("[t] =>> X match { case u => t }").asInstanceOf[Type.Lambda]
    assertEquals(project(lambda),Left("N079_TYPE_NODE_UNSUPPORTED"))
    assertEquals(projectWithin(lambda.tpe,Set("t")),Left("N079_OUTER_BINDER_UNSUPPORTED"))
    assertEquals(parseResult("X match { case List[t <: Any] => t }"),Left("N079_PARSE_ERROR"))
    // Invalid source ASTs are guarded by Scalameta before the candidate can receive them.
    def constructionRejected(cases: List[TypeCase]): Boolean =
      try
        Type.Match(Type.Name("X"),cases)
        false
      catch case scala.util.control.NonFatal(_) => true
    assert(constructionRejected(Nil))
    assert(constructionRejected(null))

  private def fit(source: String): Projection = project(parse(source)).toOption.get
  private def roundtrip(p: Projection): Unit =
    val tree = author(p.model, p.source.names).toOption.get
    assertEquals(project(tree).map(_.model), Right(p.model))

  private def parseResult(source: String): Result[Type] =
    try
      Input.String(source).parse[Type] match
        case Parsed.Success(tree) => Right(tree)
        case _: Parsed.Error => Left("N079_PARSE_ERROR")
    catch case scala.util.control.NonFatal(_) => Left("N079_PARSER_THROWN")
  // Binder spellings are an explicit closed syntax policy, not isLower-based symbol inference.
  private val binders = Set("t", "u", "v", "w", "a", "b", "c")
  private val atoms = Set("Int", "String", "Boolean", "AnyVal", "Any", "Nothing")
  private val frees = Set("X", "Y", "Z", "A", "B", "C")
  private val constructors = Map("List" -> 1, "Option" -> 1, "Either" -> 2)
  private type Scopes = List[List[String]]

  private def position(name: String, scopes: Scopes): Option[(Int, Int)] =
    scopes.zipWithIndex.collectFirst {
      case (names, depth) if names.contains(name) => (depth, names.indexOf(name))
    }

  // Decoded Type.Name alone discards quoting. A quoted source name is never guessed to bind.
  private def plain(name: Type.Name): Result[String] =
    Either.cond(name.tokens.map(_.text).mkString == name.value,
      name.value, "N079_LEXICAL_NAME_UNSUPPORTED")

  private def traverse[A, B](values: List[A])(f: A => Result[B]): Result[List[B]] =
    values.foldLeft[Result[List[B]]](Right(Nil)) { (acc, value) =>
      for
        previous <- acc
        next <- f(value)
      yield next :: previous
    }.map(_.reverse)

  private def arity[A](name: String, args: List[A], pattern: Boolean): Result[Unit] =
    for
      expected <- constructors.get(name).toRight(
        if pattern then "N079_PATTERN_CONSTRUCTOR_UNSUPPORTED" else "N079_CONSTRUCTOR_UNSUPPORTED")
      _ <- Either.cond(args != null, (), "N079_ARGUMENTS_MISSING")
      _ <- Either.cond(args.size == expected, (), "N079_CONSTRUCTOR_ARITY")
    yield ()

  private def projectWithin(source: Type, denied: Set[String]): Result[Projection] =
    val evidence = scala.collection.mutable.ArrayBuffer.empty[List[String]]
    def pattern(tree: Type, scopes: Scopes, names: scala.collection.mutable.ArrayBuffer[String]): Result[P] =
      Option(tree).toRight("N079_PATTERN_MISSING").flatMap {
        case _: Type.PatWildcard => Right(P.Wildcard)
        case name: Type.Name =>
          plain(name).flatMap { spelling =>
            if binders(spelling) then
              if names.contains(spelling) then Left("N079_DUPLICATE_BINDER")
              else
                val index = names.size
                names += spelling
                Right(P.PatternBinder(index))
            else if denied(spelling) then Left("N079_OUTER_BINDER_UNSUPPORTED")
            else if atoms(spelling) then Right(P.Atom(spelling))
            else Left("N079_PATTERN_NAME_UNSUPPORTED")
          }
        case applied: Type.Apply =>
          applied.tpe match
            case name: Type.Name =>
              for
                spelling <- plain(name)
                _ <- Either.cond(!denied(spelling) && position(spelling, scopes).isEmpty &&
                  !names.contains(spelling), (), "N079_CONSTRUCTOR_SHADOWED")
                _ <- arity(spelling, applied.args, true)
                args <- traverse(applied.args)(pattern(_, scopes, names))
              yield P.Apply(spelling, args)
            case _ => Left("N079_PATTERN_CONSTRUCTOR_UNSUPPORTED")
        case _ => Left("N079_PATTERN_NODE_UNSUPPORTED")
      }
    def tpe(tree: Type, scopes: Scopes): Result[T] =
      Option(tree).toRight("N079_TYPE_MISSING").flatMap {
        case name: Type.Name =>
          plain(name).flatMap { spelling =>
            position(spelling, scopes) match
              case Some((depth, index)) => Right(T.BoundCase(depth, index))
              case None if denied(spelling) => Left("N079_OUTER_BINDER_UNSUPPORTED")
              case None if atoms(spelling) => Right(T.Atom(spelling))
              case None if frees(spelling) => Right(T.Free(spelling))
              case None => Left("N079_UNBOUND_NAME")
          }
        case applied: Type.Apply =>
          applied.tpe match
            case name: Type.Name =>
              for
                spelling <- plain(name)
                _ <- Either.cond(!denied(spelling) && position(spelling, scopes).isEmpty,
                  (), "N079_CONSTRUCTOR_SHADOWED")
                _ <- arity(spelling, applied.args, false)
                args <- traverse(applied.args)(tpe(_, scopes))
              yield T.Apply(spelling,args)
            case _ => Left("N079_CONSTRUCTOR_UNSUPPORTED")
        case matched: Type.Match =>
          for
            scrutinee <- tpe(matched.tpe, scopes)
            cases <- Option(matched.cases).toRight("N079_CASES_MISSING")
            _ <- Either.cond(cases.nonEmpty, (), "N079_CASES_EMPTY")
            projected <- traverse(cases) { clause =>
              Option(clause).toRight("N079_CASE_MISSING").flatMap { c =>
                val names = scala.collection.mutable.ArrayBuffer.empty[String]
                for
                  p <- pattern(c.pat, scopes, names)
                  _ = evidence += names.toList
                  body <- tpe(c.body, names.toList :: scopes)
                yield C(p, names.size, body)
              }
            }
          yield T.Match(scrutinee,projected)
        case _ => Left("N079_TYPE_NODE_UNSUPPORTED")
      }
    tpe(source,Nil).map { model =>
      val span = source.pos match
        case Position.None => None
        case p => Some(p.start -> p.end)
      Projection(model,Provenance(evidence.toVector,source.tokens.map(_.text).mkString,span))
    }

  private def author(model: T, names: Vector[List[String]], denied: Set[String] = Set.empty): Result[Type] =
    if names == null then return Left("N079_PROVENANCE_MISSING")
    var nextCase = 0
    def pattern(p: P, frame: List[String], declared: scala.collection.mutable.ArrayBuffer[Int], scopes: Scopes): Result[Type] =
      Option(p).toRight("N079_PATTERN_MISSING").flatMap {
        case P.Wildcard => Right(Type.PatWildcard())
        case P.Atom(name) if denied(name) => Left("N079_OUTER_BINDER_UNSUPPORTED")
        case P.Atom(name) if atoms(name) => Right(Type.Name(name))
        case P.Atom(_) => Left("N079_PATTERN_NAME_UNSUPPORTED")
        case P.PatternBinder(index) =>
          if index != declared.size || index < 0 || index >= frame.size then
            Left("N079_BINDER_DECLARATION_ORDER")
          else
            declared += index
            Right(Type.Name(frame(index)))
        case P.Apply(constructor,args) =>
          for
            _ <- Either.cond(!denied(constructor) && position(constructor, frame :: scopes).isEmpty,
              (), "N079_CONSTRUCTOR_SHADOWED")
            _ <- arity(constructor,args,true)
            authored <- traverse(args)(pattern(_,frame,declared,scopes))
          yield Type.Apply(Type.Name(constructor),authored)
      }
    def tpe(t: T, scopes: Scopes): Result[Type] =
      Option(t).toRight("N079_TYPE_MISSING").flatMap {
        case T.Atom(name) if denied(name) => Left("N079_OUTER_BINDER_UNSUPPORTED")
        case T.Atom(name) if atoms(name) => Right(Type.Name(name))
        case T.Free(name) if denied(name) => Left("N079_OUTER_BINDER_UNSUPPORTED")
        case T.Free(name) if frees(name) => Right(Type.Name(name))
        case T.Atom(_) | T.Free(_) => Left("N079_TYPE_NAME_UNSUPPORTED")
        case T.BoundCase(depth,index) =>
          if depth < 0 || index < 0 then Left("N079_BOUND_REFERENCE_INVALID")
          else
            scopes.lift(depth).flatMap(_.lift(index)).toRight("N079_BOUND_REFERENCE_INVALID").flatMap { name =>
              Either.cond(position(name, scopes).contains((depth,index)),Type.Name(name),"N079_AUTHOR_CAPTURE")
            }
        case T.Apply(constructor,args) =>
          for
            _ <- Either.cond(!denied(constructor) && position(constructor,scopes).isEmpty,
              (), "N079_CONSTRUCTOR_SHADOWED")
            _ <- arity(constructor,args,false)
            authored <- traverse(args)(tpe(_,scopes))
          yield Type.Apply(Type.Name(constructor),authored)
        case T.Match(scrutinee,cases) =>
          for
            s <- tpe(scrutinee,scopes)
            cs <- Option(cases).toRight("N079_CASES_MISSING")
            _ <- Either.cond(cs.nonEmpty,(),"N079_CASES_EMPTY")
            authored <- traverse(cs) { c =>
              Option(c).toRight("N079_CASE_MISSING").flatMap { clause =>
                val slot = nextCase
                nextCase += 1
                for
                  frame <- names.lift(slot).toRight("N079_PROVENANCE_MISSING")
                  _ <- Either.cond(frame != null && frame.forall(binders) &&
                    frame.distinct.size == frame.size,(),"N079_BINDER_NAMES_INVALID")
                  _ <- Either.cond(frame.size == clause.binderCount,(),"N079_BINDER_COUNT_MISMATCH")
                  declared = scala.collection.mutable.ArrayBuffer.empty[Int]
                  p <- pattern(clause.pattern,frame,declared,scopes)
                  _ <- Either.cond(declared.size == clause.binderCount,(),"N079_BINDER_COUNT_MISMATCH")
                  r <- tpe(clause.result,frame :: scopes)
                yield TypeCase(p,r)
              }
            }
          yield Type.Match(s,authored)
      }
    tpe(model,Nil).flatMap { tree =>
      Either.cond(nextCase == names.size,tree,"N079_PROVENANCE_UNUSED")
    }
