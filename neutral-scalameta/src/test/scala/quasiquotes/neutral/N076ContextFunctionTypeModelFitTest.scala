package quasiquotes.neutral

import quasiquotes.parser.TypeShape
import quasiquotes.types.*
import quasiquotes.types.TypeNormalForm.*

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

/** Test-only model-fit oracle. It does not admit context-function production semantics. */
@nowarn("cat=deprecation")
final class N076ContextFunctionTypeModelFitTest extends munit.FunSuite:
  private enum FunctionKind derives CanEqual:
    case Ordinary, Contextual

  private enum CandidateType derives CanEqual:
    case Atom(name: String)
    case Apply(constructor: String, arguments: List[CandidateType])
    case Function(kind: FunctionKind, arguments: List[CandidateType], result: CandidateType)

  private final case class CandidateError(code: String, detail: String) derives CanEqual

  private val atomNames = Set("Int", "String", "Boolean", "AnyVal", "A", "B", "C")
  private val fixedConstructors = Map("List" -> 1, "Option" -> 1, "Either" -> 2)

  test("Scala 3 parses ordinary and contextual function Types as distinct AST nodes"):
    val ordinary = parseType("Int => String").asInstanceOf[Type.Function]
    val contextual = parseType("Int ?=> String").asInstanceOf[Type.ContextFunction]

    assertEquals(ordinary.params.map(_.structure), List(Type.Name("Int").structure))
    assertEquals(ordinary.res.structure, Type.Name("String").structure)
    assertEquals(contextual.params.map(_.structure), List(Type.Name("Int").structure))
    assertEquals(contextual.res.structure, Type.Name("String").structure)
    assertEquals(ordinary.productPrefix, "Type.Function")
    assertEquals(contextual.productPrefix, "Type.ContextFunction")
    assertNotEquals(ordinary.structure, contextual.structure)
    assertEquals((ordinary.pos.start, ordinary.pos.end), (0, 13))
    assertEquals((contextual.pos.start, contextual.pos.end), (0, 14))
    assertEquals(ordinary.tokens.map(_.text).mkString, "Int => String")
    assertEquals(contextual.tokens.map(_.text).mkString, "Int ?=> String")

  test("function arrows associate right and retain mixed ordinary contextual topology"):
    val contextThenOrdinary = CandidateType.Function(
      FunctionKind.Contextual,
      List(CandidateType.Atom("Int")),
      CandidateType.Function(
        FunctionKind.Ordinary,
        List(CandidateType.Atom("String")),
        CandidateType.Atom("Boolean")
      )
    )
    val ordinaryThenContext = CandidateType.Function(
      FunctionKind.Ordinary,
      List(CandidateType.Atom("Int")),
      CandidateType.Function(
        FunctionKind.Contextual,
        List(CandidateType.Atom("String")),
        CandidateType.Atom("Boolean")
      )
    )

    assertEquals(candidateProject(parseType("Int ?=> String => Boolean")), Right(contextThenOrdinary))
    assertEquals(candidateProject(parseType("Int => String ?=> Boolean")), Right(ordinaryThenContext))
    assertNotEquals(contextThenOrdinary, ordinaryThenContext)
    assertEquals(
      candidateProject(parseType("(Int ?=> String) => Boolean")),
      Right(
        CandidateType.Function(
          FunctionKind.Ordinary,
          List(
            CandidateType.Function(
              FunctionKind.Contextual,
              List(CandidateType.Atom("Int")),
              CandidateType.Atom("String")
            )
          ),
          CandidateType.Atom("Boolean")
        )
      )
    )

  test("parentheses select grouping while redundant source grouping survives only in tokens"):
    val grouped = parseType("(Int ?=> String) => Boolean")
    val ungrouped = parseType("Int ?=> String => Boolean")
    val redundant = parseType("((Int ?=> String))")
    val plain = parseType("Int ?=> String")

    assertNotEquals(grouped.structure, ungrouped.structure)
    assertEquals(redundant.structure, plain.structure)
    assertNotEquals(redundant.tokens.map(_.text), plain.tokens.map(_.text))
    assertEquals(candidateProject(redundant), candidateProject(plain))

  test("current public projection rejects contextual function roots and nested contextual children"):
    val sources = List(
      "Int ?=> String",
      "(Int, String) ?=> Boolean",
      "Int ?=> String => Boolean",
      "Int => String ?=> Boolean",
      "List[Int ?=> String]",
      "Either[Int, String ?=> Boolean]"
    )
    val expected = Left(
      NeutralProjectionError(
        "NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED",
        "unsupported Scalameta type node: Type.ContextFunction."
      )
    )

    sources.foreach { source =>
      assertEquals(ScalametaTypeNormalFormProjection.project(parseType(source)), expected, clues(source))
    }

  test("ordinary public functions remain admitted and existing authoring emits ordinary arrows only"):
    val unary = STypeFunction(List(STypeIdent("Int")), STypeIdent("String"))
    val binary =
      STypeFunction(List(STypeIdent("Int"), STypeIdent("String")), STypeIdent("Boolean"))

    assertEquals(
      ScalametaTypeNormalFormProjection.project(parseType("Int => String")).map(_.normalForm),
      Right(unary)
    )
    assertEquals(
      ScalametaTypeNormalFormProjection
        .project(parseType("(Int, String) => Boolean"))
        .map(_.normalForm),
      Right(binary)
    )
    List(unary, binary).foreach { normalForm =>
      val authored = ScalametaTypeNormalFormAuthoring.author(normalForm).toOption.get
      assertEquals(authored.productPrefix, "Type.Function")
      assert(!authored.isInstanceOf[Type.ContextFunction])
      assert(authored.syntax.contains("=>"))
      assert(!authored.syntax.contains("?=>"))
    }

  test("erasing contextual role into STypeFunction creates a collision witnessed by the candidate"):
    val ordinarySource = parseType("Int => String")
    val contextualSource = parseType("Int ?=> String")

    assertEquals(eraseFunctionKind(ordinarySource), eraseFunctionKind(contextualSource))
    assertNotEquals(candidateProject(ordinarySource), candidateProject(contextualSource))
    assertEquals(
      candidateProject(ordinarySource),
      Right(CandidateType.Function(FunctionKind.Ordinary, List(CandidateType.Atom("Int")), CandidateType.Atom("String")))
    )
    assertEquals(
      candidateProject(contextualSource),
      Right(CandidateType.Function(FunctionKind.Contextual, List(CandidateType.Atom("Int")), CandidateType.Atom("String")))
    )
    val unsupportedShape = TypeShape.Unsupported("Type.ContextFunction", "Int ?=> String")
    assert(TypeNormalForm.fromShape(unsupportedShape).isLeft)
    assert(TypeTemplate.fromShape(unsupportedShape).isLeft)

  test("the candidate preserves fixed applications and nested mixed function roles"):
    val fixtures = List(
      "List[Int ?=> String]" ->
        CandidateType.Apply(
          "List",
          List(CandidateType.Function(FunctionKind.Contextual, List(CandidateType.Atom("Int")), CandidateType.Atom("String")))
        ),
      "Option[(Int ?=> String) => Boolean]" ->
        CandidateType.Apply(
          "Option",
          List(
            CandidateType.Function(
              FunctionKind.Ordinary,
              List(CandidateType.Function(FunctionKind.Contextual, List(CandidateType.Atom("Int")), CandidateType.Atom("String"))),
              CandidateType.Atom("Boolean")
            )
          )
        ),
      "Either[Int => String, (A, B) ?=> C]" ->
        CandidateType.Apply(
          "Either",
          List(
            CandidateType.Function(FunctionKind.Ordinary, List(CandidateType.Atom("Int")), CandidateType.Atom("String")),
            CandidateType.Function(FunctionKind.Contextual, List(CandidateType.Atom("A"), CandidateType.Atom("B")), CandidateType.Atom("C"))
          )
        )
    )

    fixtures.foreach { (source, expected) =>
      assertEquals(candidateProject(parseType(source)), Right(expected), clues(source))
    }

  test("candidate authoring reprojects exact function role argument order and nesting"):
    val sources = List(
      "Int => String",
      "Int ?=> String",
      "(Int, String) ?=> Boolean",
      "Int ?=> String => Boolean",
      "Int => String ?=> Boolean",
      "(Int ?=> String) => Boolean",
      "List[Int ?=> String]",
      "Either[Int => String, (A, B) ?=> C]"
    )

    sources.foreach { source =>
      val parsed = parseType(source)
      val candidate = candidateProject(parsed).toOption.get
      val authored = candidateAuthor(candidate).toOption.get

      assertEquals(authored.structure, parsed.structure, clues(source))
      assertEquals(candidateProject(authored), Right(candidate), clues(source))
      assert(allTypeNodes(authored).forall(_.pos == Position.None), clues(source))
    }

  test("zero one two three and runtime N argument positions remain ordinary ordered lists"):
    val fixtures = List(
      "() ?=> Boolean" -> List.empty[String],
      "Int ?=> Boolean" -> List("Int"),
      "(Int, String) ?=> Boolean" -> List("Int", "String"),
      "(Int, String, AnyVal) ?=> Boolean" -> List("Int", "String", "AnyVal")
    )

    fixtures.foreach { (source, expectedArguments) =>
      val parsed = parseType(source).asInstanceOf[Type.ContextFunction]
      val candidate = candidateProject(parsed).toOption.get.asInstanceOf[CandidateType.Function]
      assertEquals(parsed.params.map(_.asInstanceOf[Type.Name].value), expectedArguments, clues(source))
      assertEquals(candidate.arguments, expectedArguments.map(CandidateType.Atom(_)), clues(source))
      assertEquals(candidate.kind, FunctionKind.Contextual)
    }

    val runtimeNames = List("Int", "String", "Boolean", "AnyVal", "A", "B", "C")
    val runtimeCandidate = CandidateType.Function(
      FunctionKind.Contextual,
      runtimeNames.map(CandidateType.Atom(_)),
      CandidateType.Atom("Boolean")
    )
    val authored = candidateAuthor(runtimeCandidate).toOption.get.asInstanceOf[Type.ContextFunction]
    assertEquals(authored.params.map(_.asInstanceOf[Type.Name].value), runtimeNames)
    assertEquals(candidateProject(authored), Right(runtimeCandidate))

  test("the candidate Type composes in alias value method parameter and result positions"):
    val alias = parseStat("type Handler = Int ?=> String").asInstanceOf[Defn.Type]
    val value = parseStat("val handler: Int ?=> String = ???").asInstanceOf[Defn.Val]
    val method = parseStat("def use(handler: Int ?=> String): String ?=> Boolean = ???").asInstanceOf[Defn.Def]
    val unaryContext = Right(
      CandidateType.Function(FunctionKind.Contextual, List(CandidateType.Atom("Int")), CandidateType.Atom("String"))
    )

    assertEquals(candidateProject(alias.body), unaryContext)
    assertEquals(candidateProject(value.decltpe.get), unaryContext)
    assertEquals(candidateProject(method.paramClauseGroups.head.paramClauses.head.values.head.decltpe.get), unaryContext)
    assertEquals(
      candidateProject(method.decltpe.get),
      Right(CandidateType.Function(FunctionKind.Contextual, List(CandidateType.Atom("String")), CandidateType.Atom("Boolean")))
    )

  test("parser public projection candidate and Term-level failures remain distinct"):
    assert(Input.String("Int ?=>").parse[Type].isInstanceOf[Parsed.Error])
    assertEquals(
      ScalametaTypeNormalFormProjection.project(parseType("Int ?=> String")).left.toOption.map(_.code),
      Some("NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED")
    )

    val rejectedSources = List(
      "Double ?=> Int",
      "pkg.A ?=> B",
      "[X] =>> X ?=> Int",
      "Int { type Out = String } ?=> Boolean",
      "(Int | String) ?=> Boolean",
      "(A match { case A => B }) ?=> C",
      "F[Int] ?=> String"
    )
    rejectedSources.foreach { source =>
      assert(candidateProject(parseType(source)).isLeft, clues(source))
    }

    val termContext = parseTerm("(using value: Int) ?=> value")
    assertEquals(termContext.productPrefix, "Term.ContextFunction")
    assert(!termContext.isInstanceOf[Type])

  test("candidate authoring fails closed for missing malformed and unsupported values"):
    val malformed = List(
      null,
      CandidateType.Atom(null),
      CandidateType.Atom("Double"),
      CandidateType.Apply(null, List(CandidateType.Atom("Int"))),
      CandidateType.Apply("List", null),
      CandidateType.Apply("List", Nil),
      CandidateType.Apply("Map", List(CandidateType.Atom("Int"), CandidateType.Atom("String"))),
      CandidateType.Function(null, List(CandidateType.Atom("Int")), CandidateType.Atom("String")),
      CandidateType.Function(FunctionKind.Contextual, null, CandidateType.Atom("String")),
      CandidateType.Function(FunctionKind.Contextual, List(null), CandidateType.Atom("String")),
      CandidateType.Function(FunctionKind.Contextual, List(CandidateType.Atom("Int")), null)
    )
    malformed.foreach { candidate =>
      assert(candidateAuthor(candidate).isLeft, clues(candidate))
    }

    val resolved = STypeResolved(
      ResolvedTypeNameId(Vector(ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "example")), "A")
    )
    assertEquals(
      ScalametaTypeNormalFormAuthoring.author(resolved).left.toOption.map(_.code),
      Some("NEUTRAL_TYPE_AUTHORING_RESOLVED_UNSUPPORTED")
    )

  private def eraseFunctionKind(sourceType: Type): Either[CandidateError, TypeNormalForm] =
    sourceType match
      case function: Type.Function => eraseFunction(function.params, function.res)
      case function: Type.ContextFunction => eraseFunction(function.params, function.res)
      case other => Left(CandidateError("N076_ERASURE_STRUCTURE_UNSUPPORTED", s"expected a function Type, found ${other.productPrefix}."))

  private def eraseFunction(arguments: List[Type], result: Type): Either[CandidateError, TypeNormalForm] =
    for
      argumentForms <- traverse(arguments)(eraseAtom)
      resultForm <- eraseAtom(result)
    yield STypeFunction(argumentForms, resultForm)

  private def eraseAtom(sourceType: Type): Either[CandidateError, TypeNormalForm] =
    sourceType match
      case name: Type.Name => validateAtom(name.value).map(STypeIdent(_))
      case other => Left(CandidateError("N076_ERASURE_ATOM_UNSUPPORTED", s"expected an atom, found ${other.productPrefix}."))

  private def candidateProject(sourceType: Type): Either[CandidateError, CandidateType] =
    Option(sourceType)
      .toRight(CandidateError("N076_CANDIDATE_MISSING_TYPE", "the Scalameta Type must be present."))
      .flatMap {
        case name: Type.Name => validateAtom(name.value).map(CandidateType.Atom(_))
        case applied: Type.Apply =>
          applied.tpe match
            case name: Type.Name =>
              for
                expectedArity <- fixedConstructors.get(name.value).toRight(
                  CandidateError("N076_CANDIDATE_CONSTRUCTOR_UNSUPPORTED", s"unsupported fixed constructor `${name.value}`.")
                )
                _ <- Either.cond(
                  applied.args.size == expectedArity,
                  (),
                  CandidateError("N076_CANDIDATE_CONSTRUCTOR_ARITY", s"`${name.value}` requires $expectedArity arguments, found ${applied.args.size}.")
                )
                arguments <- traverse(applied.args)(candidateProject)
              yield CandidateType.Apply(name.value, arguments)
            case _ => Left(CandidateError("N076_CANDIDATE_CONSTRUCTOR_UNSUPPORTED", "only fixed unqualified constructors are admitted."))
        case function: Type.Function => projectFunction(FunctionKind.Ordinary, function.params, function.res)
        case function: Type.ContextFunction => projectFunction(FunctionKind.Contextual, function.params, function.res)
        case other => Left(CandidateError("N076_CANDIDATE_STRUCTURE_UNSUPPORTED", s"unsupported Scalameta Type node: ${other.productPrefix}."))
      }

  private def projectFunction(kind: FunctionKind, arguments: List[Type], result: Type): Either[CandidateError, CandidateType] =
    for
      projectedArguments <- traverse(arguments)(candidateProject)
      projectedResult <- candidateProject(result)
    yield CandidateType.Function(kind, projectedArguments, projectedResult)

  private def candidateAuthor(candidate: CandidateType): Either[CandidateError, Type] =
    Option(candidate)
      .toRight(CandidateError("N076_CANDIDATE_MISSING_VALUE", "the candidate Type must be present."))
      .flatMap {
        case CandidateType.Atom(name) => validateAtom(name).map(Type.Name(_))
        case CandidateType.Apply(constructor, arguments) =>
          for
            presentConstructor <- Option(constructor).toRight(CandidateError("N076_CANDIDATE_CONSTRUCTOR_UNSUPPORTED", "the constructor must be present."))
            presentArguments <- Option(arguments).toRight(CandidateError("N076_CANDIDATE_ARGUMENTS_MISSING", "the argument list must be present."))
            expectedArity <- fixedConstructors.get(presentConstructor).toRight(
              CandidateError("N076_CANDIDATE_CONSTRUCTOR_UNSUPPORTED", s"unsupported fixed constructor `$presentConstructor`.")
            )
            _ <- Either.cond(
              presentArguments.size == expectedArity,
              (),
              CandidateError("N076_CANDIDATE_CONSTRUCTOR_ARITY", s"`$presentConstructor` requires $expectedArity arguments, found ${presentArguments.size}.")
            )
            authoredArguments <- traverse(presentArguments)(candidateAuthor)
          yield Type.Apply(Type.Name(presentConstructor), authoredArguments)
        case CandidateType.Function(kind, arguments, result) =>
          for
            presentKind <- Option(kind).toRight(CandidateError("N076_CANDIDATE_FUNCTION_KIND_MISSING", "the function kind must be present."))
            presentArguments <- Option(arguments).toRight(CandidateError("N076_CANDIDATE_ARGUMENTS_MISSING", "the function arguments must be present."))
            authoredArguments <- traverse(presentArguments)(candidateAuthor)
            authoredResult <- candidateAuthor(result)
          yield presentKind match
            case FunctionKind.Ordinary => Type.Function(authoredArguments, authoredResult)
            case FunctionKind.Contextual => Type.ContextFunction(authoredArguments, authoredResult)
      }

  private def validateAtom(name: String): Either[CandidateError, String] =
    Option(name).filter(atomNames).toRight(
      CandidateError("N076_CANDIDATE_ATOM_UNSUPPORTED", s"unsupported candidate atom `${Option(name).getOrElse("<missing>")}`.")
    )

  private def traverse[A, B](values: List[A])(convert: A => Either[CandidateError, B]): Either[CandidateError, List[B]] =
    Option(values)
      .toRight(CandidateError("N076_CANDIDATE_LIST_MISSING", "the recursive list must be present."))
      .flatMap(
        _.foldRight(Right(Nil): Either[CandidateError, List[B]]) { (value, accumulated) =>
          for
            head <- convert(value)
            tail <- accumulated
          yield head :: tail
        }
      )

  private def parseType(source: String): Type = Input.String(source).parse[Type].get
  private def parseTerm(source: String): Term = Input.String(source).parse[Term].get
  private def parseStat(source: String): Stat = Input.String(source).parse[Stat].get

  private def allTypeNodes(root: Type): List[Type] =
    root :: (root match
      case _: Type.Name => Nil
      case applied: Type.Apply => allTypeNodes(applied.tpe) ++ applied.args.flatMap(allTypeNodes)
      case function: Type.Function => function.params.flatMap(allTypeNodes) ++ allTypeNodes(function.res)
      case function: Type.ContextFunction => function.params.flatMap(allTypeNodes) ++ allTypeNodes(function.res)
      case other => fail(s"unexpected authored node: ${other.productPrefix}")
    )
