package quasiquotes.neutral

import quasiquotes.parser.TypeShape
import quasiquotes.types.*
import quasiquotes.types.TypeNormalForm.*

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

/** Test-only model-fit oracle. It does not admit union/intersection production semantics. */
@nowarn("cat=deprecation")
final class N075Scala3UnionIntersectionTypeModelFitTest extends munit.FunSuite:
  private enum CandidateType derives CanEqual:
    case Atom(name: String)
    case Apply(constructor: String, arguments: List[CandidateType])
    case Union(left: CandidateType, right: CandidateType)
    case Intersection(left: CandidateType, right: CandidateType)

  private final case class CandidateError(code: String, detail: String) derives CanEqual

  private val atomNames = Set("Int", "String", "Boolean", "AnyVal", "A", "B", "C")
  private val fixedConstructors = Map("List" -> 1, "Option" -> 1, "Either" -> 2)

  test("Scala 3 parses union and intersection as distinct Type.ApplyInfix operators"):
    val union = parseType("Int | String").asInstanceOf[Type.ApplyInfix]
    val intersection = parseType("Int & String").asInstanceOf[Type.ApplyInfix]

    assertEquals(union.lhs.structure, Type.Name("Int").structure)
    assertEquals(union.op.value, "|")
    assertEquals(union.rhs.structure, Type.Name("String").structure)
    assertEquals(intersection.lhs.structure, Type.Name("Int").structure)
    assertEquals(intersection.op.value, "&")
    assertEquals(intersection.rhs.structure, Type.Name("String").structure)
    assertNotEquals(union.structure, intersection.structure)
    assertEquals((union.pos.start, union.pos.end), (0, 12))
    assertEquals((intersection.pos.start, intersection.pos.end), (0, 12))

  test("Scala 3 precedence gives intersection priority and equal operators associate left"):
    assertEquals(
      candidateProject(parseType("Int | String & Boolean")),
      Right(
        CandidateType.Union(
          CandidateType.Atom("Int"),
          CandidateType.Intersection(
            CandidateType.Atom("String"),
            CandidateType.Atom("Boolean")
          )
        )
      )
    )
    assertEquals(
      candidateProject(parseType("Int & String | Boolean")),
      Right(
        CandidateType.Union(
          CandidateType.Intersection(
            CandidateType.Atom("Int"),
            CandidateType.Atom("String")
          ),
          CandidateType.Atom("Boolean")
        )
      )
    )
    assertEquals(
      candidateProject(parseType("A | B | C")),
      Right(
        CandidateType.Union(
          CandidateType.Union(CandidateType.Atom("A"), CandidateType.Atom("B")),
          CandidateType.Atom("C")
        )
      )
    )
    assertEquals(
      candidateProject(parseType("A & B & C")),
      Right(
        CandidateType.Intersection(
          CandidateType.Intersection(CandidateType.Atom("A"), CandidateType.Atom("B")),
          CandidateType.Atom("C")
        )
      )
    )

  test("parentheses select semantic grouping while tokens retain otherwise inessential spelling"):
    val explicitUnionGroup = parseType("(Int | String) & Boolean")
    val explicitIntersectionGroup = parseType("Int | (String & Boolean)")
    val precedenceEquivalent = parseType("Int | String & Boolean")
    val redundant = parseType("((Int | String))")
    val plain = parseType("Int | String")

    assertEquals(
      candidateProject(explicitUnionGroup),
      Right(
        CandidateType.Intersection(
          CandidateType.Union(CandidateType.Atom("Int"), CandidateType.Atom("String")),
          CandidateType.Atom("Boolean")
        )
      )
    )
    assertEquals(explicitIntersectionGroup.structure, precedenceEquivalent.structure)
    assertNotEquals(
      explicitIntersectionGroup.tokens.map(_.text),
      precedenceEquivalent.tokens.map(_.text)
    )
    assertEquals(redundant.structure, plain.structure)
    assertNotEquals(redundant.tokens.map(_.text), plain.tokens.map(_.text))

  test("current public projection rejects every selected union/intersection root exactly"):
    val sources = List(
      "Int | String",
      "Int & String",
      "(Int | String) & Boolean",
      "Int | (String & Boolean)",
      "Int | String & Boolean",
      "A | B | C",
      "A & B & C"
    )
    val expected = Left(
      NeutralProjectionError(
        "NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED",
        "unsupported Scalameta type node: Type.ApplyInfix."
      )
    )

    sources.foreach { source =>
      assertEquals(ScalametaTypeNormalFormProjection.project(parseType(source)), expected, clues(source))
    }

  test("current public positives remain admitted and nested union/intersection arguments stay rejected"):
    val fixtures = List(
      "Int" -> STypeIdent("Int"),
      "String" -> STypeIdent("String"),
      "Boolean" -> STypeIdent("Boolean"),
      "AnyVal" -> STypeIdent("AnyVal"),
      "List[Int]" -> STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
      "Option[String]" -> STypeApply(STypeIdent("Option"), List(STypeIdent("String"))),
      "Either[Int, Boolean]" ->
        STypeApply(STypeIdent("Either"), List(STypeIdent("Int"), STypeIdent("Boolean"))),
      "(Int, String)" -> STypeTuple(List(STypeIdent("Int"), STypeIdent("String"))),
      "(Int, String, Boolean)" ->
        STypeTuple(List(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))),
      "Int => String" -> STypeFunction(List(STypeIdent("Int")), STypeIdent("String")),
      "(Int, String) => Boolean" ->
        STypeFunction(List(STypeIdent("Int"), STypeIdent("String")), STypeIdent("Boolean"))
    )

    fixtures.foreach { (source, expected) =>
      assertEquals(
        ScalametaTypeNormalFormProjection.project(parseType(source)).map(_.normalForm),
        Right(expected),
        clues(source)
      )
    }

    List(
      "List[Int | String]",
      "Option[Int & String]",
      "Either[Int | String, Boolean]",
      "Either[Int, String & Boolean]"
    ).foreach { source =>
      assertEquals(
        ScalametaTypeNormalFormProjection.project(parseType(source)),
        Left(
          NeutralProjectionError(
            "NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED",
            "unsupported Scalameta type node: Type.ApplyInfix."
          )
        ),
        clues(source)
      )
    }

  test("existing admitted carriers cannot encode the union/intersection distinction"):
    val unionShape = TypeShape.Apply(
      TypeShape.Identifier("|"),
      List(TypeShape.Identifier("Int"), TypeShape.Identifier("String"))
    )
    val intersectionShape = TypeShape.Apply(
      TypeShape.Identifier("&"),
      List(TypeShape.Identifier("Int"), TypeShape.Identifier("String"))
    )
    val unionForm = STypeApply(STypeIdent("|"), List(STypeIdent("Int"), STypeIdent("String")))
    val intersectionForm =
      STypeApply(STypeIdent("&"), List(STypeIdent("Int"), STypeIdent("String")))

    List(unionShape, intersectionShape).foreach { shape =>
      assert(TypeNormalForm.fromShape(shape).isLeft, clues(shape))
      assert(TypeTemplate.fromShape(shape).isLeft, clues(shape))
    }
    List(unionForm, intersectionForm).foreach { normalForm =>
      assert(ScalametaTypeNormalFormAuthoring.author(normalForm).isLeft, clues(normalForm))
      assert(TypeTemplate.validateConstructed(normalForm).isLeft, clues(normalForm))
    }
    assert(
      TypeNormalForm
        .fromShape(TypeShape.Unsupported("Type.ApplyInfix", "Int | String"))
        .isLeft
    )
    assertNotEquals(
      CandidateType.Union(CandidateType.Atom("Int"), CandidateType.Atom("String")),
      CandidateType.Intersection(CandidateType.Atom("Int"), CandidateType.Atom("String"))
    )

  test("the candidate preserves fixed applications and nested operator topology"):
    val fixtures = List(
      "List[Int | String]" ->
        CandidateType.Apply(
          "List",
          List(CandidateType.Union(CandidateType.Atom("Int"), CandidateType.Atom("String")))
        ),
      "Option[(Int | String) & Boolean]" ->
        CandidateType.Apply(
          "Option",
          List(
            CandidateType.Intersection(
              CandidateType.Union(CandidateType.Atom("Int"), CandidateType.Atom("String")),
              CandidateType.Atom("Boolean")
            )
          )
        ),
      "Either[A & B, B | C]" ->
        CandidateType.Apply(
          "Either",
          List(
            CandidateType.Intersection(CandidateType.Atom("A"), CandidateType.Atom("B")),
            CandidateType.Union(CandidateType.Atom("B"), CandidateType.Atom("C"))
          )
        )
    )

    fixtures.foreach { (source, expected) =>
      assertEquals(candidateProject(parseType(source)), Right(expected), clues(source))
    }

  test("candidate authoring reprojects to the same ordered binary structure"):
    val sources = List(
      "Int | String",
      "Int & String",
      "(Int | String) & Boolean",
      "Int | (String & Boolean)",
      "A | B | C",
      "A & B & C",
      "List[Int | String]",
      "Either[A & B, B | C]"
    )

    sources.foreach { source =>
      val parsed = parseType(source)
      val candidate = candidateProject(parsed).toOption.get
      val authored = candidateAuthor(candidate).toOption.get

      assertEquals(authored.structure, parsed.structure, clues(source))
      assertEquals(candidateProject(authored), Right(candidate), clues(source))
      assert(allTypeNodes(authored).forall(_.pos == Position.None), clues(source))
    }

    val positioned = parseType("(Int | String) & Boolean")
    val fresh = candidateAuthor(candidateProject(positioned).toOption.get).toOption.get
    assertEquals(fresh.syntax, "(Int | String) & Boolean")
    assertNotEquals(fresh.tokens.map(_.text), positioned.tokens.map(_.text))

  test("candidate semantics preserve order association duplication and operator identity"):
    val a = CandidateType.Atom("A")
    val b = CandidateType.Atom("B")
    val c = CandidateType.Atom("C")

    assertNotEquals(CandidateType.Union(a, b), CandidateType.Union(b, a))
    assertNotEquals(CandidateType.Intersection(a, b), CandidateType.Intersection(b, a))
    assertNotEquals(
      CandidateType.Union(CandidateType.Union(a, b), c),
      CandidateType.Union(a, CandidateType.Union(b, c))
    )
    assertNotEquals(CandidateType.Union(a, a), a)
    assertNotEquals(CandidateType.Intersection(a, a), a)
    assertNotEquals(CandidateType.Union(a, b), CandidateType.Intersection(a, b))

  test("binary nodes scale through 0 1 2 3 and runtime N operands without a ranked carrier"):
    val atoms = List("A", "B", "C", "A", "B", "C", "A").map(CandidateType.Atom(_))

    assertEquals(foldCandidate("|", Nil), Left(CandidateError("N075_CANDIDATE_ARITY", "union requires at least two operands.")))
    assertEquals(foldCandidate("|", atoms.take(1)), Left(CandidateError("N075_CANDIDATE_ARITY", "union requires at least two operands.")))
    assertEquals(foldCandidate("|", atoms.take(2)).toOption.get, CandidateType.Union(atoms(0), atoms(1)))
    assertEquals(
      foldCandidate("&", atoms.take(3)).toOption.get,
      CandidateType.Intersection(CandidateType.Intersection(atoms(0), atoms(1)), atoms(2))
    )
    val runtimeN = foldCandidate("|", atoms).toOption.get
    assertEquals(candidateLeaves(runtimeN), atoms)

  test("the same candidate Type structure composes in alias value and method Type positions"):
    val alias = parseStat("type Alias = (A | B) & C").asInstanceOf[Defn.Type]
    val value = parseStat("val value: A | B = ???").asInstanceOf[Defn.Val]
    val method = parseStat("def choose: A & B = ???").asInstanceOf[Defn.Def]

    assertEquals(
      candidateProject(alias.body),
      Right(
        CandidateType.Intersection(
          CandidateType.Union(CandidateType.Atom("A"), CandidateType.Atom("B")),
          CandidateType.Atom("C")
        )
      )
    )
    assertEquals(
      candidateProject(value.decltpe.get),
      Right(CandidateType.Union(CandidateType.Atom("A"), CandidateType.Atom("B")))
    )
    assertEquals(
      candidateProject(method.decltpe.get),
      Right(CandidateType.Intersection(CandidateType.Atom("A"), CandidateType.Atom("B")))
    )

  test("parser public projection candidate admission and authoring failures stay distinct"):
    assert(Input.String("Int |").parse[Type].isInstanceOf[Parsed.Error])

    val publicFailure = ScalametaTypeNormalFormProjection.project(parseType("Int | String"))
    assertEquals(publicFailure.left.toOption.map(_.code), Some("NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED"))

    val rejectedSources = List(
      "Double | Int",
      "pkg.A | B",
      "[X] =>> X | Int",
      "Int { type Out = String }",
      "?",
      "A match { case A => B }"
    )
    rejectedSources.foreach { source =>
      assert(candidateProject(parseType(source)).isLeft, clues(source))
    }

    val malformedCandidates = List(
      null,
      CandidateType.Atom(null),
      CandidateType.Atom("Double"),
      CandidateType.Union(null, CandidateType.Atom("Int")),
      CandidateType.Intersection(CandidateType.Atom("Int"), null),
      CandidateType.Apply(null, List(CandidateType.Atom("Int"))),
      CandidateType.Apply("List", null),
      CandidateType.Apply("List", Nil),
      CandidateType.Apply("List", List(CandidateType.Atom("Int"), CandidateType.Atom("String"))),
      CandidateType.Apply("Map", List(CandidateType.Atom("Int"), CandidateType.Atom("String")))
    )
    malformedCandidates.foreach { candidate =>
      assert(candidateAuthor(candidate).isLeft, clues(candidate))
    }

    val unknownOperator =
      Type.ApplyInfix(Type.Name("A"), Type.Name("^"), Type.Name("B"))
    assertEquals(
      candidateProject(unknownOperator).left.toOption.map(_.code),
      Some("N075_CANDIDATE_OPERATOR_UNSUPPORTED")
    )

    val resolved = STypeResolved(
      ResolvedTypeNameId(
        Vector(ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "example")),
        "A"
      )
    )
    assertEquals(
      ScalametaTypeNormalFormAuthoring.author(resolved).left.toOption.map(_.code),
      Some("NEUTRAL_TYPE_AUTHORING_RESOLVED_UNSUPPORTED")
    )

  private def candidateProject(sourceType: Type): Either[CandidateError, CandidateType] =
    Option(sourceType)
      .toRight(CandidateError("N075_CANDIDATE_MISSING_TYPE", "the Scalameta Type must be present."))
      .flatMap {
        case name: Type.Name =>
          validateAtom(name.value).map(CandidateType.Atom(_))
        case applied: Type.Apply =>
          applied.tpe match
            case name: Type.Name =>
              for
                expectedArity <- fixedConstructors.get(name.value).toRight(
                    CandidateError(
                      "N075_CANDIDATE_CONSTRUCTOR_UNSUPPORTED",
                      s"unsupported fixed constructor `${name.value}`."
                    )
                  )
                _ <- Either.cond(
                  applied.args.size == expectedArity,
                  (),
                  CandidateError(
                    "N075_CANDIDATE_CONSTRUCTOR_ARITY",
                    s"`${name.value}` requires $expectedArity arguments, found ${applied.args.size}."
                  )
                )
                arguments <- traverse(applied.args)(candidateProject)
              yield CandidateType.Apply(name.value, arguments)
            case _ =>
              Left(
                CandidateError(
                  "N075_CANDIDATE_CONSTRUCTOR_UNSUPPORTED",
                  "only fixed unqualified constructors are admitted."
                )
              )
        case infix: Type.ApplyInfix =>
          for
            left <- candidateProject(infix.lhs)
            right <- candidateProject(infix.rhs)
            combined <- infix.op.value match
              case "|" => Right(CandidateType.Union(left, right))
              case "&" => Right(CandidateType.Intersection(left, right))
              case operator =>
                Left(
                  CandidateError(
                    "N075_CANDIDATE_OPERATOR_UNSUPPORTED",
                    s"unsupported Type operator `$operator`."
                  )
                )
          yield combined
        case other =>
          Left(
            CandidateError(
              "N075_CANDIDATE_STRUCTURE_UNSUPPORTED",
              s"unsupported Scalameta Type node: ${other.productPrefix}."
            )
          )
      }

  private def candidateAuthor(candidate: CandidateType): Either[CandidateError, Type] =
    Option(candidate)
      .toRight(CandidateError("N075_CANDIDATE_MISSING_VALUE", "the candidate Type must be present."))
      .flatMap {
        case CandidateType.Atom(name) =>
          validateAtom(name).map(Type.Name(_))
        case CandidateType.Apply(constructor, arguments) =>
          for
            presentConstructor <- Option(constructor).toRight(
              CandidateError("N075_CANDIDATE_CONSTRUCTOR_UNSUPPORTED", "the constructor must be present.")
            )
            presentArguments <- Option(arguments).toRight(
              CandidateError("N075_CANDIDATE_ARGUMENTS_MISSING", "the argument list must be present.")
            )
            expectedArity <- fixedConstructors.get(presentConstructor).toRight(
                CandidateError(
                  "N075_CANDIDATE_CONSTRUCTOR_UNSUPPORTED",
                  s"unsupported fixed constructor `$presentConstructor`."
                )
              )
            _ <- Either.cond(
              presentArguments.size == expectedArity,
              (),
              CandidateError(
                "N075_CANDIDATE_CONSTRUCTOR_ARITY",
                s"`$presentConstructor` requires $expectedArity arguments, found ${presentArguments.size}."
              )
            )
            authoredArguments <- traverse(presentArguments)(candidateAuthor)
          yield Type.Apply(Type.Name(presentConstructor), authoredArguments)
        case CandidateType.Union(left, right) =>
          authorBinary(left, "|", right)
        case CandidateType.Intersection(left, right) =>
          authorBinary(left, "&", right)
      }

  private def authorBinary(
      left: CandidateType,
      operator: String,
      right: CandidateType
  ): Either[CandidateError, Type] =
    for
      authoredLeft <- candidateAuthor(left)
      authoredRight <- candidateAuthor(right)
    yield Type.ApplyInfix(authoredLeft, Type.Name(operator), authoredRight)

  private def validateAtom(name: String): Either[CandidateError, String] =
    Option(name)
      .filter(atomNames)
      .toRight(
        CandidateError(
          "N075_CANDIDATE_ATOM_UNSUPPORTED",
          s"unsupported candidate atom `${Option(name).getOrElse("<missing>")}`."
        )
      )

  private def foldCandidate(
      operator: String,
      operands: List[CandidateType]
  ): Either[CandidateError, CandidateType] =
    operands match
      case first :: second :: rest =>
        val initial = combine(operator, first, second)
        rest.foldLeft(initial) { (accumulated, operand) =>
          accumulated.flatMap(combine(operator, _, operand))
        }
      case _ =>
        Left(
          CandidateError(
            "N075_CANDIDATE_ARITY",
            s"${if operator == "|" then "union" else "intersection"} requires at least two operands."
          )
        )

  private def combine(
      operator: String,
      left: CandidateType,
      right: CandidateType
  ): Either[CandidateError, CandidateType] =
    operator match
      case "|" => Right(CandidateType.Union(left, right))
      case "&" => Right(CandidateType.Intersection(left, right))
      case other =>
        Left(
          CandidateError(
            "N075_CANDIDATE_OPERATOR_UNSUPPORTED",
            s"unsupported Type operator `$other`."
          )
        )

  private def candidateLeaves(candidate: CandidateType): List[CandidateType] =
    candidate match
      case atom: CandidateType.Atom => List(atom)
      case CandidateType.Apply(_, arguments) => arguments.flatMap(candidateLeaves)
      case CandidateType.Union(left, right) => candidateLeaves(left) ++ candidateLeaves(right)
      case CandidateType.Intersection(left, right) => candidateLeaves(left) ++ candidateLeaves(right)

  private def traverse[A, B](
      values: List[A]
  )(convert: A => Either[CandidateError, B]): Either[CandidateError, List[B]] =
    Option(values)
      .toRight(CandidateError("N075_CANDIDATE_LIST_MISSING", "the recursive list must be present."))
      .flatMap(
        _.foldRight(Right(Nil): Either[CandidateError, List[B]]) { (value, accumulated) =>
          for
            head <- convert(value)
            tail <- accumulated
          yield head :: tail
        }
      )

  private def parseType(source: String): Type =
    Input.String(source).parse[Type].get

  private def parseStat(source: String): Stat =
    Input.String(source).parse[Stat].get

  private def allTypeNodes(root: Type): List[Type] =
    root :: (root match
      case _: Type.Name => Nil
      case applied: Type.Apply => allTypeNodes(applied.tpe) ++ applied.args.flatMap(allTypeNodes)
      case infix: Type.ApplyInfix => allTypeNodes(infix.lhs) ++ allTypeNodes(infix.op) ++ allTypeNodes(infix.rhs)
      case other => fail(s"unexpected authored node: ${other.productPrefix}")
    )
