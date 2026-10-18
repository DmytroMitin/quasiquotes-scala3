package quasiquotes.neutral

import quasiquotes.parser.TypeShape
import quasiquotes.types.{TypeNormalForm, TypeTemplate}
import quasiquotes.types.TypeNormalForm.*

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3
import scala.util.control.NonFatal

/** Test-only Scala 3 match-Type model-fit oracle. It does not admit production semantics. */
@nowarn("cat=deprecation")
final class N078Scala3MatchTypeCaseModelFitTest extends munit.FunSuite:
  private enum CandidateType derives CanEqual:
    case Atom(name: String)
    case Free(name: String)
    case Apply(constructor: String, arguments: List[CandidateType])
    case Match(scrutinee: CandidateType, cases: List[CandidateCase])

  private enum CandidatePattern derives CanEqual:
    case Wildcard
    case Atom(name: String)
    case Apply(constructor: String, arguments: List[CandidatePattern])

  private final case class CandidateCase(pattern: CandidatePattern, result: CandidateType) derives CanEqual
  private final case class CandidateError(code: String, detail: String) derives CanEqual
  private final case class SourceProvenance(
      observedTokens: List[String],
      rootSpan: Option[(Int, Int)],
      sourceBacked: Boolean
  ) derives CanEqual
  private final case class CandidateProjection(model: CandidateType, provenance: SourceProvenance) derives CanEqual

  private val atomNames = Set("Int", "String", "Boolean", "AnyVal", "Any", "Nothing")
  private val freeNames = Set("A", "B", "C", "X", "Y", "Z")
  private val fixedConstructors = Map("List" -> 1, "Option" -> 1, "Either" -> 2)

  test("a match Type projects its scrutinee and case pattern-result pairs in source order"):
    val source = parseType(
      """X match {
        |  case Int => String
        |  case _ => Boolean
        |}""".stripMargin
    )

    assertEquals(
      candidateProject(source).map(_.model),
      Right(
        CandidateType.Match(
          CandidateType.Free("X"),
          List(
            CandidateCase(CandidatePattern.Atom("Int"), CandidateType.Atom("String")),
            CandidateCase(CandidatePattern.Wildcard, CandidateType.Atom("Boolean"))
          )
        )
      )
    )

  test("Scala 3 AST exposes Type.Match and ordered TypeCase nodes with source tokens and positions"):
    val source =
      """X match {
        |  case Int => String
        |  case List[Int] => Boolean
        |  case _ => Any
        |}""".stripMargin
    val matched = parseType(source).asInstanceOf[Type.Match]

    assertEquals(matched.productPrefix, "Type.Match")
    assertEquals(matched.tpe.productPrefix, "Type.Name")
    assertEquals(matched.tpe.asInstanceOf[Type.Name].value, "X")
    assertEquals(matched.cases.map(_.productPrefix), List("TypeCase", "TypeCase", "TypeCase"))
    assertEquals(matched.cases.map(_.pat.productPrefix), List("Type.Name", "Type.Apply", "Type.PatWildcard"))
    assertEquals(matched.cases.map(_.body.productPrefix), List("Type.Name", "Type.Name", "Type.Name"))
    assertEquals(matched.cases.map(_.pat.syntax), List("Int", "List[Int]", "_"))
    assertEquals(matched.cases.map(_.body.syntax), List("String", "Boolean", "Any"))
    assertEquals((matched.pos.start, matched.pos.end), (0, source.length))
    assertEquals(matched.tokens.map(_.text).mkString, source)

    val grouped = parseType(s"($source)")
    assertEquals(grouped.structure, matched.structure)
    assertNotEquals(grouped.tokens.map(_.text).mkString, matched.tokens.map(_.text).mkString)

  test("parser and candidate preserve 1 2 3 and N cases including reordered duplicates and overlaps"):
    val sourcesAndCounts = List(
      """X match {
        |  case Int => String
        |}""".stripMargin -> 1,
      """X match {
        |  case Int => String
        |  case _ => Boolean
        |}""".stripMargin -> 2,
      """X match {
        |  case _ => Boolean
        |  case Int => String
        |  case Int => Any
        |}""".stripMargin -> 3,
      """X match {
        |  case Int => String
        |  case String => Int
        |  case Boolean => AnyVal
        |  case List[Int] => Boolean
        |  case _ => Any
        |}""".stripMargin -> 5
    )

    sourcesAndCounts.foreach { case (source, expectedCount) =>
      val parsed = parseType(source).asInstanceOf[Type.Match]
      val projected = candidateProject(parsed).toOption.get.model.asInstanceOf[CandidateType.Match]
      assertEquals(parsed.cases.size, expectedCount, clues(source))
      assertEquals(projected.cases.size, expectedCount, clues(source))
    }

    val reordered = candidateProject(parseType(sourcesAndCounts(2)._1)).toOption.get.model
    val ordinary = candidateProject(parseType(sourcesAndCounts(1)._1)).toOption.get.model
    assertNotEquals(reordered, ordinary)
    val reorderedCases = reordered.asInstanceOf[CandidateType.Match].cases
    assertEquals(
      reorderedCases,
      List(
        CandidateCase(CandidatePattern.Wildcard, CandidateType.Atom("Boolean")),
        CandidateCase(CandidatePattern.Atom("Int"), CandidateType.Atom("String")),
        CandidateCase(CandidatePattern.Atom("Int"), CandidateType.Atom("Any"))
      )
    )

  test("nested match Types retain each scrutinee and ordered inner case list"):
    val source = parseType(
      """X match {
        |  case Int => Y match {
        |    case String => Boolean
        |    case _ => Int
        |  }
        |  case _ => Z
        |}""".stripMargin
    )

    val nestedScrutinee = parseType(
      """(X match {
        |  case Int => String
        |  case _ => Boolean
        |}) match {
        |  case String => Int
        |  case _ => Boolean
        |}""".stripMargin
    )
    val projectedScrutinee = candidateProject(nestedScrutinee).toOption.get.model
      .asInstanceOf[CandidateType.Match]
    assert(projectedScrutinee.scrutinee.isInstanceOf[CandidateType.Match])
    assertEquals(
      projectedScrutinee.cases.map(_.pattern),
      List(CandidatePattern.Atom("String"), CandidatePattern.Wildcard)
    )

    assertEquals(
      candidateProject(source).map(_.model),
      Right(
        CandidateType.Match(
          CandidateType.Free("X"),
          List(
            CandidateCase(
              CandidatePattern.Atom("Int"),
              CandidateType.Match(
                CandidateType.Free("Y"),
                List(
                  CandidateCase(CandidatePattern.Atom("String"), CandidateType.Atom("Boolean")),
                  CandidateCase(CandidatePattern.Wildcard, CandidateType.Atom("Int"))
                )
              )
            ),
            CandidateCase(CandidatePattern.Wildcard, CandidateType.Free("Z"))
          )
        )
      )
    )

  test("Scalameta admits lowercase case binders but exposes no binder identity and candidate fails closed"):
    val matched = parseType(
      """X match {
        |  case List[t] => Either[t, X]
        |}""".stripMargin
    ).asInstanceOf[Type.Match]
    val clause = matched.cases.head
    val patternArgument = clause.pat.asInstanceOf[Type.Apply].args.head
    val resultArguments = clause.body.asInstanceOf[Type.Apply].args

    assertEquals(patternArgument.productPrefix, "Type.Name")
    assertEquals(patternArgument.asInstanceOf[Type.Name].value, "t")
    assertEquals(resultArguments.map(_.productPrefix), List("Type.Name", "Type.Name"))
    assertEquals(resultArguments.map(_.asInstanceOf[Type.Name].value), List("t", "X"))
    assertEquals(errorCode(candidateProject(matched)), Some("N078_PATTERN_BINDER_UNSUPPORTED"))

  test("outer binders fail closed and cannot silently shadow fixed constructors"):
    val outerReference = parseType(
      """[A] =>> A match {
        |  case Int => A
        |}""".stripMargin
    ).asInstanceOf[Type.Lambda]
    val constructorShadow = parseType(
      """[List] =>> X match {
        |  case List[Int] => String
        |}""".stripMargin
    ).asInstanceOf[Type.Lambda]

    assertEquals(
      errorCode(candidateProjectWithin(outerReference.tpe, Set("A"))),
      Some("N078_OUTER_BINDER_UNSUPPORTED")
    )
    assertEquals(
      errorCode(candidateProjectWithin(constructorShadow.tpe, Set("List"))),
      Some("N078_PATTERN_CONSTRUCTOR_SHADOWED")
    )

  test("admitted candidate values fresh-author Type.Match and TypeCase then reproject structurally"):
    val sources = List(
      """X match {
        |  case Int => String
        |  case _ => Boolean
        |}""".stripMargin,
      """X match {
        |  case List[Int] => Y match {
        |    case String => Boolean
        |    case _ => Int
        |  }
        |  case List[Int] => Any
        |}""".stripMargin
    )

    sources.foreach { source =>
      val projected = candidateProject(parseType(source)).toOption.get
      val authored = candidateAuthor(projected.model).toOption.get
      val authoredMatch = authored.asInstanceOf[Type.Match]
      val reprojected = candidateProject(authored).toOption.get

      assertEquals(authoredMatch.productPrefix, "Type.Match", clues(source))
      assert(authoredMatch.cases.forall(_.productPrefix == "TypeCase"), clues(source))
      assertEquals(authored.pos, Position.None, clues(source))
      assert(authored.tokens.nonEmpty, clues(source))
      assertEquals(reprojected.model, projected.model, clues(source))
      assert(projected.provenance.observedTokens.nonEmpty, clues(source))
      assert(projected.provenance.rootSpan.nonEmpty, clues(source))
      assert(projected.provenance.sourceBacked, clues(source))
      assertEquals(reprojected.provenance.rootSpan, None, clues(source))
      assert(!reprojected.provenance.sourceBacked, clues(source))
      assertEquals(
        reprojected.provenance.observedTokens,
        authored.tokens.map(_.text).toList,
        clues(source)
      )
    }

  test("public Type projection rejects root and nested match Types with exact stable diagnostics"):
    val root = parseType(
      """X match {
        |  case Int => String
        |  case _ => Boolean
        |}""".stripMargin
    )
    val nested = parseType(
      """List[X match {
        |  case Int => String
        |  case _ => Boolean
        |}]""".stripMargin
    )
    val expected = Left(
      NeutralProjectionError(
        "NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED",
        "unsupported Scalameta type node: Type.Match."
      )
    )

    assertEquals(ScalametaTypeNormalFormProjection.project(root), expected)
    assertEquals(ScalametaTypeNormalFormProjection.project(nested), expected)

  test("public Type projection retains its ordinary scalar apply tuple and function positives"):
    val fixtures = List(
      "Int" -> STypeIdent("Int"),
      "List[Int]" -> STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
      "Either[Int, String]" ->
        STypeApply(STypeIdent("Either"), List(STypeIdent("Int"), STypeIdent("String"))),
      "(Int, String)" -> STypeTuple(List(STypeIdent("Int"), STypeIdent("String"))),
      "Int => String" -> STypeFunction(List(STypeIdent("Int")), STypeIdent("String"))
    )

    fixtures.foreach { case (source, expected) =>
      assertEquals(
        ScalametaTypeNormalFormProjection.project(parseType(source)).map(_.normalForm),
        Right(expected),
        clues(source)
      )
    }

  test("existing TypeShape TypeNormalForm TypeTemplate and authoring cannot encode a match Type"):
    val unsupported = TypeShape.Unsupported("Type.Match", "N078 test-only structural form")
    val forgedApplication = STypeApply(
      STypeIdent("match"),
      List(STypeIdent("Int"), STypeIdent("String"))
    )

    assert(TypeNormalForm.fromShape(unsupported).isLeft)
    assert(TypeTemplate.fromShape(unsupported).isLeft)
    assert(ScalametaTypeNormalFormAuthoring.author(forgedApplication).isLeft)

  test("parser candidate and public alias layers characterize match-Type definition positions separately"):
    val alias = parseStat(
      """type Result = X match {
        |  case Int => String
        |  case _ => Boolean
        |}""".stripMargin
    ).asInstanceOf[Defn.Type]

    assertEquals(alias.body.productPrefix, "Type.Match")
    assert(candidateProject(alias.body).isRight)
    assertEquals(
      ScalametaSimpleTypeAliasProjection.project(alias).left.toOption.map(_.code),
      Some("NEUTRAL_DEFINITION_TYPE_UNSUPPORTED")
    )

  test("parser reports malformed and bounded patterns while exposing empty-case invariant failure"):
    val missingResult =
      """X match {
        |  case Int =>
        |}""".stripMargin
    val boundedWildcard =
      """X match {
        |  case _ <: Any => String
        |}""".stripMargin
    val typedPattern =
      """X match {
        |  case t: Int => String
        |}""".stripMargin
    val empty =
      """X match {
        |}""".stripMargin
    val nestedLambda =
      """X match {
        |  case Int => [A] =>> A
        |}""".stripMargin

    assertEquals(errorCode(parseTypeResult(missingResult)), Some("N078_PARSE_ERROR"))
    assertEquals(errorCode(parseTypeResult(boundedWildcard)), Some("N078_PARSE_ERROR"))
    assertEquals(errorCode(parseTypeResult(typedPattern)), Some("N078_PARSE_ERROR"))
    assertEquals(errorCode(parseTypeResult(empty)), Some("N078_PARSER_THROWN"))
    assert(parseTypeResult(empty).left.toOption.get.detail.startsWith("InvariantFailedException:"))
    assertEquals(errorCode(parseTypeResult(nestedLambda)), Some("N078_PARSER_THROWN"))

  test("candidate policy rejects literal infix qualified unresolved lambda and nested-match patterns"):
    val literal = parseType(
      """X match {
        |  case 1 => String
        |}""".stripMargin
    )
    val union = parseType(
      """X match {
        |  case Int | String => Boolean
        |}""".stripMargin
    )
    val intersection = parseType(
      """X match {
        |  case Int & AnyVal => String
        |}""".stripMargin
    )
    val qualified = parseType(
      """X match {
        |  case types.List[Int] => String
        |}""".stripMargin
    )
    val unresolved = parseType(
      """X match {
        |  case Box[Int] => String
        |}""".stripMargin
    )
    val lambdaResult = parseType("[A] =>> A")
    val nestedPattern = Type.Match(
      Type.Name("X"),
      List(
        TypeCase(
          Type.Match(Type.Name("Y"), List(TypeCase(Type.PatWildcard(), Type.Name("Int")))),
          Type.Name("String")
        )
      )
    )

    assertEquals(errorCode(candidateProject(literal)), Some("N078_PATTERN_NODE_UNSUPPORTED"))
    assertEquals(errorCode(candidateProject(union)), Some("N078_PATTERN_NODE_UNSUPPORTED"))
    assertEquals(errorCode(candidateProject(intersection)), Some("N078_PATTERN_NODE_UNSUPPORTED"))
    assertEquals(errorCode(candidateProject(qualified)), Some("N078_PATTERN_CONSTRUCTOR_UNSUPPORTED"))
    assertEquals(errorCode(candidateProject(unresolved)), Some("N078_PATTERN_CONSTRUCTOR_UNSUPPORTED"))
    assertEquals(errorCode(candidateProject(lambdaResult)), Some("N078_TYPE_NODE_UNSUPPORTED"))
    assertEquals(errorCode(candidateProject(nestedPattern)), Some("N078_PATTERN_NODE_UNSUPPORTED"))

  test("candidate authoring rejects missing empty malformed and unsupported values deterministically"):
    val oneCase = List(CandidateCase(CandidatePattern.Atom("Int"), CandidateType.Atom("String")))
    val missingScrutinee = CandidateType.Match(null, oneCase)
    val missingCases = CandidateType.Match(CandidateType.Free("X"), null)
    val emptyCases = CandidateType.Match(CandidateType.Free("X"), Nil)
    val missingCase = CandidateType.Match(CandidateType.Free("X"), List(null))
    val missingPattern = CandidateType.Match(
      CandidateType.Free("X"),
      List(CandidateCase(null, CandidateType.Atom("String")))
    )
    val missingResult = CandidateType.Match(
      CandidateType.Free("X"),
      List(CandidateCase(CandidatePattern.Atom("Int"), null))
    )
    val malformedArguments = CandidateType.Apply("List", null)
    val wrongArity = CandidateType.Apply("Either", List(CandidateType.Atom("Int")))
    val unknownConstructor = CandidateType.Apply("Box", List(CandidateType.Atom("Int")))

    assertEquals(errorCode(candidateProject(null)), Some("N078_SOURCE_TYPE_MISSING"))
    assertEquals(errorCode(candidateAuthor(null)), Some("N078_CANDIDATE_MISSING"))
    assertEquals(errorCode(candidateAuthor(missingScrutinee)), Some("N078_CANDIDATE_TYPE_MISSING"))
    assertEquals(errorCode(candidateAuthor(missingCases)), Some("N078_CANDIDATE_CASE_LIST_MISSING"))
    assertEquals(errorCode(candidateAuthor(emptyCases)), Some("N078_CANDIDATE_CASE_LIST_EMPTY"))
    assertEquals(errorCode(candidateAuthor(missingCase)), Some("N078_CANDIDATE_CASE_MISSING"))
    assertEquals(errorCode(candidateAuthor(missingPattern)), Some("N078_CANDIDATE_PATTERN_MISSING"))
    assertEquals(errorCode(candidateAuthor(missingResult)), Some("N078_CANDIDATE_TYPE_MISSING"))
    assertEquals(errorCode(candidateAuthor(malformedArguments)), Some("N078_CANDIDATE_ARGUMENT_LIST_MISSING"))
    assertEquals(errorCode(candidateAuthor(wrongArity)), Some("N078_CANDIDATE_CONSTRUCTOR_ARITY_UNSUPPORTED"))
    assertEquals(errorCode(candidateAuthor(unknownConstructor)), Some("N078_CANDIDATE_CONSTRUCTOR_UNSUPPORTED"))

  private def parseType(source: String): Type = Input.String(source).parse[Type].get

  private def parseStat(source: String): Stat = Input.String(source).parse[Stat].get

  private def parseTypeResult(source: String): Either[CandidateError, Type] =
    Option(source)
      .toRight(CandidateError("N078_SOURCE_MISSING", "the source text must be present."))
      .flatMap { present =>
        try
          Input.String(present).parse[Type] match
            case Parsed.Success(sourceType) => Right(sourceType)
            case error: Parsed.Error => Left(CandidateError("N078_PARSE_ERROR", error.message))
        catch
          case NonFatal(problem) =>
            Left(
              CandidateError(
                "N078_PARSER_THROWN",
                s"${problem.getClass.getSimpleName}:${Option(problem.getMessage).getOrElse("")}"
              )
            )
      }

  private def candidateProject(sourceType: Type): Either[CandidateError, CandidateProjection] =
    candidateProjectWithin(sourceType, Set.empty)

  private def candidateProjectWithin(
      sourceType: Type,
      outerBinders: Set[String]
  ): Either[CandidateError, CandidateProjection] =
    Option(sourceType)
      .toRight(CandidateError("N078_SOURCE_TYPE_MISSING", "the Scalameta Type must be present."))
      .flatMap { present =>
        projectType(present, outerBinders).map { model =>
          val rootSpan = present.pos match
            case Position.None => None
            case position => Some(position.start -> position.end)
          CandidateProjection(
            model,
            SourceProvenance(
              present.tokens.map(_.text).toList,
              rootSpan,
              sourceBacked = rootSpan.nonEmpty
            )
          )
        }
      }

  private def projectType(
      sourceType: Type,
      outerBinders: Set[String]
  ): Either[CandidateError, CandidateType] =
    Option(sourceType)
      .toRight(CandidateError("N078_SOURCE_TYPE_MISSING", "a recursive Scalameta Type must be present."))
      .flatMap {
        case name: Type.Name if outerBinders(name.value) =>
          Left(
            CandidateError(
              "N078_OUTER_BINDER_UNSUPPORTED",
              s"outer Type binder `${name.value}` is outside the closed N078 carrier."
            )
          )
        case name: Type.Name if atomNames(name.value) => Right(CandidateType.Atom(name.value))
        case name: Type.Name if freeNames(name.value) => Right(CandidateType.Free(name.value))
        case name: Type.Name =>
          Left(CandidateError("N078_TYPE_NAME_UNSUPPORTED", s"unsupported Type name `${name.value}`."))
        case applied: Type.Apply =>
          applied.tpe match
            case constructor: Type.Name if outerBinders(constructor.value) =>
              Left(
                CandidateError(
                  "N078_CONSTRUCTOR_SHADOWED",
                  s"fixed constructor `${constructor.value}` is shadowed by an outer Type binder."
                )
              )
            case constructor: Type.Name => projectAppliedType(constructor.value, applied.args, outerBinders)
            case other =>
              Left(
                CandidateError(
                  "N078_CONSTRUCTOR_UNSUPPORTED",
                  s"unsupported Type constructor node: ${other.productPrefix}."
                )
              )
        case matched: Type.Match =>
          for
            scrutinee <- projectType(matched.tpe, outerBinders)
            cases <- projectCases(matched.cases, outerBinders)
          yield CandidateType.Match(scrutinee, cases)
        case other =>
          Left(
            CandidateError(
              "N078_TYPE_NODE_UNSUPPORTED",
              s"unsupported Scalameta Type node: ${other.productPrefix}."
            )
          )
      }

  private def projectAppliedType(
      constructor: String,
      arguments: List[Type],
      outerBinders: Set[String]
  ): Either[CandidateError, CandidateType] =
    fixedConstructors.get(constructor) match
      case None => Left(CandidateError("N078_CONSTRUCTOR_UNSUPPORTED", s"unsupported Type constructor `$constructor`."))
      case Some(_) if arguments == null =>
        Left(CandidateError("N078_ARGUMENT_LIST_MISSING", "the Type argument list must be present."))
      case Some(expectedArity) if arguments.size != expectedArity =>
        Left(
          CandidateError(
            "N078_CONSTRUCTOR_ARITY_UNSUPPORTED",
            s"constructor `$constructor` requires $expectedArity arguments, found ${arguments.size}."
          )
        )
      case Some(_) =>
        traverse(arguments)(projectType(_, outerBinders)).map(CandidateType.Apply(constructor, _))

  private def projectCases(
      cases: List[TypeCase],
      outerBinders: Set[String]
  ): Either[CandidateError, List[CandidateCase]] =
    Option(cases)
      .toRight(CandidateError("N078_CASE_LIST_MISSING", "the match-Type case list must be present."))
      .flatMap { present =>
        Either
          .cond(
            present.nonEmpty,
            present,
            CandidateError("N078_CASE_LIST_EMPTY", "the match-Type case list must be non-empty.")
          )
          .flatMap(
            traverse(_) { sourceCase =>
              Option(sourceCase)
                .toRight(CandidateError("N078_CASE_MISSING", "each match-Type case must be present."))
                .flatMap { clause =>
                  for
                    pattern <- projectPattern(clause.pat, outerBinders)
                    result <- projectType(clause.body, outerBinders)
                  yield CandidateCase(pattern, result)
                }
            }
          )
      }

  private def projectPattern(
      pattern: Type,
      outerBinders: Set[String]
  ): Either[CandidateError, CandidatePattern] =
    Option(pattern)
      .toRight(CandidateError("N078_PATTERN_MISSING", "the match-Type case pattern must be present."))
      .flatMap {
        case _: Type.PatWildcard => Right(CandidatePattern.Wildcard)
        case name: Type.Name if isPotentialBinder(name.value) =>
          Left(
            CandidateError(
              "N078_PATTERN_BINDER_UNSUPPORTED",
              s"pattern binder `${name.value}` has no explicit Scalameta binder identity."
            )
          )
        case name: Type.Name if outerBinders(name.value) =>
          Left(
            CandidateError(
              "N078_PATTERN_OUTER_BINDER_UNSUPPORTED",
              s"outer Type binder `${name.value}` is unsupported in a case pattern."
            )
          )
        case name: Type.Name if atomNames(name.value) => Right(CandidatePattern.Atom(name.value))
        case name: Type.Name =>
          Left(
            CandidateError(
              "N078_PATTERN_NAME_UNSUPPORTED",
              s"unsupported fixed case-pattern Type name `${name.value}`."
            )
          )
        case applied: Type.Apply =>
          applied.tpe match
            case constructor: Type.Name if outerBinders(constructor.value) =>
              Left(
                CandidateError(
                  "N078_PATTERN_CONSTRUCTOR_SHADOWED",
                  s"fixed pattern constructor `${constructor.value}` is shadowed by an outer Type binder."
                )
              )
            case constructor: Type.Name => projectAppliedPattern(constructor.value, applied.args, outerBinders)
            case other =>
              Left(
                CandidateError(
                  "N078_PATTERN_CONSTRUCTOR_UNSUPPORTED",
                  s"unsupported case-pattern constructor node: ${other.productPrefix}."
                )
              )
        case other =>
          Left(
            CandidateError(
              "N078_PATTERN_NODE_UNSUPPORTED",
              s"unsupported case-pattern Type node: ${other.productPrefix}."
            )
          )
      }

  private def projectAppliedPattern(
      constructor: String,
      arguments: List[Type],
      outerBinders: Set[String]
  ): Either[CandidateError, CandidatePattern] =
    fixedConstructors.get(constructor) match
      case None =>
        Left(
          CandidateError(
            "N078_PATTERN_CONSTRUCTOR_UNSUPPORTED",
            s"unsupported case-pattern constructor `$constructor`."
          )
        )
      case Some(_) if arguments == null =>
        Left(CandidateError("N078_PATTERN_ARGUMENT_LIST_MISSING", "the pattern argument list must be present."))
      case Some(expectedArity) if arguments.size != expectedArity =>
        Left(
          CandidateError(
            "N078_PATTERN_CONSTRUCTOR_ARITY_UNSUPPORTED",
            s"pattern constructor `$constructor` requires $expectedArity arguments, found ${arguments.size}."
          )
        )
      case Some(_) =>
        traverse(arguments)(projectPattern(_, outerBinders)).map(CandidatePattern.Apply(constructor, _))

  private def candidateAuthor(candidate: CandidateType): Either[CandidateError, Type] =
    Option(candidate)
      .toRight(CandidateError("N078_CANDIDATE_MISSING", "the candidate Type must be present."))
      .flatMap(authorType)

  private def authorType(candidate: CandidateType): Either[CandidateError, Type] =
    Option(candidate)
      .toRight(CandidateError("N078_CANDIDATE_TYPE_MISSING", "a recursive candidate Type must be present."))
      .flatMap {
        case CandidateType.Atom(name) if atomNames(name) => Right(Type.Name(name))
        case CandidateType.Free(name) if freeNames(name) => Right(Type.Name(name))
        case CandidateType.Atom(name) =>
          Left(CandidateError("N078_CANDIDATE_TYPE_NAME_UNSUPPORTED", s"unsupported candidate Type name `$name`."))
        case CandidateType.Free(name) =>
          Left(CandidateError("N078_CANDIDATE_TYPE_NAME_UNSUPPORTED", s"unsupported candidate Type name `$name`."))
        case CandidateType.Apply(constructor, arguments) => authorAppliedType(constructor, arguments)
        case CandidateType.Match(scrutinee, cases) =>
          for
            authoredScrutinee <- authorType(scrutinee)
            authoredCases <- authorCases(cases)
          yield Type.Match(authoredScrutinee, authoredCases)
      }

  private def authorAppliedType(
      constructor: String,
      arguments: List[CandidateType]
  ): Either[CandidateError, Type] =
    fixedConstructors.get(constructor) match
      case None =>
        Left(
          CandidateError(
            "N078_CANDIDATE_CONSTRUCTOR_UNSUPPORTED",
            s"unsupported candidate Type constructor `$constructor`."
          )
        )
      case Some(_) if arguments == null =>
        Left(
          CandidateError(
            "N078_CANDIDATE_ARGUMENT_LIST_MISSING",
            "the candidate Type argument list must be present."
          )
        )
      case Some(expectedArity) if arguments.size != expectedArity =>
        Left(
          CandidateError(
            "N078_CANDIDATE_CONSTRUCTOR_ARITY_UNSUPPORTED",
            s"candidate constructor `$constructor` requires $expectedArity arguments, found ${arguments.size}."
          )
        )
      case Some(_) =>
        traverse(arguments)(authorType).map(authored => Type.Apply(Type.Name(constructor), authored))

  private def authorCases(cases: List[CandidateCase]): Either[CandidateError, List[TypeCase]] =
    Option(cases)
      .toRight(
        CandidateError(
          "N078_CANDIDATE_CASE_LIST_MISSING",
          "the candidate match-Type case list must be present."
        )
      )
      .flatMap { present =>
        Either
          .cond(
            present.nonEmpty,
            present,
            CandidateError(
              "N078_CANDIDATE_CASE_LIST_EMPTY",
              "the candidate match-Type case list must be non-empty."
            )
          )
          .flatMap(
            traverse(_) { candidateCase =>
              Option(candidateCase)
                .toRight(CandidateError("N078_CANDIDATE_CASE_MISSING", "each candidate case must be present."))
                .flatMap { presentCase =>
                  for
                    pattern <- authorPattern(presentCase.pattern)
                    result <- authorType(presentCase.result)
                  yield TypeCase(pattern, result)
                }
            }
          )
      }

  private def authorPattern(pattern: CandidatePattern): Either[CandidateError, Type] =
    Option(pattern)
      .toRight(CandidateError("N078_CANDIDATE_PATTERN_MISSING", "the candidate case pattern must be present."))
      .flatMap {
        case CandidatePattern.Wildcard => Right(Type.PatWildcard())
        case CandidatePattern.Atom(name) if atomNames(name) => Right(Type.Name(name))
        case CandidatePattern.Atom(name) =>
          Left(
            CandidateError(
              "N078_CANDIDATE_PATTERN_NAME_UNSUPPORTED",
              s"unsupported candidate fixed pattern name `$name`."
            )
          )
        case CandidatePattern.Apply(constructor, arguments) =>
          fixedConstructors.get(constructor) match
            case None =>
              Left(
                CandidateError(
                  "N078_CANDIDATE_PATTERN_CONSTRUCTOR_UNSUPPORTED",
                  s"unsupported candidate pattern constructor `$constructor`."
                )
              )
            case Some(_) if arguments == null =>
              Left(
                CandidateError(
                  "N078_CANDIDATE_PATTERN_ARGUMENT_LIST_MISSING",
                  "the candidate pattern argument list must be present."
                )
              )
            case Some(expectedArity) if arguments.size != expectedArity =>
              Left(
                CandidateError(
                  "N078_CANDIDATE_PATTERN_CONSTRUCTOR_ARITY_UNSUPPORTED",
                  s"candidate pattern constructor `$constructor` requires $expectedArity arguments, " +
                    s"found ${arguments.size}."
                )
              )
            case Some(_) =>
              traverse(arguments)(authorPattern)
                .map(authored => Type.Apply(Type.Name(constructor), authored))
      }

  private def isPotentialBinder(name: String): Boolean =
    name.headOption.exists(_.isLower)

  private def traverse[A, B](
      values: List[A]
  )(transform: A => Either[CandidateError, B]): Either[CandidateError, List[B]] =
    values.foldRight(Right(Nil): Either[CandidateError, List[B]]) { (value, accumulated) =>
      for
        head <- transform(value)
        tail <- accumulated
      yield head :: tail
    }

  private def errorCode[A](result: Either[CandidateError, A]): Option[String] =
    result.left.toOption.map(_.code)
