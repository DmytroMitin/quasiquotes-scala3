package quasiquotes.neutral

import quasiquotes.parser.TypeShape
import quasiquotes.types.*
import quasiquotes.types.TypeNormalForm.*

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

/** Test-only Type-lambda binder model-fit oracle. It does not admit production semantics. */
@nowarn("cat=deprecation")
final class N077TypeLambdaBinderModelFitTest extends munit.FunSuite:
  private enum CandidateType derives CanEqual:
    case Atom(name: String)
    case Free(name: String)
    case Bound(depth: Int, parameterIndex: Int)
    case Apply(constructor: String, arguments: List[CandidateType])
    case Lambda(parameterCount: Int, body: CandidateType)

  private final case class CandidateError(code: String, detail: String) derives CanEqual
  private final case class SourceProvenance(
      lambdaParameterNames: Vector[List[String]],
      tokens: List[String],
      rootSpan: Option[(Int, Int)]
  ) derives CanEqual
  private final case class CandidateProjection(model: CandidateType, provenance: SourceProvenance) derives CanEqual

  private val atomNames = Set("Int", "String", "Boolean", "AnyVal", "Any", "Nothing")
  private val freeNames = Set("A", "B", "C", "F", "X", "Y", "Z")
  private val fixedConstructors = Map("List" -> 1, "Option" -> 1, "Either" -> 2)

  test("a Type lambda projects body names to lexical binder positions"):
    val source = parseType("[A, B] =>> Either[A, B]")

    assertEquals(
      candidateProject(source).map(_.model),
      Right(
        CandidateType.Lambda(
          2,
          CandidateType.Apply(
            "Either",
            List(CandidateType.Bound(0, 0), CandidateType.Bound(0, 1))
          )
        )
      )
    )

  test("Scala 3 Type-lambda AST exposes ordered parameters bounds body tokens and positions"):
    val unarySource = "[A] =>> List[A]"
    val unary = parseType(unarySource).asInstanceOf[Type.Lambda]
    val binary = parseType("[A, B] =>> Either[A, B]").asInstanceOf[Type.Lambda]
    val bounded = parseType("[A >: Nothing <: AnyVal] =>> List[A]").asInstanceOf[Type.Lambda]
    val boundedNested =
      parseType("[A <: AnyVal] =>> [B >: Nothing] =>> Either[A, B]").asInstanceOf[Type.Lambda]

    assertEquals(unary.productPrefix, "Type.Lambda")
    assertEquals(unary.tparams.map(_.name.value), List("A"))
    assertEquals(unary.tparams.map(_.productPrefix), List("Type.Param"))
    assertEquals(unary.tparams.head.mods, Nil)
    assertEquals(unary.tparams.head.tparams, Nil)
    assertEquals(unary.tparams.head.bounds.lo, None)
    assertEquals(unary.tparams.head.bounds.hi, None)
    assertEquals(unary.tparams.head.bounds.context, Nil)
    assertEquals(unary.tparams.head.bounds.view, Nil)
    assertEquals(unary.tpe.productPrefix, "Type.Apply")
    assertEquals((unary.pos.start, unary.pos.end), (0, unarySource.length))
    assertEquals(unary.tokens.map(_.text).mkString, unarySource)
    assertEquals(binary.tparams.map(_.name.value), List("A", "B"))
    assertEquals(
      binary.tpe.asInstanceOf[Type.Apply].args.map(_.asInstanceOf[Type.Name].value),
      List("A", "B")
    )
    assertEquals(bounded.tparams.head.bounds.lo.map(_.syntax), Some("Nothing"))
    assertEquals(bounded.tparams.head.bounds.hi.map(_.syntax), Some("AnyVal"))
    assertEquals(bounded.tparams.head.bounds.context, Nil)
    assertEquals(bounded.tparams.head.bounds.view, Nil)
    assertEquals(boundedNested.tparams.head.bounds.hi.map(_.syntax), Some("AnyVal"))
    val nestedBody = boundedNested.tpe.asInstanceOf[Type.Lambda]
    assertEquals(nestedBody.tparams.head.bounds.lo.map(_.syntax), Some("Nothing"))
    assertEquals(nestedBody.tpe.syntax, "Either[A, B]")
    val grouped = parseType("([A] =>> List[A])")
    assertEquals(grouped.structure, unary.structure)
    assertNotEquals(grouped.tokens.map(_.text), unary.tokens.map(_.text))
    assert(parseTypeResult("[A = Int] =>> List[A]").isLeft)

  test("current public projection rejects Type lambdas exactly and preserves admitted Type families"):
    val expected = Left(
      NeutralProjectionError(
        "NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED",
        "unsupported Scalameta type node: Type.Lambda."
      )
    )
    List(
      "[A] =>> List[A]",
      "[A, B] =>> Either[A, B]",
      "[A] =>> [B] =>> Either[A, B]",
      "Option[[X] =>> List[X]]"
    ).foreach { source =>
      assertEquals(ScalametaTypeNormalFormProjection.project(parseType(source)), expected, clues(source))
    }

    val admitted = List(
      "Int" -> STypeIdent("Int"),
      "List[Int]" -> STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
      "Option[String]" -> STypeApply(STypeIdent("Option"), List(STypeIdent("String"))),
      "Either[Int, String]" ->
        STypeApply(STypeIdent("Either"), List(STypeIdent("Int"), STypeIdent("String"))),
      "(Int, String)" -> STypeTuple(List(STypeIdent("Int"), STypeIdent("String"))),
      "Int => String" -> STypeFunction(List(STypeIdent("Int")), STypeIdent("String"))
    )
    admitted.foreach { (source, expectedForm) =>
      assertEquals(
        ScalametaTypeNormalFormProjection.project(parseType(source)).map(_.normalForm),
        Right(expectedForm),
        clues(source)
      )
      assertEquals(
        ScalametaTypeNormalFormProjection
          .project(ScalametaTypeNormalFormAuthoring.author(expectedForm).toOption.get)
          .map(_.normalForm),
        Right(expectedForm),
        clues(source)
      )
    }
    assert(TypeNormalForm.fromShape(TypeShape.Unsupported("Type.Lambda", "[A] =>> A")).isLeft)
    assert(TypeTemplate.fromShape(TypeShape.Unsupported("Type.Lambda", "[A] =>> A")).isLeft)

  test("alpha-renamed lambdas share semantic structure but keep separate source provenance"):
    val a = candidateProject(parseType("[A] =>> List[A]")).toOption.get
    val x = candidateProject(parseType("[X] =>> List[X]")).toOption.get

    assertEquals(a.model, x.model)
    assertNotEquals(a.provenance, x.provenance)
    assertEquals(a.provenance.lambdaParameterNames, Vector(List("A")))
    assertEquals(x.provenance.lambdaParameterNames, Vector(List("X")))
    assertEquals(a.provenance.tokens.mkString, "[A] =>> List[A]")
    assertEquals(x.provenance.tokens.mkString, "[X] =>> List[X]")
    assertEquals(a.provenance.rootSpan, Some(0 -> 15))
    assertEquals(x.provenance.rootSpan, Some(0 -> 15))

  test("bound and free same-spelling Type names collide under unresolved-string erasure"):
    val free = candidateProject(parseType("A")).toOption.get.model
    val lambda = candidateProject(parseType("[A] =>> A")).toOption.get.model
    val bound = lambda.asInstanceOf[CandidateType.Lambda].body

    assertEquals(free, CandidateType.Free("A"))
    assertEquals(bound, CandidateType.Bound(0, 0))
    assertNotEquals(free, bound)
    assertEquals(eraseReference(free, "A"), Right(STypeIdent("A")))
    assertEquals(eraseReference(bound, "A"), Right(STypeIdent("A")))
    assertEquals(candidateProject(parseType("[A] =>> A")).map(_.model), candidateProject(parseType("[X] =>> X")).map(_.model))

  test("nested scopes preserve outer depth inner position reordering and same-spelling shadowing"):
    val nested = candidateProject(parseType("[A] =>> [B] =>> Either[A, B]")).toOption.get
    val reordered = candidateProject(parseType("[A] =>> [B] =>> Either[B, A]")).toOption.get
    val shadowed = candidateProject(parseType("[A] =>> [A] =>> List[A]")).toOption.get

    assertEquals(
      nested.model,
      CandidateType.Lambda(
        1,
        CandidateType.Lambda(
          1,
          CandidateType.Apply(
            "Either",
            List(CandidateType.Bound(1, 0), CandidateType.Bound(0, 0))
          )
        )
      )
    )
    assertNotEquals(nested.model, reordered.model)
    assertEquals(
      shadowed.model,
      CandidateType.Lambda(
        1,
        CandidateType.Lambda(
          1,
          CandidateType.Apply("List", List(CandidateType.Bound(0, 0)))
        )
      )
    )
    assertEquals(shadowed.provenance.lambdaParameterNames, Vector(List("A"), List("A")))

  test("fixed constructor applications shadowed by Type-lambda binders fail closed during projection"):
    val shadowed = List(
      "[List] =>> List[Int]",
      "[Option] =>> Option[Int]",
      "[Either] =>> Either[Int, String]",
      "[List] =>> [A] =>> List[A]",
      "[A] =>> [List] =>> List[A]"
    )

    shadowed.foreach { source =>
      val parsed = parseType(source)
      assertEquals(parsed.productPrefix, "Type.Lambda", clues(source))
      assertEquals(
        candidateProject(parsed).left.toOption.map(_.code),
        Some("N077_CANDIDATE_CONSTRUCTOR_SHADOWED"),
        clues(source)
      )
    }

  test("fixed constructor applications shadowed by provenance fail closed during authoring"):
    val shadowed = List(
      CandidateProjection(
        CandidateType.Lambda(1, CandidateType.Apply("List", List(CandidateType.Atom("Int")))),
        SourceProvenance(Vector(List("List")), Nil, None)
      ),
      CandidateProjection(
        CandidateType.Lambda(1, CandidateType.Apply("Option", List(CandidateType.Atom("Int")))),
        SourceProvenance(Vector(List("Option")), Nil, None)
      ),
      CandidateProjection(
        CandidateType.Lambda(
          1,
          CandidateType.Apply("Either", List(CandidateType.Atom("Int"), CandidateType.Atom("String")))
        ),
        SourceProvenance(Vector(List("Either")), Nil, None)
      ),
      CandidateProjection(
        CandidateType.Lambda(
          1,
          CandidateType.Lambda(
            1,
            CandidateType.Apply("List", List(CandidateType.Bound(0, 0)))
          )
        ),
        SourceProvenance(Vector(List("List"), List("A")), Nil, None)
      ),
      CandidateProjection(
        CandidateType.Lambda(
          1,
          CandidateType.Lambda(
            1,
            CandidateType.Apply("List", List(CandidateType.Bound(1, 0)))
          )
        ),
        SourceProvenance(Vector(List("A"), List("List")), Nil, None)
      )
    )

    shadowed.foreach { projection =>
      assertEquals(
        candidateAuthor(projection).left.toOption.map(_.code),
        Some("N077_CANDIDATE_CONSTRUCTOR_SHADOWED")
      )
    }

  test("unambiguous fixed-name binders and unrelated atom shadowing keep lexical identity"):
    assertEquals(
      candidateProject(parseType("[List] =>> Int")).map(_.model),
      Right(CandidateType.Lambda(1, CandidateType.Atom("Int")))
    )
    assertEquals(
      candidateProject(parseType("[Int] =>> Int")).map(_.model),
      Right(CandidateType.Lambda(1, CandidateType.Bound(0, 0)))
    )

  test("fresh authoring and reprojection preserve binding clause order source names and shadowing"):
    val sources = List(
      "[A] =>> List[A]",
      "[A, B] =>> Either[A, B]",
      "[A] =>> [B] =>> Either[A, B]",
      "[A] =>> [A] =>> List[A]",
      "[X] =>> Option[X]",
      "[A, B, C] =>> Either[List[A], Option[C]]"
    )

    sources.foreach { source =>
      val projected = candidateProject(parseType(source)).toOption.get
      val authored = candidateAuthor(projected).toOption.get
      val reprojected = candidateProject(authored).toOption.get

      assertEquals(reprojected.model, projected.model, clues(source))
      assertEquals(
        reprojected.provenance.lambdaParameterNames,
        projected.provenance.lambdaParameterNames,
        clues(source)
      )
      assertEquals(authored.structure, parseType(source).structure, clues(source))
      assertEquals(authored.syntax, source, clues(source))
      assert(allTrees(authored).forall(_.pos == Position.None), clues(source))
    }

  test("one two three and runtime-N parameter clauses remain ordered while zero parameters are parser-invalid"):
    val rows = List(
      1 -> "[A] =>> List[A]",
      2 -> "[A, B] =>> Either[A, B]",
      3 -> "[A, B, C] =>> Either[A, C]",
      7 -> "[A, B, C, X, Y, Z, F] =>> Either[A, F]"
    )

    rows.foreach { (count, source) =>
      val projected = candidateProject(parseType(source)).toOption.get
      val lambda = projected.model.asInstanceOf[CandidateType.Lambda]
      assertEquals(lambda.parameterCount, count, clues(source))
      assertEquals(projected.provenance.lambdaParameterNames.head.size, count, clues(source))
      assertEquals(candidateProject(candidateAuthor(projected).toOption.get).map(_.model), Right(projected.model))
    }
    assert(parseTypeResult("[] =>> Int").isLeft)

  test("Type aliases host the same source Type-lambda node without adding Definition semantics"):
    val alias = parseStat("type Mapper = [A] =>> List[A]").asInstanceOf[Defn.Type]
    val body = alias.body.asInstanceOf[Type.Lambda]

    assertEquals(body.productPrefix, "Type.Lambda")
    assertEquals(body.tparams.map(_.name.value), List("A"))
    assertEquals(
      candidateProject(body).map(_.model),
      Right(CandidateType.Lambda(1, CandidateType.Apply("List", List(CandidateType.Bound(0, 0)))))
    )

  test("parser failures candidate unsupported shapes and public rejection remain distinct"):
    assert(parseTypeResult(null).isLeft)
    assert(parseTypeResult("[A] =>>").isLeft)
    assert(parseTypeResult("[A = Int] =>> List[A]").isLeft)
    assertEquals(candidateProject(null).left.toOption.map(_.code), Some("N077_CANDIDATE_TYPE_MISSING"))

    val bounded = parseType("[A <: AnyVal] =>> List[A]")
    assertEquals(candidateProject(bounded).left.toOption.map(_.code), Some("N077_CANDIDATE_PARAMETER_UNSUPPORTED"))
    val duplicate = parseType("[A, A] =>> A")
    assertEquals(candidateProject(duplicate).left.toOption.map(_.code), Some("N077_CANDIDATE_DUPLICATE_PARAMETER"))

    val unsupported = List(
      "[A] =>> pkg.Box[A]",
      "[A] =>> A { type Out = Int }",
      "[A] =>> A match { case Int => String }",
      "[A] =>> A | String",
      "[A] =>> A & AnyVal",
      "[A] =>> Vector[A]",
      "[F[_]] =>> F[Int]"
    )
    unsupported.foreach { source =>
      val parsed = parseType(source)
      assert(candidateProject(parsed).isLeft, clues(source))
      assert(ScalametaTypeNormalFormProjection.project(parsed).isLeft, clues(source))
    }

  test("fresh malformed candidate values fail closed without inventing binder or constructor meaning"):
    val emptyLambda = CandidateProjection(
      CandidateType.Lambda(0, CandidateType.Atom("Int")),
      SourceProvenance(Vector(List.empty), Nil, None)
    )
    val missingClause = CandidateProjection(
      CandidateType.Lambda(1, CandidateType.Bound(0, 0)),
      SourceProvenance(Vector.empty, Nil, None)
    )
    val wrongClauseSize = CandidateProjection(
      CandidateType.Lambda(2, CandidateType.Bound(0, 0)),
      SourceProvenance(Vector(List("A")), Nil, None)
    )
    val unknownBound = CandidateProjection(
      CandidateType.Lambda(1, CandidateType.Bound(1, 0)),
      SourceProvenance(Vector(List("A")), Nil, None)
    )
    val capturedFree = CandidateProjection(
      CandidateType.Lambda(1, CandidateType.Free("A")),
      SourceProvenance(Vector(List("A")), Nil, None)
    )
    val unsupportedConstructor = CandidateProjection(
      CandidateType.Apply("Vector", List(CandidateType.Atom("Int"))),
      SourceProvenance(Vector.empty, Nil, None)
    )

    assertEquals(candidateAuthor(null).left.toOption.map(_.code), Some("N077_CANDIDATE_PROJECTION_MISSING"))
    assertEquals(candidateAuthor(emptyLambda).left.toOption.map(_.code), Some("N077_CANDIDATE_EMPTY_PARAMETER_CLAUSE"))
    assertEquals(candidateAuthor(missingClause).left.toOption.map(_.code), Some("N077_CANDIDATE_PROVENANCE_MISSING"))
    assertEquals(candidateAuthor(wrongClauseSize).left.toOption.map(_.code), Some("N077_CANDIDATE_PROVENANCE_MISMATCH"))
    assertEquals(candidateAuthor(unknownBound).left.toOption.map(_.code), Some("N077_CANDIDATE_BOUND_REFERENCE_UNRESOLVED"))
    assertEquals(candidateAuthor(capturedFree).left.toOption.map(_.code), Some("N077_CANDIDATE_FREE_NAME_CAPTURE"))
    assertEquals(candidateAuthor(unsupportedConstructor).left.toOption.map(_.code), Some("N077_CANDIDATE_CONSTRUCTOR_UNSUPPORTED"))

  private def parseType(source: String): Type = Input.String(source).parse[Type].get

  private def parseTypeResult(source: String): Either[String, Type] =
    Option(source).toRight("N077_SOURCE_MISSING").flatMap { present =>
      Input.String(present).parse[Type] match
        case Parsed.Success(sourceType) => Right(sourceType)
        case error: Parsed.Error => Left(error.message)
    }

  private def parseStat(source: String): Stat = Input.String(source).parse[Stat].get

  private def eraseReference(
      candidate: CandidateType,
      boundDisplayName: String
  ): Either[String, TypeNormalForm] =
    candidate match
      case CandidateType.Free(name) => Right(STypeIdent(name))
      case CandidateType.Bound(_, _) => Right(STypeIdent(boundDisplayName))
      case other => Left(s"N077_ERASURE_REFERENCE_UNSUPPORTED:${other.productPrefix}")

  private def candidateAuthor(projection: CandidateProjection): Either[CandidateError, Type] =
    Option(projection)
      .toRight(CandidateError("N077_CANDIDATE_PROJECTION_MISSING", "the candidate projection must be present."))
      .flatMap { present =>
        for
          model <- Option(present.model).toRight(
            CandidateError("N077_CANDIDATE_MODEL_MISSING", "the candidate model must be present.")
          )
          provenance <- Option(present.provenance).toRight(
            CandidateError("N077_CANDIDATE_PROVENANCE_MISSING", "source provenance must be present.")
          )
          clauses <- Option(provenance.lambdaParameterNames).toRight(
            CandidateError("N077_CANDIDATE_PROVENANCE_MISSING", "lambda-clause provenance must be present.")
          )
          authored <- authorType(model, Nil, clauses.toList)
          _ <- Either.cond(
            authored._2.isEmpty,
            (),
            CandidateError(
              "N077_CANDIDATE_PROVENANCE_MISMATCH",
              "unused lambda-clause provenance remains after authoring."
            )
          )
        yield authored._1
      }

  private def authorType(
      candidate: CandidateType,
      scopes: List[List[String]],
      clauses: List[List[String]]
  ): Either[CandidateError, (Type, List[List[String]])] =
    Option(candidate)
      .toRight(CandidateError("N077_CANDIDATE_MODEL_MISSING", "the candidate model must be present."))
      .flatMap {
        case CandidateType.Atom(name) =>
          if atomNames(name) then Right(Type.Name(name) -> clauses)
          else
            Left(
              CandidateError(
                "N077_CANDIDATE_ATOM_UNSUPPORTED",
                s"unsupported candidate atom `${Option(name).getOrElse("<missing>")}`."
              )
            )
        case CandidateType.Free(name) =>
          if !freeNames(name) then
            Left(
              CandidateError(
                "N077_CANDIDATE_NAME_UNSUPPORTED",
                s"unsupported free candidate name `${Option(name).getOrElse("<missing>")}`."
              )
            )
          else if scopes.exists(_.contains(name)) then
            Left(
              CandidateError(
                "N077_CANDIDATE_FREE_NAME_CAPTURE",
                s"free Type name `$name` would be captured by an authored lambda parameter."
              )
            )
          else Right(Type.Name(name) -> clauses)
        case CandidateType.Bound(depth, parameterIndex) =>
          scopes.lift(depth).flatMap(_.lift(parameterIndex)) match
            case Some(name) => Right(Type.Name(name) -> clauses)
            case None =>
              Left(
                CandidateError(
                  "N077_CANDIDATE_BOUND_REFERENCE_UNRESOLVED",
                  s"no authored Type parameter exists at lexical depth $depth and index $parameterIndex."
                )
              )
        case CandidateType.Apply(constructor, arguments) =>
          Option(arguments)
            .toRight(CandidateError("N077_CANDIDATE_LIST_MISSING", "application arguments must be present."))
            .flatMap { presentArguments =>
              fixedConstructors.get(constructor) match
                case Some(_) if boundPosition(constructor, scopes).nonEmpty =>
                  val (depth, index) = boundPosition(constructor, scopes).get
                  Left(
                    CandidateError(
                      "N077_CANDIDATE_CONSTRUCTOR_SHADOWED",
                      s"fixed constructor `$constructor` is shadowed at lexical depth $depth and parameter index $index."
                    )
                  )
                case Some(expectedArity) if presentArguments.size == expectedArity =>
                  authorTypes(presentArguments, scopes, clauses).map { (authoredArguments, remaining) =>
                    Type.Apply(Type.Name(constructor), authoredArguments) -> remaining
                  }
                case Some(expectedArity) =>
                  Left(
                    CandidateError(
                      "N077_CANDIDATE_CONSTRUCTOR_ARITY_UNSUPPORTED",
                      s"`$constructor` expects $expectedArity arguments, found ${presentArguments.size}."
                    )
                  )
                case None =>
                  Left(
                    CandidateError(
                      "N077_CANDIDATE_CONSTRUCTOR_UNSUPPORTED",
                      s"unsupported candidate constructor `${Option(constructor).getOrElse("<missing>")}`."
                    )
                  )
            }
        case CandidateType.Lambda(parameterCount, body) =>
          if parameterCount <= 0 then
            Left(
              CandidateError(
                "N077_CANDIDATE_EMPTY_PARAMETER_CLAUSE",
                "a candidate Type lambda needs at least one parameter."
              )
            )
          else
            clauses match
              case Nil =>
                Left(
                  CandidateError(
                    "N077_CANDIDATE_PROVENANCE_MISSING",
                    "lambda parameter-name provenance is missing."
                  )
                )
              case names :: remainingClauses =>
                Option(names)
                  .toRight(
                    CandidateError(
                      "N077_CANDIDATE_PROVENANCE_MISSING",
                      "lambda parameter-name provenance is missing."
                    )
                  )
                  .flatMap { presentNames =>
                    for
                      _ <- Either.cond(
                        presentNames.size == parameterCount,
                        (),
                        CandidateError(
                          "N077_CANDIDATE_PROVENANCE_MISMATCH",
                          s"expected $parameterCount lambda names, found ${presentNames.size}."
                        )
                      )
                      _ <- Either.cond(
                        presentNames.distinct.size == presentNames.size && presentNames.forall(validBinderName),
                        (),
                        CandidateError(
                          "N077_CANDIDATE_PROVENANCE_MISMATCH",
                          "lambda names must be distinct nonempty source identifiers."
                        )
                      )
                      authoredBody <- authorType(body, presentNames :: scopes, remainingClauses)
                    yield
                      Type.Lambda(presentNames.map(simpleParameter), authoredBody._1) -> authoredBody._2
                  }
      }

  private def authorTypes(
      candidates: List[CandidateType],
      scopes: List[List[String]],
      clauses: List[List[String]]
  ): Either[CandidateError, (List[Type], List[List[String]])] =
    candidates.foldLeft(
      Right((List.empty[Type], clauses)): Either[CandidateError, (List[Type], List[List[String]])]
    ) { (accumulated, candidate) =>
      for
        state <- accumulated
        authored <- authorType(candidate, scopes, state._2)
      yield (state._1 :+ authored._1, authored._2)
    }

  private def simpleParameter(name: String): Type.Param =
    Type.Param(Nil, Type.Name(name), Type.ParamClause(Nil), Type.Bounds.empty)

  private def validBinderName(name: String): Boolean =
    Option(name).exists(value => value.nonEmpty && value.head.isUpper && value.tail.forall(_.isLetterOrDigit))

  private def allTrees(root: Tree): List[Tree] = root :: root.children.flatMap(allTrees)

  private def candidateProject(sourceType: Type): Either[CandidateError, CandidateProjection] =
    Option(sourceType)
      .toRight(CandidateError("N077_CANDIDATE_TYPE_MISSING", "the source Type must be present."))
      .flatMap { present =>
        projectType(present, Nil).map { (model, clauses) =>
          val span = Option.when(present.pos != Position.None)((present.pos.start, present.pos.end))
          CandidateProjection(
            model,
            SourceProvenance(clauses, present.tokens.map(_.text).toList, span)
          )
        }
      }

  private def projectType(
      sourceType: Type,
      scopes: List[List[String]]
  ): Either[CandidateError, (CandidateType, Vector[List[String]])] =
    Option(sourceType)
      .toRight(CandidateError("N077_CANDIDATE_TYPE_MISSING", "the source Type must be present."))
      .flatMap {
        case name: Type.Name =>
          boundPosition(name.value, scopes) match
            case Some((depth, index)) => Right(CandidateType.Bound(depth, index) -> Vector.empty)
            case None if atomNames(name.value) => Right(CandidateType.Atom(name.value) -> Vector.empty)
            case None if freeNames(name.value) => Right(CandidateType.Free(name.value) -> Vector.empty)
            case None =>
              Left(
                CandidateError(
                  "N077_CANDIDATE_NAME_UNSUPPORTED",
                  s"unsupported candidate Type name `${name.value}`."
                )
              )
        case applied: Type.Apply =>
          applied.tpe match
            case constructor: Type.Name =>
              fixedConstructors.get(constructor.value) match
                case Some(_) if boundPosition(constructor.value, scopes).nonEmpty =>
                  val (depth, index) = boundPosition(constructor.value, scopes).get
                  Left(
                    CandidateError(
                      "N077_CANDIDATE_CONSTRUCTOR_SHADOWED",
                      s"fixed constructor `${constructor.value}` is shadowed at lexical depth $depth and parameter index $index."
                    )
                  )
                case Some(expectedArity) if applied.args.size == expectedArity =>
                  traverse(applied.args)(projectType(_, scopes)).map { projected =>
                    val (arguments, clauses) = projected.unzip
                    CandidateType.Apply(constructor.value, arguments) -> clauses.flatten.toVector
                  }
                case Some(expectedArity) =>
                  Left(
                    CandidateError(
                      "N077_CANDIDATE_CONSTRUCTOR_ARITY_UNSUPPORTED",
                      s"`${constructor.value}` expects $expectedArity arguments, found ${applied.args.size}."
                    )
                  )
                case None =>
                  Left(
                    CandidateError(
                      "N077_CANDIDATE_CONSTRUCTOR_UNSUPPORTED",
                      s"unsupported candidate constructor `${constructor.value}`."
                    )
                  )
            case other =>
              Left(
                CandidateError(
                  "N077_CANDIDATE_CONSTRUCTOR_UNSUPPORTED",
                  s"unsupported candidate constructor node `${other.productPrefix}`."
                )
              )
        case lambda: Type.Lambda =>
          val names = lambda.tparams.map(_.name.value)
          for
            _ <- Either.cond(
              names.nonEmpty,
              (),
              CandidateError("N077_CANDIDATE_EMPTY_PARAMETER_CLAUSE", "a candidate Type lambda needs at least one parameter.")
            )
            _ <- Either.cond(
              names.distinct.size == names.size,
              (),
              CandidateError("N077_CANDIDATE_DUPLICATE_PARAMETER", "a candidate Type-lambda clause needs distinct names.")
            )
            _ <- traverse(lambda.tparams)(validateSimpleParameter).map(_ => ())
            projectedBody <- projectType(lambda.tpe, names :: scopes)
          yield CandidateType.Lambda(names.size, projectedBody._1) -> (Vector(names) ++ projectedBody._2)
        case other =>
          Left(
            CandidateError(
              "N077_CANDIDATE_STRUCTURE_UNSUPPORTED",
              s"unsupported candidate Type node `${other.productPrefix}`."
            )
          )
      }

  private def validateSimpleParameter(parameter: Type.Param): Either[CandidateError, Unit] =
    Either.cond(
      parameter.mods.isEmpty &&
        parameter.tparams.isEmpty &&
        parameter.bounds.lo.isEmpty &&
        parameter.bounds.hi.isEmpty &&
        parameter.bounds.context.isEmpty &&
        parameter.bounds.view.isEmpty,
      (),
      CandidateError(
        "N077_CANDIDATE_PARAMETER_UNSUPPORTED",
        s"candidate parameter `${parameter.name.value}` must be an unmodified, unbounded first-order Type parameter."
      )
    )

  private def boundPosition(name: String, scopes: List[List[String]]): Option[(Int, Int)] =
    scopes.zipWithIndex.collectFirst {
      case (scope, depth) if scope.contains(name) => depth -> scope.indexOf(name)
    }

  private def traverse[A, B](
      values: List[A]
  )(convert: A => Either[CandidateError, B]): Either[CandidateError, List[B]] =
    Option(values)
      .toRight(CandidateError("N077_CANDIDATE_LIST_MISSING", "the recursive list must be present."))
      .flatMap(
        _.foldRight(Right(Nil): Either[CandidateError, List[B]]) { (value, accumulated) =>
          for
            head <- convert(value)
            tail <- accumulated
          yield head :: tail
        }
      )
