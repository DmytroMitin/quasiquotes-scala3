package quasiquotes.neutral

import _root_.quasiquotes.parser.TermShape

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

/** Test-only N074 model-fit oracle. It is not production admission policy. */
@nowarn("cat=deprecation")
final class N074NamedArgumentTermShapeModelFitTest extends munit.FunSuite:
  private val free = TermShape.Identifier("f", isPlaceholder = false)
  private val one = TermShape.Literal("1")

  test("Scala 3 syntax carries named labels RHS values order tokens and argument-position context"):
    val direct = parsed("f(x = 1, y = g(2))").asInstanceOf[Term.Apply]
    val directAssignments = direct.argClause.values.map(_.asInstanceOf[Term.Assign])

    assertEquals(direct.argClause.mod, None)
    assertEquals(directAssignments.map(assignmentLabel), List("x", "y"))
    assertEquals(directAssignments.map(_.rhs.syntax), List("1", "g(2)"))
    assertEquals(directAssignments.map(_.tokens.map(_.text).mkString), List("x = 1", "y = g(2)"))
    assert(directAssignments(1).rhs.isInstanceOf[Term.Apply])

    val reordered = parsed("f(y = 2, x = 1)").asInstanceOf[Term.Apply]
    assertEquals(
      reordered.argClause.values.map(value => assignmentLabel(value.asInstanceOf[Term.Assign])),
      List("y", "x")
    )

    val duplicate = parsed("f(x = 1, x = 2)").asInstanceOf[Term.Apply]
    assertEquals(
      duplicate.argClause.values.map(value => assignmentLabel(value.asInstanceOf[Term.Assign])),
      List("x", "x")
    )

    val fresh = parsed("new Box(x = 1)").asInstanceOf[Term.New]
    val freshAssignment = fresh.init.argClauses.head.values.head.asInstanceOf[Term.Assign]
    assertEquals(assignmentLabel(freshAssignment), "x")
    assertEquals(freshAssignment.rhs.syntax, "1")

    val nested = parsed("outer(f(x = 1))").asInstanceOf[Term.Apply]
    val inner = nested.argClause.values.head.asInstanceOf[Term.Apply]
    assert(inner.argClause.values.head.isInstanceOf[Term.Assign])

    val standalone = parsed("x = 1")
    assert(standalone.isInstanceOf[Term.Assign])
    assertEquals(standalone.productPrefix, directAssignments.head.productPrefix)
    assertEquals(directAssignments.map(_.pos.start), List(2, 9))

  test("current public projection retains exact named rejection locks and positional success"):
    val applyDetail = "ordinary Apply arguments must be positional Terms, found Term.Assign."
    val newDetail = "constructor-new arguments must be positional Terms, found Term.Assign."

    List("f(x = 1)", "f(0)(x = 1)", "f(x = 1)(0)").foreach { source =>
      assertPublicFailure(parsed(source), "NEUTRAL_APPLY_ARGUMENT_UNSUPPORTED", applyDetail)
    }
    assertPublicFailure(
      parsed("new Box(x = 1)"),
      "NEUTRAL_NEW_ARGUMENT_UNSUPPORTED",
      newDetail
    )

    assertEquals(publicShape(parsed("f(1)")), TermShape.Apply(free, List(one)))
    assertEquals(
      publicShape(parsed("f(1)(2)")),
      TermShape.Apply(TermShape.Apply(free, List(one)), List(TermShape.Literal("2")))
    )
    assertEquals(publicShape(parsed("new Box(1)")), TermShape.New("Box", List(one)))

  test("erasing labels into current TermShape values produces minimal semantic collisions"):
    val positionalApply = eraseToCurrentShape(parsed("f(1)"))
    val namedApply = eraseToCurrentShape(parsed("f(x = 1)"))
    val positionalNew = eraseToCurrentShape(parsed("new Box(1)"))
    val namedNew = eraseToCurrentShape(parsed("new Box(x = 1)"))

    assertEquals(namedApply, positionalApply)
    assertEquals(namedNew, positionalNew)
    assertNotEquals(projectCandidate(parsed("f(x = 1)")), projectCandidate(parsed("f(1)")))
    assertNotEquals(projectCandidate(parsed("new Box(x = 1)")), projectCandidate(parsed("new Box(1)")))

    val labelsXY = projectCandidate(parsed("f(x = 1, y = 1)"))
    val labelsYX = projectCandidate(parsed("f(y = 1, x = 1)"))
    assertEquals(
      eraseToCurrentShape(parsed("f(x = 1, y = 1)")),
      eraseToCurrentShape(parsed("f(y = 1, x = 1)"))
    )
    assertNotEquals(labelsXY, labelsYX)

  test("closed test-local candidate projects and authors fresh named Apply and New clauses"):
    val sources = List(
      "f(x = 1)",
      "f(x = 1, y = g(2))",
      "f(y = 2, x = 1)",
      "f(0)(x = 1)",
      "new Box(x = 1)",
      "new Box(x = g(2), y = 3)"
    )

    sources.foreach { source =>
      val projected = candidate(source)
      val authored = candidateAuthor(projected)
      assertEquals(projectCandidate(authored), Right(projected), clues(source, authored.structure))
      assert(allTrees(authored).forall(_.pos == Position.None), clues(source))
    }

    val reordered = candidate("f(y = 2, x = 1)")
    assertEquals(argumentLabels(reordered), List("y", "x"))
    val nested = candidate("f(0)(x = 1)").asInstanceOf[CandidateInvocation.Apply]
    assertEquals(nested.function, TermShape.Apply(free, List(TermShape.Literal("0"))))

  test("test-local oracle fails closed and separates parser errors from semantic policy"):
    assert(Input.String("f(x = )").parse[Term].isInstanceOf[Parsed.Error])
    assert(Input.String("f(1x = 2)").parse[Term].isInstanceOf[Parsed.Error])

    val projectionCases = List(
      (null: Term) -> "N074_CANDIDATE_MISSING_TERM",
      parsed("x = 1") -> "N074_CANDIDATE_INVOCATION_UNSUPPORTED",
      parsed("f(1, x = 2)") -> "N074_CANDIDATE_MIXED_ARGUMENT_ROLES",
      parsed("f(x = 1, x = 2)") -> "N074_CANDIDATE_DUPLICATE_LABEL",
      parsed("f(values*)") -> "N074_CANDIDATE_REPEATED_ARGUMENT_UNSUPPORTED",
      parsed("f(using 1)") -> "N074_CANDIDATE_CONTEXTUAL_CLAUSE_UNSUPPORTED",
      parsed("f[Int](x = 1)") -> "N074_CANDIDATE_TYPE_APPLICATION_UNSUPPORTED",
      parsed("new Box(x = 1)(y = 2)") -> "N074_CANDIDATE_NEW_CLAUSE_COUNT_UNSUPPORTED",
      parsed("new Box(using 1)") -> "N074_CANDIDATE_CONTEXTUAL_CLAUSE_UNSUPPORTED",
      parsed("new Box[Int](x = 1)") -> "N074_EXISTING_TERM_UNSUPPORTED",
      parsed("f(x = 1)(2)") -> "N074_EXISTING_TERM_UNSUPPORTED"
    )
    projectionCases.foreach { (term, expected) =>
      assertCandidateProjectionCode(term, expected)
    }

    val malformedLabel = Term.Apply(
      Term.Name("f"),
      Term.ArgClause(List(Term.Assign(Term.Name("bad-name"), Lit.Int(1))))
    )
    val selectedLabel = Term.Apply(
      Term.Name("f"),
      Term.ArgClause(List(Term.Assign(Term.Select(Term.Name("obj"), Term.Name("x")), Lit.Int(1))))
    )
    assertCandidateProjectionCode(malformedLabel, "N074_CANDIDATE_LABEL_UNSUPPORTED")
    assertCandidateProjectionCode(selectedLabel, "N074_CANDIDATE_LABEL_UNSUPPORTED")
    assertCandidateProjectionCode(parsed("f(`type` = 1)"), "N074_CANDIDATE_LABEL_UNSUPPORTED")

    val authoringCases = List(
      (null: CandidateInvocation) -> "N074_CANDIDATE_MISSING_INVOCATION",
      CandidateInvocation.Apply(free, null) -> "N074_CANDIDATE_MISSING_ARGUMENTS",
      CandidateInvocation.Apply(free, List(null)) -> "N074_CANDIDATE_MISSING_ARGUMENT",
      CandidateInvocation.Apply(free, List(CandidateArgument.Named(null, one))) ->
        "N074_CANDIDATE_LABEL_UNSUPPORTED",
      CandidateInvocation.Apply(free, List(CandidateArgument.Named("bad-name", one))) ->
        "N074_CANDIDATE_LABEL_UNSUPPORTED",
      CandidateInvocation.Apply(free, List(CandidateArgument.Named("if", one))) ->
        "N074_CANDIDATE_LABEL_UNSUPPORTED",
      CandidateInvocation.Apply(free, List(CandidateArgument.Named("x", null))) ->
        "N074_CANDIDATE_MISSING_VALUE",
      CandidateInvocation.Apply(
        free,
        List(CandidateArgument.Positional(one), CandidateArgument.Named("x", one))
      ) -> "N074_CANDIDATE_MIXED_ARGUMENT_ROLES",
      CandidateInvocation.Apply(
        free,
        List(CandidateArgument.Named("x", one), CandidateArgument.Named("x", one))
      ) -> "N074_CANDIDATE_DUPLICATE_LABEL"
    )
    authoringCases.foreach { (invocation, expected) =>
      assertEquals(authorCandidate(invocation).left.toOption.map(_.code), Some(expected))
    }

  test("candidate scales over 0 1 2 3 N slots without inventing parameter identity"):
    List(0, 1, 2, 3, 7).foreach { cardinality =>
      val terms = (0 until cardinality).toList.map { index =>
        Term.Assign(Term.Name(s"x$index"), Lit.Int(index)): Term
      }
      val source = Term.Apply(Term.Name("f"), Term.ArgClause(terms))
      val invocation = candidate(source).asInstanceOf[CandidateInvocation.Apply]

      assertEquals(invocation.arguments.size, cardinality)
      assertEquals(argumentLabels(invocation), (0 until cardinality).toList.map(index => s"x$index"))
      assertEquals(projectCandidate(candidateAuthor(invocation)), Right(invocation))
    }

    val sameSourceLabel = CandidateArgument.Named("x", one)
    val onF = CandidateInvocation.Apply(TermShape.Identifier("f", false), List(sameSourceLabel))
    val onG = CandidateInvocation.Apply(TermShape.Identifier("g", false), List(sameSourceLabel))
    assertEquals(
      onF.asInstanceOf[CandidateInvocation.Apply].arguments,
      onG.asInstanceOf[CandidateInvocation.Apply].arguments
    )
    assertNotEquals(onF, onG)

  private enum CandidateArgument derives CanEqual:
    case Positional(value: TermShape)
    case Named(label: String, value: TermShape)

  private enum CandidateInvocation derives CanEqual:
    case Apply(function: TermShape, arguments: List[CandidateArgument])
    case New(constructor: String, arguments: List[CandidateArgument])

  private final case class CandidateError(code: String, detail: String) derives CanEqual

  private val PlainLabel = "[A-Za-z_][A-Za-z0-9_]*".r
  private val Keywords = Set(
    "abstract", "as", "case", "catch", "class", "def", "derives", "do", "else", "end",
    "enum", "export", "extends", "extension", "false", "final", "finally", "for", "forSome",
    "given", "if", "implicit", "import", "infix", "inline", "lazy", "macro", "match", "new",
    "null", "object", "opaque", "open", "override", "package", "private", "protected", "return",
    "sealed", "super", "then", "this", "throw", "trait", "transparent", "true", "try", "type",
    "using", "val", "var", "while", "with", "yield"
  )

  private def projectCandidate(term: Term): Either[CandidateError, CandidateInvocation] =
    Option(term)
      .toRight(CandidateError("N074_CANDIDATE_MISSING_TERM", "the source Term must be present."))
      .flatMap {
        case application: Term.Apply => projectCandidateApply(application)
        case fresh: Term.New => projectCandidateNew(fresh)
        case other =>
          Left(
            CandidateError(
              "N074_CANDIDATE_INVOCATION_UNSUPPORTED",
              s"the test-local oracle admits only root Apply or New, found ${other.productPrefix}."
            )
          )
      }

  private def projectCandidateApply(application: Term.Apply): Either[CandidateError, CandidateInvocation] =
    for
      _ <- requireCandidate(
        application.argClause.mod.isEmpty,
        "N074_CANDIDATE_CONTEXTUAL_CLAUSE_UNSUPPORTED",
        "the test-local oracle admits one ordinary argument clause."
      )
      _ <- application.fun match
        case _: Term.ApplyType =>
          Left(
            CandidateError(
              "N074_CANDIDATE_TYPE_APPLICATION_UNSUPPORTED",
              "type-applied functions are outside the test-local oracle."
            )
          )
        case _ => Right(())
      raw <- Option(application.argClause.values)
        .toRight(CandidateError("N074_CANDIDATE_MISSING_ARGUMENTS", "argument values must be present."))
      stripped <- strippedValues(raw)
      shape <- projectExisting(Term.Apply(application.fun, Term.ArgClause(stripped)))
      result <- shape match
        case TermShape.Apply(function, values) =>
          classifyArguments(raw, values).map(CandidateInvocation.Apply(function, _))
        case other =>
          Left(CandidateError("N074_CANDIDATE_INTERNAL_SHAPE", s"expected Apply, found ${other.render}."))
    yield result

  private def projectCandidateNew(fresh: Term.New): Either[CandidateError, CandidateInvocation] =
    for
      clause <- fresh.init.argClauses.toList match
        case value :: Nil => Right(value)
        case other =>
          Left(
            CandidateError(
              "N074_CANDIDATE_NEW_CLAUSE_COUNT_UNSUPPORTED",
              s"the test-local New oracle requires exactly one clause, found ${other.size}."
            )
          )
      _ <- requireCandidate(
        clause.mod.isEmpty,
        "N074_CANDIDATE_CONTEXTUAL_CLAUSE_UNSUPPORTED",
        "the test-local oracle admits one ordinary argument clause."
      )
      raw <- Option(clause.values)
        .toRight(CandidateError("N074_CANDIDATE_MISSING_ARGUMENTS", "argument values must be present."))
      stripped <- strippedValues(raw)
      positional = fresh.copy(
        init = Init(fresh.init.tpe, fresh.init.name, List(Term.ArgClause(stripped)))
      )
      shape <- projectExisting(positional)
      result <- shape match
        case TermShape.New(constructor, values) =>
          classifyArguments(raw, values).map(CandidateInvocation.New(constructor, _))
        case other =>
          Left(CandidateError("N074_CANDIDATE_INTERNAL_SHAPE", s"expected New, found ${other.render}."))
    yield result

  private def strippedValues(arguments: List[Term]): Either[CandidateError, List[Term]] =
    traverse(arguments) {
      case null =>
        Left(CandidateError("N074_CANDIDATE_MISSING_VALUE", "argument values must be present."))
      case assignment: Term.Assign =>
        Option(assignment.rhs)
          .toRight(CandidateError("N074_CANDIDATE_MISSING_VALUE", "named RHS values must be present."))
      case _: Term.Repeated =>
        Left(
          CandidateError(
            "N074_CANDIDATE_REPEATED_ARGUMENT_UNSUPPORTED",
            "star/repeated arguments are outside the named-argument oracle."
          )
        )
      case positional => Right(positional)
    }

  private def classifyArguments(
      raw: List[Term],
      values: List[TermShape]
  ): Either[CandidateError, List[CandidateArgument]] =
    if raw.size != values.size then
      Left(CandidateError("N074_CANDIDATE_INTERNAL_SHAPE", "argument cardinality changed during projection."))
    else
      traverse(raw.zip(values)) {
        case (assignment: Term.Assign, value) =>
          assignment.lhs match
            case name: Term.Name =>
              val tokenText = name.tokens.toList match
                case Nil => None
                case tokens => Some(tokens.map(_.text).mkString)
              validateLabel(name.value, tokenText).map(CandidateArgument.Named(_, value))
            case _ =>
              Left(
                CandidateError(
                  "N074_CANDIDATE_LABEL_UNSUPPORTED",
                  "named argument labels must be direct plain Term.Name values."
                )
              )
        case (_, value) => Right(CandidateArgument.Positional(value))
      }.flatMap(validateArguments)

  private def validateArguments(
      arguments: List[CandidateArgument]
  ): Either[CandidateError, List[CandidateArgument]] =
    Option(arguments)
      .toRight(CandidateError("N074_CANDIDATE_MISSING_ARGUMENTS", "argument values must be present."))
      .flatMap { present =>
        if present.exists(_ == null) then
          Left(CandidateError("N074_CANDIDATE_MISSING_ARGUMENT", "each argument must be present."))
        else if present.exists(argumentValue(_) == null) then
          Left(CandidateError("N074_CANDIDATE_MISSING_VALUE", "each argument value must be present."))
        else
          traverse(present) {
            case named @ CandidateArgument.Named(label, _) =>
              validateLabel(label, None).map(_ => named)
            case positional => Right(positional)
          }.flatMap { validated =>
            val hasPositional = validated.exists(_.isInstanceOf[CandidateArgument.Positional])
            val labels = validated.collect { case CandidateArgument.Named(label, _) => label }
            if hasPositional && labels.nonEmpty then
              Left(
                CandidateError(
                  "N074_CANDIDATE_MIXED_ARGUMENT_ROLES",
                  "the closed test-local oracle does not select mixed positional/named policy."
                )
              )
            else if labels.distinct.size != labels.size then
              Left(
                CandidateError(
                  "N074_CANDIDATE_DUPLICATE_LABEL",
                  "the closed test-local oracle rejects duplicate source labels."
                )
              )
            else Right(validated)
          }
      }

  private def validateLabel(
      label: String,
      positionedTokenText: Option[String]
  ): Either[CandidateError, String] =
    Either.cond(
      Option(label).exists(value =>
        PlainLabel.matches(value) && !Keywords(value) && positionedTokenText.forall(_ == value)
      ),
      label,
      CandidateError(
        "N074_CANDIDATE_LABEL_UNSUPPORTED",
        "labels must be present unquoted plain non-keyword source names."
      )
    )

  private def authorCandidate(invocation: CandidateInvocation): Either[CandidateError, Term] =
    Option(invocation)
      .toRight(
        CandidateError("N074_CANDIDATE_MISSING_INVOCATION", "the candidate invocation must be present.")
      )
      .flatMap {
        case CandidateInvocation.Apply(function, arguments) =>
          for
            validated <- validateArguments(arguments)
            positional <- authorExisting(TermShape.Apply(function, validated.map(argumentValue)))
            application <- positional match
              case value: Term.Apply => Right(value)
              case other =>
                Left(
                  CandidateError(
                    "N074_CANDIDATE_INTERNAL_SHAPE",
                    s"expected authored Apply, found ${other.productPrefix}."
                  )
                )
            wrapped <- wrapArguments(validated, application.argClause.values)
          yield Term.Apply(application.fun, Term.ArgClause(wrapped))
        case CandidateInvocation.New(constructor, arguments) =>
          for
            validated <- validateArguments(arguments)
            positional <- authorExisting(TermShape.New(constructor, validated.map(argumentValue)))
            fresh <- positional match
              case value: Term.New => Right(value)
              case other =>
                Left(
                  CandidateError(
                    "N074_CANDIDATE_INTERNAL_SHAPE",
                    s"expected authored New, found ${other.productPrefix}."
                  )
                )
            wrapped <- wrapArguments(validated, fresh.init.argClauses.head.values)
          yield fresh.copy(
            init = Init(fresh.init.tpe, fresh.init.name, List(Term.ArgClause(wrapped)))
          )
      }

  private def wrapArguments(
      arguments: List[CandidateArgument],
      values: List[Term]
  ): Either[CandidateError, List[Term]] =
    if arguments.size != values.size then
      Left(CandidateError("N074_CANDIDATE_INTERNAL_SHAPE", "authored argument cardinality changed."))
    else
      Right(
        arguments.zip(values).map {
          case (CandidateArgument.Positional(_), value) => value
          case (CandidateArgument.Named(label, _), value) => Term.Assign(Term.Name(label), value)
        }
      )

  private def argumentValue(argument: CandidateArgument): TermShape =
    argument match
      case CandidateArgument.Positional(value) => value
      case CandidateArgument.Named(_, value) => value

  private def projectExisting(term: Term): Either[CandidateError, TermShape] =
    ScalametaTermProjection.project(term)
      .map(_.shape)
      .left
      .map(error => CandidateError("N074_EXISTING_TERM_UNSUPPORTED", error.message))

  private def authorExisting(shape: TermShape): Either[CandidateError, Term] =
    ScalametaTermShapeAuthoring.author(shape)
      .left
      .map(error => CandidateError("N074_EXISTING_AUTHORING_UNSUPPORTED", error.message))

  private def eraseToCurrentShape(term: Term): TermShape =
    val stripped = term match
      case application: Term.Apply =>
        val values = strippedValues(application.argClause.values).toOption.get
        Term.Apply(application.fun, Term.ArgClause(values))
      case fresh: Term.New =>
        val values = strippedValues(fresh.init.argClauses.head.values).toOption.get
        fresh.copy(init = Init(fresh.init.tpe, fresh.init.name, List(Term.ArgClause(values))))
      case other => fail(s"expected Apply/New collision fixture, found ${other.productPrefix}")
    publicShape(stripped)

  private def candidate(source: String): CandidateInvocation = candidate(parsed(source))

  private def candidate(term: Term): CandidateInvocation =
    projectCandidate(term) match
      case Right(value) => value
      case Left(error) => fail(s"${error.code}: ${error.detail}")

  private def candidateAuthor(invocation: CandidateInvocation): Term =
    authorCandidate(invocation) match
      case Right(value) => value
      case Left(error) => fail(s"${error.code}: ${error.detail}")

  private def publicShape(term: Term): TermShape =
    ScalametaTermProjection.project(term) match
      case Right(value) => value.shape
      case Left(error) => fail(error.message)

  private def assertPublicFailure(term: Term, expectedCode: String, expectedDetail: String): Unit =
    assertEquals(
      ScalametaTermProjection.project(term),
      Left(NeutralProjectionError(expectedCode, expectedDetail))
    )

  private def assertCandidateProjectionCode(term: Term, expected: String): Unit =
    assertEquals(projectCandidate(term).left.toOption.map(_.code), Some(expected), clues(term))

  private def assignmentLabel(assignment: Term.Assign): String =
    assignment.lhs match
      case name: Term.Name => name.value
      case other => fail(s"expected direct label name, found ${other.productPrefix}")

  private def argumentLabels(invocation: CandidateInvocation): List[String] =
    val arguments = invocation match
      case CandidateInvocation.Apply(_, values) => values
      case CandidateInvocation.New(_, values) => values
    arguments.collect { case CandidateArgument.Named(label, _) => label }

  private def parsed(source: String): Term =
    Input.String(source).parse[Term].get

  private def requireCandidate(
      condition: Boolean,
      code: String,
      detail: String
  ): Either[CandidateError, Unit] =
    Either.cond(condition, (), CandidateError(code, detail))

  private def traverse[A, B](
      values: List[A]
  )(convert: A => Either[CandidateError, B]): Either[CandidateError, List[B]] =
    Option(values)
      .toRight(CandidateError("N074_CANDIDATE_MISSING_ARGUMENTS", "argument values must be present."))
      .flatMap(
        _.foldRight(Right(Nil): Either[CandidateError, List[B]]) { (value, accumulated) =>
          for
            head <- convert(value)
            tail <- accumulated
          yield head :: tail
        }
      )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
