package quasiquotes.hybrid

import scala.meta.dialects
import scala.compiletime.testing.typeCheckErrors
import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.matching.DefinitionPattern

class Q006ScalametaDefinitionFeasibilityTest extends munit.FunSuite:
  test("Scalameta projects the exact ordinary single-parameter Definition slice"):
    val projection = Q006DefinitionFrontendProbe
      .project("def id(x: Int): Int = x")
      .toOption
      .get

    assertEquals(projection.methodName, "id")
    assertEquals(projection.parameterName, "x")
    assertEquals(projection.parameterTypeSource, "Int")
    assertEquals(projection.resultTypeSource, "Int")
    assertEquals(projection.bodyParameterName, "x")

  test("the admitted Definition AST is stable across the supported dialect policy"):
    val source = "def id(x: List[Int]): List[Int] = x"
    val projections = List(
      Q006DefinitionFrontendProbe.project(source, dialects.Scala3),
      Q006DefinitionFrontendProbe.project(source, dialects.Scala38)
    )

    assertEquals(projections.distinct.size, 1)
    assertEquals(
      projections.head.map(value => value.parameterTypeSource -> value.resultTypeSource),
      Right("List[Int]" -> "List[Int]")
    )

  test("projection rejects every representative topology outside the current bounded slice"):
    val rejected = List(
      "val id: Int = 1",
      "def id = 1",
      "def id(x: Int) = x",
      "def id[A](x: A): A = x",
      "def id(x: Int, y: Int): Int = x",
      "def id(x: Int)(y: Int): Int = x",
      "def id(using x: Int): Int = x",
      "def id(x: Int): Int = x + 1",
      "def id(x: Int): Int = other",
      "def `id`(x: Int): Int = x",
      "def id(`x`: Int): Int = `x`",
      "def id(/*comment*/ x: Int): Int = x"
    )

    rejected.foreach(source =>
      assert(Q006DefinitionFrontendProbe.project(source).isLeft, source)
    )

  test("Definition Type fields reuse the existing Scalameta Type semantics"):
    val supported = List(
      "def id(x: Int): Int = x" -> "STypeIdent(Int)",
      "def id(x: List[Int]): List[Int] = x" ->
        "STypeApply(STypeIdent(List), [STypeIdent(Int)])",
      "def id(x: (Int, String)): (Int, String) = x" ->
        "STypeTuple([STypeIdent(Int), STypeIdent(String)])",
      "def id(x: Int => String): Int => String = x" ->
        "STypeFunction([STypeIdent(Int)], STypeIdent(String))"
    )

    supported.foreach { case (source, expected) =>
      val result = Q006DefinitionFrontendProbe
        .project(source)
        .flatMap(Q006DefinitionFrontendProbe.semanticTypes)
      assertEquals(result, Right(expected -> expected), source)
    }

  test("body-pattern assembly uses a collision-free complete-body sentinel"):
    val ordinary = Q006DefinitionFrontendProbe.projectBodyPattern(
      Seq("def id(x: Int): Int = ", "")
    )
    val collision = Q006DefinitionFrontendProbe.projectBodyPattern(
      Seq("def __qq_q006_body_0(x: Int): Int = ", "")
    )

    assertEquals(ordinary.map(_.captureName), Right("__qq_q006_body_0"))
    assertEquals(collision.map(_.captureName), Right("__qq_q006_body_1"))
    assertEquals(ordinary.map(_.methodName), Right("id"))

  test("construction assembly places two collision-free Type sentinels only in declared-Type fields"):
    val ordinary = Q006DefinitionFrontendProbe.projectConstruction(
      Seq("def id(x: ", "): ", " = x")
    )
    val collision = Q006DefinitionFrontendProbe.projectConstruction(
      Seq("def __qq_q006_type_0__qq_q006_type_1(x: ", "): ", " = x")
    )

    assertEquals(
      ordinary.map(value => value.parameterTypeSource -> value.resultTypeSource),
      Right("__qq_q006_type_0" -> "__qq_q006_type_1")
    )
    assertEquals(
      collision.map(value => value.parameterTypeSource -> value.resultTypeSource),
      Right("__qq_q006_type_2" -> "__qq_q006_type_3")
    )
    assertEquals(
      Q006DefinitionFrontendProbe.projectConstruction(Seq("def id(x: ", ") = x")).left.toOption,
      Some("TYPE_SPLICE_ARITY_UNSUPPORTED")
    )

  test("the current typed lowerer and matcher preserve owner, binder, and body identity"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val callerType = TypeRepr.of[Int]
      val definition = _root_.quasiquotes.construct.Quasiquotes.dqr(
        StringContext("def id(x: ", "): ", " = x")
      )(using q)(callerType, callerType)
      val parameter = definition.paramss.head.asInstanceOf[TermParamClause].params.head
      val body = definition.rhs.get
      val captured = DefinitionPattern
        .singleParameter("def id(x: Int): Int = $body")
        .toOption
        .flatMap(_.unapply(using q)(definition))
        .get
      val typedCaptured: q.reflect.Term = captured
      val mismatch = DefinitionPattern
        .singleParameter("def other(x: Int): Int = $body")
        .toOption
        .flatMap(_.unapply(using q)(definition))
      val linkedReference = body match
        case reference: Ref => reference.symbol == parameter.symbol
        case _ => false

      (
        definition.name,
        parameter.name,
        parameter.tpt.tpe =:= callerType,
        definition.returnTpt.tpe =:= callerType,
        parameter.tpt.tpe.asInstanceOf[AnyRef] eq callerType.asInstanceOf[AnyRef],
        definition.returnTpt.tpe.asInstanceOf[AnyRef] eq callerType.asInstanceOf[AnyRef],
        definition.symbol.owner == Symbol.spliceOwner,
        parameter.symbol.owner == definition.symbol,
        linkedReference,
        typedCaptured.asInstanceOf[AnyRef] eq body.asInstanceOf[AnyRef],
        mismatch.isEmpty
      )

    assertEquals(
      evidence,
      ("id", "x", true, true, true, true, true, true, true, true, true)
    )

  test("malformed templates, unequal Types, and Definition rank syntax fail closed"):
    val malformed = Q006DefinitionFrontendProbe.project("def id(")
    val unequal = Q006DefinitionFrontendProbe
      .project("def id(x: Int): String = x")
      .flatMap(Q006DefinitionFrontendProbe.semanticTypes)

    val rankMessages = typeCheckErrors(
      """import scala.quoted.*
        import quasiquotes.matching.DefinitionPattern.*
        def attempt(using q: Quotes)(definition: q.reflect.DefDef) = definition match
          case dqq"def id(x: Int): Int = ..$body" => ()
          case _ => ()
      """
    ).map(_.message)

    assert(malformed.left.exists(_.startsWith("SCALAMETA_PARSE_FAILURE:")), malformed)
    assertEquals(unequal.left.toOption, Some("PARAMETER_RESULT_TYPE_MISMATCH"))
    assert(
      rankMessages.exists(_.contains("rank-2 captures are not supported for Definition patterns")),
      rankMessages.mkString(" | ")
    )
