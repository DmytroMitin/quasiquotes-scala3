package quasiquotes.neutral

import _root_.quasiquotes.parser.{BinderId, TermShape}
import _root_.quasiquotes.terms.ConstructedTerm

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaLambda1ProjectionTest extends munit.FunSuite:
  test("Scalameta structurally distinguishes ordinary and context Lambda1 nodes"):
    val ordinary = parsed("(x: Int) => x")
    val contextual = parsed("(x: Int) ?=> x")

    ordinary match
      case function: Term.Function =>
        assertEquals(function.paramClause.values.size, 1)
        function.paramClause.values.head.decltpe match
          case Some(name: Type.Name) => assertEquals(name.value, "Int")
          case other => fail(s"expected direct Int type name, got $other")
        assertEquals(function.paramClause.values.head.mods, Nil)
      case other => fail(s"expected Term.Function, got ${other.productPrefix}")

    assert(contextual.isInstanceOf[Term.ContextFunction])
    assert(!contextual.isInstanceOf[Term.Function])

  test("projects one explicitly typed Lambda1 with bound identity distinct from spelling"):
    val identity = project(parsed("(x: Int) => x")).shape
    val application = project(parsed("(x: Int) => f(x)")).shape

    identity match
      case TermShape.Lambda1(binderId, "x", "Int", TermShape.BoundReference(referenceId, "x")) =>
        assertEquals(binderId, BinderId(0))
        assertEquals(referenceId, binderId)
      case other => fail(s"unexpected identity shape: ${other.render}")

    assertEquals(
      application,
      TermShape.Lambda1(
        BinderId(0),
        "x",
        "Int",
        TermShape.Apply(
          TermShape.Identifier("f", false),
          List(TermShape.BoundReference(BinderId(0), "x"))
        )
      )
    )

  test("propagates Lambda1 scope through every admitted recursive N003 family"):
    val fixtures = List(
      "(x: Int) => x + free" ->
        "Lambda1(x: Int, Infix(BoundRef(x), +, Ident(free)))",
      "(x: Int) => (x, free)" ->
        "Lambda1(x: Int, Tuple([BoundRef(x), Ident(free)]))",
      "(x: Int) => if flag then x else free" ->
        "Lambda1(x: Int, If(Ident(flag), BoundRef(x), Ident(free)))",
      "(x: Int) => !pred(x)" ->
        "Lambda1(x: Int, Unary(!, Apply(Ident(pred), [BoundRef(x)])))",
      "(x: Int) => x.toString" ->
        "Lambda1(x: Int, Select(BoundRef(x), toString))"
    )

    fixtures.foreach { (source, expected) =>
      assertEquals(project(parsed(source)).shape.render, expected, clues(source))
    }

  test("uses the existing Core construction path for alpha equivalence"):
    val left = ConstructedTerm.fromShape(project(parsed("(x: Int) => x + 1")).shape)
      .fold(error => fail(error.message), identity)
    val right = ConstructedTerm.fromShape(project(parsed("(renamed: Int) => renamed + 1")).shape)
      .fold(error => fail(error.message), identity)

    assertEquals(left, right)
    assertEquals(project(Term.Name("x")).shape, TermShape.Identifier("x", false))

  test("preserves the established concrete Lambda1 parameter type normalization"):
    val fixtures = List(
      "(x: Int) => x" -> "Int",
      "(x: scala.Int) => x" -> "Int",
      "(x: String) => x" -> "String",
      "(x: scala.String) => x" -> "String",
      "(x: java.lang.String) => x" -> "String",
      "(x: Boolean) => x" -> "Boolean",
      "(x: scala.Boolean) => x" -> "Boolean"
    )

    fixtures.foreach { (source, expectedType) =>
      project(parsed(source)).shape match
        case TermShape.Lambda1(_, _, parameterType, _) =>
          assertEquals(parameterType, expectedType, clues(source))
        case other => fail(s"expected Lambda1 for $source, got ${other.render}")
    }

  test("preserves truthful positioned and unpositioned Lambda1 root spans"):
    val source = "(x: Int) => x"
    assertEquals(
      project(parsed(source)).sourceSpan,
      Some(NeutralSourceSpan(0, source.length))
    )

    val unpositioned = Term.Function(
      Term.ParamClause(
        List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None))
      ),
      Term.Name("x")
    )
    assertEquals(unpositioned.pos, Position.None)
    assertEquals(project(unpositioned).sourceSpan, None)

  test("rejects excluded Lambda1 topology with stable neutral categories"):
    val modified = Term.Function(
      Term.ParamClause(
        List(
          Term.Param(
            List(Mod.Implicit()),
            Term.Name("x"),
            Some(Type.Name("Int")),
            None
          )
        )
      ),
      Term.Name("x")
    )
    val zeroParameter = Term.Function(Term.ParamClause(Nil), Lit.Int(1))
    val cases = List(
      parsed("x => x") -> "NEUTRAL_LAMBDA_PARAMETER_TYPE_REQUIRED",
      parsed("(x: Int, y: Int) => x") -> "NEUTRAL_LAMBDA_PARAMETER_CLAUSE_UNSUPPORTED",
      zeroParameter -> "NEUTRAL_LAMBDA_PARAMETER_CLAUSE_UNSUPPORTED",
      parsed("(x: Int) => ((y: Int) => y)") -> "NEUTRAL_LAMBDA_NESTED_UNSUPPORTED",
      parsed("(x: Int) ?=> x") -> "NEUTRAL_LAMBDA_CONTEXT_FUNCTION_UNSUPPORTED",
      modified -> "NEUTRAL_LAMBDA_PARAMETER_MODIFIERS_UNSUPPORTED",
      parsed("(x: List[Int]) => x") -> "NEUTRAL_LAMBDA_PARAMETER_TYPE_UNSUPPORTED",
      parsed("(x: custom.Type) => x") -> "NEUTRAL_LAMBDA_PARAMETER_TYPE_UNSUPPORTED",
      parsed("(x: Int) => new java.lang.StringBuilder(16)") -> "NEUTRAL_TERM_UNSUPPORTED"
    )

    cases.foreach { (term, expectedCode) =>
      assertErrorCode(term, expectedCode)
    }
    assertEquals(
      ScalametaTermProjection.project(null),
      Left(
        NeutralProjectionError(
          "NEUTRAL_TERM_MISSING",
          "the Scalameta term must be present."
        )
      )
    )

  test("rejects parameter names outside the neutral source-name policy"):
    val invalid = Term.Function(
      Term.ParamClause(
        List(Term.Param(Nil, Term.Name("bad.name"), Some(Type.Name("Int")), None))
      ),
      Term.Name("bad.name")
    )

    assertErrorCode(invalid, "NEUTRAL_LAMBDA_PARAMETER_NAME_UNSUPPORTED")

  private def parsed(source: String): Term =
    Input.String(source).parse[Term].get

  private def project(source: Term): ProjectedTermShape =
    ScalametaTermProjection.project(source) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def assertErrorCode(source: Term, expected: String): Unit =
    assertEquals(
      ScalametaTermProjection.project(source).left.toOption.map(_.code),
      Some(expected),
      clues(source.structure)
    )
