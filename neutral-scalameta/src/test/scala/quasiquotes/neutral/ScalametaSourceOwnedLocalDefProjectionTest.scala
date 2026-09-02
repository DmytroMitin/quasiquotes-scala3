package quasiquotes.neutral

import _root_.quasiquotes.parser.{BinderId, BlockStatement, TermShape, TypeShape}
import _root_.quasiquotes.terms.TermShapeTraversal

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaSourceOwnedLocalDefProjectionTest extends munit.FunSuite:
  test("Scalameta exposes the exact selected source-owned local-def fields"):
    parsed("{ def id(x: Int): Int = x; id }") match
      case Term.Block((definition: Defn.Def) :: (result: Term.Name) :: Nil) =>
        assertEquals(definition.mods, Nil)
        assertEquals(definition.name.value, "id")
        definition.paramClauseGroups match
          case group :: Nil =>
            assertEquals(group.tparamClause.values, Nil)
            group.paramClauses match
              case clause :: Nil =>
                assertEquals(clause.mod, None)
                clause.values match
                  case parameter :: Nil =>
                    assertEquals(parameter.mods, Nil)
                    assertEquals(parameter.name.value, "x")
                    parameter.decltpe match
                      case Some(name: Type.Name) => assertEquals(name.value, "Int")
                      case other => fail(s"expected explicit Int parameter Type, got $other")
                    assertEquals(parameter.default, None)
                  case other => fail(s"expected one parameter, got $other")
              case other => fail(s"expected one parameter clause, got $other")
          case other => fail(s"expected one parameter-clause group, got $other")
        definition.decltpe match
          case Some(name: Type.Name) => assertEquals(name.value, "Int")
          case other => fail(s"expected explicit Int result Type, got $other")
        definition.body match
          case name: Term.Name => assertEquals(name.value, "x")
          case other => fail(s"expected direct parameter body, got ${other.productPrefix}")
        assertEquals(result.value, "id")
      case other => fail(s"expected exact local-def block, got ${other.structure}")

    parsed("{ inline def id(x: Int): Int = x; id }") match
      case Term.Block((definition: Defn.Def) :: _ :: Nil) =>
        assert(definition.mods.exists(_.isInstanceOf[Mod.Inline]), definition.mods)
      case other => fail(s"expected modified local def, got ${other.structure}")

  test("projects one local def with distinct method and parameter identities"):
    project(parsed("{ def id(x: Int): Int = x; id }")).shape match
      case TermShape.Block(
            List(
              BlockStatement.LocalDef(
                methodId,
                "id",
                parameterId,
                "x",
                TypeShape.Identifier("Int"),
                TypeShape.Identifier("Int"),
                TermShape.BoundReference(bodyId, "x")
              )
            ),
            TermShape.BoundReference(resultId, "id")
          ) =>
        assertEquals(methodId, BinderId(0))
        assertEquals(parameterId, BinderId(1))
        assertNotEquals(methodId, parameterId)
        assertEquals(bodyId, parameterId)
        assertEquals(resultId, methodId)
      case other => fail(s"unexpected local-def shape: ${other.render}")

  test("projects the current fixed concrete Type family structurally"):
    val fixtures = List(
      "{ def id(x: Int): Int = x; id }" -> TypeShape.Identifier("Int"),
      "{ def id(x: String): String = x; id }" -> TypeShape.Identifier("String"),
      "{ def id(x: Boolean): Boolean = x; id }" -> TypeShape.Identifier("Boolean"),
      "{ def id(x: scala.Int): scala.Int = x; id }" -> TypeShape.Identifier("Int")
    )

    fixtures.foreach { (source, expectedType) =>
      project(parsed(source)).shape match
        case TermShape.Block(List(local: BlockStatement.LocalDef), _) =>
          assertEquals(local.parameterType, expectedType, clues(source))
          assertEquals(local.resultType, expectedType, clues(source))
        case other => fail(s"expected local def for $source, got ${other.render}")
    }

  test("alpha-normalizes renamed method and parameter binders through Core semantics"):
    val first = TermShapeTraversal.alphaNormalize(
      project(parsed("{ def id(x: Int): Int = x; id }")).shape
    )
    val renamed = TermShapeTraversal.alphaNormalize(
      project(parsed("{ def renamed(argument: Int): Int = argument; renamed }")).shape
    )
    assertEquals(first, renamed)

  test("keeps same-text names free outside the local-def scopes"):
    assertEquals(
      project(parsed("id")).shape,
      TermShape.Identifier("id", isPlaceholder = false)
    )
    assertEquals(
      project(parsed("x")).shape,
      TermShape.Identifier("x", isPlaceholder = false)
    )

  test("preserves positioned and unpositioned local-def root spans"):
    val source = "{ def id(x: Int): Int = x; id }"
    val positioned = parsed(source)
    assertEquals(
      project(positioned).sourceSpan,
      Some(NeutralSourceSpan(0, source.length))
    )

    val unpositioned = positioned match
      case block: Term.Block => block.copy()
      case other => fail(s"expected Term.Block, got ${other.productPrefix}")
    assertEquals(unpositioned.pos, Position.None)
    assertEquals(project(unpositioned).sourceSpan, None)

  test("rejects malformed declarations, Types, bodies, and final results precisely"):
    val missingParameterType = Term.Block(
      List(
        Defn.Def(
          Nil,
          Term.Name("id"),
          List(
            Member.ParamClauseGroup(
              Type.ParamClause(Nil),
              List(
                Term.ParamClause(
                  List(Term.Param(Nil, Term.Name("x"), None, None)),
                  None
                )
              )
            )
          ),
          Some(Type.Name("Int")),
          Term.Name("x")
        ),
        Term.Name("id")
      )
    )
    val cases = List(
      parsed("{ inline def id(x: Int): Int = x; id }") ->
        "NEUTRAL_LOCAL_DEF_MODIFIERS_UNSUPPORTED",
      parsed("{ def id[A](x: Int): Int = x; id }") ->
        "NEUTRAL_LOCAL_DEF_TYPE_PARAMETERS_UNSUPPORTED",
      parsed("{ def id: Int = 1; id }") ->
        "NEUTRAL_LOCAL_DEF_PARAMETER_CLAUSE_UNSUPPORTED",
      parsed("{ def id(x: Int, y: Int): Int = x; id }") ->
        "NEUTRAL_LOCAL_DEF_PARAMETER_CLAUSE_UNSUPPORTED",
      parsed("{ def id(x: Int)(y: Int): Int = x; id }") ->
        "NEUTRAL_LOCAL_DEF_PARAMETER_CLAUSE_UNSUPPORTED",
      parsed("{ def id(using x: Int): Int = x; id }") ->
        "NEUTRAL_LOCAL_DEF_PARAMETER_CLAUSE_UNSUPPORTED",
      missingParameterType ->
        "NEUTRAL_LOCAL_DEF_PARAMETER_TYPE_REQUIRED",
      parsed("{ def id(x: List[Int]): List[Int] = x; id }") ->
        "NEUTRAL_LOCAL_DEF_PARAMETER_TYPE_UNSUPPORTED",
      parsed("{ def id(x: Int) = x; id }") ->
        "NEUTRAL_LOCAL_DEF_RESULT_TYPE_REQUIRED",
      parsed("{ def id(x: Int): List[Int] = x; id }") ->
        "NEUTRAL_LOCAL_DEF_RESULT_TYPE_UNSUPPORTED",
      parsed("{ def id(x: Int): String = x; id }") ->
        "NEUTRAL_LOCAL_DEF_INCOMPATIBLE_TYPES_UNSUPPORTED",
      parsed("{ def id(x: Int): Int = 1; id }") ->
        "NEUTRAL_LOCAL_DEF_BODY_UNSUPPORTED",
      parsed("{ def id(x: Int): Int = id; id }") ->
        "NEUTRAL_LOCAL_DEF_RECURSION_UNSUPPORTED",
      parsed("{ def id(x: Int): Int = x; id(1) }") ->
        "NEUTRAL_LOCAL_DEF_RESULT_UNSUPPORTED"
    )

    cases.foreach { (term, expectedCode) => assertErrorCode(term, expectedCode) }

  test("rejects additional, second, nested, P2, and Lambda combinations fail closed"):
    val cases = List(
      parsed("{ def first(x: Int): Int = x; def second(y: Int): Int = y; first }") ->
        "NEUTRAL_LOCAL_DEF_EXACTLY_ONE_UNSUPPORTED",
      parsed("{ seed; def id(x: Int): Int = x; id }") ->
        "NEUTRAL_LOCAL_DEF_EXACTLY_ONE_UNSUPPORTED",
      parsed("{ def id(x: Int): Int = x; seed; id }") ->
        "NEUTRAL_LOCAL_DEF_EXACTLY_ONE_UNSUPPORTED",
      parsed("{ seed; { def id(x: Int): Int = x; id } }") ->
        "NEUTRAL_LOCAL_DEF_SECOND_OR_NESTED_UNSUPPORTED",
      parsed("(outer: Int) => { def id(x: Int): Int = x; id }") ->
        "NEUTRAL_LOCAL_DEF_SECOND_OR_NESTED_UNSUPPORTED",
      parsed("{ val seed: Int = 1; { def id(x: Int): Int = x; id } }") ->
        "NEUTRAL_LOCAL_DEF_SECOND_OR_NESTED_UNSUPPORTED",
      parsed("{ def id(x: Int): Int = { val y: Int = x; y }; id }") ->
        "NEUTRAL_LOCAL_DEF_BODY_UNSUPPORTED",
      parsed("{ def id(x: Int): Int = ((y: Int) => y); id }") ->
        "NEUTRAL_LOCAL_DEF_BODY_UNSUPPORTED",
      parsed("{ def id(x: Int): Int = x; (y: Int) => y }") ->
        "NEUTRAL_LOCAL_DEF_RESULT_UNSUPPORTED"
    )

    cases.foreach { (term, expectedCode) => assertErrorCode(term, expectedCode) }

  test("retains the existing null-root failure"):
    assertEquals(
      ScalametaTermProjection.project(null),
      Left(
        NeutralProjectionError(
          "NEUTRAL_TERM_MISSING",
          "the Scalameta term must be present."
        )
      )
    )

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
