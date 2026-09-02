package quasiquotes.neutral

import _root_.quasiquotes.parser.{BinderId, BlockStatement, TermShape}
import _root_.quasiquotes.terms.ConstructedTerm

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaP2LocalValProjectionTest extends munit.FunSuite:
  test("Scalameta exposes the exact selected P2 local-val fields structurally"):
    parsed("{ val x: Int = 1; x }") match
      case Term.Block((definition: Defn.Val) :: (result: Term.Name) :: Nil) =>
        assertEquals(definition.mods, Nil)
        definition.pats match
          case Pat.Var(name) :: Nil => assertEquals(name.value, "x")
          case other => fail(s"expected one Pat.Var, got $other")
        definition.decltpe match
          case Some(name: Type.Name) => assertEquals(name.value, "Int")
          case other => fail(s"expected direct Int type, got $other")
        definition.rhs match
          case Lit.Int(value) => assertEquals(value, 1)
          case other => fail(s"expected literal initializer, got ${other.productPrefix}")
        assertEquals(result.value, "x")
      case other => fail(s"expected exact P2 block, got ${other.structure}")

  test("projects one P2 LocalVal whose result shares its BinderId"):
    project(parsed("{ val x: Int = 1; x }")).shape match
      case TermShape.Block(
            List(BlockStatement.LocalVal(binderId, "x", "Int", TermShape.Literal("1"))),
            TermShape.BoundReference(referenceId, "x")
          ) =>
        assertEquals(binderId, BinderId(0))
        assertEquals(referenceId, binderId)
      case other => fail(s"unexpected P2 shape: ${other.render}")

  test("projects the initializer in the old scope and the result in the P2 scope"):
    project(parsed("{ val x: Int = x; x }")).shape match
      case TermShape.Block(
            List(
              BlockStatement.LocalVal(
                binderId,
                "x",
                "Int",
                TermShape.Identifier("x", false)
              )
            ),
            TermShape.BoundReference(referenceId, "x")
          ) =>
        assertEquals(referenceId, binderId)
      case other => fail(s"unexpected old/new-scope shape: ${other.render}")

  test("reuses N002 normal forms for direct and applied declared types"):
    val fixtures = List(
      "{ val x: Int = 1; x }" -> "Int",
      "{ val xs: List[Int] = source; xs }" -> "List[Int]"
    )

    fixtures.foreach { (source, expectedType) =>
      project(parsed(source)).shape match
        case TermShape.Block(
              List(BlockStatement.LocalVal(_, _, declaredType, _)),
              _
            ) => assertEquals(declaredType, expectedType, clues(source))
        case other => fail(s"expected P2 for $source, got ${other.render}")
    }

  test("recursively projects admitted N003 and nested P0/P1 children in the correct scopes"):
    assertEquals(
      project(
        parsed("{ val x: Int = { seed(); if flag then 1 else 2 }; { consume(x); x + free } }")
      ).shape.render,
      "Block([LocalVal(x: Int = Block([Apply(Ident(seed), [])], If(Ident(flag), Literal(1), Literal(2))))], Block([Apply(Ident(consume), [BoundRef(x)])], Infix(BoundRef(x), +, Ident(free))))"
    )

  test("composes distinct-name Lambda1 and P2 binders in either nesting direction"):
    project(parsed("(outer: Int) => { val x: Int = outer; x }")).shape match
      case TermShape.Lambda1(
            outerId,
            "outer",
            "Int",
            TermShape.Block(
              List(
                BlockStatement.LocalVal(
                  localId,
                  "x",
                  "Int",
                  TermShape.BoundReference(initializerId, "outer")
                )
              ),
              TermShape.BoundReference(resultId, "x")
            )
          ) =>
        assertNotEquals(outerId, localId)
        assertEquals(initializerId, outerId)
        assertEquals(resultId, localId)
      case other => fail(s"unexpected Lambda1-to-P2 shape: ${other.render}")

    project(parsed("{ val x: Int = 1; (inner: Int) => x + inner }")).shape match
      case TermShape.Block(
            List(BlockStatement.LocalVal(localId, "x", "Int", TermShape.Literal("1"))),
            TermShape.Lambda1(
              innerId,
              "inner",
              "Int",
              TermShape.Infix(
                TermShape.BoundReference(leftId, "x"),
                "+",
                TermShape.BoundReference(rightId, "inner")
              )
            )
          ) =>
        assertNotEquals(localId, innerId)
        assertEquals(leftId, localId)
        assertEquals(rightId, innerId)
      case other => fail(s"unexpected P2-to-Lambda1 shape: ${other.render}")

  test("allocates deterministic distinct IDs to sibling binders in source order"):
    project(parsed("{ ((first: Int) => first); (second: Int) => second }")).shape match
      case TermShape.Block(
            List(TermShape.Lambda1(firstId, "first", _, _)),
            TermShape.Lambda1(secondId, "second", _, _)
          ) =>
        assertEquals(firstId, BinderId(0))
        assertEquals(secondId, BinderId(1))
      case other => fail(s"unexpected sibling-binder shape: ${other.render}")

  test("retains existing Core alpha semantics across renamed P2 binders"):
    val left = ConstructedTerm.fromShape(project(parsed("{ val x: Int = 1; x + 1 }")).shape)
      .fold(error => fail(error.message), identity)
    val right = ConstructedTerm.fromShape(project(parsed("{ val renamed: Int = 1; renamed + 1 }")).shape)
      .fold(error => fail(error.message), identity)

    assertEquals(left, right)

  test("preserves positioned and unpositioned P2 root spans"):
    val source = "{ val x: Int = 1; x }"
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

  test("rejects malformed P2 declarations and neighboring statement topology precisely"):
    val invalidName = Term.Block(
      List(
        Defn.Val(
          Nil,
          List(Pat.Var(Term.Name("bad.name"))),
          Some(Type.Name("Int")),
          Lit.Int(1)
        ),
        Term.Name("bad.name")
      )
    )
    val modified = Term.Block(
      List(
        Defn.Val(
          List(Mod.Final()),
          List(Pat.Var(Term.Name("x"))),
          Some(Type.Name("Int")),
          Lit.Int(1)
        ),
        Term.Name("x")
      )
    )
    val cases = List(
      parsed("{ val x = 1; x }") -> "NEUTRAL_P2_TYPE_REQUIRED",
      parsed("{ var x: Int = 1; x }") -> "NEUTRAL_P2_MUTABLE_UNSUPPORTED",
      parsed("{ lazy val x: Int = 1; x }") -> "NEUTRAL_P2_LAZY_UNSUPPORTED",
      parsed("{ val (x, y): (Int, Int) = (1, 2); x }") -> "NEUTRAL_P2_PATTERN_UNSUPPORTED",
      invalidName -> "NEUTRAL_P2_BINDER_NAME_UNSUPPORTED",
      modified -> "NEUTRAL_P2_MODIFIERS_UNSUPPORTED",
      parsed("{ val x: Map[Int, String] = source; x }") ->
        "NEUTRAL_P2_DECLARED_TYPE_UNSUPPORTED",
      parsed("{ val x: Int = new java.lang.StringBuilder(16); x }") ->
        "NEUTRAL_P2_INITIALIZER_UNSUPPORTED",
      parsed("{ val x: Int = 1; new java.lang.StringBuilder(16) }") ->
        "NEUTRAL_P2_RESULT_UNSUPPORTED",
      parsed("{ val x: Int = 1; val y: Int = 2; y }") ->
        "NEUTRAL_P2_EXACTLY_ONE_LOCAL_VAL_UNSUPPORTED",
      parsed("{ 0; val x: Int = 1; x }") ->
        "NEUTRAL_P2_EXACTLY_ONE_LOCAL_VAL_UNSUPPORTED",
      parsed("{ def f = 1; f }") -> "NEUTRAL_LOCAL_DEF_PARAMETER_CLAUSE_UNSUPPORTED",
      parsed("{ import scala.util.Try; result }") -> "NEUTRAL_BLOCK_STATEMENT_UNSUPPORTED"
    )

    cases.foreach { (term, expectedCode) => assertErrorCode(term, expectedCode) }

  test("enforces one whole-tree P2 and the established Lambda/P2 shadowing policy"):
    val cases = List(
      parsed("{ val x: Int = 1; { val y: Int = 2; y } }") ->
        "NEUTRAL_P2_SECOND_OR_NESTED_LOCAL_VAL_UNSUPPORTED",
      parsed("{ val x: Int = { val y: Int = 2; y }; x }") ->
        "NEUTRAL_P2_SECOND_OR_NESTED_LOCAL_VAL_UNSUPPORTED",
      parsed("{ { val x: Int = 1; x }; val y: Int = 2; y }") ->
        "NEUTRAL_P2_EXACTLY_ONE_LOCAL_VAL_UNSUPPORTED",
      parsed("(x: Int) => { val x: Int = 1; x }") ->
        "NEUTRAL_P2_SOURCE_BINDER_SHADOWING_UNSUPPORTED",
      parsed("{ val x: Int = 1; (x: Int) => x }") ->
        "NEUTRAL_P2_SOURCE_BINDER_SHADOWING_UNSUPPORTED",
      parsed("(x: Int) => ((y: Int) => y)") -> "NEUTRAL_LAMBDA_NESTED_UNSUPPORTED"
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
