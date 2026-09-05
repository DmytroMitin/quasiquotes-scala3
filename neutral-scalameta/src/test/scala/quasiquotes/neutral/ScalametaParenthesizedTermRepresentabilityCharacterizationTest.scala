package quasiquotes.neutral

import _root_.quasiquotes.parser.{BinderId, TermShape}

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaParenthesizedTermRepresentabilityCharacterizationTest extends munit.FunSuite:
  private final case class ParsedFixture(
      source: String,
      rootKind: String,
      expectedShape: TermShape
  )

  test("parsed grouping is retained by origin tokens and positions but has no structural Term node"):
    val fixtures = List(
      ParsedFixture("(x)", "Term.Name", identifier("x")),
      ParsedFixture("((x))", "Term.Name", identifier("x")),
      ParsedFixture(
        "(1 + 2)",
        "Term.ApplyInfix",
        TermShape.Infix(literal("1"), "+", literal("2"))
      ),
      ParsedFixture(
        "((1 + 2))",
        "Term.ApplyInfix",
        TermShape.Infix(literal("1"), "+", literal("2"))
      ),
      ParsedFixture(
        "(obj.f(1))",
        "Term.Apply",
        TermShape.Apply(
          TermShape.Select(identifier("obj"), "f"),
          List(literal("1"))
        )
      )
    )

    fixtures.foreach { fixture =>
      val parsed = parse(fixture.source)
      val projected = project(parsed)

      assertEquals(parsed.productPrefix, fixture.rootKind, clues(fixture.source))
      assertEquals(parsed.tokens.map(_.text).mkString, fixture.source, clues(fixture.source))
      assertEquals(parsed.pos.start, 0, clues(fixture.source))
      assertEquals(parsed.pos.end, fixture.source.length, clues(fixture.source))
      assertEquals(projected.shape, fixture.expectedShape, clues(fixture.source))
      assertEquals(
        projected.sourceSpan,
        Some(NeutralSourceSpan(0, fixture.source.length)),
        clues(fixture.source)
      )
    }

  test("nested parsed parentheses change only root origin tokens and child offsets"):
    val single = parse("(1 + 2)").asInstanceOf[Term.ApplyInfix]
    val nested = parse("((1 + 2))").asInstanceOf[Term.ApplyInfix]

    assertEquals(single.structure, nested.structure)
    assertEquals(single.tokens.map(_.text).mkString, "(1 + 2)")
    assertEquals(nested.tokens.map(_.text).mkString, "((1 + 2))")
    assertEquals(single.lhs.tokens.map(_.text).mkString, "1")
    assertEquals(single.op.tokens.map(_.text).mkString, "+")
    assertEquals(single.argClause.values.head.tokens.map(_.text).mkString, "2")
    assertEquals((single.lhs.pos.start, single.lhs.pos.end), (1, 2))
    assertEquals((nested.lhs.pos.start, nested.lhs.pos.end), (2, 3))
    assertEquals(project(single).shape, project(nested).shape)

  test("fresh direct inner Terms are unpositioned and project without a Parenthesized carrier"):
    val freshName = Term.Name("x")
    val freshInfix = Term.ApplyInfix(
      Lit.Int(1),
      Term.Name("+"),
      Type.ArgClause(Nil),
      Term.ArgClause(List(Lit.Int(2)))
    )
    val freshApply = Term.Apply(
      Term.Select(Term.Name("obj"), Term.Name("f")),
      Term.ArgClause(List(Lit.Int(1)))
    )
    val fixtures = List(
      freshName -> identifier("x"),
      freshInfix -> TermShape.Infix(literal("1"), "+", literal("2")),
      freshApply -> TermShape.Apply(
        TermShape.Select(identifier("obj"), "f"),
        List(literal("1"))
      )
    )

    assertEquals(freshName.tokens.map(_.text).mkString, "x")
    assertEquals(freshInfix.tokens.map(_.text).mkString, "1 + 2")
    assertEquals(freshApply.tokens.map(_.text).mkString, "obj.f(1)")
    fixtures.foreach { (term, expectedShape) =>
      val projected = project(term)

      assert(allTrees(term).forall(_.pos == Position.None), clues(term.productPrefix))
      assertEquals(projected, ProjectedTermShape(expectedShape, None))
      assert(!projected.shape.isInstanceOf[TermShape.Parenthesized])
    }

  test("fresh block and tuple candidates remain distinct from Parenthesized"):
    val singletonBlock = Term.Block(List(Term.Name("x")))
    val ordinaryBlock = Term.Block(List(Lit.Int(1), Lit.Int(2)))
    val singletonTuple = Term.Tuple(List(Term.Name("x")))
    val pairTuple = Term.Tuple(List(Term.Name("x"), Term.Name("y")))

    assertEquals(singletonBlock.productPrefix, "Term.Block")
    assertEquals(project(singletonBlock), ProjectedTermShape(identifier("x"), None))
    assertEquals(
      project(ordinaryBlock),
      ProjectedTermShape(TermShape.Block(List(literal("1")), literal("2")), None)
    )
    assertEquals(
      ScalametaTermProjection.project(singletonTuple).left.toOption.map(_.code),
      Some("NEUTRAL_TUPLE_ARITY_UNSUPPORTED")
    )
    assertEquals(
      project(pairTuple),
      ProjectedTermShape(TermShape.Tuple(List(identifier("x"), identifier("y"))), None)
    )
    List(singletonBlock, ordinaryBlock, singletonTuple, pairTuple).foreach(term =>
      assert(allTrees(term).forall(_.pos == Position.None), clues(term.productPrefix))
    )

  test("authoring keeps Parenthesized and public root BoundReference outside the bounded family"):
    val inner = TermShape.Apply(identifier("f"), List(literal("1")))
    val authoredInner = author(inner)

    assertEquals(project(authoredInner), ProjectedTermShape(inner, None))
    assertErrorCode(
      TermShape.Parenthesized(inner),
      "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED"
    )
    assertErrorCode(
      TermShape.Parenthesized(TermShape.Parenthesized(inner)),
      "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED"
    )
    assertErrorCode(
      TermShape.BoundReference(BinderId(0), "x"),
      "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED"
    )

  private def parse(source: String): Term =
    Input.String(source).parse[Term].get

  private def project(term: Term): ProjectedTermShape =
    ScalametaTermProjection.project(term) match
      case Right(projected) => projected
      case Left(problem) => fail(problem.message)

  private def author(shape: TermShape): Term =
    ScalametaTermShapeAuthoring.author(shape) match
      case Right(term) => term
      case Left(problem) => fail(problem.message)

  private def assertErrorCode(shape: TermShape, expected: String): Unit =
    assertEquals(
      ScalametaTermShapeAuthoring.author(shape).left.toOption.map(_.code),
      Some(expected),
      clues(shape)
    )

  private def identifier(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def literal(value: String): TermShape =
    TermShape.Literal(value)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
