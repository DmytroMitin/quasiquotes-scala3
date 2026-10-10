package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.neutral.{ScalametaTermProjection, ScalametaTermShapeAuthoring}
import quasiquotes.parser.{TermShape, TermShapeInspector, TinyTermParser}
import quasiquotes.terms.ConstructedTerm

import scala.meta.*
import scala.meta.dialects.Scala3

final class CompletedMulticlauseApplyExactBackendModelFitCharacterizationTest
    extends munit.FunSuite:
  private final case class Fixture(
      source: String,
      shape: TermShape,
      clauseArities: List[Int],
      rawStructure: String,
      spans: Vector[(String, Int, Int, Int)]
  )

  private val fixtures = Vector(
    Fixture(
      "f(a)(b)",
      apply(apply(ident("f"), ident("a")), ident("b")),
      List(1, 1),
      "Apply(Apply(Ident(f), [Ident(a)]), [Ident(b)])",
      Vector(
        ("Apply", 0, 7, 4),
        ("Apply", 0, 4, 1),
        ("Ident(f)", 0, 1, 0),
        ("Ident(a)", 2, 3, 2),
        ("Ident(b)", 5, 6, 5)
      )
    ),
    Fixture(
      "f()(b)",
      apply(apply(ident("f")), ident("b")),
      List(0, 1),
      "Apply(Apply(Ident(f), []), [Ident(b)])",
      Vector(
        ("Apply", 0, 6, 3),
        ("Apply", 0, 3, 1),
        ("Ident(f)", 0, 1, 0),
        ("Ident(b)", 4, 5, 4)
      )
    ),
    Fixture(
      "f(a)()(c, d)",
      apply(apply(apply(ident("f"), ident("a"))), ident("c"), ident("d")),
      List(1, 0, 2),
      "Apply(Apply(Apply(Ident(f), [Ident(a)]), []), [Ident(c), Ident(d)])",
      Vector(
        ("Apply", 0, 12, 6),
        ("Apply", 0, 6, 4),
        ("Apply", 0, 4, 1),
        ("Ident(f)", 0, 1, 0),
        ("Ident(a)", 2, 3, 2),
        ("Ident(c)", 7, 8, 7),
        ("Ident(d)", 10, 11, 10)
      )
    )
  )

  test("the Dotty parser produces exact nested Apply topology including empty clauses"):
    withContext:
      fixtures.foreach: fixture =>
        val parsed = TinyTermParser.parseOrThrow(fixture.source)
        assertEquals(parsed.shape, fixture.shape, clues(fixture.source))
        assertEquals(parsed.rawStructure, fixture.rawStructure, clues(fixture.source))
        assertEquals(spanSnapshot(parsed.rawTree), fixture.spans, clues(fixture.source))
        assertEquals(allTrees(parsed.rawTree).size, fixture.spans.size, clues(fixture.source))
        assertPreTyper(parsed.rawTree)
        allTrees(parsed.rawTree).foreach: node =>
          assert(node.source eq parsed.rawTree.source, clues(fixture.source, node))

  test("neutral projection and authoring preserve Scalameta completed clause topology"):
    fixtures.foreach: fixture =>
      val term = parseScalameta(fixture.source)
      assertEquals(scalametaClauseArities(term), fixture.clauseArities, clues(fixture.source))
      val projection = ScalametaTermProjection.project(term).fold(error => fail(error.message), identity)
      assertEquals(projection.shape, fixture.shape, clues(fixture.source))
      val authored = ScalametaTermShapeAuthoring.author(fixture.shape).fold(error => fail(error.message), identity)
      assertEquals(scalametaClauseArities(authored), fixture.clauseArities, clues(fixture.source))
      assertEquals(ScalametaTermProjection.project(authored).map(_.shape), Right(fixture.shape))
      assert(allMetaTrees(authored).forall(_.pos == Position.None), clues(fixture.source))

    val oneClause = apply(ident("f"), ident("a"))
    assert(ScalametaTermProjection.project(parseScalameta("f(a)")).isRight)
    assert(ScalametaTermShapeAuthoring.author(oneClause).isRight)

  test("the lower-level constructed backend exactly lowers every completed nested Apply"):
    withContext:
      fixtures.foreach: fixture =>
        val completed = complete(fixture.shape)
        val first = lowerConstructed(completed)
        val second = lowerConstructed(completed)
        assertEquals(TermShapeInspector.rawStructure(first), fixture.rawStructure, clues(fixture.source))
        assertEquals(
          TermShapeInspector.rawStructure(first),
          TinyTermParser.parseOrThrow(fixture.source).rawStructure,
          clues(fixture.source)
        )
        assertSourceFree(first)
        assertFresh(first, second)

  test("the lower-level generated-origin adapter renders and positions every completed clause"):
    withContext:
      fixtures.zipWithIndex.foreach: (fixture, index) =>
        val completed = complete(fixture.shape)
        val path = s"generated/u050-$index.scala"
        val first = lowerGenerated(completed, path)
        val second = lowerGenerated(completed, path)
        assertEquals(first.generatedSource, fixture.source, clues(fixture.source))
        assertEquals(first.sourceFile.path, path, clues(fixture.source))
        assertEquals(first.sourceFile.content.mkString, fixture.source, clues(fixture.source))
        assertEquals(TermShapeInspector.rawStructure(first.tree), fixture.rawStructure, clues(fixture.source))
        assertEquals(spanSnapshot(first.tree), fixture.spans, clues(fixture.source))
        allTrees(first.tree).foreach: node =>
          assert(node.source eq first.sourceFile, clues(fixture.source, node))
          assertEquals(node.symbol, NoSymbol, clues(fixture.source, node))
          assert(!node.isInstanceOf[untpd.TypedSplice], clues(fixture.source, node))
        assertFresh(first.tree, second.tree)
        assert(!(first.sourceFile eq second.sourceFile), clues(fixture.source))

  test("the public semantic facades use the accepted completed nested-Apply backend"):
    withContext:
      fixtures.foreach: fixture =>
        val sourceFree = TermUntypedLowering
          .lower(fixture.shape)
          .fold(problem => fail(problem.message), identity)
        assertEquals(TermShapeInspector.rawStructure(sourceFree), fixture.rawStructure, clues(fixture.source))
        assertSourceFree(sourceFree)

        val generated = TermGeneratedOriginLowering
          .lower(fixture.shape, "generated/U050Public.scala")
          .fold(problem => fail(problem.message), identity)
        assertEquals(generated.generatedSource, fixture.source, clues(fixture.source))
        assertEquals(TermShapeInspector.rawStructure(generated.tree), fixture.rawStructure, clues(fixture.source))
        assertEquals(spanSnapshot(generated.tree), fixture.spans, clues(fixture.source))

  test("the direct Core lowerer preserves exact completed nested Apply topology"):
    withContext:
      fixtures.foreach: fixture =>
        val first = CoreTermShapeUntypedLowerer.lower(fixture.shape).fold(error => fail(error.message), identity)
        val second = CoreTermShapeUntypedLowerer.lower(fixture.shape).fold(error => fail(error.message), identity)
        assertEquals(TermShapeInspector.rawStructure(first), fixture.rawStructure, clues(fixture.source))
        assertSourceFree(first)
        assertFresh(first, second)

      val oneClause = apply(ident("f"), ident("a"))
      val raw = CoreTermShapeUntypedLowerer.lower(oneClause).fold(error => fail(error.message), identity)
      assertEquals(TermShapeInspector.rawStructure(raw), "Apply(Ident(f), [Ident(a)])")
      assertSourceFree(raw)

  test("both public Scalameta bridges admit the coordinated completed multi-list family"):
    withContext:
      fixtures.foreach: fixture =>
        val term = parseScalameta(fixture.source)
        val direct = ScalametaTermUntypedBridge.lower(term).fold(problem => fail(problem.detail), identity)
        assertEquals(TermShapeInspector.rawStructure(direct), fixture.rawStructure, clues(fixture.source))
        assertSourceFree(direct)

        val generated = ScalametaTermGeneratedOriginBridge
          .lower(term, "generated/U050Scalameta.scala")
          .fold(problem => fail(problem.detail), identity)
        assertEquals(generated.generatedSource, fixture.source, clues(fixture.source))
        assertEquals(TermShapeInspector.rawStructure(generated.tree), fixture.rawStructure, clues(fixture.source))
        assertEquals(spanSnapshot(generated.tree), fixture.spans, clues(fixture.source))

      assert(ScalametaTermUntypedBridge.lower(parseScalameta("f(a)")).isRight)
      assert(
        ScalametaTermGeneratedOriginBridge
          .lower(parseScalameta("f(a)"), "generated/U050OneClause.scala")
          .isRight
      )

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def apply(function: TermShape, arguments: TermShape*): TermShape =
    TermShape.Apply(function, arguments.toList)

  private def complete(shape: TermShape): ConstructedTerm =
    ConstructedTerm.fromShape(shape).fold(problem => fail(problem.message), identity)

  private def lowerConstructed(completed: ConstructedTerm): untpd.Tree =
    ConstructedTermUntypedBackend.lower(completed).fold(problem => fail(problem.message), identity)

  private def lowerGenerated(completed: ConstructedTerm, path: String)(using Context): GeneratedOriginTermResult =
    ConstructedTermGeneratedOriginAdapter.lower(completed, path).fold(problem => fail(problem.message), identity)

  private def parseScalameta(source: String): Term =
    Scala3(source).parse[Term].get

  private def scalametaClauseArities(term: Term): List[Int] =
    def loop(current: Term, reversed: List[Int]): List[Int] = current match
      case Term.Apply(function, clause) => loop(function, clause.values.size :: reversed)
      case _: Term.Name => reversed
      case other => fail(s"unexpected Scalameta multi-clause topology: ${other.structure}")
    loop(term, Nil)

  private def allMetaTrees(tree: Tree): List[Tree] =
    tree :: tree.children.toList.flatMap(allMetaTrees)

  private def spanSnapshot(tree: untpd.Tree)(using Context): Vector[(String, Int, Int, Int)] =
    allTrees(tree).map: node =>
      val label = node match
        case _: untpd.Apply => "Apply"
        case value: untpd.Ident => s"Ident(${value.name})"
        case other => other.getClass.getSimpleName
      (label, node.span.start, node.span.end, node.span.point)

  private def assertSourceFree(tree: untpd.Tree)(using Context): Unit =
    allTrees(tree).foreach: node =>
      assert(!node.source.exists, clues(node))
      assert(!node.span.exists, clues(node))
      assertEquals(node.symbol, NoSymbol, clues(node))
      assert(!node.isInstanceOf[untpd.TypedSplice], clues(node))

  private def assertPreTyper(tree: untpd.Tree)(using Context): Unit =
    allTrees(tree).foreach: node =>
      assertEquals(node.symbol, NoSymbol, clues(node))
      assert(!node.isInstanceOf[untpd.TypedSplice], clues(node))

  private def assertFresh(first: untpd.Tree, second: untpd.Tree)(using Context): Unit =
    val firstNodes = allTrees(first)
    val secondNodes = allTrees(second)
    assertEquals(firstNodes.size, secondNodes.size)
    firstNodes.zip(secondNodes).foreach: (left, right) =>
      assert(!(left eq right), clues(left, right))

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree +: (tree match
      case value: untpd.Apply => allTrees(value.fun) ++ value.args.toVector.flatMap(allTrees)
      case value: untpd.Select => allTrees(value.qualifier)
      case value: untpd.New => allTrees(value.tpt)
      case value: untpd.InfixOp => allTrees(value.left) ++ allTrees(value.op) ++ allTrees(value.right)
      case value: untpd.PrefixOp => allTrees(value.op) ++ allTrees(value.od)
      case value: untpd.Tuple => value.trees.toVector.flatMap(allTrees)
      case value: untpd.If => allTrees(value.cond) ++ allTrees(value.thenp) ++ allTrees(value.elsep)
      case value: untpd.Parens => allTrees(value.t)
      case _ => Vector.empty)

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)
