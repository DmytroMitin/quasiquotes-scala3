package quasiquotes.neutral

import _root_.quasiquotes.construct.SelectedMemberName
import _root_.quasiquotes.matching.*
import _root_.quasiquotes.parser.TermShape

import scala.meta.*

final class ScalametaSelectedNamePatternMatcherIntegrationTest
    extends munit.FunSuite:
  private val transport = "__selected_name_transport"
  private val receiver = TermShape.Identifier("receiver", isPlaceholder = false)

  test("fresh plain Select projection matches and captures its qualifier and selected name"):
    val fresh = Term.Select(Term.Name("receiver"), Term.Name("ordinary"))
    val projected = project(fresh)
    val pattern = validated(
      TermPattern.Select(TermPattern.Hole("receiver"), transport)
    )

    assert(allTrees(fresh).forall(_.pos == Position.None))
    assertEquals(projected, TermShape.Select(receiver, "ordinary"))
    assertEquals(
      matched(pattern, projected),
      NeutralSelectedNameMatchResult(
        Map("receiver" -> receiver),
        Map("member" -> selected("ordinary"))
      )
    )

  test("fresh unary selected application projection matches qualifier argument and name"):
    val fresh = Term.Apply(
      Term.Select(Term.Name("receiver"), Term.Name("ordinary")),
      Term.ArgClause(List(Lit.Int(1)))
    )
    val projected = project(fresh)
    val pattern = validated(
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("receiver"), transport),
        List(TermPattern.Hole("argument"))
      )
    )

    assert(allTrees(fresh).forall(_.pos == Position.None))
    assertEquals(
      projected,
      TermShape.Apply(
        TermShape.Select(receiver, "ordinary"),
        List(TermShape.Literal("1"))
      )
    )
    assertEquals(
      matched(pattern, projected),
      NeutralSelectedNameMatchResult(
        Map(
          "receiver" -> receiver,
          "argument" -> TermShape.Literal("1")
        ),
        Map("member" -> selected("ordinary"))
      )
    )

  test("authored recursive selected applications remain position-free and reproject to a match"):
    val shape = TermShape.Apply(
      TermShape.Select(
        TermShape.Select(receiver, "fixed"),
        "ordinary"
      ),
      List(TermShape.Literal("1"))
    )
    val authored = ScalametaTermShapeAuthoring
      .author(shape)
      .fold(error => fail(error.message), identity)
    val pattern = validated(
      TermPattern.Apply(
        TermPattern.Select(
          TermPattern.Select(TermPattern.Hole("receiver"), "fixed"),
          transport
        ),
        List(TermPattern.Hole("argument"))
      )
    )

    assert(allTrees(authored).forall(_.pos == Position.None))
    assertEquals(project(authored), shape)
    assertEquals(
      matched(pattern, project(authored)),
      NeutralSelectedNameMatchResult(
        Map(
          "receiver" -> receiver,
          "argument" -> TermShape.Literal("1")
        ),
        Map("member" -> selected("ordinary"))
      )
    )

  test("symbolic keyword spaced wildcard Unicode and dollar names stop at the neutral lexical boundary"):
    val pattern = validated(
      TermPattern.Select(TermPattern.Hole("receiver"), transport)
    )

    Vector("+", "type", "safe spaced name", "_", "naïve", "$internal").foreach { name =>
      val fresh = Term.Select(Term.Name("receiver"), Term.Name(name))

      assert(allTrees(fresh).forall(_.pos == Position.None), clues(name))
      assert(ScalametaTermProjection.project(fresh).isLeft, clues(name))
      assertEquals(
        SelectedNamePatternMatcher.matchTarget(
          pattern,
          TermShape.Select(receiver, name)
        ),
        Left(NeutralSelectedNameMatchError.SelectedNameLexicalUnsupported(name)),
        clues(name)
      )
    }

  private def validated(pattern: TermPattern): ValidatedSelectedNamePattern =
    ValidatedSelectedNamePattern
      .create(
        pattern,
        SelectedNamePatternOccurrence("member", 0, transport)
      )
      .fold(error => fail(error.message), identity)

  private def matched(
      pattern: ValidatedSelectedNamePattern,
      target: TermShape
  ): NeutralSelectedNameMatchResult =
    SelectedNamePatternMatcher
      .matchTarget(pattern, target)
      .fold(error => fail(error.message), identity)

  private def project(term: Term): TermShape =
    ScalametaTermProjection
      .project(term)
      .fold(error => fail(error.message), _.shape)

  private def selected(name: String): SelectedMemberName =
    SelectedMemberName.from(name).fold(error => fail(error.message), identity)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
