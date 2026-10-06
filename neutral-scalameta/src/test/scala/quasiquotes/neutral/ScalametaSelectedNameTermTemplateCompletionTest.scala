package quasiquotes.neutral

import _root_.quasiquotes.construct.SelectedMemberName
import _root_.quasiquotes.parser.TermShape
import _root_.quasiquotes.source.*
import _root_.quasiquotes.terms.*

import scala.meta.*

final class ScalametaSelectedNameTermTemplateCompletionTest
    extends munit.FunSuite:
  private val transport = "__selected_name_transport"
  private val receiver = TermShape.Identifier("receiver", isPlaceholder = false)

  test("completed selected names remain inside neutral authoring and projection"):
    val template = TermTemplate
      .createWithSelectedNameHole(
        TermShape.Select(receiver, transport),
        GeneratedHoleIndex.empty,
        Vector.empty,
        GeneratedHoleIndex.empty,
        Vector.empty,
        index("member", transport),
        Vector(SelectedNameHoleOccurrence("member", selectOrdinal = 0)),
        GeneratedHoleIndex.empty,
        Vector.empty
      )
      .fold(error => fail(error.message), identity)

    Vector("ordinary", "member2", "_privateLike").foreach { value =>
      val completed = template
        .completeWithSelectedNames(
          Map.empty,
          Map.empty,
          Map("member" -> selected(value)),
          Map.empty
        )
        .fold(error => fail(error.message), identity)
      val expected = TermShape.Select(receiver, value)
      val authored = ScalametaTermShapeAuthoring
        .author(completed.root)
        .fold(error => fail(error.message), identity)

      assertEquals(completed.root, expected)
      assertEquals(authored.structure, s"Term.Select(\n  Term.Name(\"receiver\"),\n  Term.Name(\"$value\")\n)")
      assert(allTrees(authored).forall(_.pos == Position.None))
      assertEquals(
        ScalametaTermProjection.project(authored).map(_.shape),
        Right(expected)
      )
    }

  test("fixed selected names and old empty-category completion stay unchanged"):
    val root = TermShape.Select(receiver, "fixed")
    val template = TermTemplate
      .create(
        root,
        GeneratedHoleIndex.empty,
        Vector.empty,
        GeneratedHoleIndex.empty,
        Vector.empty
      )
      .fold(error => fail(error.message), identity)

    assertEquals(template.complete(Map.empty, Map.empty).map(_.root), Right(root))
    assertEquals(
      ScalametaTermShapeAuthoring.author(root).flatMap(term =>
        ScalametaTermProjection.project(term).map(_.shape).left.map(error =>
          ScalametaTermShapeAuthoring.Error(error.code, error.detail)
        )
      ),
      Right(root)
    )

  private def selected(value: String): SelectedMemberName =
    SelectedMemberName.from(value).fold(error => fail(error.message), identity)

  private def index(
      semantic: String,
      generated: String
  ): GeneratedHoleIndex =
    GeneratedHoleIndex.fromOccurrences(
      Vector(
        HoleOccurrence(
          semantic,
          generated,
          SourceSpan(0, 1),
          SourceSpan(0, 1),
          HoleRole.TermTemplate
        )
      )
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
