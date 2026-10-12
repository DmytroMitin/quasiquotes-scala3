package quasiquotes.neutral

import _root_.quasiquotes.construct.SelectedMemberName
import _root_.quasiquotes.parser.TermShape
import _root_.quasiquotes.source.*
import _root_.quasiquotes.terms.*

import scala.meta.*

final class N069TwoDistinctSelectedNameTermTemplateIntegrationTest
    extends munit.FunSuite:
  private val firstTransport = "__first_selected_name_transport"
  private val secondTransport = "__second_selected_name_transport"
  private val receiver = TermShape.Identifier("receiver", isPlaceholder = false)

  test("two completed selected names survive neutral authoring and projection"):
    val root = TermShape.Select(
      TermShape.Select(receiver, firstTransport),
      secondTransport
    )
    val template = TermTemplate
      .createWithTwoSelectedNameHoles(
        root,
        GeneratedHoleIndex.empty,
        Vector.empty,
        GeneratedHoleIndex.empty,
        Vector.empty,
        selectedNameIndex,
        Vector(
          SelectedNameHoleOccurrence("outer", selectOrdinal = 0),
          SelectedNameHoleOccurrence("inner", selectOrdinal = 1)
        ),
        GeneratedHoleIndex.empty,
        Vector.empty
      )
      .fold(error => fail(error.message), identity)

    val expected = TermShape.Select(
      TermShape.Select(receiver, "innerName"),
      "outerName"
    )
    val completed = template
      .completeWithSelectedNames(
        Map.empty,
        Map.empty,
        Map(
          "inner" -> selected("innerName"),
          "outer" -> selected("outerName")
        ),
        Map.empty
      )
      .fold(error => fail(error.message), identity)
    val authored = ScalametaTermShapeAuthoring
      .author(completed.root)
      .fold(error => fail(error.message), identity)

    assertEquals(template.requiredSelectedNameBindings, Vector("outer", "inner"))
    assertEquals(completed.root, expected)
    assert(allTrees(authored).forall(_.pos == Position.None))
    assertEquals(ScalametaTermProjection.project(authored).map(_.shape), Right(expected))

  private def selected(value: String): SelectedMemberName =
    SelectedMemberName.from(value).fold(error => fail(error.message), identity)

  private def selectedNameIndex: GeneratedHoleIndex =
    GeneratedHoleIndex.fromOccurrences(
      Vector(
        HoleOccurrence(
          "inner",
          firstTransport,
          SourceSpan(0, 1),
          SourceSpan(0, 1),
          HoleRole.TermTemplate
        ),
        HoleOccurrence(
          "outer",
          secondTransport,
          SourceSpan(2, 3),
          SourceSpan(2, 3),
          HoleRole.TermTemplate
        )
      )
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
