package quasiquotes.neutral

import _root_.quasiquotes.construct.SelectedMemberName
import _root_.quasiquotes.parser.TermShape
import _root_.quasiquotes.source.GeneratedHoleIndex
import _root_.quasiquotes.terms.*

import scala.meta.*

final class N071TwoRepeatedSelectedNameTermTemplateIntegrationTest
    extends munit.FunSuite:
  private val innerTransport = "__n071_neutral_inner"
  private val outerTransport = "__n071_neutral_outer"
  private val receiver = TermShape.Identifier("receiver", isPlaceholder = false)

  test("one repeated selected binding survives neutral authoring and projection"):
    val template = TermTemplate
      .createWithTwoRepeatedSelectedNameHoles(
        TermShape.Select(
          TermShape.Select(receiver, innerTransport),
          outerTransport
        ),
        GeneratedHoleIndex.empty,
        Vector.empty,
        GeneratedHoleIndex.empty,
        Vector.empty,
        Vector(
          SelectedNameHoleOccurrenceWithTransport(
            "member",
            selectOrdinal = 1,
            innerTransport
          ),
          SelectedNameHoleOccurrenceWithTransport(
            "member",
            selectOrdinal = 0,
            outerTransport
          )
        ),
        GeneratedHoleIndex.empty,
        Vector.empty
      )
      .fold(error => fail(error.message), identity)

    val expected = TermShape.Select(
      TermShape.Select(receiver, "same"),
      "same"
    )
    val completed = template
      .completeWithSelectedNames(
        Map.empty,
        Map.empty,
        Map("member" -> selected("same")),
        Map.empty
      )
      .fold(error => fail(error.message), identity)
    val authored = ScalametaTermShapeAuthoring
      .author(completed.root)
      .fold(error => fail(error.message), identity)

    assertEquals(template.requiredSelectedNameBindings, Vector("member"))
    assertEquals(completed.root, expected)
    assert(allTrees(authored).forall(_.pos == Position.None))
    assertEquals(ScalametaTermProjection.project(authored).map(_.shape), Right(expected))

  private def selected(value: String): SelectedMemberName =
    SelectedMemberName.from(value).fold(error => fail(error.message), identity)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
