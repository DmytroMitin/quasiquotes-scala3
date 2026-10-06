package quasiquotes.neutral

import _root_.quasiquotes.construct.SelectedMemberName
import _root_.quasiquotes.parser.TermShape
import _root_.quasiquotes.source.*
import _root_.quasiquotes.terms.*

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

/** Characterization of completed selected-name origin erasure and the private template boundary. */
@nowarn("cat=deprecation")
final class ScalametaDynamicSelectedMemberModelFitCharacterizationTest
    extends munit.FunSuite:
  private val receiverShape = TermShape.Identifier("receiver", isPlaceholder = false)

  test("fresh static runtime and validated selected names complete to the same tree and shape"):
    val runtimeName = Vector("ordinary").head
    val selectedName = selected("ordinary").decoded
    val static = freshSelect("ordinary")
    val runtime = freshSelect(runtimeName)
    val validated = freshSelect(selectedName)

    val exactStructure =
      """Term.Select(
        |  Term.Name("receiver"),
        |  Term.Name("ordinary")
        |)""".stripMargin
    List(static, runtime, validated).foreach { term =>
      assertEquals(term.structure, exactStructure)
      assertEquals(term.qual.structure, "Term.Name(\"receiver\")")
      assertEquals(term.name.value, "ordinary")
      assert(allTrees(term).forall(_.pos == Position.None))
      assertEquals(project(term), TermShape.Select(receiverShape, "ordinary"))
    }

    assertEquals(static.structure, runtime.structure)
    assertEquals(runtime.structure, validated.structure)
    val shapes = List(static, runtime, validated).map(project)
    assertEquals(shapes.distinct.size, 1)
    assertEquals(shapes.head.render, "Select(Ident(receiver), ordinary)")
    assertEquals(
      TermShapeTraversal.identifierEntries(shapes.head).map(_.name),
      Vector("receiver")
    )
    assertEquals(TermShapeTraversal.nonIdentifierFields(shapes.head), Vector("ordinary"))

  test("fresh and parsed selected-name spellings share structure but not source provenance"):
    val fixtures = List(
      ("ordinary", "receiver.ordinary"),
      ("+", "receiver.+"),
      ("type", "receiver.`type`"),
      ("safe spaced name", "receiver.`safe spaced name`")
    )

    fixtures.foreach { (decoded, source) =>
      val fresh = freshSelect(decoded)
      val parsed = Input.String(source).parse[Term].get
      val parsedSelect = parsed match
        case selection: Term.Select => selection
        case other => fail(s"expected parsed Select for $source, got ${other.structure}")

      assertEquals(fresh.structure, parsedSelect.structure, clues(source))
      assertEquals(fresh.name.value, decoded, clues(source))
      assertEquals(parsedSelect.name.value, decoded, clues(source))
      assert(allTrees(fresh).forall(_.pos == Position.None), clues(source))
      assertNotEquals(parsedSelect.pos, Position.None, clues(source))
      assertEquals(parsedSelect.pos.start, 0, clues(source))
      assertEquals(parsedSelect.pos.end, source.length, clues(source))
    }

  test("the selected-name lexical matrix is broader than neutral Select admission"):
    val admitted = freshSelect("ordinary")
    assertEquals(
      project(admitted),
      TermShape.Select(receiverShape, "ordinary")
    )
    val authored = author(TermShape.Select(receiverShape, "ordinary"))
    assertEquals(authored.structure, admitted.structure)
    assert(allTrees(authored).forall(_.pos == Position.None))
    assertEquals(
      ScalametaTermProjection.project(authored),
      Right(ProjectedTermShape(TermShape.Select(receiverShape, "ordinary"), None))
    )

    val source = "receiver.ordinary"
    val parsed = Input.String(source).parse[Term].get
    assertEquals(
      ScalametaTermProjection.project(parsed),
      Right(
        ProjectedTermShape(
          TermShape.Select(receiverShape, "ordinary"),
          Some(NeutralSourceSpan(0, source.length))
        )
      )
    )

    List("+", "type", "safe spaced name").foreach { decoded =>
      val carrier = selected(decoded)
      val fresh = freshSelect(carrier.decoded)

      assertEquals(fresh.name.value, decoded)
      assertProjectionCode(fresh, "NEUTRAL_SELECTION_NAME_UNSUPPORTED")
      assertAuthoringCode(
        TermShape.Select(receiverShape, decoded),
        "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED"
      )
    }

  test("the conservative carrier rejects control cases that direct Scalameta names retain as values"):
    val invalid = List(
      "$plus" -> "encoded-name",
      "literal`tick" -> "lexical-escape",
      "line\nbreak" -> "control-character",
      "member.name" -> "dotted-name",
      "<init>" -> "compiler-special-name",
      "naïve" -> "unicode-name",
      "two  spaces" -> "unsupported-grammar"
    )

    invalid.foreach { (decoded, expectedCode) =>
      assertEquals(SelectedMemberName.from(decoded).left.toOption.map(_.code), Some(expectedCode))

      val fresh = freshSelect(decoded)
      assertEquals(fresh.name.value, decoded)
      assert(allTrees(fresh).forall(_.pos == Position.None))
      assertProjectionCode(fresh, "NEUTRAL_SELECTION_NAME_UNSUPPORTED")
    }

  test("current TermTemplate scalar metadata cannot address Select.name"):
    val generated = "__selected_name_transport"
    val root = TermShape.Select(receiverShape, generated)
    val holeIndex = GeneratedHoleIndex.fromOccurrences(
      Vector(
        HoleOccurrence(
          "selectedName",
          generated,
          SourceSpan(0, 1),
          SourceSpan(0, 1),
          HoleRole.TermTemplate
        )
      )
    )

    assertEquals(
      TermShapeTraversal.identifierEntries(root).map(_.name),
      Vector("receiver")
    )
    assertEquals(TermShapeTraversal.nonIdentifierFields(root), Vector(generated))
    assertEquals(
      TermTemplate.create(
        root,
        holeIndex,
        Vector(TermHoleOccurrence("selectedName", identifierOrdinal = 0)),
        GeneratedHoleIndex.empty,
        Vector.empty
      ),
      Left(TermConstructionError.InvalidTermHolePosition("selectedName"))
    )

  private def freshSelect(decoded: String): Term.Select =
    Term.Select(Term.Name("receiver"), Term.Name(decoded))

  private def selected(decoded: String): SelectedMemberName =
    SelectedMemberName.from(decoded).fold(error => fail(error.message), identity)

  private def project(term: Term): TermShape =
    ScalametaTermProjection.project(term) match
      case Right(value) => value.shape
      case Left(error) => fail(error.message)

  private def author(shape: TermShape): Term =
    ScalametaTermShapeAuthoring.author(shape) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def assertProjectionCode(term: Term, expected: String): Unit =
    assertEquals(
      ScalametaTermProjection.project(term).left.toOption.map(_.code),
      Some(expected),
      clues(term.structure)
    )

  private def assertAuthoringCode(shape: TermShape, expected: String): Unit =
    assertEquals(
      ScalametaTermShapeAuthoring.author(shape).left.toOption.map(_.code),
      Some(expected),
      clues(shape.render)
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
