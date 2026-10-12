package quasiquotes.terms

import quasiquotes.construct.SelectedMemberName
import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.source.*
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

import scala.compiletime.testing.typeCheckErrors

final class N069TwoDistinctSelectedNameTermTemplateTest
    extends munit.FunSuite:
  import TermCoreTestFixtures.*

  private val firstTransport = "__n069_first"
  private val secondTransport = "__n069_second"
  private val scalarTransport = "__n069_scalar"
  private val repeatedTransport = "__n069_repeated"
  private val typeTransport = "__n069_type"

  test("completes exactly two distinct selected-name holes in root-first order"):
    val template = twoSelectedTemplate(
      selectedChain(firstTransport, secondTransport)
    ).fold(error => fail(error.message), identity)

    assertEquals(
      template.requiredSelectedNameBindings,
      Vector("second", "first")
    )
    assertEquals(
      template
        .completeWithSelectedNames(
          Map.empty,
          Map.empty,
          Map(
            "first" -> selected("inner"),
            "second" -> selected("outer")
          ),
          Map.empty
        )
        .map(_.root),
      Right(selectedChain("inner", "outer"))
    )

  test("completes adjacent holes in direct nullary and unary selected chains"):
    val direct = selectedChain(firstTransport, secondTransport)
    val roots = Vector(
      direct,
      TermShape.Apply(direct, Nil),
      TermShape.Apply(direct, List(TermShape.Literal("1")))
    )
    val expected = Vector(
      selectedChain("inner", "outer"),
      TermShape.Apply(selectedChain("inner", "outer"), Nil),
      TermShape.Apply(
        selectedChain("inner", "outer"),
        List(TermShape.Literal("1"))
      )
    )

    roots.zip(expected).foreach { case (root, wanted) =>
      val template = twoSelectedTemplate(root).fold(
        error => fail(error.message),
        identity
      )
      assertEquals(complete(template).map(_.root), Right(wanted))
    }

  test("uses root-first non-adjacent ordinals with a fixed middle name"):
    val root = TermShape.Select(
      TermShape.Select(
        TermShape.Select(ident("receiver"), firstTransport),
        "fixed"
      ),
      secondTransport
    )
    val template = twoSelectedTemplate(
      root,
      selectedOccurrences = Vector(
        SelectedNameHoleOccurrence("first", 2),
        SelectedNameHoleOccurrence("second", 0)
      )
    ).fold(error => fail(error.message), identity)

    assertEquals(
      complete(template).map(_.root),
      Right(
        TermShape.Select(
          TermShape.Select(
            TermShape.Select(ident("receiver"), "inner"),
            "fixed"
          ),
          "outer"
        )
      )
    )

  test("preserves scalar repeated and Type completion through the same engine"):
    val root = TermShape.Typed(
      TermShape.Apply(
        selectedChain(firstTransport, secondTransport),
        List(ident(scalarTransport), ident(repeatedTransport))
      ),
      typeTransport
    )
    val template = twoSelectedTemplate(
      root,
      scalarEntries = Vector("value" -> scalarTransport),
      scalarOccurrences = Vector(TermHoleOccurrence("value", 1)),
      repeatedEntries = Vector("args" -> repeatedTransport),
      repeatedOccurrences =
        Vector(RepeatedTermHoleOccurrence("args", 2)),
      typeEntries = Vector("result" -> typeTransport),
      ascriptions = Vector(TypeTemplate.TTHole("result"))
    ).fold(error => fail(error.message), identity)

    val completed = template
      .completeWithSelectedNames(
        Map("value" -> constructed(TermShape.Literal("1"))),
        Map(
          "args" -> Vector(
            constructed(ident("left")),
            constructed(ident("right"))
          )
        ),
        selectedBindings,
        Map("result" -> TypeNormalForm.STypeIdent("Int"))
      )
      .fold(error => fail(error.message), identity)

    assertEquals(
      completed.root,
      TermShape.Typed(
        TermShape.Apply(
          selectedChain("inner", "outer"),
          List(TermShape.Literal("1"), ident("left"), ident("right"))
        ),
        "Int"
      )
    )
    assertEquals(
      completed.ascriptionTypes,
      Vector(TypeNormalForm.STypeIdent("Int"))
    )

  test("preserves Lambda1 binder identity BoundReference and typed sidecars"):
    val binder = BinderId(69069)
    val root = TermShape.Lambda1(
      binder,
      "value",
      "Int",
      TermShape.Typed(
        TermShape.Select(
          TermShape.Select(
            TermShape.BoundReference(binder, "value"),
            firstTransport
          ),
          secondTransport
        ),
        typeTransport
      )
    )
    val template = twoSelectedTemplate(
      root,
      typeEntries = Vector("result" -> typeTransport),
      ascriptions = Vector(
        TypeTemplate.TTIdent("Int"),
        TypeTemplate.TTHole("result")
      )
    ).fold(error => fail(error.message), identity)
    val completed = template
      .completeWithSelectedNames(
        Map.empty,
        Map.empty,
        selectedBindings,
        Map("result" -> TypeNormalForm.STypeIdent("String"))
      )
      .fold(error => fail(error.message), identity)

    assertEquals(
      completed.root,
      TermShape.Lambda1(
        binder,
        "value",
        "Int",
        TermShape.Typed(
          TermShape.Select(
            TermShape.Select(
              TermShape.BoundReference(binder, "value"),
              "inner"
            ),
            "outer"
          ),
          "String"
        )
      )
    )
    assertEquals(
      completed.ascriptionTypes,
      Vector(
        TypeNormalForm.STypeIdent("Int"),
        TypeNormalForm.STypeIdent("String")
      )
    )

  test("requires exactly two distinct logical names occurrences ordinals and transports"):
    val root = selectedChain(firstTransport, secondTransport)

    assertEquals(
      twoSelectedTemplate(
        root,
        selectedEntries = Vector.empty,
        selectedOccurrences = Vector.empty
      ),
      Left(
        TermConstructionError.SelectedNameHoleCardinalityMismatch(2, 0, 0)
      )
    )
    assertEquals(
      twoSelectedTemplate(
        root,
        selectedEntries = Vector("first" -> firstTransport),
        selectedOccurrences =
          Vector(SelectedNameHoleOccurrence("first", 1))
      ),
      Left(
        TermConstructionError.SelectedNameHoleCardinalityMismatch(2, 1, 1)
      )
    )
    assertEquals(
      twoSelectedTemplate(
        TermShape.Select(root, "__n069_third"),
        selectedEntries = Vector(
          "first" -> firstTransport,
          "second" -> secondTransport,
          "third" -> "__n069_third"
        ),
        selectedOccurrences = Vector(
          SelectedNameHoleOccurrence("first", 1),
          SelectedNameHoleOccurrence("second", 0),
          SelectedNameHoleOccurrence("third", 0)
        )
      ),
      Left(
        TermConstructionError.SelectedNameHoleCardinalityMismatch(2, 3, 3)
      )
    )
    assertEquals(
      twoSelectedTemplate(
        root,
        selectedOccurrences = Vector(
          SelectedNameHoleOccurrence("first", 1),
          SelectedNameHoleOccurrence("first", 0)
        )
      ),
      Left(TermConstructionError.DuplicateSelectedNameLogicalName("first"))
    )
    assertEquals(
      twoSelectedTemplate(
        root,
        selectedOccurrences = Vector(
          SelectedNameHoleOccurrence("first", 0),
          SelectedNameHoleOccurrence("second", 0)
        )
      ),
      Left(TermConstructionError.DuplicateSelectedNameHoleAddress(0))
    )

    val duplicateTransport = intercept[IllegalArgumentException] {
      index("first" -> firstTransport, "second" -> firstTransport)
    }
    assert(
      duplicateTransport.getMessage.contains(
        "A generated hole identifier must map to exactly one semantic hole name"
      )
    )

  test("rejects mismatched Select positions illicit transport reuse and category collisions"):
    val root = selectedChain(firstTransport, secondTransport)
    assertEquals(
      twoSelectedTemplate(
        root,
        selectedOccurrences = Vector(
          SelectedNameHoleOccurrence("first", 0),
          SelectedNameHoleOccurrence("second", 1)
        )
      ),
      Left(TermConstructionError.InvalidSelectedNameHolePosition("first"))
    )
    assertEquals(
      twoSelectedTemplate(
        TermShape.Apply(root, List(ident(firstTransport))),
        scalarOccurrences = Vector.empty
      ),
      Left(TermConstructionError.InvalidSelectedNameHolePosition("first"))
    )
    assertEquals(
      twoSelectedTemplate(
        TermShape.Apply(root, List(ident(scalarTransport))),
        scalarEntries = Vector("first" -> scalarTransport),
        scalarOccurrences = Vector(TermHoleOccurrence("first", 1))
      ),
      Left(TermConstructionError.SelectedNameHoleCategoryConflict("first"))
    )
    assertEquals(
      twoSelectedTemplate(
        TermShape.Apply(root, List(ident(repeatedTransport))),
        repeatedEntries = Vector("second" -> repeatedTransport),
        repeatedOccurrences =
          Vector(RepeatedTermHoleOccurrence("second", 1))
      ),
      Left(TermConstructionError.SelectedNameHoleCategoryConflict("second"))
    )
    assertEquals(
      twoSelectedTemplate(
        root,
        typeEntries = Vector("typeValue" -> firstTransport)
      ),
      Left(TermConstructionError.DuplicateGeneratedIdentifier(firstTransport))
    )

  test("requires the exact typed binding set and current neutral lexical intersection"):
    val template = twoSelectedTemplate(
      selectedChain(firstTransport, secondTransport)
    ).fold(error => fail(error.message), identity)

    assertEquals(
      template.completeWithSelectedNames(
        Map.empty,
        Map.empty,
        Map("first" -> selected("inner")),
        Map.empty
      ),
      Left(TermConstructionError.MissingSelectedNameBinding("second"))
    )
    assertEquals(
      template.completeWithSelectedNames(
        Map.empty,
        Map.empty,
        selectedBindings + ("extra" -> selected("extra")),
        Map.empty
      ),
      Left(TermConstructionError.ExtraSelectedNameBinding("extra"))
    )
    assertEquals(
      template.completeWithSelectedNames(
        Map.empty,
        Map.empty,
        Map("first" -> selected("inner"), "second" -> null),
        Map.empty
      ),
      Left(
        TermConstructionError.InvalidSelectedNameBinding(
          "second",
          "binding value is null"
        )
      )
    )
    assertEquals(
      template.completeWithSelectedNames(
        Map.empty,
        Map.empty,
        null,
        Map.empty
      ),
      Left(
        TermConstructionError.InvalidSelectedNameBinding(
          "<bindings>",
          "selected-name binding map is null"
        )
      )
    )
    Vector("+", "type", "two words", "_").foreach { invalid =>
      assertEquals(
        template.completeWithSelectedNames(
          Map.empty,
          Map.empty,
          Map(
            "first" -> selected(invalid),
            "second" -> selected("ordinary")
          ),
          Map.empty
        ),
        Left(TermConstructionError.SelectedNameLexicalUnsupported(invalid))
      )
    }
    Vector("naïve", "$internal").foreach { invalid =>
      assert(SelectedMemberName.from(invalid).isLeft)
    }
    assert(
      typeCheckErrors(
        """val bindings: Map[String, quasiquotes.construct.SelectedMemberName] = Map("first" -> "raw", "second" -> "raw")"""
      ).nonEmpty
    )

  test("semantic equality ignores transports but retains logical identity and position"):
    def accepted(
        firstLogical: String,
        secondLogical: String,
        firstGenerated: String,
        secondGenerated: String,
        firstOrdinal: Int,
        secondOrdinal: Int
    ): TermTemplate =
      twoSelectedTemplate(
        selectedChain(firstGenerated, secondGenerated),
        selectedEntries = Vector(
          firstLogical -> firstGenerated,
          secondLogical -> secondGenerated
        ),
        selectedOccurrences = Vector(
          SelectedNameHoleOccurrence(firstLogical, firstOrdinal),
          SelectedNameHoleOccurrence(secondLogical, secondOrdinal)
        )
      ).fold(error => fail(error.message), identity)

    val original = accepted(
      "first",
      "second",
      "__first_a",
      "__second_a",
      1,
      0
    )
    val renamed = accepted(
      "first",
      "second",
      "__first_b",
      "__second_b",
      1,
      0
    )
    val moved = twoSelectedTemplate(
      selectedChain("__second_c", "__first_c"),
      selectedEntries = Vector(
        "first" -> "__first_c",
        "second" -> "__second_c"
      ),
      selectedOccurrences = Vector(
        SelectedNameHoleOccurrence("first", 0),
        SelectedNameHoleOccurrence("second", 1)
      )
    ).fold(error => fail(error.message), identity)
    val otherLogical = accepted(
      "other",
      "second",
      "__first_d",
      "__second_d",
      1,
      0
    )

    assertEquals(original, renamed)
    assertEquals(original.hashCode, renamed.hashCode)
    assertNotEquals(original, moved)
    assertNotEquals(original, otherLogical)

  test("the N059 constructor still rejects two holes with its original category"):
    val result = TermTemplate.createWithSelectedNameHole(
      selectedChain(firstTransport, secondTransport),
      GeneratedHoleIndex.empty,
      Vector.empty,
      GeneratedHoleIndex.empty,
      Vector.empty,
      index("first" -> firstTransport, "second" -> secondTransport),
      Vector(
        SelectedNameHoleOccurrence("first", 1),
        SelectedNameHoleOccurrence("second", 0)
      ),
      GeneratedHoleIndex.empty,
      Vector.empty
    )

    assertEquals(
      result,
      Left(TermConstructionError.DuplicateSelectedNameHole())
    )

  test("located metadata remains fail-closed for selected-name templates"):
    val template = twoSelectedTemplate(
      selectedChain(firstTransport, secondTransport)
    ).fold(error => fail(error.message), identity)
    val sourceMap = GeneratedSourceMap(
      "",
      SourceId("n069-selected-name-template"),
      Vector.empty
    )

    assertEquals(
      LocatedTermTemplate.create(
        template,
        sourceMap,
        Vector.empty,
        Vector.empty
      ),
      Left(
        TermConstructionError.InvalidLocatedTemplateMetadata(
          "located scalar term templates do not support selected-name metadata"
        )
      )
    )

  private def twoSelectedTemplate(
      root: TermShape,
      scalarEntries: Vector[(String, String)] = Vector.empty,
      scalarOccurrences: Vector[TermHoleOccurrence] = Vector.empty,
      repeatedEntries: Vector[(String, String)] = Vector.empty,
      repeatedOccurrences: Vector[RepeatedTermHoleOccurrence] = Vector.empty,
      selectedEntries: Vector[(String, String)] =
        Vector("first" -> firstTransport, "second" -> secondTransport),
      selectedOccurrences: Vector[SelectedNameHoleOccurrence] =
        Vector(
          SelectedNameHoleOccurrence("first", 1),
          SelectedNameHoleOccurrence("second", 0)
        ),
      typeEntries: Vector[(String, String)] = Vector.empty,
      ascriptions: Vector[TypeTemplate] = Vector.empty
  ): Either[TermConstructionError, TermTemplate] =
    TermTemplate.createWithTwoSelectedNameHoles(
      root,
      index(scalarEntries*),
      scalarOccurrences,
      index(repeatedEntries*),
      repeatedOccurrences,
      index(selectedEntries*),
      selectedOccurrences,
      index(typeEntries*),
      ascriptions
    )

  private def complete(
      template: TermTemplate
  ): Either[TermConstructionError, ConstructedTerm] =
    template.completeWithSelectedNames(
      Map.empty,
      Map.empty,
      selectedBindings,
      Map.empty
    )

  private def selectedBindings: Map[String, SelectedMemberName] =
    Map("first" -> selected("inner"), "second" -> selected("outer"))

  private def selectedChain(first: String, second: String): TermShape =
    TermShape.Select(
      TermShape.Select(ident("receiver"), first),
      second
    )

  private def selected(value: String): SelectedMemberName =
    SelectedMemberName.from(value).fold(error => fail(error.message), identity)

  private def constructed(shape: TermShape): ConstructedTerm =
    ConstructedTerm.fromShape(shape).fold(
      error => fail(error.message),
      identity
    )
