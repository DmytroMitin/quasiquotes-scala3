package quasiquotes.terms

import quasiquotes.construct.SelectedMemberName
import quasiquotes.matching.{SelectedNameSourceNormalizer, SelectedNameSourcePosition}
import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.source.*
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

import scala.compiletime.testing.typeCheckErrors

final class N071TwoRepeatedSelectedNameTermTemplateTest
    extends munit.FunSuite:
  import TermCoreTestFixtures.*

  private val innerTransport = "__n071_inner"
  private val outerTransport = "__n071_outer"
  private val scalarTransport = "__n071_scalar"
  private val repeatedTransport = "__n071_repeated"
  private val typeTransport = "__n071_type"

  test("one logical selected-name binding completes exactly two occurrence transports"):
    val template = repeatedTemplate().fold(error => fail(error.message), identity)

    assertEquals(template.requiredSelectedNameBindings, Vector("member"))
    assertEquals(
      complete(template).map(_.root),
      Right(selectedChain("same", "same"))
    )

  test("completes direct nullary and unary envelopes after ordinal normalization"):
    val direct = selectedChain(innerTransport, outerTransport)
    val cases = Vector(
      direct -> selectedChain("same", "same"),
      TermShape.Apply(direct, Nil) ->
        TermShape.Apply(selectedChain("same", "same"), Nil),
      TermShape.Apply(direct, List(TermShape.Literal("1"))) ->
        TermShape.Apply(
          selectedChain("same", "same"),
          List(TermShape.Literal("1"))
        )
    )

    cases.foreach { case (root, expected) =>
      val template = repeatedTemplate(
        root,
        Vector(
          occurrence("member", 1, innerTransport),
          occurrence("member", 0, outerTransport)
        )
      ).fold(error => fail(error.message), identity)
      assertEquals(template.selectedNameHoleOccurrences.map(_.selectOrdinal), Vector(0, 1))
      assertEquals(complete(template).map(_.root), Right(expected))
    }

  test("preserves fixed selected names and nested qualifiers"):
    val root = TermShape.Select(
      TermShape.Select(
        TermShape.Select(
          TermShape.Select(ident("receiver"), "namespace"),
          innerTransport
        ),
        "fixed"
      ),
      outerTransport
    )
    val template = repeatedTemplate(
      root,
      Vector(
        occurrence("member", 2, innerTransport),
        occurrence("member", 0, outerTransport)
      )
    ).fold(error => fail(error.message), identity)

    assertEquals(
      complete(template).map(_.root),
      Right(
        TermShape.Select(
          TermShape.Select(
            TermShape.Select(
              TermShape.Select(ident("receiver"), "namespace"),
              "same"
            ),
            "fixed"
          ),
          "same"
        )
      )
    )

  test("preserves scalar repeated and Type completion on binder-free shapes"):
    val root = TermShape.Typed(
      TermShape.Apply(
        selectedChain(innerTransport, outerTransport),
        List(ident(scalarTransport), ident(repeatedTransport))
      ),
      typeTransport
    )
    val template = repeatedTemplate(
      root,
      scalarEntries = Vector("value" -> scalarTransport),
      scalarOccurrences = Vector(TermHoleOccurrence("value", 1)),
      repeatedEntries = Vector("args" -> repeatedTransport),
      repeatedOccurrences = Vector(RepeatedTermHoleOccurrence("args", 2)),
      typeEntries = Vector("member" -> typeTransport),
      ascriptions = Vector(TypeTemplate.TTHole("member"))
    ).fold(error => fail(error.message), identity)

    val completed = template.completeWithSelectedNames(
      Map("value" -> constructed(TermShape.Literal("1"))),
      Map(
        "args" -> Vector(
          constructed(ident("left")),
          constructed(ident("right"))
        )
      ),
      Map("member" -> selected("same")),
      Map("member" -> TypeNormalForm.STypeIdent("String"))
    ).fold(error => fail(error.message), identity)

    assertEquals(
      completed.root,
      TermShape.Typed(
        TermShape.Apply(
          selectedChain("same", "same"),
          List(TermShape.Literal("1"), ident("left"), ident("right"))
        ),
        "String"
      )
    )
    assertEquals(
      completed.ascriptionTypes,
      Vector(TypeNormalForm.STypeIdent("String"))
    )

  test("preserves independent binder identity scope and Type sidecars"):
    val binder = BinderId(71071)
    val root = TermShape.Lambda1(
      binder,
      "value",
      "Int",
      TermShape.Typed(
        TermShape.Select(
          TermShape.Select(
            TermShape.BoundReference(binder, "value"),
            innerTransport
          ),
          outerTransport
        ),
        typeTransport
      )
    )
    val template = repeatedTemplate(
      root,
      typeEntries = Vector("member" -> typeTransport),
      ascriptions = Vector(
        TypeTemplate.TTIdent("Int"),
        TypeTemplate.TTHole("member")
      )
    ).fold(error => fail(error.message), identity)

    val completed = template.completeWithSelectedNames(
      Map.empty,
      Map.empty,
      Map("member" -> selected("same")),
      Map("member" -> TypeNormalForm.STypeIdent("String"))
    ).fold(error => fail(error.message), identity)

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
              "same"
            ),
            "same"
          ),
          "String"
        )
      )
    )

  test("semantic equality ignores transports but retains positions and logical identity"):
    val original = repeatedTemplate().fold(error => fail(error.message), identity)
    val renamed = repeatedTemplate(
      selectedChain("__n071_inner_renamed", "__n071_outer_renamed"),
      Vector(
        occurrence("member", 1, "__n071_inner_renamed"),
        occurrence("member", 0, "__n071_outer_renamed")
      )
    ).fold(error => fail(error.message), identity)
    val moved = repeatedTemplate(
      TermShape.Select(
        TermShape.Select(
          TermShape.Select(ident("receiver"), innerTransport),
          "fixed"
        ),
        outerTransport
      ),
      Vector(
        occurrence("member", 2, innerTransport),
        occurrence("member", 0, outerTransport)
      )
    ).fold(error => fail(error.message), identity)
    val otherLogical = repeatedTemplate(
      occurrences = Vector(
        occurrence("other", 1, innerTransport),
        occurrence("other", 0, outerTransport)
      )
    ).fold(error => fail(error.message), identity)

    assertEquals(original, renamed)
    assertEquals(original.hashCode, renamed.hashCode)
    assertNotEquals(original, moved)
    assertNotEquals(original, otherLogical)

  test("rejects wrong cardinality different names duplicate addresses and transports"):
    assertEquals(
      repeatedTemplate(occurrences = Vector.empty),
      Left(TermConstructionError.RepeatedSelectedNameHoleCardinalityMismatch(0))
    )
    assertEquals(
      repeatedTemplate(occurrences = Vector(occurrence("member", 0, outerTransport))),
      Left(TermConstructionError.RepeatedSelectedNameHoleCardinalityMismatch(1))
    )
    assertEquals(
      repeatedTemplate(
        TermShape.Select(selectedChain(innerTransport, outerTransport), "__n071_third"),
        repeatedOccurrences :+ occurrence("member", 0, "__n071_third")
      ),
      Left(TermConstructionError.RepeatedSelectedNameHoleCardinalityMismatch(3))
    )
    assertEquals(
      repeatedTemplate(
        occurrences = Vector(
          occurrence("first", 1, innerTransport),
          occurrence("second", 0, outerTransport)
        )
      ),
      Left(TermConstructionError.RepeatedSelectedNameLogicalNameMismatch())
    )
    assertEquals(
      repeatedTemplate(
        occurrences = Vector(
          occurrence("member", 0, innerTransport),
          occurrence("member", 0, outerTransport)
        )
      ),
      Left(TermConstructionError.DuplicateSelectedNameHoleAddress(0))
    )
    assertEquals(
      repeatedTemplate(
        selectedChain(innerTransport, innerTransport),
        Vector(
          occurrence("member", 1, innerTransport),
          occurrence("member", 0, innerTransport)
        )
      ),
      Left(TermConstructionError.DuplicateSelectedNameTransport(innerTransport))
    )

  test("rejects stale malformed reused and cross-category transports"):
    assert(
      repeatedTemplate(
        occurrences = Vector(
          occurrence("member", 0, innerTransport),
          occurrence("member", 1, outerTransport)
        )
      ).isLeft
    )
    assert(
      repeatedTemplate(
        occurrences = Vector(
          occurrence("member", -1, innerTransport),
          occurrence("member", 0, outerTransport)
        )
      ).isLeft
    )
    assert(
      repeatedTemplate(
        TermShape.Apply(
          selectedChain(innerTransport, outerTransport),
          List(ident(innerTransport))
        )
      ).isLeft
    )
    assert(
      repeatedTemplate(occurrences = null).isLeft
    )
    assert(
      repeatedTemplate(
        occurrences = Vector(repeatedOccurrences.head, null)
      ).isLeft
    )
    assert(
      repeatedTemplate(
        selectedChain(innerTransport, outerTransport),
        scalarEntries = Vector("member" -> scalarTransport),
        scalarOccurrences = Vector(TermHoleOccurrence("member", 1))
      ).isLeft
    )
    assert(
      repeatedTemplate(
        selectedChain(innerTransport, outerTransport),
        repeatedEntries = Vector("member" -> repeatedTransport),
        repeatedOccurrences = Vector(RepeatedTermHoleOccurrence("member", 1))
      ).isLeft
    )
    Vector(
      (Vector("value" -> innerTransport), Vector.empty, Vector.empty),
      (Vector.empty, Vector("args" -> innerTransport), Vector.empty),
      (Vector.empty, Vector.empty, Vector("result" -> innerTransport))
    ).foreach { case (scalarEntries, repeatedEntries, typeEntries) =>
      assert(
        repeatedTemplate(
          scalarEntries = scalarEntries,
          repeatedEntries = repeatedEntries,
          typeEntries = typeEntries
        ).isLeft
      )
    }

  test("requires one exact typed binding and retains lexical admission"):
    val template = repeatedTemplate().fold(error => fail(error.message), identity)
    assertEquals(
      template.completeWithSelectedNames(Map.empty, Map.empty, Map.empty, Map.empty),
      Left(TermConstructionError.MissingSelectedNameBinding("member"))
    )
    assertEquals(
      template.completeWithSelectedNames(
        Map.empty,
        Map.empty,
        Map("member" -> selected("same"), "extra" -> selected("extra")),
        Map.empty
      ),
      Left(TermConstructionError.ExtraSelectedNameBinding("extra"))
    )
    assert(
      template.completeWithSelectedNames(Map.empty, Map.empty, null, Map.empty).isLeft
    )
    assert(
      template.completeWithSelectedNames(
        Map.empty,
        Map.empty,
        Map("member" -> null),
        Map.empty
      ).isLeft
    )
    Vector("ordinary", "member2", "_privateLike").foreach { value =>
      assert(
        template.completeWithSelectedNames(
          Map.empty,
          Map.empty,
          Map("member" -> selected(value)),
          Map.empty
        ).isRight
      )
    }
    Vector("+", "type", "two words", "_").foreach { invalid =>
      assertEquals(
        template.completeWithSelectedNames(
          Map.empty,
          Map.empty,
          Map("member" -> selected(invalid)),
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
        """val bindings: Map[String, quasiquotes.construct.SelectedMemberName] = Map("member" -> "raw")"""
      ).nonEmpty
    )

  test("adapts accepted N068 normalized occurrence data without changing N068"):
    val normalized = SelectedNameSourceNormalizer
      .normalize(
        "$receiver.$member.$member",
        Vector(
          SelectedNameSourcePosition(1, 1),
          SelectedNameSourcePosition(2, 0)
        )
      )
      .fold(error => fail(error.message), identity)
    val byOrdinal = normalized.selectedOccurrences.map(occurrence =>
      occurrence.selectOrdinal -> occurrence
    ).toMap
    val template = repeatedTemplate(
      selectedChain(byOrdinal(1).transport, byOrdinal(0).transport),
      normalized.selectedOccurrences.map(occurrence =>
        SelectedNameHoleOccurrenceWithTransport(
          occurrence.name,
          occurrence.selectOrdinal,
          occurrence.transport
        )
      )
    ).fold(error => fail(error.message), identity)

    assertEquals(complete(template).map(_.root), Right(selectedChain("same", "same")))

  test("keeps N059 and N069 admission behavior unchanged"):
    val sharedIndex = index("member" -> innerTransport)
    assertEquals(
      TermTemplate.createWithSelectedNameHole(
        selectedChain(innerTransport, innerTransport),
        GeneratedHoleIndex.empty,
        Vector.empty,
        GeneratedHoleIndex.empty,
        Vector.empty,
        sharedIndex,
        Vector(
          SelectedNameHoleOccurrence("member", 1),
          SelectedNameHoleOccurrence("member", 0)
        ),
        GeneratedHoleIndex.empty,
        Vector.empty
      ),
      Left(TermConstructionError.DuplicateSelectedNameHole())
    )
    assertEquals(
      TermTemplate.createWithTwoSelectedNameHoles(
        selectedChain(innerTransport, innerTransport),
        GeneratedHoleIndex.empty,
        Vector.empty,
        GeneratedHoleIndex.empty,
        Vector.empty,
        sharedIndex,
        Vector(
          SelectedNameHoleOccurrence("member", 1),
          SelectedNameHoleOccurrence("member", 0)
        ),
        GeneratedHoleIndex.empty,
        Vector.empty
      ),
      Left(TermConstructionError.SelectedNameHoleCardinalityMismatch(2, 1, 2))
    )

  test("fails closed for located metadata and repeated Term holes beneath binders"):
    val template = repeatedTemplate().fold(error => fail(error.message), identity)
    assertEquals(
      LocatedTermTemplate.create(
        template,
        GeneratedSourceMap("", SourceId("n071"), Vector.empty),
        Vector.empty,
        Vector.empty
      ),
      Left(
        TermConstructionError.InvalidLocatedTemplateMetadata(
          "located scalar term templates do not support selected-name metadata"
        )
      )
    )

    val binder = BinderId(71171)
    val root = TermShape.Lambda1(
      binder,
      "value",
      "Int",
      TermShape.Apply(
        selectedChain(innerTransport, outerTransport),
        List(ident(repeatedTransport))
      )
    )
    assertEquals(
      repeatedTemplate(
        root,
        repeatedEntries = Vector("args" -> repeatedTransport),
        repeatedOccurrences = Vector(RepeatedTermHoleOccurrence("args", 1)),
        ascriptions = Vector(TypeTemplate.TTIdent("Int"))
      ),
      Left(TermConstructionError.RepeatedTermScopeUnsupported("args"))
    )

  test("rejects null root and malformed occurrence fields without throwing"):
    assert(
      repeatedTemplate(root = null).isLeft
    )
    assert(
      repeatedTemplate(
        occurrences = Vector(
          occurrence(null, 1, innerTransport),
          occurrence(null, 0, outerTransport)
        )
      ).isLeft
    )
    assert(
      repeatedTemplate(
        occurrences = Vector(
          occurrence("member", 1, null),
          occurrence("member", 0, outerTransport)
        )
      ).isLeft
    )
    assertEquals(
      repeatedTemplate(
        selectedChain("type", outerTransport),
        Vector(
          occurrence("member", 1, "type"),
          occurrence("member", 0, outerTransport)
        )
      ),
      Left(TermConstructionError.InvalidSelectedNameTransport("type"))
    )


  private def repeatedTemplate(
      root: TermShape = selectedChain(innerTransport, outerTransport),
      occurrences: Vector[SelectedNameHoleOccurrenceWithTransport] = repeatedOccurrences,
      scalarEntries: Vector[(String, String)] = Vector.empty,
      scalarOccurrences: Vector[TermHoleOccurrence] = Vector.empty,
      repeatedEntries: Vector[(String, String)] = Vector.empty,
      repeatedOccurrences: Vector[RepeatedTermHoleOccurrence] = Vector.empty,
      typeEntries: Vector[(String, String)] = Vector.empty,
      ascriptions: Vector[TypeTemplate] = Vector.empty
  ): Either[TermConstructionError, TermTemplate] =
    TermTemplate.createWithTwoRepeatedSelectedNameHoles(
      root,
      index(scalarEntries*),
      scalarOccurrences,
      index(repeatedEntries*),
      repeatedOccurrences,
      occurrences,
      index(typeEntries*),
      ascriptions
    )

  private def repeatedOccurrences
      : Vector[SelectedNameHoleOccurrenceWithTransport] =
    Vector(
      occurrence("member", 1, innerTransport),
      occurrence("member", 0, outerTransport)
    )

  private def occurrence(
      name: String,
      ordinal: Int,
      transport: String
  ): SelectedNameHoleOccurrenceWithTransport =
    SelectedNameHoleOccurrenceWithTransport(name, ordinal, transport)

  private def complete(
      template: TermTemplate
  ): Either[TermConstructionError, ConstructedTerm] =
    template.completeWithSelectedNames(
      Map.empty,
      Map.empty,
      Map("member" -> selected("same")),
      Map.empty
    )

  private def selectedChain(inner: String, outer: String): TermShape =
    TermShape.Select(TermShape.Select(ident("receiver"), inner), outer)

  private def selected(value: String): SelectedMemberName =
    SelectedMemberName.from(value).fold(error => fail(error.message), identity)

  private def constructed(shape: TermShape): ConstructedTerm =
    ConstructedTerm.fromShape(shape).fold(
      error => fail(error.message),
      identity
    )
