package quasiquotes.terms

import quasiquotes.construct.SelectedMemberName
import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.source.{GeneratedSourceMap, SourceId}
import quasiquotes.types.{TypeNormalForm, TypeTemplate}
import scala.compiletime.testing.typeCheckErrors

final class TermTemplateSelectedNameCompletionTest extends munit.FunSuite:
  import TermCoreTestFixtures.*

  private val selectedTransport = "__selected_name_transport"
  private val scalarTransport = "__scalar_term_transport"
  private val repeatedTransport = "__repeated_term_transport"
  private val typeTransport = "__type_transport"

  private def selected(value: String): SelectedMemberName =
    SelectedMemberName.from(value).fold(error => fail(error.message), identity)

  private def constructed(shape: TermShape): ConstructedTerm =
    ConstructedTerm.fromShape(shape).fold(error => fail(error.message), identity)

  private def selectedTemplate(
      root: TermShape,
      selectedEntries: Vector[(String, String)] =
        Vector("member" -> selectedTransport),
      selectedOccurrences: Vector[SelectedNameHoleOccurrence] =
        Vector(SelectedNameHoleOccurrence("member", 0)),
      scalarEntries: Vector[(String, String)] = Vector.empty,
      scalarOccurrences: Vector[TermHoleOccurrence] = Vector.empty,
      repeatedEntries: Vector[(String, String)] = Vector.empty,
      repeatedOccurrences: Vector[RepeatedTermHoleOccurrence] = Vector.empty,
      typeEntries: Vector[(String, String)] = Vector.empty,
      ascriptions: Vector[TypeTemplate] = Vector.empty
  ): Either[TermConstructionError, TermTemplate] =
    TermTemplate.createWithSelectedNameHole(
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
      template: TermTemplate,
      member: String,
      scalar: Map[String, ConstructedTerm] = Map.empty,
      repeated: Map[String, Vector[ConstructedTerm]] = Map.empty,
      types: Map[String, TypeNormalForm] = Map.empty
  ): Either[TermConstructionError, ConstructedTerm] =
    template.completeWithSelectedNames(
      scalar,
      repeated,
      Map("member" -> selected(member)),
      types
    )

  test("completes the one selected-name hole with the neutral plain-name intersection"):
    val template = selectedTemplate(
      TermShape.Select(ident("service"), selectedTransport)
    ).fold(error => fail(error.message), identity)

    Vector("ordinary", "member2", "_privateLike").foreach { name =>
      assertEquals(
        complete(template, name).map(_.root),
        Right(TermShape.Select(ident("service"), name))
      )
    }
    assertEquals(template.requiredSelectedNameBindings, Vector("member"))
    assert(template.render.contains("selectedNameHoles=[member@0]"))
    val nested = selectedTemplate(
      TermShape.Select(
        TermShape.Select(ident("outer"), "inner"),
        selectedTransport
      )
    ).fold(error => fail(error.message), identity)
    assertEquals(
      complete(nested, "leaf").map(_.root),
      Right(
        TermShape.Select(
          TermShape.Select(ident("outer"), "inner"),
          "leaf"
        )
      )
    )


  test("uses a dedicated Select preorder independent of identifier and generic field ordinals"):
    val root = TermShape.Apply(
      TermShape.Select(
        TermShape.Select(ident("service"), selectedTransport),
        "fixedOuter"
      ),
      List(ident("argument"))
    )
    val template = selectedTemplate(
      root,
      selectedOccurrences = Vector(SelectedNameHoleOccurrence("member", 1))
    )
      .fold(error => fail(error.message), identity)

    assertEquals(
      complete(template, "renamed").map(_.root),
      Right(
        TermShape.Apply(
          TermShape.Select(
            TermShape.Select(ident("service"), "renamed"),
            "fixedOuter"
          ),
          List(ident("argument"))
        )
      )
    )

  test("coexists with scalar repeated and type completion without changing cardinalities"):
    val root = TermShape.Typed(
      TermShape.Apply(
        TermShape.Select(ident("service"), selectedTransport),
        List(ident(scalarTransport), ident(repeatedTransport))
      ),
      typeTransport
    )
    val template = selectedTemplate(
      root,
      scalarEntries = Vector("value" -> scalarTransport),
      scalarOccurrences = Vector(TermHoleOccurrence("value", 1)),
      repeatedEntries = Vector("args" -> repeatedTransport),
      repeatedOccurrences = Vector(RepeatedTermHoleOccurrence("args", 2)),
      typeEntries = Vector("tpe" -> typeTransport),
      ascriptions = Vector(TypeTemplate.TTHole("tpe"))
    ).fold(error => fail(error.message), identity)
    val scalar = constructed(TermShape.Literal("1"))
    val repeated = Vector(constructed(ident("left")), constructed(ident("right")))
    val completed = complete(
      template,
      "invoke",
      scalar = Map("value" -> scalar),
      repeated = Map("args" -> repeated),
      types = Map("tpe" -> TypeNormalForm.STypeIdent("Int"))
    ).fold(error => fail(error.message), identity)

    assertEquals(
      completed.root,
      TermShape.Typed(
        TermShape.Apply(
          TermShape.Select(ident("service"), "invoke"),
          List(TermShape.Literal("1"), ident("left"), ident("right"))
        ),
        "Int"
      )
    )
    assertEquals(completed.ascriptionTypes, Vector(TypeNormalForm.STypeIdent("Int")))

  test("preserves bound-reference scope while changing only the selected name"):
    val binder = BinderId(7)
    val root = TermShape.Lambda1(
      binder,
      "receiver",
      "Int",
      TermShape.Select(
        TermShape.BoundReference(binder, "receiver"),
        selectedTransport
      )
    )
    val template = selectedTemplate(
      root,
      ascriptions = Vector(TypeTemplate.TTIdent("Int"))
    ).fold(error => fail(error.message), identity)

    assertEquals(
      complete(template, "value").map(_.root),
      Right(
        TermShape.Lambda1(
          binder,
          "receiver",
          "Int",
          TermShape.Select(
            TermShape.BoundReference(binder, "receiver"),
            "value"
          )
        )
      )
    )

  test("rejects symbolic keyword spaced and wildcard selected-name bindings"):
    val template = selectedTemplate(
      TermShape.Select(ident("service"), selectedTransport)
    ).fold(error => fail(error.message), identity)

    Vector("+", "type", "safe spaced name", "_").foreach { name =>
      assertEquals(
        complete(template, name),
        Left(TermConstructionError.SelectedNameLexicalUnsupported(name))
      )
    }
    Vector("$encoded", "literal`tick", "line\nbreak", "member.name", "naïve")
      .foreach(value => assert(SelectedMemberName.from(value).isLeft))

  test("the selected-name binding API rejects raw Strings at compile time"):
    assert(
      typeCheckErrors(
        """val bindings: Map[String, quasiquotes.construct.SelectedMemberName] = Map("member" -> "raw")"""
      ).nonEmpty
    )


  test("reports deterministic missing extra null and category-conflict failures"):
    val template = selectedTemplate(
      TermShape.Select(ident("service"), selectedTransport)
    ).fold(error => fail(error.message), identity)

    assertEquals(
      template.completeWithSelectedNames(Map.empty, Map.empty, Map.empty, Map.empty),
      Left(TermConstructionError.MissingSelectedNameBinding("member"))
    )
    assertEquals(
      template.completeWithSelectedNames(
        Map.empty,
        Map.empty,
        Map("member" -> selected("value"), "extra" -> selected("other")),
        Map.empty
      ),
      Left(TermConstructionError.ExtraSelectedNameBinding("extra"))
    )
    assertEquals(
      template.completeWithSelectedNames(
        Map.empty,
        Map.empty,
        Map("member" -> null),
        Map.empty
      ),
      Left(
        TermConstructionError.InvalidSelectedNameBinding(
          "member",
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
      Left(TermConstructionError.InvalidSelectedNameBinding("<bindings>", "selected-name binding map is null"))
    )


    val scalarConflict = selectedTemplate(
      TermShape.Apply(
        TermShape.Select(ident("service"), selectedTransport),
        List(ident(scalarTransport))
      ),
      scalarEntries = Vector("member" -> scalarTransport),
      scalarOccurrences = Vector(TermHoleOccurrence("member", 1))
    )
    assertEquals(
      scalarConflict,
      Left(TermConstructionError.SelectedNameHoleCategoryConflict("member"))
    )

    val repeatedConflict = selectedTemplate(
      TermShape.Apply(
        TermShape.Select(ident("service"), selectedTransport),
        List(ident(repeatedTransport))
      ),
      repeatedEntries = Vector("member" -> repeatedTransport),
      repeatedOccurrences = Vector(
        RepeatedTermHoleOccurrence("member", 1)
      )
    )
    assertEquals(
      repeatedConflict,
      Left(TermConstructionError.SelectedNameHoleCategoryConflict("member"))
    )

  test("selected-name and type bindings may retain the established same logical text"):
    val template = selectedTemplate(
      TermShape.Typed(
        TermShape.Select(ident("service"), selectedTransport),
        typeTransport
      ),
      typeEntries = Vector("member" -> typeTransport),
      ascriptions = Vector(TypeTemplate.TTHole("member"))
    ).fold(error => fail(error.message), identity)

    assertEquals(
      complete(
        template,
        "value",
        types = Map("member" -> TypeNormalForm.STypeIdent("Int"))
      ).map(_.root),
      Right(TermShape.Typed(TermShape.Select(ident("service"), "value"), "Int"))
    )

  test("rejects missing duplicate and misplaced selected-name occurrence metadata"):
    val missing = selectedTemplate(
      TermShape.Select(ident("service"), selectedTransport),
      selectedOccurrences = Vector.empty
    )
    val duplicateSameName = selectedTemplate(
      TermShape.Select(
        TermShape.Select(ident("service"), selectedTransport),
        selectedTransport
      ),
      selectedOccurrences = Vector(
        SelectedNameHoleOccurrence("member", 0),
        SelectedNameHoleOccurrence("member", 1)
      )
    )
    val duplicateDifferentNames = selectedTemplate(
      TermShape.Select(
        TermShape.Select(ident("service"), "__first_selected"),
        "__second_selected"
      ),
      selectedEntries = Vector(
        "first" -> "__first_selected",
        "second" -> "__second_selected"
      ),
      selectedOccurrences = Vector(
        SelectedNameHoleOccurrence("first", 0),
        SelectedNameHoleOccurrence("second", 1)
      )
    )
    val identifierPosition = selectedTemplate(ident(selectedTransport))
    val wrongOrdinal = selectedTemplate(
      TermShape.Select(ident("service"), selectedTransport),
      selectedOccurrences = Vector(SelectedNameHoleOccurrence("member", 1))
    )

    assertEquals(
      missing,
      Left(TermConstructionError.InvalidSelectedNameHolePosition("member"))
    )
    assertEquals(
      duplicateSameName,
      Left(TermConstructionError.DuplicateSelectedNameHole())
    )
    assertEquals(
      duplicateDifferentNames,
      Left(TermConstructionError.DuplicateSelectedNameHole())
    )
    assertEquals(
      identifierPosition,
      Left(TermConstructionError.InvalidSelectedNameHolePosition("member"))
    )
    assertEquals(
      wrongOrdinal,
      Left(TermConstructionError.InvalidSelectedNameHolePosition("member"))
    )

  test("selected-name semantic identity ignores transport but preserves role and logical name"):
    def one(logical: String, transport: String): TermTemplate =
      selectedTemplate(
        TermShape.Select(ident("service"), transport),
        selectedEntries = Vector(logical -> transport),
        selectedOccurrences = Vector(SelectedNameHoleOccurrence(logical, 0))
      ).fold(error => fail(error.message), identity)

    val first = one("member", "__selected_a")
    val second = one("member", "__selected_b")
    val otherLogicalName = one("other", "__selected_c")
    val fixed = template(
      TermShape.Select(ident("service"), "member")
    ).fold(error => fail(error.message), identity)

    assertEquals(first, second)
    assertEquals(first.hashCode, second.hashCode)
    assertNotEquals(first, otherLogicalName)
    assertNotEquals(first, fixed)

  test("located source metadata rejects selected-name templates without inventing a span"):
    val template = selectedTemplate(
      TermShape.Select(ident("service"), selectedTransport)
    ).fold(error => fail(error.message), identity)
    val sourceMap = GeneratedSourceMap(
      "",
      SourceId("selected-name-template"),
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
