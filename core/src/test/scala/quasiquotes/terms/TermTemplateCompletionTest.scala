package quasiquotes.terms

import quasiquotes.parser.TermShape
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

class TermTemplateCompletionTest extends munit.FunSuite:
  import TermCoreTestFixtures.*

  private val intForm = TypeNormalForm.STypeIdent("Int")
  private val stringForm = TypeNormalForm.STypeIdent("String")
  private val booleanForm = TypeNormalForm.STypeIdent("Boolean")
  private val generatedTerm = "__term_transport"
  private val generatedType = "__type_transport"

  private def constructed(shape: TermShape): ConstructedTerm =
    ConstructedTerm.fromShape(shape).toOption.get

  private def oneTermHole(root: TermShape, ordinal: Int = 0): TermTemplate =
    template(
      root,
      termEntries = Vector("value" -> generatedTerm),
      termOccurrences = Vector(TermHoleOccurrence("value", ordinal))
    ).toOption.get

  test("completes a template with no holes") {
    val root =
      TermShape.Apply(ident("f"), List(TermShape.Literal("1")))
    val completed =
      template(root).toOption.get.complete(Map.empty, Map.empty).toOption.get

    assertEquals(completed, ConstructedTerm.fromShape(root).toOption.get)
  }

  test("substitutes one term hole") {
    val bound = constructed(TermShape.Select(ident("service"), "answer"))
    val completed =
      oneTermHole(ident(generatedTerm))
        .complete(Map("value" -> bound), Map.empty)
        .toOption
        .get

    assertEquals(completed, bound)
  }

  test("substitutes repeated occurrences from one binding") {
    val bound = constructed(TermShape.Literal("1"))
    val repeated =
      template(
        TermShape.Tuple(
          List(ident(generatedTerm), ident(generatedTerm))
        ),
        termEntries = Vector("value" -> generatedTerm),
        termOccurrences = Vector(
          TermHoleOccurrence("value", 0),
          TermHoleOccurrence("value", 1)
        )
      ).toOption.get
    val completed =
      repeated
        .complete(Map("value" -> bound), Map.empty)
        .toOption
        .get

    assertEquals(
      completed.root,
      TermShape.Tuple(
        List(TermShape.Literal("1"), TermShape.Literal("1"))
      )
    )
  }

  test("rejects missing and extra term bindings") {
    val hole = oneTermHole(ident(generatedTerm))
    val value = constructed(TermShape.Literal("1"))
    val noHoles = template(TermShape.Literal("1")).toOption.get

    assertEquals(
      hole.complete(Map.empty, Map.empty),
      Left(TermConstructionError.MissingTermBinding("value"))
    )
    assertEquals(
      noHoles.complete(Map("extra" -> value), Map.empty),
      Left(TermConstructionError.ExtraTermBinding("extra"))
    )
  }

  test("completes one type hole") {
    val typed =
      template(
        TermShape.Typed(ident("value"), generatedType),
        typeEntries = Vector("tpe" -> generatedType),
        ascriptions = Vector(TypeTemplate.TTHole("tpe"))
      ).toOption.get
    val completed =
      typed
        .complete(Map.empty, Map("tpe" -> stringForm))
        .toOption
        .get

    assertEquals(
      completed.root,
      TermShape.Typed(ident("value"), "String")
    )
    assertEquals(completed.ascriptionTypes, Vector(stringForm))
  }

  test("rejects missing and extra type bindings") {
    val typed =
      template(
        TermShape.Typed(ident("value"), generatedType),
        typeEntries = Vector("tpe" -> generatedType),
        ascriptions = Vector(TypeTemplate.TTHole("tpe"))
      ).toOption.get
    val noHoles = template(TermShape.Literal("1")).toOption.get

    assertEquals(
      typed.complete(Map.empty, Map.empty),
      Left(TermConstructionError.MissingTypeBinding("tpe"))
    )
    assertEquals(
      noHoles.complete(Map.empty, Map("extra" -> intForm)),
      Left(TermConstructionError.ExtraTypeBinding("extra"))
    )
  }

  test("preserves category separation for identical semantic text") {
    val templateValue =
      template(
        TermShape.Typed(ident(generatedTerm), generatedType),
        termEntries = Vector("same" -> generatedTerm),
        termOccurrences = Vector(TermHoleOccurrence("same", 0)),
        typeEntries = Vector("same" -> generatedType),
        ascriptions = Vector(TypeTemplate.TTHole("same"))
      ).toOption.get
    val completed =
      templateValue
        .complete(
          Map("same" -> constructed(TermShape.Literal("1"))),
          Map("same" -> intForm)
        )
        .toOption
        .get

    assertEquals(
      completed.root,
      TermShape.Typed(TermShape.Literal("1"), "Int")
    )
    assertEquals(completed.ascriptionTypes, Vector(intForm))
  }

  test("rejects an invalid completed type binding before traversal") {
    val typed =
      template(
        TermShape.Typed(ident("value"), generatedType),
        typeEntries = Vector("tpe" -> generatedType),
        ascriptions = Vector(TypeTemplate.TTHole("tpe"))
      ).toOption.get
    val invalid = TypeNormalForm.STypeIdent("AnyVal")
    val result = typed.complete(Map.empty, Map("tpe" -> invalid))

    assert(result.isLeft)
    assert(
      result.left.toOption.get
        .isInstanceOf[TermConstructionError.TypeBindingConstructionFailure]
    )
  }

  test("splices nested constructed-term sidecars at the structural hole position") {
    val boundRoot =
      TermShape.Tuple(
        List(
          TermShape.Typed(ident("left"), "String"),
          TermShape.Typed(ident("right"), "Boolean")
        )
      )
    val bound =
      ConstructedTerm
        .create(boundRoot, Vector(stringForm, booleanForm))
        .toOption
        .get
    val root =
      TermShape.Typed(
        TermShape.Apply(
          ident(generatedTerm),
          List(
            TermShape.Typed(ident("argument"), "Int"),
            ident("ordinary")
          )
        ),
        "Boolean"
      )
    val templateValue =
      template(
        root,
        termEntries = Vector("value" -> generatedTerm),
        termOccurrences = Vector(TermHoleOccurrence("value", 0)),
        ascriptions = Vector(
          TypeTemplate.TTIdent("Boolean"),
          TypeTemplate.TTIdent("Int")
        )
      ).toOption.get
    val completed =
      templateValue
        .complete(Map("value" -> bound), Map.empty)
        .toOption
        .get

    assertEquals(
      completed.ascriptionTypes,
      Vector(booleanForm, stringForm, booleanForm, intForm)
    )
    assertEquals(
      TermShapeTraversal.typedNames(completed.root),
      Vector("Boolean", "String", "Boolean", "Int")
    )
  }

  test("preserves exact sidecar order for a root insertion") {
    val bound =
      ConstructedTerm
        .create(
          TermShape.Typed(ident("bound"), "String"),
          Vector(stringForm)
        )
        .toOption
        .get
    val completed =
      oneTermHole(ident(generatedTerm))
        .complete(Map("value" -> bound), Map.empty)
        .toOption
        .get

    assertEquals(completed, bound)
  }

  test("preserves exact sidecar order for insertion inside tuple and if branches") {
    val bound =
      ConstructedTerm
        .create(
          TermShape.Typed(ident("bound"), "String"),
          Vector(stringForm)
        )
        .toOption
        .get
    val root =
      TermShape.Tuple(
        List(
          TermShape.Typed(ident("before"), "Int"),
          TermShape.If(
            ident("condition"),
            ident(generatedTerm),
            TermShape.Typed(ident("after"), "Boolean")
          )
        )
      )
    val templateValue =
      template(
        root,
        termEntries = Vector("value" -> generatedTerm),
        termOccurrences = Vector(TermHoleOccurrence("value", 2)),
        ascriptions = Vector(
          TypeTemplate.TTIdent("Int"),
          TypeTemplate.TTIdent("Boolean")
        )
      ).toOption.get
    val completed =
      templateValue
        .complete(Map("value" -> bound), Map.empty)
        .toOption
        .get

    assertEquals(
      completed.ascriptionTypes,
      Vector(intForm, stringForm, booleanForm)
    )
  }

  test("does not replace an ordinary generated-looking identifier") {
    val ordinaryName = "__term_transport_similar"
    val root =
      TermShape.Apply(
        ident(generatedTerm),
        List(ident(ordinaryName))
      )
    val completed =
      oneTermHole(root)
        .complete(
          Map("value" -> constructed(TermShape.Literal("1"))),
          Map.empty
        )
        .toOption
        .get

    assertEquals(
      completed.root,
      TermShape.Apply(
        TermShape.Literal("1"),
        List(ident(ordinaryName))
      )
    )
  }

  test("leaves no owned transport marker after completion") {
    val completed =
      oneTermHole(ident(generatedTerm))
        .complete(
          Map("value" -> constructed(TermShape.Literal("1"))),
          Map.empty
        )
        .toOption
        .get

    assert(!completed.root.render.contains(generatedTerm))
    assert(
      TermShapeTraversal
        .identifierEntries(completed.root)
        .forall(!_.isPlaceholder)
    )
  }

  test("binding error order is missing term extra term missing type extra type") {
    val value = constructed(TermShape.Literal("1"))
    val both =
      template(
        TermShape.Typed(ident(generatedTerm), generatedType),
        termEntries = Vector("term" -> generatedTerm),
        termOccurrences = Vector(TermHoleOccurrence("term", 0)),
        typeEntries = Vector("tpe" -> generatedType),
        ascriptions = Vector(TypeTemplate.TTHole("tpe"))
      ).toOption.get

    assertEquals(
      both.complete(
        Map("extra" -> value),
        Map("extraType" -> intForm)
      ),
      Left(TermConstructionError.MissingTermBinding("term"))
    )
    assertEquals(
      both.complete(
        Map("term" -> value, "extra" -> value),
        Map("extraType" -> intForm)
      ),
      Left(TermConstructionError.ExtraTermBinding("extra"))
    )
    assertEquals(
      both.complete(
        Map("term" -> value),
        Map("extraType" -> intForm)
      ),
      Left(TermConstructionError.MissingTypeBinding("tpe"))
    )
    assertEquals(
      both.complete(
        Map("term" -> value),
        Map("tpe" -> intForm, "extraType" -> intForm)
      ),
      Left(TermConstructionError.ExtraTypeBinding("extraType"))
    )
  }
