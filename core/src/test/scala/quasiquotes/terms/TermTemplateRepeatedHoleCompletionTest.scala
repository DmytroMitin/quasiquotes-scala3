package quasiquotes.terms

import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.source.{GeneratedSourceMap, SourceId}
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

class TermTemplateRepeatedHoleCompletionTest extends munit.FunSuite:
  import TermCoreTestFixtures.*

  private val scalarGenerated = "__scalar_term_transport"
  private val repeatedGenerated = "__repeated_term_transport"
  private val intForm = TypeNormalForm.STypeIdent("Int")
  private val stringForm = TypeNormalForm.STypeIdent("String")
  private val booleanForm = TypeNormalForm.STypeIdent("Boolean")

  private def constructed(shape: TermShape): ConstructedTerm =
    ConstructedTerm.fromShape(shape).fold(error => fail(error.message), identity)

  private def repeatedTemplate(
      root: TermShape,
      repeatedEntries: Vector[(String, String)] = Vector("args" -> repeatedGenerated),
      repeatedOccurrences: Vector[RepeatedTermHoleOccurrence] =
        Vector(RepeatedTermHoleOccurrence("args", 1)),
      scalarEntries: Vector[(String, String)] = Vector.empty,
      scalarOccurrences: Vector[TermHoleOccurrence] = Vector.empty,
      ascriptions: Vector[TypeTemplate] = Vector.empty
  ): Either[TermConstructionError, TermTemplate] =
    TermTemplate.createWithRepeatedTerms(
      root,
      index(scalarEntries*),
      scalarOccurrences,
      index(repeatedEntries*),
      repeatedOccurrences,
      emptyIndex,
      ascriptions
    )

  private def applyTemplate(arguments: List[TermShape]): TermTemplate =
    repeatedTemplate(
      TermShape.Apply(ident("f"), arguments)
    ).fold(error => fail(error.message), identity)

  private def newTemplate(arguments: List[TermShape]): TermTemplate =
    repeatedTemplate(
      TermShape.New("pkg.C", arguments),
      repeatedOccurrences = Vector(RepeatedTermHoleOccurrence("args", 0))
    ).fold(error => fail(error.message), identity)

  private def completeRepeated(
      template: TermTemplate,
      values: Vector[ConstructedTerm],
      scalar: Map[String, ConstructedTerm] = Map.empty
  ): ConstructedTerm =
    template
      .completeWithRepeatedTerms(scalar, Map("args" -> values), Map.empty)
      .fold(error => fail(error.message), identity)

  test("splices empty one and multiple repeated bindings into Apply and New"):
    val values = Vector(
      constructed(TermShape.Literal("1")),
      constructed(ident("middle")),
      constructed(TermShape.Literal("3"))
    )

    List(
      applyTemplate(List(ident(repeatedGenerated, placeholder = true))) -> ((arguments: List[TermShape]) =>
        TermShape.Apply(ident("f"), arguments)),
      newTemplate(List(ident(repeatedGenerated, placeholder = true))) -> ((arguments: List[TermShape]) =>
        TermShape.New("pkg.C", arguments))
    ).foreach { case (template, expectedRoot) =>
      assertEquals(completeRepeated(template, Vector.empty).root, expectedRoot(Nil))
      assertEquals(completeRepeated(template, values.take(1)).root, expectedRoot(List(values(0).root)))
      assertEquals(completeRepeated(template, values.take(2)).root, expectedRoot(values.take(2).map(_.root).toList))
      assertEquals(completeRepeated(template, values).root, expectedRoot(values.map(_.root).toList))
    }

  test("preserves fixed prefix repeated order and suffix for Apply and New"):
    val repeated = Vector(
      constructed(ident("left")),
      constructed(ident("right"))
    )
    val input = List(
      TermShape.Literal("1"),
      ident(repeatedGenerated),
      TermShape.Literal("2")
    )
    val expectedArguments = List(
      TermShape.Literal("1"),
      ident("left"),
      ident("right"),
      TermShape.Literal("2")
    )
    val fixedOnly = List(TermShape.Literal("1"), TermShape.Literal("2"))

    assertEquals(
      completeRepeated(applyTemplate(input), Vector.empty).root,
      TermShape.Apply(ident("f"), fixedOnly)
    )
    assertEquals(
      completeRepeated(newTemplate(input), Vector.empty).root,
      TermShape.New("pkg.C", fixedOnly)
    )
    assertEquals(
      completeRepeated(applyTemplate(input), repeated).root,
      TermShape.Apply(ident("f"), expectedArguments)
    )
    assertEquals(
      completeRepeated(newTemplate(input), repeated).root,
      TermShape.New("pkg.C", expectedArguments)
    )

  test("keeps scalar and repeated bindings as separate cardinalities"):
    val template = repeatedTemplate(
      TermShape.Apply(
        ident("f"),
        List(ident(scalarGenerated), ident(repeatedGenerated))
      ),
      repeatedOccurrences = Vector(RepeatedTermHoleOccurrence("args", 2)),
      scalarEntries = Vector("value" -> scalarGenerated),
      scalarOccurrences = Vector(TermHoleOccurrence("value", 1))
    ).fold(error => fail(error.message), identity)
    val scalarTuple = constructed(
      TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("2")))
    )
    val repeated = Vector(constructed(ident("left")), constructed(ident("right")))
    val completed = completeRepeated(template, repeated, Map("value" -> scalarTuple))

    assertEquals(
      completed.root,
      TermShape.Apply(
        ident("f"),
        List(scalarTuple.root, ident("left"), ident("right"))
      )
    )

  test("splices completed type sidecars in exact emitted-child order"):
    val root = TermShape.Apply(
      ident("f"),
      List(
        TermShape.Typed(ident("before"), "Int"),
        ident(repeatedGenerated),
        TermShape.Typed(ident("after"), "Boolean")
      )
    )
    val template = repeatedTemplate(
      root,
      repeatedOccurrences = Vector(RepeatedTermHoleOccurrence("args", 2)),
      ascriptions = Vector(
        TypeTemplate.TTIdent("Int"),
        TypeTemplate.TTIdent("Boolean")
      )
    ).fold(error => fail(error.message), identity)
    val left = ConstructedTerm
      .create(TermShape.Typed(ident("left"), "String"), Vector(stringForm))
      .fold(error => fail(error.message), identity)
    val middle = constructed(ident("middle"))
    val right = ConstructedTerm
      .create(TermShape.Typed(ident("right"), "Int"), Vector(intForm))
      .fold(error => fail(error.message), identity)
    val completed = completeRepeated(template, Vector(left, middle, right))

    assertEquals(
      completed.root,
      TermShape.Apply(
        ident("f"),
        List(
          TermShape.Typed(ident("before"), "Int"),
          left.root,
          middle.root,
          right.root,
          TermShape.Typed(ident("after"), "Boolean")
        )
      )
    )
    assertEquals(
      completed.ascriptionTypes,
      Vector(intForm, stringForm, intForm, booleanForm)
    )
    assertEquals(
      TermShapeTraversal.typedNames(completed.root),
      Vector("Int", "String", "Int", "Boolean")
    )

  test("requires a repeated occurrence to be one direct Apply or New argument"):
    val candidates = Vector(
      TermShape.Apply(ident(repeatedGenerated), Nil) -> 0,
      TermShape.Select(ident(repeatedGenerated), "member") -> 0,
      TermShape.Tuple(List(ident(repeatedGenerated), TermShape.Literal("1"))) -> 0,
      TermShape.If(ident("condition"), ident(repeatedGenerated), TermShape.Literal("0")) -> 1,
      TermShape.InterpolatedString("s", List("", ""), List(ident(repeatedGenerated))) -> 0,
      TermShape.Block(List(ident(repeatedGenerated)), TermShape.Literal("0")) -> 0,
      TermShape.Typed(ident(repeatedGenerated), "Int") -> 0,
      TermShape.New(repeatedGenerated, Nil) -> 0
    )

    candidates.foreach { case (root, ordinal) =>
      assertEquals(
        repeatedTemplate(
          root,
          repeatedOccurrences = Vector(RepeatedTermHoleOccurrence("args", ordinal)),
          ascriptions =
            if root.isInstanceOf[TermShape.Typed] then Vector(TypeTemplate.TTIdent("Int"))
            else Vector.empty
        ),
        Left(TermConstructionError.InvalidRepeatedTermHolePosition("args")),
        clues(root.render)
      )
    }

  test("rejects two repeated occurrences including reuse of one repeated name"):
    val differentNames = repeatedTemplate(
      TermShape.Apply(
        ident("f"),
        List(ident("__left_repeated"), ident("__right_repeated"))
      ),
      repeatedEntries = Vector(
        "left" -> "__left_repeated",
        "right" -> "__right_repeated"
      ),
      repeatedOccurrences = Vector(
        RepeatedTermHoleOccurrence("left", 1),
        RepeatedTermHoleOccurrence("right", 2)
      )
    )
    val sameName = repeatedTemplate(
      TermShape.Apply(
        ident("f"),
        List(ident(repeatedGenerated), ident(repeatedGenerated))
      ),
      repeatedOccurrences = Vector(
        RepeatedTermHoleOccurrence("args", 1),
        RepeatedTermHoleOccurrence("args", 2)
      )
    )

    assertEquals(differentNames, Left(TermConstructionError.DuplicateRepeatedTermHole()))
    assertEquals(sameName, Left(TermConstructionError.DuplicateRepeatedTermHole()))

  test("rejects scalar and repeated metadata or bindings with the same category name"):
    val metadataConflict = repeatedTemplate(
      TermShape.Apply(
        ident("f"),
        List(ident(scalarGenerated), ident(repeatedGenerated))
      ),
      repeatedEntries = Vector("value" -> repeatedGenerated),
      repeatedOccurrences = Vector(RepeatedTermHoleOccurrence("value", 2)),
      scalarEntries = Vector("value" -> scalarGenerated),
      scalarOccurrences = Vector(TermHoleOccurrence("value", 1))
    )
    val repeated = applyTemplate(List(ident(repeatedGenerated)))
    val scalar = template(
      TermShape.Apply(ident("f"), List(ident(scalarGenerated))),
      termEntries = Vector("value" -> scalarGenerated),
      termOccurrences = Vector(TermHoleOccurrence("value", 1))
    ).fold(error => fail(error.message), identity)
    val value = constructed(TermShape.Literal("1"))

    assertEquals(
      metadataConflict,
      Left(TermConstructionError.RepeatedTermHoleCategoryConflict("value"))
    )
    assertEquals(
      repeated.completeWithRepeatedTerms(Map("args" -> value), Map.empty, Map.empty),
      Left(TermConstructionError.RepeatedTermHoleCategoryConflict("args"))
    )
    assertEquals(
      scalar.completeWithRepeatedTerms(Map.empty, Map("value" -> Vector(value)), Map.empty),
      Left(TermConstructionError.RepeatedTermHoleCategoryConflict("value"))
    )

  test("validates exact repeated binding coverage"):
    val repeated = applyTemplate(List(ident(repeatedGenerated)))
    val value = constructed(TermShape.Literal("1"))

    assertEquals(
      repeated.completeWithRepeatedTerms(Map.empty, Map.empty, Map.empty),
      Left(TermConstructionError.MissingRepeatedTermBinding("args"))
    )
    assertEquals(
      repeated.completeWithRepeatedTerms(
        Map.empty,
        Map("args" -> Vector(value), "extra" -> Vector(value)),
        Map.empty
      ),
      Left(TermConstructionError.ExtraRepeatedTermBinding("extra"))
    )

  test("fails closed for null repeated maps sequences and elements"):
    val repeated = applyTemplate(List(ident(repeatedGenerated)))

    assertEquals(
      repeated.completeWithRepeatedTerms(
        Map.empty,
        null.asInstanceOf[Map[String, Vector[ConstructedTerm]]],
        Map.empty
      ),
      Left(
        TermConstructionError.InvalidRepeatedTermBinding(
          "<bindings>",
          "repeated term binding map is null"
        )
      )
    )
    assertEquals(
      repeated.completeWithRepeatedTerms(
        Map.empty,
        Map("args" -> null.asInstanceOf[Vector[ConstructedTerm]]),
        Map.empty
      ),
      Left(
        TermConstructionError.InvalidRepeatedTermBinding(
          "args",
          "binding sequence is null"
        )
      )
    )
    assertEquals(
      repeated.completeWithRepeatedTerms(
        Map.empty,
        Map("args" -> Vector(null.asInstanceOf[ConstructedTerm])),
        Map.empty
      ),
      Left(
        TermConstructionError.InvalidRepeatedTermBinding(
          "args",
          "binding sequence contains a null term at index 0"
        )
      )
    )

  test("rejects binder-bearing repeated sites and nonglobal repeated elements"):
    val binder = BinderId(41)
    val scopedTemplate = repeatedTemplate(
      TermShape.Lambda1(
        binder,
        "value",
        "Int",
        TermShape.Apply(ident("f"), List(ident(repeatedGenerated)))
      ),
      repeatedOccurrences = Vector(RepeatedTermHoleOccurrence("args", 1)),
      ascriptions = Vector(TypeTemplate.TTIdent("Int"))
    )
    assertEquals(
      scopedTemplate,
      Left(TermConstructionError.RepeatedTermScopeUnsupported("args"))
    )

    val repeated = applyTemplate(List(ident(repeatedGenerated)))
    val scopedElement = ConstructedTerm
      .fromShapeInScope(TermShape.BoundReference(binder, "value"), binder)
      .fold(error => fail(error.message), identity)
    val result = repeated.completeWithRepeatedTerms(
      Map.empty,
      Map("args" -> Vector(scopedElement)),
      Map.empty
    )
    assert(result.isLeft)
    assert(
      result.left.toOption.get
        .isInstanceOf[TermConstructionError.InvalidRepeatedTermBinding]
    )

  test("repeated metadata participates in equality hash rendering and final-marker removal"):
    def repeated(transport: String): TermTemplate =
      repeatedTemplate(
        TermShape.Apply(ident("f"), List(ident(transport))),
        repeatedEntries = Vector("args" -> transport)
      ).fold(error => fail(error.message), identity)
    val first = repeated("__repeated_a")
    val second = repeated("__repeated_b")
    val scalar = template(
      TermShape.Apply(ident("f"), List(ident("__repeated_a"))),
      termEntries = Vector("args" -> "__repeated_a"),
      termOccurrences = Vector(TermHoleOccurrence("args", 1))
    ).fold(error => fail(error.message), identity)
    val completed = completeRepeated(first, Vector(constructed(ident("value"))))

    assertEquals(first, second)
    assertEquals(first.hashCode, second.hashCode)
    assert(first != scalar)
    assert(first.render.contains("repeatedTermHoles=[args@1]"))
    assertEquals(first.requiredRepeatedTermBindings, Vector("args"))
    assert(!completed.root.render.contains("__repeated_a"))
    assert(TermShapeTraversal.identifierEntries(completed.root).forall(!_.isPlaceholder))

  test("located scalar source metadata refuses repeated templates until a frontend adapter exists"):
    val repeated = applyTemplate(List(ident(repeatedGenerated)))
    val result = LocatedTermTemplate.create(
      repeated,
      GeneratedSourceMap("", SourceId("n049-generated"), Vector.empty),
      Vector.empty,
      Vector.empty
    )

    assertEquals(
      result,
      Left(
        TermConstructionError.InvalidLocatedTemplateMetadata(
          "located scalar term templates do not support repeated-term metadata"
        )
      )
    )
