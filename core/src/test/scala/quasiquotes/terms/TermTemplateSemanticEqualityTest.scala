package quasiquotes.terms

import quasiquotes.parser.TermShape
import quasiquotes.types.TypeTemplate

class TermTemplateSemanticEqualityTest extends munit.FunSuite:
  import TermCoreTestFixtures.*

  private def accepted(
      root: TermShape,
      termEntries: Vector[(String, String)] = Vector.empty,
      termOccurrences: Vector[TermHoleOccurrence] = Vector.empty,
      typeEntries: Vector[(String, String)] = Vector.empty,
      ascriptions: Vector[TypeTemplate] = Vector.empty
  ): TermTemplate =
    template(
      root,
      termEntries,
      termOccurrences,
      typeEntries,
      ascriptions
    ).fold(error => fail(error.message), identity)

  private def typed(
      sidecar: TypeTemplate,
      typeEntries: Vector[(String, String)] = Vector.empty
  ): TermTemplate =
    val typeIndex = index(typeEntries*)
    val rendered =
      TermShapeTraversal
        .renderTypeTemplate(sidecar, typeIndex.generatedNameFor)
        .fold(fail(_), identity)
    accepted(
      TermShape.Typed(ident("value"), rendered),
      typeEntries = typeEntries,
      ascriptions = Vector(sidecar)
    )

  test("distinguishes the concrete tuple and literal serialization collision") {
    val first =
      accepted(
        TermShape.Tuple(
          List(
            TermShape.Literal("x),Literal(y"),
            TermShape.Literal("z")
          )
        )
      )
    val second =
      accepted(
        TermShape.Tuple(
          List(
            TermShape.Literal("x"),
            TermShape.Literal("y"),
            TermShape.Literal("z")
          )
        )
      )

    assert(first != second)
  }

  test("treats delimiter-rich literal values as structural fields") {
    val values = Vector(
      ",()[]Literal(Tuple([",
      "\"quoted\" and \\\\backslashes\\\\",
      "line one\nline two",
      "escaped newline: \\n",
      "x),Literal(y"
    )
    val templates = values.map(value => accepted(TermShape.Literal(value)))

    templates.indices.foreach { left =>
      templates.indices.foreach { right =>
        assertEquals(templates(left) == templates(right), left == right)
      }
    }
  }

  test("distinguishes constructors from rendering-like field text") {
    val literal = accepted(TermShape.Literal("Ident(value)"))
    val identifier = accepted(ident("Literal(value)"))
    val parenthesized =
      accepted(TermShape.Parenthesized(TermShape.Literal("Ident(value)")))

    assert(literal != identifier)
    assert(literal != parenthesized)
    assert(identifier != parenthesized)
  }

  test("distinguishes child count and child order without flattening fields") {
    val twoArguments =
      accepted(
        TermShape.Apply(
          ident("f"),
          List(
            TermShape.Literal("x),Literal(y"),
            TermShape.Literal("z")
          )
        )
      )
    val threeArguments =
      accepted(
        TermShape.Apply(
          ident("f"),
          List(
            TermShape.Literal("x"),
            TermShape.Literal("y"),
            TermShape.Literal("z")
          )
        )
      )
    val ordered =
      accepted(
        TermShape.Tuple(
          List(TermShape.Literal("left"), TermShape.Literal("right"))
        )
      )
    val reversed =
      accepted(
        TermShape.Tuple(
          List(TermShape.Literal("right"), TermShape.Literal("left"))
        )
      )

    assert(twoArguments != threeArguments)
    assert(ordered != reversed)
  }

  test("constructor templates preserve identity and recursively complete argument holes") {
    val template = accepted(
      TermShape.New(
        "java.lang.StringBuilder",
        List(ident("__capacity"))
      ),
      termEntries = Vector("capacity" -> "__capacity"),
      termOccurrences = Vector(TermHoleOccurrence("capacity", 0))
    )
    val capacity = ConstructedTerm.fromShape(TermShape.Literal("16")).toOption.get

    assertEquals(
      template.complete(Map("capacity" -> capacity), Map.empty).map(_.root),
      Right(TermShape.New("java.lang.StringBuilder", List(TermShape.Literal("16"))))
    )
    assertNotEquals(
      template,
      accepted(TermShape.New("java.lang.RuntimeException", List(TermShape.Literal("16"))))
    )
  }

  test("ignores generated term and type transport names across a triple") {
    def one(termTransport: String, typeTransport: String): TermTemplate =
      accepted(
        TermShape.Typed(ident(termTransport), typeTransport),
        termEntries = Vector("term" -> termTransport),
        termOccurrences = Vector(TermHoleOccurrence("term", 0)),
        typeEntries = Vector("tpe" -> typeTransport),
        ascriptions = Vector(TypeTemplate.TTHole("tpe"))
      )

    val first = one("__term_a", "__type_a")
    val second = one("__term_b", "__type_b")
    val third = one("__term_c", "__type_c")

    assertEquals(first, second)
    assertEquals(second, third)
    assertEquals(first, third)
    assertEquals(first.hashCode, second.hashCode)
    assertEquals(second.hashCode, third.hashCode)
  }

  test("uses logical type-template identity structure and child order") {
    val firstTransport =
      typed(
        TypeTemplate.TTHole("element"),
        Vector("element" -> "__type_a")
      )
    val secondTransport =
      typed(
        TypeTemplate.TTHole("element"),
        Vector("element" -> "__type_b")
      )
    val differentLogicalName =
      typed(
        TypeTemplate.TTHole("other"),
        Vector("other" -> "__type_c")
      )
    val ordered =
      typed(
        TypeTemplate.TTTuple(
          List(
            TypeTemplate.TTIdent("Int"),
            TypeTemplate.TTIdent("String")
          )
        )
      )
    val reversed =
      typed(
        TypeTemplate.TTTuple(
          List(
            TypeTemplate.TTIdent("String"),
            TypeTemplate.TTIdent("Int")
          )
        )
      )

    assertEquals(firstTransport, secondTransport)
    assertEquals(firstTransport.hashCode, secondTransport.hashCode)
    assert(firstTransport != differentLogicalName)
    assert(ordered != reversed)
  }

  test("satisfies bounded equality and hash laws") {
    val generatedA =
      accepted(
        ident("__generated_a"),
        termEntries = Vector("value" -> "__generated_a"),
        termOccurrences = Vector(TermHoleOccurrence("value", 0))
      )
    val generatedB =
      accepted(
        ident("__generated_b"),
        termEntries = Vector("value" -> "__generated_b"),
        termOccurrences = Vector(TermHoleOccurrence("value", 0))
      )
    val generatedC =
      accepted(
        ident("__generated_c"),
        termEntries = Vector("value" -> "__generated_c"),
        termOccurrences = Vector(TermHoleOccurrence("value", 0))
      )
    val fixtures = Vector(
      accepted(ident("ordinary")),
      accepted(TermShape.Literal("ordinary")),
      accepted(
        TermShape.Apply(
          ident("f"),
          List(TermShape.Literal("a"), TermShape.Literal("b"))
        )
      ),
      accepted(
        TermShape.Tuple(
          List(TermShape.Literal("a"), TermShape.Literal("b"))
        )
      ),
      typed(TypeTemplate.TTIdent("Int")),
      generatedA,
      generatedB,
      generatedC
    )

    fixtures.foreach(value => assertEquals(value, value))
    fixtures.foreach { left =>
      fixtures.foreach { right =>
        assertEquals(left == right, right == left)
        if left == right then assertEquals(left.hashCode, right.hashCode)
      }
    }
    assertEquals(generatedA, generatedB)
    assertEquals(generatedB, generatedC)
    assertEquals(generatedA, generatedC)
  }
