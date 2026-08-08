package quasiquotes.terms

import quasiquotes.parser.TermShape
import quasiquotes.types.TypeTemplate

class TermTemplateValidationTest extends munit.FunSuite:
  import TermCoreTestFixtures.*

  private val termGenerated = "__term_transport"

  test("accepts one term hole at the root") {
    val result =
      template(
        ident(termGenerated, placeholder = true),
        termEntries = Vector("value" -> termGenerated),
        termOccurrences = Vector(TermHoleOccurrence("value", 0))
      )

    assert(result.isRight)
    assertEquals(
      result.toOption.get.root,
      ident(termGenerated, placeholder = false)
    )
  }

  test("accepts term holes in apply infix tuple and if positions") {
    val candidates = Vector(
      (
        TermShape.Apply(ident("f"), List(ident(termGenerated))),
        1
      ),
      (
        TermShape.Infix(ident("left"), "+", ident(termGenerated)),
        1
      ),
      (
        TermShape.Tuple(List(ident(termGenerated), TermShape.Literal("1"))),
        0
      ),
      (
        TermShape.If(
          ident("condition"),
          ident(termGenerated),
          TermShape.Literal("0")
        ),
        1
      ),
      (
        TermShape.InterpolatedString(
          "s",
          List("hello ", ""),
          List(ident(termGenerated))
        ),
        0
      )
    )

    candidates.foreach { case (root, ordinal) =>
      assert(
        template(
          root,
          termEntries = Vector("value" -> termGenerated),
          termOccurrences = Vector(TermHoleOccurrence("value", ordinal))
        ).isRight,
        root.render
      )
    }
  }

  test("accepts repeated occurrences sharing one logical term hole") {
    val result =
      template(
        TermShape.Tuple(
          List(ident(termGenerated), ident(termGenerated))
        ),
        termEntries = Vector("value" -> termGenerated),
        termOccurrences = Vector(
          TermHoleOccurrence("value", 0),
          TermHoleOccurrence("value", 1)
        )
      )

    assert(result.isRight)
  }

  test("accepts the same semantic text in disjoint term and type categories") {
    val typeGenerated = "__type_transport"
    val result =
      template(
        TermShape.Typed(ident(termGenerated), typeGenerated),
        termEntries = Vector("same" -> termGenerated),
        termOccurrences = Vector(TermHoleOccurrence("same", 0)),
        typeEntries = Vector("same" -> typeGenerated),
        ascriptions = Vector(TypeTemplate.TTHole("same"))
      )

    assert(result.isRight)
  }

  test("rejects generated identifier reuse across categories") {
    val result =
      template(
        TermShape.Typed(ident(termGenerated), termGenerated),
        termEntries = Vector("term" -> termGenerated),
        termOccurrences = Vector(TermHoleOccurrence("term", 0)),
        typeEntries = Vector("tpe" -> termGenerated),
        ascriptions = Vector(TypeTemplate.TTHole("tpe"))
      )

    assertEquals(
      result,
      Left(
        TermConstructionError.DuplicateGeneratedIdentifier(termGenerated)
      )
    )
  }

  test("rejects unknown term occurrence identities") {
    val result =
      template(
        ident(termGenerated),
        termEntries = Vector("known" -> termGenerated),
        termOccurrences = Vector(TermHoleOccurrence("unknown", 0))
      )

    assertEquals(
      result,
      Left(TermConstructionError.UnknownTermOccurrence("unknown", 0))
    )
  }

  test("rejects duplicate occurrence addresses") {
    val result =
      template(
        ident(termGenerated),
        termEntries = Vector("value" -> termGenerated),
        termOccurrences = Vector(
          TermHoleOccurrence("value", 0),
          TermHoleOccurrence("value", 0)
        )
      )

    assertEquals(
      result,
      Left(TermConstructionError.DuplicateTermOccurrenceAddress(0))
    )
  }

  test("rejects an occurrence address pointing at an ordinary identifier") {
    val result =
      template(
        TermShape.Tuple(
          List(ident("ordinary"), ident(termGenerated))
        ),
        termEntries = Vector("value" -> termGenerated),
        termOccurrences = Vector(TermHoleOccurrence("value", 0))
      )

    assertEquals(
      result,
      Left(TermConstructionError.InvalidTermHolePosition("value"))
    )
  }

  test("rejects an owned generated identifier in an unrecorded position") {
    val result =
      template(
        ident(termGenerated),
        termEntries = Vector("value" -> termGenerated)
      )

    assertEquals(
      result,
      Left(
        TermConstructionError.UnownedGeneratedMarker(termGenerated, 0)
      )
    )
  }

  test("rejects type-hole transport in an ordinary term identifier position") {
    val typeGenerated = "__type_transport"
    val result =
      template(
        ident(typeGenerated),
        typeEntries = Vector("tpe" -> typeGenerated)
      )

    assertEquals(
      result,
      Left(TermConstructionError.TypeHoleMarkerInTermPosition(0))
    )
  }

  test("rejects a registered term hole with no occurrence") {
    val result =
      template(
        TermShape.Literal("1"),
        termEntries = Vector("value" -> termGenerated)
      )

    assertEquals(
      result,
      Left(TermConstructionError.MissingTermOccurrence("value"))
    )
  }

  test("rejects a term hole outside a complete identifier position") {
    val result =
      template(
        TermShape.Select(ident("service"), termGenerated),
        termEntries = Vector("value" -> termGenerated)
      )

    assertEquals(
      result,
      Left(TermConstructionError.InvalidTermHolePosition("value"))
    )
  }

  test("requires exactly one type template per typed node") {
    val root =
      TermShape.Tuple(
        List(
          TermShape.Typed(ident("first"), "Int"),
          TermShape.Typed(ident("second"), "String")
        )
      )

    assertEquals(
      template(
        root,
        ascriptions = Vector(TypeTemplate.TTIdent("Int"))
      ),
      Left(TermConstructionError.TypedSidecarCountMismatch(2, 1))
    )
  }

  test("rejects typed sidecar ordering or rendering disagreement") {
    val root =
      TermShape.Tuple(
        List(
          TermShape.Typed(ident("first"), "Int"),
          TermShape.Typed(ident("second"), "String")
        )
      )
    val result =
      template(
        root,
        ascriptions = Vector(
          TypeTemplate.TTIdent("String"),
          TypeTemplate.TTIdent("Int")
        )
      )

    assertEquals(
      result,
      Left(
        TermConstructionError.TypedSidecarRenderingMismatch(
          0,
          "String",
          "Int"
        )
      )
    )
  }

  test("rejects a registered type hole with no sidecar occurrence") {
    val result =
      template(
        TermShape.Literal("1"),
        typeEntries = Vector("tpe" -> "__type_transport")
      )

    assert(result.isLeft)
    assert(
      result.left.toOption.get.message
        .contains("registered type hole `tpe` has no sidecar occurrence")
    )
  }

  test("canonicalizes placeholder flags after authoritative reconciliation") {
    val result =
      template(
        TermShape.Apply(
          ident(termGenerated, placeholder = true),
          List(ident("ordinary", placeholder = true))
        ),
        termEntries = Vector("value" -> termGenerated),
        termOccurrences = Vector(TermHoleOccurrence("value", 0))
      )

    assertEquals(
      result,
      Left(
        TermConstructionError.UnownedGeneratedMarker("ordinary", 1)
      )
    )

    val accepted =
      template(
        ident(termGenerated, placeholder = true),
        termEntries = Vector("value" -> termGenerated),
        termOccurrences = Vector(TermHoleOccurrence("value", 0))
      ).toOption.get
    assertEquals(
      TermShapeTraversal.identifierEntries(accepted.root).map(_.isPlaceholder),
      Vector(false)
    )
  }

  test("leaves an ordinary generated-looking identifier nonsemantic") {
    val ordinary = ident("__qq_term_hole_resembling_transport")
    val accepted = template(ordinary).toOption.get

    assertEquals(accepted.root, ordinary)
    assertEquals(accepted.termHoleOccurrences, Vector.empty)
  }

  test("semantic equality ignores collision-safe transport names") {
    val first =
      template(
        TermShape.Apply(
          ident("__fresh_a"),
          List(ident("ordinary"))
        ),
        termEntries = Vector("value" -> "__fresh_a"),
        termOccurrences = Vector(TermHoleOccurrence("value", 0))
      ).toOption.get
    val second =
      template(
        TermShape.Apply(
          ident("__fresh_b"),
          List(ident("ordinary"))
        ),
        termEntries = Vector("value" -> "__fresh_b"),
        termOccurrences = Vector(TermHoleOccurrence("value", 0))
      ).toOption.get

    assertEquals(first, second)
    assertEquals(first.hashCode, second.hashCode)
  }

  test("semantic equality distinguishes logical identity position structure and ordinary identifiers") {
    def one(
        holeName: String,
        root: TermShape,
        ordinal: Int
    ): TermTemplate =
      template(
        root,
        termEntries = Vector(holeName -> "__fresh"),
        termOccurrences = Vector(TermHoleOccurrence(holeName, ordinal))
      ).toOption.get

    val baseline =
      one(
        "value",
        TermShape.Apply(ident("__fresh"), List(ident("ordinary"))),
        0
      )
    val differentName =
      one(
        "other",
        TermShape.Apply(ident("__fresh"), List(ident("ordinary"))),
        0
      )
    val differentOrdinary =
      one(
        "value",
        TermShape.Apply(ident("__fresh"), List(ident("changed"))),
        0
      )
    val differentPosition =
      one(
        "value",
        TermShape.Apply(ident("ordinary"), List(ident("__fresh"))),
        1
      )
    val differentStructure =
      one(
        "value",
        TermShape.Tuple(
          List(ident("__fresh"), ident("ordinary"))
        ),
        0
      )

    assert(baseline != differentName)
    assert(baseline != differentOrdinary)
    assert(baseline != differentPosition)
    assert(baseline != differentStructure)
  }

  test("semantic equality distinguishes logical type-template sidecars") {
    def typed(
        logicalName: String
    ): TermTemplate =
      template(
        TermShape.Typed(ident("value"), "__type_transport"),
        typeEntries = Vector(logicalName -> "__type_transport"),
        ascriptions = Vector(TypeTemplate.TTHole(logicalName))
      ).toOption.get

    assert(typed("first") != typed("second"))
  }

  test("debug rendering shows wrapper role logical holes and sidecar order") {
    val result =
      template(
        TermShape.Typed(ident(termGenerated), "Int"),
        termEntries = Vector("value" -> termGenerated),
        termOccurrences = Vector(TermHoleOccurrence("value", 0)),
        ascriptions = Vector(TypeTemplate.TTIdent("Int"))
      ).toOption.get

    assert(result.render.startsWith("TermTemplate(root="))
    assert(result.render.contains("termHoles=[value@0]"))
    assert(result.render.contains("typeSidecars=[TypeIdent(Int)]"))
  }
