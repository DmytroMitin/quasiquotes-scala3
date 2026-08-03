package quasiquotes.terms

import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

class ConstructedTermTest extends munit.FunSuite:
  import TermCoreTestFixtures.*

  private val intForm = TypeNormalForm.STypeIdent("Int")
  private val stringForm = TypeNormalForm.STypeIdent("String")

  test("accepts every existing bounded hole-free term shape") {
    val simpleShapes = Vector(
      ident("value"),
      TermShape.Literal("1"),
      TermShape.Select(ident("service"), "answer"),
      TermShape.Apply(ident("f"), List(TermShape.Literal("1"))),
      TermShape.Infix(TermShape.Literal("1"), "+", TermShape.Literal("2")),
      TermShape.Unary("-", TermShape.Literal("1")),
      TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("2"))),
      TermShape.If(
        ident("condition"),
        TermShape.Literal("1"),
        TermShape.Literal("2")
      ),
      TermShape.Parenthesized(TermShape.Literal("1"))
    )

    simpleShapes.foreach(shape => assert(ConstructedTerm.fromShape(shape).isRight))
  }

  test("accepts an explicitly aligned completed type sidecar") {
    val shape = TermShape.Typed(ident("value"), "List[Int]")
    val normalForm =
      TypeNormalForm.STypeApply(
        TypeNormalForm.STypeIdent("List"),
        List(intForm)
      )

    val constructed = ConstructedTerm.create(shape, Vector(normalForm)).toOption.get

    assertEquals(constructed.root, shape)
    assertEquals(constructed.ascriptionTypes, Vector(normalForm))
  }

  test("fromShape derives only simple Int String and Boolean ascriptions") {
    Vector("Int", "String", "Boolean").foreach { typeName =>
      assert(
        ConstructedTerm
          .fromShape(TermShape.Typed(ident("value"), typeName))
          .isRight
      )
    }

    assert(
      ConstructedTerm
        .fromShape(TermShape.Typed(ident("value"), "List[Int]"))
        .isLeft
    )
  }

  test("rejects root and nested unsupported shapes") {
    val unsupported = TermShape.Unsupported("RawNode", "detail")
    val nested = TermShape.Apply(ident("f"), List(unsupported))

    assertEquals(
      ConstructedTerm.fromShape(unsupported),
      Left(TermConstructionError.UnsupportedTermShape())
    )
    assertEquals(
      ConstructedTerm.fromShape(nested),
      Left(TermConstructionError.UnsupportedTermShape())
    )
  }

  test("rejects unsupported unary operators") {
    assertEquals(
      ConstructedTerm.fromShape(
        TermShape.Unary("++", TermShape.Literal("1"))
      ),
      Left(TermConstructionError.UnsupportedUnaryOperator("++"))
    )
  }

  test("rejects tuple arity outside Tuple2 through Tuple22") {
    val one = TermShape.Tuple(List(TermShape.Literal("1")))
    val twentyThree =
      TermShape.Tuple(List.fill(23)(TermShape.Literal("1")))

    assertEquals(
      ConstructedTerm.fromShape(one),
      Left(TermConstructionError.InvalidTupleArity(1))
    )
    assertEquals(
      ConstructedTerm.fromShape(twentyThree),
      Left(TermConstructionError.InvalidTupleArity(23))
    )
  }

  test("canonicalizes all identifier placeholder flags to false") {
    val shape =
      TermShape.Apply(
        ident("__hole0", placeholder = true),
        List(
          TermShape.Select(
            ident("__hole1", placeholder = true),
            "value"
          )
        )
      )
    val constructed = ConstructedTerm.fromShape(shape).toOption.get

    assertEquals(
      TermShapeTraversal.identifierEntries(constructed.root).map(_.isPlaceholder),
      Vector(false, false)
    )
  }

  test("rejects completed sidecar count mismatch") {
    val shape = TermShape.Typed(ident("value"), "Int")

    assertEquals(
      ConstructedTerm.create(shape, Vector.empty),
      Left(TermConstructionError.TypedSidecarCountMismatch(1, 0))
    )
  }

  test("rejects completed sidecar rendering mismatch") {
    val shape = TermShape.Typed(ident("value"), "String")

    assertEquals(
      ConstructedTerm.create(shape, Vector(intForm)),
      Left(
        TermConstructionError.TypedSidecarRenderingMismatch(
          0,
          "Int",
          "String"
        )
      )
    )
  }

  test("rejects a completed type outside the admitted subset") {
    val anyVal = TypeNormalForm.STypeIdent("AnyVal")
    val result =
      ConstructedTerm.create(
        TermShape.Typed(ident("value"), "AnyVal"),
        Vector(anyVal)
      )

    assert(result.isLeft)
    assert(
      result.left.toOption.get.message
        .startsWith("Invalid type-template sidecar at typed ordinal 0:")
    )
  }

  test("has stable structural equality hash and debug rendering") {
    val first =
      ConstructedTerm
        .create(
          TermShape.Typed(ident("value"), "Int"),
          Vector(intForm)
        )
        .toOption
        .get
    val second =
      ConstructedTerm
        .create(
          TermShape.Typed(ident("value"), "Int"),
          Vector(intForm)
        )
        .toOption
        .get
    val different =
      ConstructedTerm
        .create(
          TermShape.Typed(ident("value"), "String"),
          Vector(stringForm)
        )
        .toOption
        .get

    assertEquals(first, second)
    assertEquals(first.hashCode, second.hashCode)
    assert(first != different)
    assertEquals(
      first.render,
      "ConstructedTerm(root=Typed(Ident(value), Type(Int)), ascriptions=[STypeIdent(Int)])"
    )
    assertEquals(first.toString, first.render)
  }
