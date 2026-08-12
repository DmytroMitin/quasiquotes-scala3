package quasiquotes.terms

import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

class Lambda1BinderSemanticsTest extends munit.FunSuite:
  import TermCoreTestFixtures.*

  private val intType = TypeTemplate.TTIdent("Int")

  private def lambda(
      id: Int,
      displayName: String,
      body: TermShape
  ): TermShape =
    TermShape.Lambda1(BinderId(id), displayName, "Int", body)

  private def bound(id: Int, displayName: String): TermShape =
    TermShape.BoundReference(BinderId(id), displayName)

  private def accepted(root: TermShape): TermTemplate =
    template(root, ascriptions = Vector(intType)).fold(error => fail(error.message), identity)

  test("Lambda1 templates compare bound references alpha-structurally") {
    val left = accepted(lambda(0, "x", bound(0, "x")))
    val right = accepted(lambda(17, "y", bound(17, "y")))

    assertEquals(left, right)
    assertEquals(left.hashCode, right.hashCode)
  }

  test("Lambda1 templates distinguish a free same-text reference from a bound reference") {
    val boundBody = accepted(lambda(0, "x", bound(0, "x")))
    val freeBody = accepted(lambda(0, "x", ident("x")))

    assertNotEquals(boundBody, freeBody)
  }

  test("Lambda1 parameter type remains part of structural equality") {
    val intLambda = accepted(lambda(0, "x", bound(0, "x")))
    val stringLambda = template(
      TermShape.Lambda1(BinderId(0), "x", "String", bound(0, "x")),
      ascriptions = Vector(TypeTemplate.TTIdent("String"))
    ).toOption.get

    assertNotEquals(intLambda, stringLambda)
  }

  test("identifier preorder excludes binder declarations and bound references") {
    val shape = lambda(
      0,
      "x",
      TermShape.Tuple(
        List(
          bound(0, "x"),
          ident("free"),
          ident("__body", placeholder = true)
        )
      )
    )

    assertEquals(
      TermShapeTraversal.identifierEntries(shape).map(entry => entry.name -> entry.ordinal),
      Vector("free" -> 0, "__body" -> 1)
    )
    assertEquals(TermShapeTraversal.typedNames(shape), Vector("Int"))
  }

  test("completion preserves a same-text external term as free under Lambda1") {
    val root = lambda(0, "x", ident("__body", placeholder = true))
    val value = ConstructedTerm.fromShape(ident("x")).toOption.get
    val lambdaTemplate = template(
      root,
      termEntries = Vector("body" -> "__body"),
      termOccurrences = Vector(TermHoleOccurrence("body", 0)),
      ascriptions = Vector(intType)
    ).toOption.get

    val completed = lambdaTemplate
      .complete(Map("body" -> value), Map.empty)
      .toOption.get

    assertEquals(
      completed.root,
      lambda(0, "x", ident("x"))
    )
    assertEquals(completed.ascriptionTypes, Vector(TypeNormalForm.STypeIdent("Int")))
  }

  test("constructed Lambda1 values use alpha equality without beta or expression rewriting") {
    val left = ConstructedTerm.fromShape(
      lambda(0, "x", TermShape.Infix(bound(0, "x"), "+", TermShape.Literal("1")))
    ).toOption.get
    val right = ConstructedTerm.fromShape(
      lambda(9, "y", TermShape.Infix(bound(9, "y"), "+", TermShape.Literal("1")))
    ).toOption.get
    val reordered = ConstructedTerm.fromShape(
      lambda(9, "y", TermShape.Infix(TermShape.Literal("1"), "+", bound(9, "y")))
    ).toOption.get

    assertEquals(left, right)
    assertNotEquals(left, reordered)
  }
