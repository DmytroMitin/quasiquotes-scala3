package quasiquotes.terms

import quasiquotes.parser.{BinderId, BlockStatement, TermShape}
import quasiquotes.types.TypeTemplate

class P2LocalValBinderSemanticsTest extends munit.FunSuite:
  import TermCoreTestFixtures.*

  private def bound(id: Int, displayName: String): TermShape =
    TermShape.BoundReference(BinderId(id), displayName)

  private def localVal(
      id: Int,
      displayName: String,
      initializer: TermShape
  ): BlockStatement =
    BlockStatement.LocalVal(BinderId(id), displayName, "Int", initializer)

  private def block(
      id: Int,
      displayName: String,
      initializer: TermShape,
      result: TermShape
  ): TermShape =
    TermShape.Block(List(localVal(id, displayName, initializer)), result)

  private def accepted(shape: TermShape): TermTemplate =
    template(shape, ascriptions = Vector(TypeTemplate.TTIdent("Int")))
      .fold(error => fail(error.message), identity)

  test("single typed local val is a block statement with initializer and result kept distinct") {
    val shape = block(0, "x", TermShape.Literal("1"), bound(0, "x"))

    assertEquals(
      shape.render,
      "Block([LocalVal(x: Int = Literal(1))], BoundRef(x))"
    )
    assertEquals(TermShapeTraversal.typedNames(shape), Vector("Int"))
    assertEquals(TermShapeTraversal.identifierEntries(shape), Vector.empty)
  }

  test("single typed local val templates compare alpha-structurally across binder spelling and identity") {
    val left = accepted(block(0, "x", TermShape.Literal("1"), bound(0, "x")))
    val right = accepted(block(17, "y", TermShape.Literal("1"), bound(17, "y")))

    assertEquals(left, right)
    assertEquals(left.hashCode, right.hashCode)
  }

  test("single typed local val keeps a free same-text result distinct from a bound reference") {
    val boundResult = accepted(block(0, "x", TermShape.Literal("1"), bound(0, "x")))
    val freeResult = accepted(block(0, "x", TermShape.Literal("1"), ident("x")))

    assertNotEquals(boundResult, freeResult)
  }

  test("single typed local val initializer is outside binder scope") {
    val recursive = block(0, "x", bound(0, "x"), bound(0, "x"))

    assert(ConstructedTerm.fromShape(recursive).isLeft)
    assert(ConstructedTerm.fromShape(block(0, "x", TermShape.Literal("1"), bound(0, "x"))).isRight)
  }

  test("P2 admission rejects multiple local binders and mixed P1/P2 statement lists") {
    val first = localVal(0, "x", TermShape.Literal("1"))
    val second = localVal(1, "y", TermShape.Literal("2"))
    val multiple = TermShape.Block(List(first, second), bound(1, "y"))
    val mixed = TermShape.Block(List(TermShape.Literal("0"), first), bound(0, "x"))

    assert(ConstructedTerm.fromShape(multiple).isLeft)
    assert(ConstructedTerm.fromShape(mixed).isLeft)
  }

  test("P2 admission rejects a second local binder anywhere recursively in one term tree") {
    val inner = block(1, "y", TermShape.Literal("2"), bound(1, "y"))
    val nestedResult = block(0, "x", TermShape.Literal("1"), inner)
    val nestedInitializer = block(0, "x", inner, bound(0, "x"))
    val nestedThroughP1Prefix = block(
      0,
      "x",
      TermShape.Literal("1"),
      TermShape.Block(List(inner), bound(0, "x"))
    )

    List(nestedResult, nestedInitializer, nestedThroughP1Prefix).foreach { shape =>
      assert(ConstructedTerm.fromShape(shape).isLeft, shape.render)
    }
  }

  test("P2 admission rejects same-name source-binder shadowing in either Lambda1 direction") {
    val p2ShadowsLambda = TermShape.Lambda1(
      BinderId(0),
      "x",
      "Int",
      block(1, "x", TermShape.Literal("1"), bound(1, "x"))
    )
    val lambdaShadowsP2 = block(
      0,
      "x",
      TermShape.Literal("1"),
      TermShape.Lambda1(BinderId(1), "x", "Int", bound(1, "x"))
    )

    assert(ConstructedTerm.fromShape(p2ShadowsLambda).isLeft)
    assert(ConstructedTerm.fromShape(lambdaShadowsP2).isLeft)
  }

  test("P2 admission retains one local binder combined with a distinct-name Lambda1 binder") {
    val p2InsideLambda = TermShape.Lambda1(
      BinderId(0),
      "outer",
      "Int",
      block(1, "x", TermShape.Literal("1"), bound(1, "x"))
    )
    val lambdaInsideP2 = block(
      0,
      "x",
      TermShape.Literal("1"),
      TermShape.Lambda1(BinderId(1), "inner", "Int", bound(1, "inner"))
    )

    assert(ConstructedTerm.fromShape(p2InsideLambda).isRight)
    assert(ConstructedTerm.fromShape(lambdaInsideP2).isRight)
  }
