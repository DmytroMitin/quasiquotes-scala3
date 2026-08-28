package external.consumer

final class ScalametaReflectedConstructorTypeTest extends munit.FunSuite:
  test("Scalameta qr accepts TypeRepr.of as the complete constructor type"):
    assertEquals(ScalametaReflectedConstructorTypeMacros.fromTypeRepr(20), 20)

  test("Scalameta qr accepts TypeTree.of.tpe as the complete constructor type"):
    assertEquals(ScalametaReflectedConstructorTypeMacros.fromTypeTreeTpe(21), 21)

  test("a current tqr result stacks directly into Scalameta qr"):
    assertEquals(ScalametaReflectedConstructorTypeMacros.fromTqr(22), 22)
