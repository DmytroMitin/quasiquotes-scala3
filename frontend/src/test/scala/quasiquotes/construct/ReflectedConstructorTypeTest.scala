package quasiquotes.construct

final class ReflectedConstructorTypeTest extends munit.FunSuite:
  test("TypeRepr.of supplies the complete qr constructor type"):
    assertEquals(ReflectedConstructorTypeMacros.fromTypeRepr(17), 17)

  test("TypeTree.of.tpe supplies the complete qr constructor type"):
    assertEquals(ReflectedConstructorTypeMacros.fromTypeTreeTpe(18), 18)

  test("tqr stacks directly into the complete qr constructor type"):
    assertEquals(ReflectedConstructorTypeMacros.fromTqr(19), 19)
