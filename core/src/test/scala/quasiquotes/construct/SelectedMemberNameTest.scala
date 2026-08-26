package quasiquotes.construct

final class SelectedMemberNameTest extends munit.FunSuite:
  private def accepted(value: String): SelectedMemberName =
    SelectedMemberName.from(value).fold(failure => fail(failure.message), identity)

  test("admits the bounded decoded selected-member grammar"):
    val values = Vector(
      "size",
      "_private2",
      "+",
      "&&",
      "type",
      "safe spaced name"
    )

    assertEquals(values.map(SelectedMemberName.from).map(_.isRight), Vector.fill(values.size)(true))
    assertEquals(values.map(value => accepted(value).decoded), values)

  test("rejects invalid, encoded-looking, special, dotted, control, and Unicode values"):
    val values = Vector(
      null,
      "",
      "$plus",
      "contains`tick",
      "line\nbreak",
      "tab\tname",
      "member.name",
      "<init>",
      "naïve",
      "two  spaces",
      " leading",
      "trailing "
    )

    val failures = values.map(SelectedMemberName.from)
    assert(failures.forall(_.isLeft))
    assert(failures.collect { case Left(failure) => failure.code }.forall(_.nonEmpty))
    assert(failures.collect { case Left(failure) => failure.message }.forall(_.nonEmpty))

  test("uses decoded-name equality and hash semantics only"):
    val first = accepted("selected")
    val second = accepted("selected")
    val different = accepted("other")

    assertEquals(first, second)
    assertEquals(first.hashCode, second.hashCode)
    assertNotEquals(first, different)
