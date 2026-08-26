package quasiquotes.construct.hybrid

import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.construct.SelectedMemberName

object ScalametaSelectedMemberNameTarget:
  def ordinary(value: Int): Int = value + 1
  def +(value: Int): Int = value + 2
  def overloaded(value: Int): Int = value
  def overloaded(value: String): Int = value.length
  private def hidden(value: Int): Int = value

final class ScalametaSelectedMemberNameHoleTest extends munit.FunSuite:
  private def name(value: String): SelectedMemberName =
    SelectedMemberName.from(value).fold(failure => fail(failure.message), identity)

  private def build(parts: Seq[String], selected: SelectedMemberName, applied: Boolean) =
    given Compiler = Compiler.make(getClass.getClassLoader)
    withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*
      val receiver = '{ ScalametaSelectedMemberNameTarget }.asTerm
      val argument = '{ 40 }.asTerm
      val arguments =
        if applied then Seq(receiver, selected, argument)
        else Seq(receiver, selected)
      ScalametaTermFrontend
        .lower(using q)(parts, arguments)
        .left.map(failure => failure.category -> failure.detail)
        .map(_.show)

  test("Scalameta opt-in constructs the same explicit dynamic selections and calls"):
    val selected = build(Seq("", ".", ""), name("ordinary"), applied = false)
    val ordinary = build(Seq("", ".", "(", ")"), name("ordinary"), applied = true)
    val operator = build(Seq("", ".", "(", ")"), name("+"), applied = true)

    assert(selected.isRight, selected.toString)
    assert(ordinary.isRight, ordinary.toString)
    assert(operator.isRight, operator.toString)

  test("Scalameta opt-in fails closed with stable missing and overload categories"):
    val missing = build(Seq("", ".", ""), name("missing"), applied = false)
    val inaccessible = build(Seq("", ".", ""), name("hidden"), applied = false)
    val overloaded = build(Seq("", ".", ""), name("overloaded"), applied = false)

    assertEquals(
      missing.left.map(_._1),
      Left("SELECTED_MEMBER_MISSING_OR_INACCESSIBLE")
    )
    assertEquals(
      overloaded.left.map(_._1),
      Left("SELECTED_MEMBER_NOT_UNIQUE")
    )
    assertEquals(
      inaccessible.left.map(_._1),
      Left("SELECTED_MEMBER_MISSING_OR_INACCESSIBLE")
    )
    assert(!missing.left.toOption.get._2.contains("dotty"))
    assert(!overloaded.left.toOption.get._2.contains("dotty"))

  test("Scalameta opt-in rejects the name wrapper in bare and dynamic infix positions"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val categories = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*
      val receiver = '{ ScalametaSelectedMemberNameTarget }.asTerm
      val argument = '{ 40 }.asTerm
      val selected = name("+")
      val bare = ScalametaTermFrontend.lower(using q)(Seq("", ""), Seq(selected))
      val infix = ScalametaTermFrontend.lower(using q)(
        Seq("", " ", " ", ""),
        Seq(receiver, selected, argument)
      )
      (bare.left.map(_.category), infix.left.map(_.category))

    assertEquals(categories._1, Left("UNSUPPORTED_SELECTED_MEMBER_NAME_POSITION"))
    assertEquals(categories._2, Left("UNSUPPORTED_SELECTED_MEMBER_NAME_POSITION"))
