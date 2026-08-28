package quasiquotes.construct

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.staging.{Compiler, withQuotes}

object SelectedMemberNameTarget:
  def ordinary(value: Int): Int = value + 1
  def +(value: Int): Int = value + 2
  def `type`(value: Int): Int = value + 3
  def `safe spaced name`(value: Int): Int = value + 4
  def overloaded(value: Int): Int = value
  def overloaded(value: String): Int = value.length
  private def hidden(value: Int): Int = value

final class SelectedMemberNameHoleTest extends munit.FunSuite:
  private def name(value: String): SelectedMemberName =
    SelectedMemberName.from(value).fold(failure => fail(failure.message), identity)

  private def build(parts: Seq[String], arguments: Seq[Any]) =
    given Compiler = Compiler.make(getClass.getClassLoader)
    withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*
      val receiver = '{ SelectedMemberNameTarget }.asTerm
      val argument = '{ 40 }.asTerm
      val mapped = arguments.map {
        case "receiver" => receiver
        case "argument" => argument
        case selected: SelectedMemberName => selected
      }
      QuasiquoteBuilder.build(using q)(
        parts,
        mapped.asInstanceOf[
          Seq[q.reflect.Term | q.reflect.TypeRepr | QuasiTypeSplice | SelectedMemberName]
        ]
      ).left.map(_.message).map(_.show)

  test("constructs an explicit dynamic selection and existing ordinary application"):
    val selected = build(Seq("", ".", ""), Seq("receiver", name("ordinary")))
    val applied = build(
      Seq("", ".", "(", ")"),
      Seq("receiver", name("ordinary"), "argument")
    )

    assert(selected.exists(_.contains("SelectedMemberNameTarget.ordinary")))
    assert(applied.exists(_.contains("SelectedMemberNameTarget.ordinary(40)")))

  test("constructs supported operator, keyword, and safe spaced decoded names as explicit calls"):
    val decodedNames = Vector("+", "type", "safe spaced name")
    val results = decodedNames.map { decoded =>
      build(
        Seq("", ".", "(", ")"),
        Seq("receiver", name(decoded), "argument")
      )
    }

    assert(results.forall(_.isRight), results.toString)

  test("uses a dedicated collision-safe deterministic name placeholder"):
    val selected = name("ordinary")
    val first = PlaceholderSource.synthesizeCategorized(
      Seq("__qq_name_hole_1; ", ".", ""),
      Seq(
        QuasiquoteHole.Term("receiver"),
        QuasiquoteHole.SelectedMemberNameSplice(selected)
      )
    ).toOption.get
    val second = PlaceholderSource.synthesizeCategorized(
      Seq("__qq_name_hole_1; ", ".", ""),
      Seq(
        QuasiquoteHole.Term("receiver"),
        QuasiquoteHole.SelectedMemberNameSplice(selected)
      )
    ).toOption.get

    assertEquals(first, second)
    assertEquals(first.bindings.map(_.name), Vector("__qq_term_hole_0", "__qq_name_hole_1_1"))

  test("rejects a name wrapper outside an explicit selection name field"):
    val bare = build(Seq("", ""), Seq(name("ordinary")))
    val constructor = build(Seq("new ", "()"), Seq(name("ordinary")))
    val infix = build(
      Seq("", " ", " ", ""),
      Seq("receiver", name("+"), "argument")
    )

    assertEquals(
      bare,
      Left(
        "Selected-member name hole is not supported in term position; only the name field of an explicit receiver selection is supported."
      )
    )
    assert(constructor.left.exists(_.contains("only the name field of an explicit receiver selection")))
    assert(infix.left.exists(_.contains("only the name field of an explicit receiver selection")))

  test("fails closed for missing and overloaded selected members without backend detail"):
    val missing = build(Seq("", ".", ""), Seq("receiver", name("missing")))
    val inaccessible = build(Seq("", ".", ""), Seq("receiver", name("hidden")))
    val overloaded = build(Seq("", ".", ""), Seq("receiver", name("overloaded")))

    assertEquals(
      missing,
      Left("Selected member 'missing' is missing or inaccessible on the explicit receiver.")
    )
    assertEquals(
      overloaded,
      Left("Selected member 'overloaded' is not unique on the explicit receiver.")
    )
    assertEquals(
      inaccessible,
      Left("Selected member 'hidden' is missing or inaccessible on the explicit receiver.")
    )
    assert(!missing.left.toOption.get.contains("dotty"))
    assert(!overloaded.left.toOption.get.contains("dotty"))

  test("ordinary String is not accepted as a selected-member name hole"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.Quotes
        import quasiquotes.construct.Quasiquotes.*
        def attempt(using q: Quotes)(receiver: q.reflect.Term, raw: String) =
          qr\"$receiver.$raw\"
      }"""
    )

    assert(errors.nonEmpty)

  test("qq matching surface does not accept a selected-member name value"):
    val errors = typeCheckErrors(
      """{
        import quasiquotes.matching.Quasiquotes.*
        val selected = quasiquotes.construct.SelectedMemberName.from(\"ordinary\").toOption.get
        qq\"target.$selected\"
      }"""
    )

    assert(errors.nonEmpty)
