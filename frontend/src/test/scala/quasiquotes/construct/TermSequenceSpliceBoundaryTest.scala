package quasiquotes.construct

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class TermSequenceSpliceBoundaryTest extends munit.FunSuite:
  private def messages(parts: Seq[String], argumentKinds: Seq[String]): Either[String, String] =
    given Compiler = Compiler.make(getClass.getClassLoader)
    withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*
      val term = Expr(1).asTerm
      val sequence = TermSequenceSplices.termSplice(Seq(term))
      val reflectedType = TypeRepr.of[java.lang.StringBuilder]
      val selectedName = SelectedMemberName.from("size").toOption.get
      val arguments = argumentKinds.map {
        case "term" => term
        case "sequence" => sequence
        case "type" => reflectedType
        case "name" => selectedName
      }
      QuasiquoteBuilder
        .buildLocated(using q)(
          parts,
          arguments.asInstanceOf[
            Seq[Term | TypeRepr | QuasiTypeSplice | SelectedMemberName | TermSequenceSplice[Term]]
          ]
        )
        .left
        .map(_.error.message)
        .map(_.show)

  test("rank marker and carrier must occur together"):
    assertEquals(
      messages(Seq("identity(", ")"), Seq("sequence")),
      Left("Sequence-Term splice argument 0 requires an immediately adjacent `..` rank marker.")
    )
    assertEquals(
      messages(Seq("identity(..", ")"), Seq("term")),
      Left("The `..` rank marker before argument 0 requires a Sequence-Term splice carrier, not a Term splice.")
    )

  test("sequence carriers are limited to direct Apply/New argument-list elements"):
    assertEquals(
      messages(Seq("..", ""), Seq("sequence")),
      Left("Sequence-Term splice `__qq_terms_hole_0` is not supported in term position; only a direct Apply or one-list New argument is supported.")
    )

  test("one repeated hole per list and one ordinary argument list are enforced"):
    assertEquals(
      messages(Seq("identity2(..", ", ..", ")"), Seq("sequence", "sequence")),
      Left("Only one Sequence-Term splice is supported per argument list; found 2.")
    )
    assertEquals(
      messages(Seq("identity(", ")(..", ")"), Seq("term", "sequence")),
      Left("Sequence-Term splices are not supported in additional argument-list topology.")
    )

  test("wrong categories and orphan markers fail before guest parsing"):
    assertEquals(
      messages(Seq("new ..", "()"), Seq("type")),
      Left("The `..` rank marker before argument 0 requires a Sequence-Term splice carrier, not a Reflected-Type splice.")
    )
    assertEquals(
      messages(Seq("identity(..", ")"), Seq("name")),
      Left("The `..` rank marker before argument 0 requires a Sequence-Term splice carrier, not a Selected-member name splice.")
    )
    assertEquals(
      messages(Seq("identity(..)"), Seq.empty),
      Left("Orphan or ambiguous `..` rank marker in literal part 0.")
    )

  test("double dots inside guest strings and comments are not rank markers"):
    val insideString = PlaceholderSource.synthesizeCategorized(
      Seq("s\"", " ..\""),
      Seq(QuasiquoteHole.Term("term"))
    )
    val insideComment = PlaceholderSource.synthesizeCategorized(
      Seq("identity(/* .. */ ", ")"),
      Seq(QuasiquoteHole.Term("term"))
    )
    val sequenceInsideString = PlaceholderSource.synthesizeCategorized(
      Seq("s\"..", "\""),
      Seq(QuasiquoteHole.TermSequence(Vector("term")))
    )

    assert(insideString.isRight)
    assert(insideComment.isRight)
    assertEquals(
      sequenceInsideString.left.map(_.message),
      Left("Sequence-Term splice argument 0 requires an immediately adjacent `..` rank marker.")
    )

  test("an arbitrary Seq is not an interpolation carrier"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.Quotes
        import quasiquotes.construct.Quasiquotes.*
        def attempt(using q: Quotes)(terms: Seq[q.reflect.Term]) =
          qr\"identity(..$terms)\"
      }"""
    )

    assert(errors.nonEmpty)

  test("covariance does not admit definition statements as Term sequences"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.Quotes
        import quasiquotes.construct.Quasiquotes.*
        import quasiquotes.construct.TermSequenceSplices.termSplice
        def attempt(using q: Quotes)(definition: q.reflect.DefDef) =
          val definitions = termSplice(Seq(definition))
          qr\"identity(..$definitions)\"
      }"""
    )

    assert(errors.nonEmpty)

  test("matching-side sequence capture remains unavailable"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.Quotes
        import quasiquotes.construct.TermSequenceSplices.termSplice
        import quasiquotes.matching.QuasiPattern.qq
        def attempt(using q: Quotes)(term: q.reflect.Term) =
          val captures = termSplice(Seq(term))
          qq\"identity(..$captures)\"
      }"""
    )

    assert(errors.nonEmpty)
