package quasiquotes.q072

import scala.compiletime.testing.typeCheckErrors

import quasiquotes.construct.SelectedMemberName
import quasiquotes.definitions.DefinitionName
import quasiquotes.matching.{SelectedNameSourceNormalizer, SelectedNameSourcePosition}

final class Q072PatternBinderSyntaxFeasibilityTest extends munit.FunSuite:
  private inline def messages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  test("Q071 E005 is independent of a deduplicated two-field extractor"):
    val errors = typeCheckErrors(
      """{
        final class Extractor2:
          def unapply(value: Int): Option[(Int, String)] = Some((value, "same"))
        extension (sc: StringContext)
          def q2: Extractor2 = new Extractor2
        def probe(value: Int) = value match
          case q2"$receiver.$same.$same" => (receiver, same)
          case _ => (value, "")
      }"""
    )
    val duplicate = errors.find(_.message.contains("duplicate pattern variable: same"))
    assert(duplicate.nonEmpty, clues(errors.map(_.message)))
    assert(
      duplicate.get.lineContent.contains("case q2\"$receiver.$same.$same\""),
      clues(duplicate.get)
    )
    assert(duplicate.get.column >= 0, clues(duplicate.get))

  test("distinct direct and unary binders compile with three- and four-field products"):
    val direct = messages(
      """{
        final class Extractor3:
          def unapply(value: Int): Option[(Int, String, String)] =
            Some((value, "inner", "outer"))
        extension (sc: StringContext)
          def q3: Extractor3 = new Extractor3
        def probe(value: Int) = value match
          case q3"$receiver.$first.$second" => (receiver, first, second)
          case _ => (value, "", "")
      }"""
    )
    val unary = messages(
      """{
        final class Extractor4:
          def unapply(value: Int): Option[(Int, String, String, Int)] =
            Some((value, "inner", "outer", 1))
        extension (sc: StringContext)
          def q4: Extractor4 = new Extractor4
        def probe(value: Int) = value match
          case q4"$receiver.$first.$second($argument)" =>
            (receiver, first, second, argument)
          case _ => (value, "", "", 0)
      }"""
    )
    assertEquals(direct, Nil)
    assertEquals(unary, Nil)

  test("StringContext hole spelling is not validated by a fixed stub tuple arity"):
    val tooFew = messages(
      """{
        final class Extractor3:
          def unapply(value: Int): Option[(Int, String, String)] = None
        extension (sc: StringContext)
          def q3: Extractor3 = new Extractor3
        def probe(value: Int) = value match
          case q3"$receiver.$first" => (receiver, first)
          case _ => (value, "")
      }"""
    )
    val tooMany = messages(
      """{
        final class Extractor3:
          def unapply(value: Int): Option[(Int, String, String)] = None
        extension (sc: StringContext)
          def q3: Extractor3 = new Extractor3
        def probe(value: Int) = value match
          case q3"$receiver.$first.$second.$third" =>
            (receiver, first, second, third)
          case _ => (value, "", "", "")
      }"""
    )
    assertEquals(tooFew, Nil)
    assertEquals(tooMany, Nil)

  test("wildcard and stable identifier holes are compiler-legal but mean different things"):
    val wildcard = messages(
      """{
        final class Extractor3:
          def unapply(value: Int): Option[(Int, String, String)] = None
        extension (sc: StringContext)
          def q3: Extractor3 = new Extractor3
        def probe(value: Int) = value match
          case q3"$receiver.$first.${_}" => (receiver, first)
          case _ => (value, "")
      }"""
    )
    val stable = messages(
      """{
        final class Extractor3:
          def unapply(value: Int): Option[(Int, String, String)] = None
        extension (sc: StringContext)
          def q3: Extractor3 = new Extractor3
        def probe(value: Int, expected: String) = value match
          case q3"$receiver.${`expected`}.${`expected`}" => receiver
          case _ => -1
      }"""
    )
    assertEquals(wildcard, Nil)
    assertEquals(stable, Nil)

  test("test-only typed route separates guards, wildcards, constants and atomic marker equality"):
    assertEquals(
      Q072PublicPatternObservation.value,
      List.fill(11)("true").mkString(":")
    )

  test("StringContext parts can distinguish an explicit marker but not binder from wildcard"):
    assertEquals(StringContext("", ".", ".", "").parts.toList, List("", ".", ".", ""))
    assertEquals(StringContext("", ".", ".#repeat").parts.toList, List("", ".", ".#repeat"))
    assert(SelectedMemberName.from("#repeat").isLeft)

  test("current public qq classifier remains an independent rejection stage"):
    val distinct = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second" => (receiver, first, second)
          case _ => ()"""
    )
    val marker = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$same.#repeat" => (receiver, same)
          case _ => ()"""
    )
    Vector(distinct, marker).foreach { errors =>
      val classifier = errors.find(_.message.contains("Invalid qq term-pattern template"))
      assert(classifier.nonEmpty, clues(errors.map(_.message)))
      assert(classifier.get.lineContent.contains("case qq\""), clues(classifier.get))
      assert(classifier.get.column >= 0, clues(classifier.get))
    }

  test("source occurrence evidence preserves transports and root-first ordinals 1 then 0"):
    val normalized = SelectedNameSourceNormalizer
      .normalize(
        "$receiver.$first.$second",
        Vector(SelectedNameSourcePosition(1, 1), SelectedNameSourcePosition(2, 0))
      )
      .toOption
      .get
    assertEquals(normalized.selectedOccurrences.map(_.name), Vector("first", "second"))
    assertEquals(normalized.selectedOccurrences.map(_.selectOrdinal), Vector(1, 0))
    assertEquals(normalized.selectedOccurrences.map(_.transport).distinct.size, 2)
    assert(normalized.selectedOccurrences.forall(value =>
      normalized.ordinaryHoleIndex.semanticNameFor(value.transport).isEmpty
    ))

  test("category collisions and invalid occurrence plans fail closed"):
    val positions = Vector(SelectedNameSourcePosition(1, 1), SelectedNameSourcePosition(2, 0))
    assert(SelectedNameSourceNormalizer.normalize("$first.$first.$second", positions).isLeft)
    assert(
      SelectedNameSourceNormalizer
        .normalize("$receiver.$first.$second", Vector(positions.head, positions.head))
        .isLeft
    )
    val thirdOrdinary = SelectedNameSourceNormalizer
      .normalize("$receiver.$first.$second.$third", positions)
      .toOption
      .get
    assertEquals(thirdOrdinary.selectedOccurrences.map(_.name), Vector("first", "second"))
    assertEquals(thirdOrdinary.occurrences.map(_.name), Vector("receiver", "first", "second", "third"))

  test("Q target lexing remains wider than neutral definition-name transport"):
    Vector("+", "type", "safe spaced name").foreach { name =>
      assert(SelectedMemberName.from(name).isRight, clues(name))
      assert(DefinitionName.plain(name).isLeft, clues(name))
    }
    Vector("naïve", "$internal", "#repeat").foreach { name =>
      assert(SelectedMemberName.from(name).isLeft, clues(name))
    }

  test("unsupported three-name source is rejected by the bounded test macro"):
    val errors = messages(
      """import scala.quoted.*
        import quasiquotes.q072.Q072PublicPatternProbe.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case q072qq"$receiver.$first.$second.$third" => ()
          case _ => ()"""
    )
    assert(errors.exists(_.contains("Q072 probe does not implement")), clues(errors))
