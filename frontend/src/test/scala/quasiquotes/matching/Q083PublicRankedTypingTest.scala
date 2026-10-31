package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors

final class Q083PublicRankedTypingTest extends munit.FunSuite:
  test("accepted one-selected-name rank2 is the public ordinary Unicode binder control"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$réceiver.$selected(..$ärguments)" =>
          val r: q.reflect.Term = réceiver
          val as: Seq[q.reflect.Term] = ärguments
          (r, selected, as)
        case _ => ()
    }""")
    assertEquals(errors, Nil)

  test("public ordinary Unicode receiver and sequence binders retain their Term roles"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$réceiver.$first.$second(..$ärguments)" =>
          val r: q.reflect.Term = réceiver
          val a: SelectedMemberName = first
          val b: SelectedMemberName = second
          val as: Seq[q.reflect.Term] = ärguments
          (r, a, b, as)
        case _ => ()
    }""")
    assertEquals(errors, Nil)

  test("standard public two-name terminal rank2 infers all four source-role types"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second(..$arguments)" =>
          val r: q.reflect.Term = receiver
          val a: SelectedMemberName = first
          val b: SelectedMemberName = second
          val as: Seq[q.reflect.Term] = arguments
          (r, a, b, as)
        case _ => ()
    }""")
    assertEquals(errors, Nil)

  test("receiver rejects selected-name assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second(..$arguments)" =>
          val invalid: SelectedMemberName = receiver
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("SelectedMemberName")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("first selected-name rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second(..$arguments)" =>
          val invalid: q.reflect.Term = first
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("SelectedMemberName")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("second selected-name rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second(..$arguments)" =>
          val invalid: q.reflect.Term = second
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("SelectedMemberName")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("sequence rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second(..$arguments)" =>
          val invalid: q.reflect.Term = arguments
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("Seq")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("sequence rejects selected-name element assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second(..$arguments)" =>
          val invalid: Seq[SelectedMemberName] = arguments
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("SelectedMemberName")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("missing tuple field is a static arity error"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second(..$arguments)" =>
          val invalid: (q.reflect.Term, SelectedMemberName, SelectedMemberName) = (receiver, first, second, arguments)
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("Required")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("extra tuple field is a static arity error"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second(..$arguments)" =>
          val invalid: (q.reflect.Term, SelectedMemberName, SelectedMemberName, Seq[q.reflect.Term], q.reflect.Term) = (receiver, first, second, arguments)
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("Required")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("N3 mixed remains outside the selected public grammar"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c(..$arguments)" =>
          ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, errors.toString)

  test("multiple ranks remains outside the selected public grammar"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second(..$arguments, ..$others)" =>
          ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, errors.toString)

  test("rank3 remains outside the selected public grammar"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second(...$arguments)" =>
          ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, errors.toString)

  test("extra scalar prefix remains outside the selected public grammar"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second($before, ..$arguments)" =>
          ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, errors.toString)

  test("extra scalar suffix remains outside the selected public grammar"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second(..$arguments, $after)" =>
          ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, errors.toString)

  test("nested ranked argument remains outside the selected public grammar"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second(wrap(..$arguments))" =>
          ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, errors.toString)

  test("nonterminal clause remains outside the selected public grammar"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second(..$arguments)(1)" =>
          ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, errors.toString)

  test("curried source remains outside the selected public grammar"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second()(..$arguments)" =>
          ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, errors.toString)

  test("type-applied source remains outside the selected public grammar"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second[Int](..$arguments)" =>
          ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, errors.toString)

  test("contextual source remains outside the selected public grammar"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second(using ..$arguments)" =>
          ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, errors.toString)

  test("named sequence source remains outside the selected public grammar"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second(value = ..$arguments)" =>
          ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, errors.toString)

  test("repeat marker remains outside the selected public grammar"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.#repeat(..$arguments)" =>
          ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, errors.toString)

  test("selected infix source remains outside the selected public grammar"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver $first $second(..$arguments)" =>
          val invalid: SelectedMemberName = first
        case _ => ()
    }""")
    assert(errors.nonEmpty, errors.toString)

  test("native repeated selected source binders retain E005"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$same.$same(..$arguments)" =>
          ()
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("duplicate pattern variable: same")), errors.toString)
