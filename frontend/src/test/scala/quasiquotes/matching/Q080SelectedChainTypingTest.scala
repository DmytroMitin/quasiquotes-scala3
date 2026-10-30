package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors

final class Q080SelectedChainTypingTest extends munit.FunSuite:
  test("public N=3 selected field 1 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c" =>
          val invalid: q.reflect.Term = a
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=3 selected field 2 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c" =>
          val invalid: q.reflect.Term = b
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=3 selected field 3 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c" =>
          val invalid: q.reflect.Term = c
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=4 selected field 1 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d" =>
          val invalid: q.reflect.Term = a
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=4 selected field 3 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d" =>
          val invalid: q.reflect.Term = c
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=4 selected field 4 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d" =>
          val invalid: q.reflect.Term = d
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=5 selected field 1 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e" =>
          val invalid: q.reflect.Term = a
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=5 selected field 3 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e" =>
          val invalid: q.reflect.Term = c
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=5 selected field 5 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e" =>
          val invalid: q.reflect.Term = e
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=6 selected field 1 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f" =>
          val invalid: q.reflect.Term = a
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=6 selected field 4 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f" =>
          val invalid: q.reflect.Term = d
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=6 selected field 6 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f" =>
          val invalid: q.reflect.Term = f
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=7 selected field 1 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f.$g" =>
          val invalid: q.reflect.Term = a
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=7 selected field 4 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f.$g" =>
          val invalid: q.reflect.Term = d
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=7 selected field 7 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f.$g" =>
          val invalid: q.reflect.Term = g
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=8 selected field 1 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" =>
          val invalid: q.reflect.Term = a
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=8 selected field 5 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" =>
          val invalid: q.reflect.Term = e
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=8 selected field 8 rejects Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" =>
          val invalid: q.reflect.Term = h
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Invalid qq")), errors.toString)

  test("public N=3 receiver rejects SelectedMemberName assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c" =>
          val invalid: SelectedMemberName = receiver
          invalid
        case _ => ()
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("public N=4 receiver rejects SelectedMemberName assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d" =>
          val invalid: SelectedMemberName = receiver
          invalid
        case _ => ()
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("public N=5 receiver rejects SelectedMemberName assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e" =>
          val invalid: SelectedMemberName = receiver
          invalid
        case _ => ()
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("public N=6 receiver rejects SelectedMemberName assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f" =>
          val invalid: SelectedMemberName = receiver
          invalid
        case _ => ()
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("public N=7 receiver rejects SelectedMemberName assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f.$g" =>
          val invalid: SelectedMemberName = receiver
          invalid
        case _ => ()
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("public N=8 receiver rejects SelectedMemberName assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" =>
          val invalid: SelectedMemberName = receiver
          invalid
        case _ => ()
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("public direct N=9 reports bounded capacity before tuple pattern typing"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$s0.$s1.$s2.$s3.$s4.$s5.$s6.$s7.$s8" => ()
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("selected-chain capacity")), errors.toString)
    assert(!errors.exists(_.message.contains("Wrong number of argument patterns")), errors.toString)

  test("public direct N=22 reports bounded capacity before tuple pattern typing"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$s0.$s1.$s2.$s3.$s4.$s5.$s6.$s7.$s8.$s9.$s10.$s11.$s12.$s13.$s14.$s15.$s16.$s17.$s18.$s19.$s20.$s21" => ()
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("selected-chain capacity")), errors.toString)
    assert(!errors.exists(_.message.contains("Wrong number of argument patterns")), errors.toString)

  test("unsupported repeated source remains rejected"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$same.$same.$last" => ()
        case _ => ()
    }""")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("duplicate pattern variable: same")), errors.toString)

  test("unsupported ranked source remains rejected"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c(..$args)" => ()
        case _ => ()
    }""")
    assert(errors.nonEmpty)

  test("unsupported repeat-marker source remains rejected"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c#repeat" => ()
        case _ => ()
    }""")
    assert(errors.nonEmpty)

  test("unsupported malformed-dots source remains rejected"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a..$b.$c" => ()
        case _ => ()
    }""")
    assert(errors.nonEmpty)

  test("legacy infix middle hole cannot capture a SelectedMemberName"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$left $operator $right" =>
          val name: quasiquotes.construct.SelectedMemberName = operator
          name
        case _ => ()
    }""")
    assert(errors.nonEmpty)

  test("foreign Quotes direct public extractor unapply is rejected"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.TermPatternProductExtractor
      def check(using q1: Quotes)(extractor: TermPatternProductExtractor[q1.reflect.Term,
        (q1.reflect.Term, SelectedMemberName, SelectedMemberName, SelectedMemberName)])
        (using q2: Quotes)(target: q2.reflect.Term) = extractor.unapply(target)
    }""")
    assert(errors.exists(_.message.contains("Type Mismatch")) ||
      errors.exists(e => e.message.contains("q1") && e.message.contains("q2")), errors.toString)
