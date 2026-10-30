package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors

final class Q078ScalametaGenericSelectedChainTest extends munit.FunSuite:
  test("Scalameta generated selected-chain NoSpan reports missing source safely"):
    val result = Q078NoSpanProbe.route((context, caller, q) => Q078ScalametaMacro.extractor(context, caller)(using q))
    assert(result.startsWith("sourceAbsent=true;static=true;"), result)
    assert(result.contains("Q078 selected chain requires recoverable original source"), result)
    assert(!result.contains("end of NoSpan"), result)
  test("Scalameta primary shares generic tuple synthesis at N=1,2,3,4,8"):
    assertEquals(Q078ScalametaProbe.observations, (1 to 4).map(n => s"n$n:true").toList :+ "n8:true")

  test("generated extractor preserves recursive matcher boundaries atomically"):
    assertEquals(Q078ScalametaProbe.boundaries, List(
      "wrapped", "long-suffix", "anchored-positive", "anchored-root", "equal-targets", "lexical", "unicode", "short",
      "apply", "typeapply", "named", "named-apply", "contextual", "right-associative", "null"
    ).map(_ + ":true"))

  test("N=1 selected binder 1 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a" =>
          val invalid: q.reflect.Term = a
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=2 selected binder 1 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b" =>
          val invalid: q.reflect.Term = a
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=2 selected binder 2 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b" =>
          val invalid: q.reflect.Term = b
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=3 selected binder 1 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b.$c" =>
          val invalid: q.reflect.Term = a
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=3 selected binder 2 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b.$c" =>
          val invalid: q.reflect.Term = b
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=3 selected binder 3 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b.$c" =>
          val invalid: q.reflect.Term = c
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=4 selected binder 1 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b.$c.$d" =>
          val invalid: q.reflect.Term = a
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=4 selected binder 2 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b.$c.$d" =>
          val invalid: q.reflect.Term = b
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=4 selected binder 3 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b.$c.$d" =>
          val invalid: q.reflect.Term = c
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=4 selected binder 4 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b.$c.$d" =>
          val invalid: q.reflect.Term = d
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=8 selected binder 1 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" =>
          val invalid: q.reflect.Term = a
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=8 selected binder 2 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" =>
          val invalid: q.reflect.Term = b
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=8 selected binder 3 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" =>
          val invalid: q.reflect.Term = c
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=8 selected binder 4 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" =>
          val invalid: q.reflect.Term = d
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=8 selected binder 5 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" =>
          val invalid: q.reflect.Term = e
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=8 selected binder 6 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" =>
          val invalid: q.reflect.Term = f
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=8 selected binder 7 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" =>
          val invalid: q.reflect.Term = g
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("N=8 selected binder 8 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" =>
          val invalid: q.reflect.Term = h
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)

  test("receiver is caller Term"):
    val errors = typeCheckErrors("""{
 import scala.quoted.*
 import quasiquotes.construct.SelectedMemberName
 import quasiquotes.matching.Q078ScalametaInterpolator.*
 def check(using q: Quotes)(target: q.reflect.Term) = target match
   case q078"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" =>
     val invalid: SelectedMemberName = receiver
     invalid
   case _ => ()
 }""")
    assert(errors.exists(_.message.contains("SelectedMemberName")), errors.toString)

  test("native E005 remains"):
    val errors = typeCheckErrors("""{
 import scala.quoted.*
 import quasiquotes.matching.Q078ScalametaInterpolator.*
 def check(using q: Quotes)(target: q.reflect.Term) = target match
   case q078"$receiver.$same.$same" => ()
   case _ => ()
 }""")
    assert(errors.exists(_.message.contains("duplicate pattern variable")), errors.toString)

  test("direct generic extractor invocation rejects a foreign Quotes Term"):
    val errors = typeCheckErrors("""{
 import scala.quoted.*
 import quasiquotes.matching.*
 type Roles = ProductTermCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: EmptyTuple
 def check(using q: Quotes)(plan: Q077StandardFlatModel.Plan)(using other: Quotes)(target: other.reflect.Term) =
   Q078TupleFactory.extractor[Roles](using q)(plan).unapply(target)
 }""")
    assert(errors.exists(_.message.contains("Term")), errors.toString)

  test("real public flat eight now compiles"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.scalameta.ScalametaQuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" => ()
        case _ => ()
    }""")
    assertEquals(errors, Nil)

  test("nonliteral StringContext does not infer a fabricated static tuple"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078ScalametaInterpolator.*
      def check(using q: Quotes)(sc: StringContext) = sc.q078
    }""")
    assert(errors.exists(_.message.contains("literal StringContext")), errors.toString)
