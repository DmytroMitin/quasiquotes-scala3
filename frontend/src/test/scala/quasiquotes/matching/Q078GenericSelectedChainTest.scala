package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors

final class Q078GenericSelectedChainTest extends munit.FunSuite:
  test("generated selected-chain NoSpan reports missing source safely"):
    val result = Q078NoSpanProbe.standard
    assert(result.startsWith("sourceAbsent=true;static=true;"), result)
    assert(result.contains("Q078 selected chain requires recoverable original source"), result)
    assert(!result.contains("end of NoSpan"), result)
  test("one macro synthesizes typed source-order products at N=1,2,3,4,8"):
    assertEquals(Q078StandardProbe.observations, (1 to 4).map(n => s"n$n:true").toList :+ "n8:true")

  test("generated extractor preserves recursive matcher boundaries atomically"):
    assertEquals(Q078StandardProbe.boundaries, List(
      "wrapped", "long-suffix", "anchored-positive", "anchored-root", "equal-targets", "lexical", "unicode", "short",
      "apply", "typeapply", "named", "named-apply", "contextual", "right-associative", "null"
    ).map(_ + ":true"))

  test("N=1 selected binder 1 has inferred SelectedMemberName type"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.construct.SelectedMemberName
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
      import quasiquotes.matching.Q078StandardInterpolator.*
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
 import quasiquotes.matching.Q078StandardInterpolator.*
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
 import quasiquotes.matching.Q078StandardInterpolator.*
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

  test("generic planner scales beyond the Q077 fixture names and preserves lexical occurrence order"):
    for depth <- List(1, 2, 3, 4, 8, 24) do
      val binders = Vector.tabulate(depth)(i => s"slot$i")
      val source = "$origin" + binders.map(name => s".$$${name}").mkString
      val parts = List("") ++ List.fill(depth)(".") ++ List("")
      val plan = Q077StandardFlatModel.planSource(source, parts).fold(fail(_), identity)
      assertEquals(plan.selectedOccurrences.map(_.name), binders)
      assertEquals(plan.selectedOccurrences.map(_.selectOrdinal), (0 until depth).reverse.toVector)
      assertEquals(plan.sourceStarts.distinct.size, depth + 1)
      assertEquals(plan.sourceStarts.sorted, plan.sourceStarts)
    assert(Q077StandardFlatModel.planSource("$receiver.$same.$same", List("", ".", ".", "")).isLeft)
    assert(Q077StandardFlatModel.planSource("$receiver.$naïve", List("", ".", "")).isLeft)
    assert(Q077StandardFlatModel.planSource(null, List("", ".", "")).isLeft)
    assert(Q077StandardFlatModel.planSource("$receiver", List("", "")).isLeft)

  test("checked tuple builder refuses both too few and surplus names before indexing"):
    import quasiquotes.construct.SelectedMemberName
    type Roles = ProductTermCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: EmptyTuple
    val name = SelectedMemberName.from("same").fold(error => fail(error.toString), identity)
    assertEquals(Q078TupleFactory.checkedTuple[String, Roles]("receiver", Vector.empty), None)
    assertEquals(Q078TupleFactory.checkedTuple[String, Roles]("receiver", Vector(name)), None)
    assertEquals(Q078TupleFactory.checkedTuple[String, Roles]("receiver", Vector.fill(3)(name)), None)
    val tuple: Option[(String, SelectedMemberName, SelectedMemberName)] =
      Q078TupleFactory.checkedTuple[String, Roles]("receiver", Vector(name, name))
    assertEquals(tuple.map(_.productArity), Some(3))
    assertEquals(tuple, Some(("receiver", name, name)))

  test("real public flat eight remains unsupported"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" => ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, errors.toString)

  test("nonliteral StringContext does not infer a fabricated static tuple"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(sc: StringContext) = sc.q078
    }""")
    assert(errors.exists(_.message.contains("literal StringContext")), errors.toString)
