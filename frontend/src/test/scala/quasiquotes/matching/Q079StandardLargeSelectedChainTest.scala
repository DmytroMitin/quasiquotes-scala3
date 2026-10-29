package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors

final class Q079StandardLargeSelectedChainTest extends munit.FunSuite:
  test("unchanged generic q078 really types and executes large positional products"):
    assertEquals(Q079StandardProbe.observations, List("n8:arity=9:identity=true:public=true:names=true:owners=true", "n12:arity=13:identity=true:public=true:names=true:owners=true", "n16:arity=17:identity=true:public=true:names=true:owners=true", "n21:arity=22:identity=true:public=true:names=true:owners=true"))

  test("N=8 selected field 1 refuses a Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8" =>
          val invalid: q.reflect.Term = slot1
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Wrong number of argument patterns")), errors.toString)

  test("N=8 selected field 4 refuses a Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8" =>
          val invalid: q.reflect.Term = slot4
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Wrong number of argument patterns")), errors.toString)

  test("N=8 selected field 8 refuses a Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8" =>
          val invalid: q.reflect.Term = slot8
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Wrong number of argument patterns")), errors.toString)

  test("N=12 selected field 1 refuses a Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12" =>
          val invalid: q.reflect.Term = slot1
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Wrong number of argument patterns")), errors.toString)

  test("N=12 selected field 6 refuses a Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12" =>
          val invalid: q.reflect.Term = slot6
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Wrong number of argument patterns")), errors.toString)

  test("N=12 selected field 12 refuses a Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12" =>
          val invalid: q.reflect.Term = slot12
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Wrong number of argument patterns")), errors.toString)

  test("N=16 selected field 1 refuses a Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12.$slot13.$slot14.$slot15.$slot16" =>
          val invalid: q.reflect.Term = slot1
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Wrong number of argument patterns")), errors.toString)

  test("N=16 selected field 8 refuses a Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12.$slot13.$slot14.$slot15.$slot16" =>
          val invalid: q.reflect.Term = slot8
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Wrong number of argument patterns")), errors.toString)

  test("N=16 selected field 16 refuses a Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12.$slot13.$slot14.$slot15.$slot16" =>
          val invalid: q.reflect.Term = slot16
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Wrong number of argument patterns")), errors.toString)

  test("N=21 selected field 1 refuses a Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12.$slot13.$slot14.$slot15.$slot16.$slot17.$slot18.$slot19.$slot20.$slot21" =>
          val invalid: q.reflect.Term = slot1
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Wrong number of argument patterns")), errors.toString)

  test("N=21 selected field 11 refuses a Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12.$slot13.$slot14.$slot15.$slot16.$slot17.$slot18.$slot19.$slot20.$slot21" =>
          val invalid: q.reflect.Term = slot11
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Wrong number of argument patterns")), errors.toString)

  test("N=21 selected field 21 refuses a Term assignment"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12.$slot13.$slot14.$slot15.$slot16.$slot17.$slot18.$slot19.$slot20.$slot21" =>
          val invalid: q.reflect.Term = slot21
          invalid
        case _ => target
    }""")
    assert(errors.exists(e => e.message.contains("SelectedMemberName") && e.message.contains("Term")), errors.toString)
    assert(!errors.exists(_.message.contains("Wrong number of argument patterns")), errors.toString)

  test("N=22 original q078 flat product reaches the extractor pattern arity boundary"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term): Unit = target match
        case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12.$slot13.$slot14.$slot15.$slot16.$slot17.$slot18.$slot19.$slot20.$slot21.$slot22" => ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, "expected measured compiler boundary")
    assert(errors.head.message.contains("Wrong number of argument patterns"), errors.toString)
    assert(!errors.exists(e => e.message.contains("duplicate pattern variable") || e.message.contains("recoverable original source") || e.message.contains("requires a direct flat")), errors.toString)
    println("Q079 Standard N=22 E107: " + errors.head.message)

  test("N=23 original q078 flat product reaches the extractor pattern arity boundary"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term): Unit = target match
        case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12.$slot13.$slot14.$slot15.$slot16.$slot17.$slot18.$slot19.$slot20.$slot21.$slot22.$slot23" => ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, "expected measured compiler boundary")
    assert(errors.head.message.contains("Wrong number of argument patterns"), errors.toString)
    assert(!errors.exists(e => e.message.contains("duplicate pattern variable") || e.message.contains("recoverable original source") || e.message.contains("requires a direct flat")), errors.toString)
    println("Q079 Standard N=23 E107: " + errors.head.message)

  test("N=24 original q078 flat product reaches the extractor pattern arity boundary"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term): Unit = target match
        case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12.$slot13.$slot14.$slot15.$slot16.$slot17.$slot18.$slot19.$slot20.$slot21.$slot22.$slot23.$slot24" => ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, "expected measured compiler boundary")
    assert(errors.head.message.contains("Wrong number of argument patterns"), errors.toString)
    assert(!errors.exists(e => e.message.contains("duplicate pattern variable") || e.message.contains("recoverable original source") || e.message.contains("requires a direct flat")), errors.toString)
    println("Q079 Standard N=24 E107: " + errors.head.message)

  test("N=32 original q078 flat product reaches the extractor pattern arity boundary"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term): Unit = target match
        case q078"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12.$slot13.$slot14.$slot15.$slot16.$slot17.$slot18.$slot19.$slot20.$slot21.$slot22.$slot23.$slot24.$slot25.$slot26.$slot27.$slot28.$slot29.$slot30.$slot31.$slot32" => ()
        case _ => ()
    }""")
    assert(errors.nonEmpty, "expected measured compiler boundary")
    assert(errors.head.message.contains("Wrong number of argument patterns"), errors.toString)
    assert(!errors.exists(e => e.message.contains("duplicate pattern variable") || e.message.contains("recoverable original source") || e.message.contains("requires a direct flat")), errors.toString)
    println("Q079 Standard N=32 E107: " + errors.head.message)

  test("public qq N=3 stays rejected by the public selected source classifier"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term): Unit = target match
        case qq"$receiver.$slot1.$slot2.$slot3" => ()
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("Invalid qq term-pattern template")), errors.toString)
    assert(!errors.exists(_.message.contains("duplicate pattern variable")), errors.toString)

  test("public qq N=23 stays rejected by the public selected source classifier"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term): Unit = target match
        case qq"$receiver.$slot1.$slot2.$slot3.$slot4.$slot5.$slot6.$slot7.$slot8.$slot9.$slot10.$slot11.$slot12.$slot13.$slot14.$slot15.$slot16.$slot17.$slot18.$slot19.$slot20.$slot21.$slot22.$slot23" => ()
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("Invalid qq term-pattern template")), errors.toString)
    assert(!errors.exists(_.message.contains("duplicate pattern variable")), errors.toString)

  test("independent optional Tuple22 extractor compiler control"):
    val errors = typeCheckErrors("""{
      object Extract:
        def unapply(value: Int): Option[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = None
      def check(value: Int): Unit = value match
        case Extract(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21) => ()
        case _ => ()
    }""")
    assertEquals(errors, Nil)

  test("independent optional Tuple23 extractor compiler control"):
    val errors = typeCheckErrors("""{
      object Extract:
        def unapply(value: Int): Option[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = None
      def check(value: Int): Unit = value match
        case Extract(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22) => ()
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("Wrong number of argument patterns")), errors.toString)
    println("Q079 independent Option[Tuple23]: " + errors.head.message)

  test("direct typed 25-field tuple executes beyond the flat pattern boundary"):
    assertEquals(Q079StandardDirectProbe.observations, List("direct24:true", "count-short:true", "count-surplus:true", "suffix-vs-root:true"))

  test("large source occurrence plans retain order and unique transports before pattern typing"):
    for depth <- List(8, 12, 16, 21, 22, 23, 24, 32) do
      val names = Vector.tabulate(depth)(i => if i % 2 == 0 then s"slot${i}__q077_selected_${depth - i - 1}" else s"slot$i")
      val source = "$origin" + names.map(name => s".$$${name}").mkString
      val parts = List("") ++ List.fill(depth)(".") ++ List("")
      val plan = Q077StandardFlatModel.planSource(source, parts).fold(fail(_), identity)
      assertEquals(plan.selectedOccurrences.map(_.name), names)
      assertEquals(plan.selectedOccurrences.map(_.selectOrdinal), (0 until depth).reverse.toVector)
      assertEquals(plan.selectedOccurrences.map(_.transport).distinct.size, depth)
      assertEquals(plan.sourceStarts.distinct.size, depth + 1)
      assertEquals(plan.sourceStarts.sorted, plan.sourceStarts)
      val mapped = PatternSource.synthesizeMapped(source).fold(e => fail(e.message), identity)
      assert(plan.selectedOccurrences.forall(o => !mapped.occurrences.map(_.generatedName).contains(o.transport)))
      val compiled = QuasiPattern.term(plan.effectiveSource).fold(e => fail(e.message), identity)
      assertEquals(Q077StandardFlatModel.validate(compiled.pattern, plan.receiverName, plan.selectedOccurrences), Right(()))

  test("large planner source failures remain distinct from target-name decoding"):
    assert(Q077StandardFlatModel.planSource(null, List("", ".", "")).isLeft)
    assert(Q077StandardFlatModel.planSource("$receiver.$naïve", List("", ".", "")).isLeft)
    assert(Q077StandardFlatModel.planSource("$receiver.$same.$same", List("", ".", ".", "")).isLeft)
    val source = "$receiver" + (1 to 24).map(i => s".$$slot$i").mkString
    assert(Q077StandardFlatModel.planSource(source, List("", ".", "")).isLeft)

  test("q078 rejects non-direct selected parts $a(..$arguments)"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term): Unit = target match
        case q078"$receiver.$a(..$arguments)" => ()
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("Q078 requires a direct flat selected chain")), errors.toString)

  test("q078 rejects non-direct selected parts $a.$b($argument)"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term): Unit = target match
        case q078"$receiver.$a.$b($argument)" => ()
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("Q078 requires a direct flat selected chain")), errors.toString)

  test("q078 rejects non-direct selected parts $a...$b"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.matching.Q078StandardInterpolator.*
      def check(using q: Quotes)(target: q.reflect.Term): Unit = target match
        case q078"$receiver.$a...$b" => ()
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("Q078 requires a direct flat selected chain")), errors.toString)
