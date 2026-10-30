package quasiquotes.scalameta

import scala.compiletime.testing.typeCheckErrors

final class Q080HybridBoundaryTest extends munit.FunSuite:
  test("opt-in hybrid public N=3 is still outside its accepted N<=2 surface"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.scalameta.ScalametaQuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c" => ()
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("selected-name capture is supported only")), errors.toString)

  test("opt-in hybrid public N=4 is still outside its accepted N<=2 surface"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.scalameta.ScalametaQuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d" => ()
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("selected-name capture is supported only")), errors.toString)

  test("opt-in hybrid public N=5 is still outside its accepted N<=2 surface"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.scalameta.ScalametaQuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e" => ()
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("selected-name capture is supported only")), errors.toString)

  test("opt-in hybrid public N=6 is still outside its accepted N<=2 surface"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.scalameta.ScalametaQuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f" => ()
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("selected-name capture is supported only")), errors.toString)

  test("opt-in hybrid public N=7 is still outside its accepted N<=2 surface"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.scalameta.ScalametaQuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f.$g" => ()
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("selected-name capture is supported only")), errors.toString)

  test("opt-in hybrid public N=8 is still outside its accepted N<=2 surface"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.scalameta.ScalametaQuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" => ()
        case _ => ()
    }""")
    assert(errors.exists(_.message.contains("selected-name capture is supported only")), errors.toString)

