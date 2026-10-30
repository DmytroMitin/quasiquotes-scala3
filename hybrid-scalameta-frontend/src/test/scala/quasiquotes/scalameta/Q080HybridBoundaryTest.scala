package quasiquotes.scalameta

import scala.compiletime.testing.typeCheckErrors

final class Q080HybridBoundaryTest extends munit.FunSuite:
  test("opt-in hybrid public N=3 now compiles through the parity route"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.scalameta.ScalametaQuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c" => ()
        case _ => ()
    }""")
    assertEquals(errors, Nil)

  test("opt-in hybrid public N=4 now compiles through the parity route"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.scalameta.ScalametaQuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d" => ()
        case _ => ()
    }""")
    assertEquals(errors, Nil)

  test("opt-in hybrid public N=5 now compiles through the parity route"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.scalameta.ScalametaQuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e" => ()
        case _ => ()
    }""")
    assertEquals(errors, Nil)

  test("opt-in hybrid public N=6 now compiles through the parity route"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.scalameta.ScalametaQuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f" => ()
        case _ => ()
    }""")
    assertEquals(errors, Nil)

  test("opt-in hybrid public N=7 now compiles through the parity route"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.scalameta.ScalametaQuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f.$g" => ()
        case _ => ()
    }""")
    assertEquals(errors, Nil)

  test("opt-in hybrid public N=8 now compiles through the parity route"):
    val errors = typeCheckErrors("""{
      import scala.quoted.*
      import quasiquotes.scalameta.ScalametaQuasiPattern.*
      def check(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$a.$b.$c.$d.$e.$f.$g.$h" => ()
        case _ => ()
    }""")
    assertEquals(errors, Nil)
