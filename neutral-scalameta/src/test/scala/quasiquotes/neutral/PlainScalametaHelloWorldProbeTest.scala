package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3
import scala.meta.quasiquotes.{q as nqq}
import scala.meta.quasiquotes.{q as nqr}

/** The aliases are consumer-owned names for upstream Scalameta `q`, not library APIs. */
@nowarn("cat=deprecation")
final class PlainScalametaHelloWorldProbeTest extends munit.FunSuite:
  test("plain Scalameta builds and matches a minimal addition"):
    val left: Term = Lit.Int(20)
    val right: Term = Lit.Int(22)
    val addition: Term = nqr"$left + $right"

    val sum = addition match
      case nqq"${Lit.Int(a)} + ${Lit.Int(b)}" => a + b
      case other => fail(s"Unexpected Scalameta term: ${other.syntax}")

    assertEquals(sum, 42)
