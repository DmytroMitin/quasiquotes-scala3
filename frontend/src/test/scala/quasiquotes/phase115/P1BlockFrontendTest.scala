package quasiquotes.phase115

import scala.collection.mutable.ArrayBuffer

import quasiquotes.matching.{PatternError, QuasiPattern}
import quasiquotes.parser.{TermShape, TinyTermParser}
import quasiquotes.source.DiagnosticPrecision

private object P1BlockFrontendFixtures:
  def mark(log: ArrayBuffer[Int], value: Int): Int =
    log += value
    value

  def constant(value: Int): Int = value

class P1BlockFrontendTest extends munit.FunSuite:
  import P1BlockFrontendFixtures.*

  test("raw parser exposes a P1 block with ordered expression prefixes and a final result") {
    val parsed = TinyTermParser.parse("{ first(); second(2); result }").toOption.get
    assertEquals(
      parsed.shape,
      TermShape.Block(
        List(
          TermShape.Apply(TermShape.Identifier("first", false), Nil),
          TermShape.Apply(
            TermShape.Identifier("second", false),
            List(TermShape.Literal("2"))
          )
        ),
        TermShape.Identifier("result", false)
      )
    )
  }

  test("P0 braces collapse while P1 braces retain an explicit block shape") {
    assertEquals(
      TinyTermParser.parse("{ result }").toOption.map(_.shape),
      Some(TermShape.Identifier("result", false))
    )
    assert(
      TinyTermParser.parse("{ first(); result }").toOption.exists(_.shape.isInstanceOf[TermShape.Block])
    )
  }

  test("qr preserves P1 prefix execution order and keeps the result last") {
    val log = ArrayBuffer.empty[Int]
    val result = P1BlockMacros.constructOrdered(
      { mark(log, 1); () },
      { mark(log, 2); () },
      mark(log, 3)
    )

    assertEquals(log.toList, List(1, 2, 3))
    assertEquals(result, 3)
  }

  test("qq captures prefix and result children in order and preserves reflected identity") {
    val captured = P1BlockMacros.captureOrdered {
      constant(1)
      constant(2)
      constant(3)
    }

    assertEquals(captured, (1, 2, 3))
    assert(
      P1BlockMacros.captureIdentity {
        constant(4)
        constant(5)
        constant(6)
      }
    )
  }

  test("P1 matching falls through when child count differs") {
    assert(
      !P1BlockMacros.matchesThreeChildren {
        constant(1)
        constant(2)
      }
    )
  }

  test("programmatic matching supports repeated holes and generated original identities") {
    assertEquals(P1BlockMacros.programmaticEvidence, (true, true, true))
  }

  test("local val and local def neighbors are rejected with block-family diagnostics") {
    val messages = P1BlockMacros.rejectionMessages
    messages.productIterator.map(_.toString).foreach { message =>
      assert(message.toLowerCase.contains("block"), message)
      assert(!message.contains("dotty.tools"), message)
    }
    assert(messages._1.toLowerCase.contains("local val"), messages._1)
    assert(messages._2.toLowerCase.contains("local def"), messages._2)
    assert(messages._3.toLowerCase.contains("local val"), messages._3)
    assert(messages._4.toLowerCase.contains("local def"), messages._4)
  }

  test("unsupported and malformed block diagnostics retain truthful bounded locations") {
    List("{ val x = 1; x }" -> "val x", "{ def x = 1; x }" -> "def x").foreach {
      case (source, offendingText) =>
        val failure = QuasiPattern.termLocated(source).swap.toOption.get
        val location = failure.location.get
        assertEquals(location.precision, DiagnosticPrecision.ExactOccurrence)
        assert(source.slice(location.span.start, location.span.end).contains(offendingText))
        assert(location.span.length < source.length)
        assert(failure.diagnostic.message.toLowerCase.contains("block"))
    }

    val malformed = QuasiPattern.termLocated("{ first();").swap.toOption.get
    assert(malformed.diagnostic.isInstanceOf[PatternError.ParseFailure])
    assert(malformed.location.forall(_.precision == DiagnosticPrecision.ExactOccurrence))
  }
