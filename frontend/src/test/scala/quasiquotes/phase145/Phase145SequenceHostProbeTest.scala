package quasiquotes.phase145

import scala.compiletime.testing.typeCheckErrors

import Phase145SequenceHostProbe.*
import quasiquotes.parser.TinyTermParser

private object Phase145SequenceHostExamples:
  private val args = Seq(1, 2)
  private val wrapped = Phase145ProbeTermSequence(args)

  val applyRankMarker = sequenceHostProbe"f(..$args)"
  val newRankMarker = sequenceHostProbe"new C(..$args)"
  val explicitWrapper = sequenceHostProbe"f($wrapped)"

class Phase145SequenceHostProbeTest extends munit.FunSuite:
  private inline def sequenceSyntaxErrors(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  test("Scala-2-style rank text remains literal while the sequence is one host argument") {
    assertEquals(
      Phase145SequenceHostExamples.applyRankMarker,
      Phase145SequenceHostEvidence(List("f(..", ")"), 1, List("args"))
    )
    assertEquals(
      Phase145SequenceHostExamples.newRankMarker,
      Phase145SequenceHostEvidence(List("new C(..", ")"), 1, List("args"))
    )
  }

  test("a wrapper-only spelling hides sequence rank from the quasiquote source") {
    assertEquals(
      Phase145SequenceHostExamples.explicitWrapper,
      Phase145SequenceHostEvidence(List("f(", ")"), 1, List("wrapped"))
    )
  }

  test("Scala-3 repeated-argument syntax is not a legal custom-interpolator argument expression") {
    val errors = sequenceSyntaxErrors(
      """import quasiquotes.phase145.Phase145SequenceHostProbe.*
        val args = Seq(1, 2)
        sequenceHostProbe"f(${args*})"
      """
    )
    assert(errors.nonEmpty, "`${args*}` unexpectedly compiled as a custom-interpolator argument")
  }

  test("the rank marker must be classified and consumed before the one guest parse") {
    val direct = TinyTermParser.parse("f(..__phase145_sequence_hole_0)")
    val adapted = TinyTermParser.parse("f(__phase145_sequence_hole_0)")

    assert(direct.isLeft, "Dotty unexpectedly parsed a Scala-2-style rank marker as ordinary Scala 3")
    assertEquals(
      adapted.map(_.shape.render),
      Right("Apply(Ident(f), [Ident(__phase145_sequence_hole_0)])")
    )
  }
