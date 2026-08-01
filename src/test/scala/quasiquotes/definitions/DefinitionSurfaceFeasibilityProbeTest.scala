package quasiquotes.definitions

import ProbeDefinitionArguments.*
import DefinitionSurfaceFeasibilityProbe.*

private object DefinitionSurfaceProbeExamples:
  val mixed: DefinitionSurfaceProbeEvidence =
    definitionSurfaceProbe"def convert: ${definitionType("List[T]")} = (${bodyTerm("value")}: ${bodyType("Option[T]")})"

  private val repeated = bodyTerm("value")

  val repeatedTerm: DefinitionSurfaceProbeEvidence =
    definitionSurfaceProbe"def pair: (Int, Int) = (${repeated}, ${repeated})"

class DefinitionSurfaceFeasibilityProbeTest extends munit.FunSuite:
  test("macro recovers exact StringContext parts and descriptor categories") {
    assertEquals(
      DefinitionSurfaceProbeExamples.mixed.parts,
      List("def convert: ", " = (", ": ", ")")
    )
    assertEquals(
      DefinitionSurfaceProbeExamples.mixed.categories,
      List("DefinitionType", "BodyTerm", "BodyType")
    )
  }

  test("literal parts and argument expressions retain valid separate positions") {
    val evidence = DefinitionSurfaceProbeExamples.mixed

    assert(evidence.argumentStarts.zip(evidence.argumentEnds).forall {
      case (start, end) => start >= 0 && end > start
    })
    assert(evidence.partStarts.zip(evidence.partEnds).forall {
      case (start, end) => start >= 0 && end > start
    })
    assertEquals(evidence.argumentStarts.distinct.size, 3)
  }

  test("reusing one descriptor expression remains two ordered occurrences") {
    val evidence = DefinitionSurfaceProbeExamples.repeatedTerm

    assertEquals(evidence.categories, List("BodyTerm", "BodyTerm"))
    assertEquals(evidence.argumentStarts.size, 2)
    assert(evidence.argumentStarts.head < evidence.argumentStarts(1))
    assert(evidence.argumentEnds.head <= evidence.argumentStarts(1))
  }
