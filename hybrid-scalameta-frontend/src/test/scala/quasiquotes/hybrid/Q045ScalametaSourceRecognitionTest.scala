package quasiquotes.hybrid

import quasiquotes.definitions.hybrid.ScalametaDefinitionFrontend as Frontend

final class Q045ScalametaSourceRecognitionTest extends munit.FunSuite:
  test("omitted source sentinels preserve both exact clause grammars and mode families"):
    val mixed = Vector("def ", "(..", ")(implicit ..", "): ", " = ", "")
    val ordinary = Vector("def ", "(..", "): ", " = ", "")
    val projectedMixed = Frontend.compileCapturedNameMixedOrdinaryScala2ImplicitParameterSequencesCapturedResultPattern(mixed)
    val projectedOrdinary = Frontend.compileCapturedNameOrdinaryParameterSequenceCapturedResultPattern(ordinary)
    assert(projectedMixed.isRight, projectedMixed)
    val projection = projectedOrdinary.toOption.get
    assertEquals(projection.parameterTypeFamilies, List("Type.Name", "Type.ByName", "Type.Name", "Type.Repeated"))
    assertEquals(projection.parameterDefaultPresence, List(false, false, true, false))
    val modifiedPrefixes = List("private def ", "protected def ", "final def ", "override def ", "inline def ", "transparent inline def ", "implicit def ", "infix def ", "@deprecated def ", "private[quasiquotes] def ", "protected[quasiquotes] def ")
    modifiedPrefixes.foreach { prefix =>
      assert(Frontend.compileCapturedNameMixedOrdinaryScala2ImplicitParameterSequencesCapturedResultPattern(mixed.updated(0, prefix)).isLeft, prefix)
      assert(Frontend.compileCapturedNameOrdinaryParameterSequenceCapturedResultPattern(ordinary.updated(0, prefix)).isLeft, prefix)
    }
    assert(Frontend.compileCapturedNameMixedOrdinaryScala2ImplicitParameterSequencesCapturedResultPattern(Vector("", " def ") ++ mixed.tail).isLeft)
    assert(Frontend.compileCapturedNameOrdinaryParameterSequenceCapturedResultPattern(Vector("", " def ") ++ ordinary.tail).isLeft)
    assert(Frontend.compileCapturedNameMixedOrdinaryScala2ImplicitParameterSequencesCapturedResultPattern(mixed.updated(2, ")(using ..")).isLeft)
    assert(Frontend.compileCapturedNameOrdinaryParameterSequenceCapturedResultPattern(ordinary.updated(1, "(...")).isLeft)
