package quasiquotes.construct

import scala.compiletime.testing.typeCheckErrors

class DefinitionQuasiquoteMacroCallerTest extends munit.FunSuite:
  test("real macro caller consumes the completed result and source evidence") {
    assertEquals(
      DefinitionQuasiquoteMacroExamples.successfulCaller(7),
      "ConstructedParameterlessDef(name=PlainName(answer), resultType=STypeIdent(Int), body=ConstructedTerm(root=Literal(1), ascriptions=[]))|argument=0|category=DefinitionTypeSplice"
    )
  }

  test("all three definition categories resolve to their exact argument terms") {
    assertEquals(
      DefinitionQuasiquoteMacroExamples.exactPositionSummary(1, 2, 3),
      "exact=true,true,true|categories=DefinitionTypeSplice,DefinitionBodyTermSplice,DefinitionBodyTypeSplice|messages-clean=true"
    )
  }

  test("whole missing out-of-range and unusable positions fall back to macro expansion") {
    assertEquals(
      DefinitionQuasiquoteMacroExamples.fallbackPositionSummary(1),
      "whole-fallback=true missing-fallback=true out-of-range-fallback=true generated-fallback=true identity-unattributed=true"
    )
  }

  test("production caller reports an exact structured definition argument failure") {
    val errors = typeCheckErrors(
      "quasiquotes.construct.DefinitionQuasiquoteMacroExamples.invalidDefinitionType(\"bad type\")"
    )

    assertEquals(errors.size, 1)
    assert(errors.head.message.contains("Definition interpolation argument 0"))
    assert(errors.head.message.contains("definition type"))
    assert(!errors.head.message.contains("definitionArgument"))
    assert(!errors.head.message.contains("__qq_dt_"))
  }

  test("literal syntax failure reports through macro-expansion fallback") {
    val errors = typeCheckErrors(
      "quasiquotes.construct.DefinitionQuasiquoteMacroExamples.invalidLiteralSyntax"
    )

    assertEquals(errors.size, 1)
    assert(errors.head.message.contains("Definition quasiquote parsing failure"))
  }

  test("caller arity disagreement is an explicit macro-layer invariant failure") {
    val errors = typeCheckErrors(
      "quasiquotes.construct.DefinitionQuasiquoteMacroExamples.invalidCallerArity(\"anchor\")"
    )

    assertEquals(errors.size, 1)
    assertEquals(
      errors.head.message,
      "Definition quasiquote macro caller invariant failed: received 1 compiler-free arguments and 0 macro argument anchors."
    )
  }
