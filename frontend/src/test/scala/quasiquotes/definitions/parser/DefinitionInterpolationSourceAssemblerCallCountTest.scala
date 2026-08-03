package quasiquotes.definitions.parser

import quasiquotes.definitions.*
import quasiquotes.definitions.dotty.RawDefinitionParser
import quasiquotes.source.*

class DefinitionInterpolationSourceAssemblerCallCountTest
    extends munit.FunSuite:
  import DefinitionQuasiquoteTestFixtures.*

  test("successful construction invokes one full parser and one completion") {
    val assembly = assemble(
      Vector("def answer: ", " = ", ""),
      Vector(
        DefinitionArguments.definitionType(tpe("Int")),
        DefinitionArguments.bodyTerm(term("1"))
      )
    )
    val (result, parseCalls, completionCalls) = instrument(assembly)

    assert(result.isRight)
    assertEquals(parseCalls, 1)
    assertEquals(completionCalls, 1)
  }

  test("frontend failure invokes one full parser and no completion") {
    val assembly = assemble(Vector("def answer: Int = ("), Vector.empty)
    val (result, parseCalls, completionCalls) = instrument(assembly)

    assert(result.isLeft)
    assertEquals(parseCalls, 1)
    assertEquals(completionCalls, 0)
  }

  test("descriptor and arity failures invoke neither parser nor completion") {
    var parseCalls = 0
    var completionCalls = 0
    val result =
      DefinitionQuasiquoteAssembly
        .create(
          Vector("def answer: Int = "),
          Vector(DefinitionArguments.bodyTerm(term("1")))
        )
        .flatMap { assembly =>
          DefinitionInterpolationSourceAssembler.constructUsing(assembly)(
            (source, occurrences, initialMap) =>
              parseCalls += 1
              DefinitionTemplateSourceAdapter.parseLocatedMappedUsing(
                source,
                occurrences,
                initialMap
              )(RawDefinitionParser.parseEnvelopeStandalone),
            (located, termBindings, typeBindings) =>
              completionCalls += 1
              located.complete(termBindings, typeBindings)
          )
        }

    assert(result.isLeft)
    assertEquals(parseCalls, 0)
    assertEquals(completionCalls, 0)
  }

  private def instrument(
      assembly: DefinitionQuasiquoteAssembly
  ): (
      Either[LocatedDiagnostic[DefinitionQuasiquoteError], DefinitionQuasiquoteResult],
      Int,
      Int
  ) =
    var parseCalls = 0
    var completionCalls = 0
    val result =
      DefinitionInterpolationSourceAssembler.constructUsing(assembly)(
        (source, occurrences, initialMap) =>
          parseCalls += 1
          DefinitionTemplateSourceAdapter.parseLocatedMappedUsing(
            source,
            occurrences,
            initialMap
          )(RawDefinitionParser.parseEnvelopeStandalone),
        (located, termBindings, typeBindings) =>
          completionCalls += 1
          located.complete(termBindings, typeBindings)
      )
    (result, parseCalls, completionCalls)

  private def assemble(
      parts: Vector[String],
      arguments: Vector[DefinitionQuasiquoteArgument]
  ): DefinitionQuasiquoteAssembly =
    DefinitionQuasiquoteAssembly
      .create(parts, arguments)
      .fold(error => fail(error.diagnostic.message), identity)
