package quasiquotes.definitions

import quasiquotes.source.*

class DefinitionQuasiquoteAssemblyTest extends munit.FunSuite:
  import DefinitionQuasiquoteError.*
  import DefinitionQuasiquoteTestFixtures.*

  test("assembly derives exact categories identities bindings and complete origins before parsing") {
    val typeValue = tpe("String")
    val termValue = term("value")
    val arguments = Vector(
      DefinitionArguments.definitionType(typeValue),
      DefinitionArguments.bodyTerm(termValue),
      DefinitionArguments.bodyType(typeValue)
    )
    val assembly = DefinitionQuasiquoteAssembly
      .create(
        Vector("def convert: ", " = (", ": ", ")"),
        arguments
      )
      .fold(error => fail(error.diagnostic.message), identity)

    assertEquals(
      assembly.source,
      "def convert: $definitionArgument0 = ($definitionArgument1: $definitionArgument2)"
    )
    assertEquals(
      assembly.occurrences.map(_.semanticIdentity),
      Vector("definitionArgument0", "definitionArgument1", "definitionArgument2")
    )
    assertEquals(
      assembly.occurrences.map(_.category),
      Vector(
        InterpolationCategory.DefinitionTypeSplice,
        InterpolationCategory.DefinitionBodyTermSplice,
        InterpolationCategory.DefinitionBodyTypeSplice
      )
    )
    assertEquals(
      assembly.occurrences.map(_.role),
      Vector(
        HoleRole.DefinitionTypeTemplate,
        HoleRole.DefinitionBodyTermTemplate,
        HoleRole.DefinitionBodyTypeTemplate
      )
    )
    assertEquals(assembly.termBindings, Map("definitionArgument1" -> termValue))
    assertEquals(
      assembly.typeBindings,
      Map("definitionArgument0" -> typeValue, "definitionArgument2" -> typeValue)
    )
    assertComplete(assembly.sourceMap)
    assertEquals(
      assembly.sourceMap.segments.collect {
        case GeneratedSegment(_, origin: SourceOrigin.InterpolationArgument) =>
          origin.argumentIndex -> origin.category
      },
      Vector(
        0 -> InterpolationCategory.DefinitionTypeSplice,
        1 -> InterpolationCategory.DefinitionBodyTermSplice,
        2 -> InterpolationCategory.DefinitionBodyTypeSplice
      )
    )
  }

  test("reusing one descriptor creates distinct per-occurrence identities and origins") {
    val value = term("value")
    val repeated = DefinitionArguments.bodyTerm(value)
    val assembly = DefinitionQuasiquoteAssembly
      .create(
        Vector("def pair: (Int, Int) = (", ", ", ")"),
        Vector(repeated, repeated)
      )
      .toOption
      .get

    assertEquals(
      assembly.occurrences.map(_.semanticIdentity),
      Vector("definitionArgument0", "definitionArgument1")
    )
    assertEquals(
      assembly.occurrences.map(_.origin.argumentIndex),
      Vector(0, 1)
    )
    assertEquals(
      assembly.termBindings,
      Map("definitionArgument0" -> value, "definitionArgument1" -> value)
    )
  }

  test("literal parts remain exact UTF-16 input including CR LF CRLF and supplementary text") {
    val parts = Vector("def emoji😀: Int = /* CR\r LF\n CRLF\r\n */ ", "")
    val assembly = DefinitionQuasiquoteAssembly
      .create(parts, Vector(DefinitionArguments.bodyTerm(term("1"))))
      .toOption
      .get

    assert(assembly.source.startsWith(parts.head))
    assertEquals(assembly.source.length, parts.head.length + "$definitionArgument0".length)
    val occurrence = assembly.occurrences.head
    assertEquals(
      occurrence.assembledMarkerSpan,
      SourceSpan(parts.head.length, assembly.source.length)
    )
    assertComplete(assembly.sourceMap)
  }

  test("arity null descriptors and null payloads fail deterministically") {
    val arity = DefinitionQuasiquoteAssembly.create(Vector("only"), Vector(bodyTerm("1")))
    assertEquals(arity.left.toOption.get.diagnostic, InvalidPartsArgumentArity(1, 1))

    val descriptor = DefinitionQuasiquoteAssembly.create(
      Vector("def answer: Int = ", ""),
      Vector(null.asInstanceOf[DefinitionQuasiquoteArgument])
    )
    assertEquals(descriptor.left.toOption.get.diagnostic, NullDescriptor(0))

    val payload = DefinitionQuasiquoteAssembly.create(
      Vector("def answer: Int = ", ""),
      Vector(
        DefinitionArguments.bodyTerm(
          null.asInstanceOf[quasiquotes.terms.ConstructedTerm]
        )
      )
    )
    val payloadError = payload.left.toOption.get
    assertEquals(payloadError.diagnostic, NullDescriptorPayload(0, "body term"))
    assertEquals(
      payloadError.location.toVector.flatMap(_.origins).collect {
        case origin: SourceOrigin.InterpolationArgument => origin.argumentIndex
      },
      Vector(0)
    )
  }

  private def assertComplete(sourceMap: GeneratedSourceMap): Unit =
    val spans = sourceMap.segments.map(_.generatedSpan)
    assertEquals(spans.head.start, 0)
    assertEquals(spans.last.end, sourceMap.generatedSource.length)
    assert(spans.zip(spans.drop(1)).forall { case (left, right) =>
      left.end == right.start
    })
