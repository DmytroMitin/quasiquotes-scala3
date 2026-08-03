package quasiquotes.definitions.parser

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import quasiquotes.definitions.*
import quasiquotes.definitions.dotty.RawDefinitionParser
import quasiquotes.source.*

class DefinitionInterpolationSourceAssemblerStructuredAttributionTest
    extends munit.FunSuite:
  import DefinitionQuasiquoteError.CompletionFailure
  import DefinitionQuasiquoteTestFixtures.*

  test("free-form body failure identity-like text stays whole-source and unattributed") {
    val assembly = assemble(
      Vector("def answer: Int = ", ""),
      Vector(DefinitionArguments.bodyTerm(term("1")))
    )
    val underlying = DefinitionConstructionError.BodyConstructionFailure(
      "failure involving literal identifier definitionArgument0 and __qq_dt_body_term_0"
    )
    val (failure, parseCalls, completionCalls) = injectedFailure(
      assembly,
      (located, _, _) =>
        LocatedDiagnostic(underlying, wholeDefinitionLocation(located))
    )
    val projected = failure.diagnostic.asInstanceOf[CompletionFailure]
    val location = failure.location.getOrElse(fail("Expected whole-source location"))

    assertEquals(projected.underlying, underlying)
    assertEquals(projected.argumentIndex, None)
    assertEquals(projected.role, None)
    assertEquals(location.precision, DiagnosticPrecision.WholeSource)
    assert(location.origins.exists(_.isInstanceOf[SourceOrigin.LiteralPart]))
    assert(
      !projected.message.contains("definition interpolation argument 0"),
      projected.message
    )
    assert(projected.message.contains("definitionArgument0"), projected.message)
    assert(!projected.message.contains("__qq_dt_"), projected.message)
    assertEquals(parseCalls, 1)
    assertEquals(completionCalls, 1)
  }

  test("structured missing term binding retains exact argument attribution") {
    val assembly = assemble(
      Vector("def answer: Int = ", ""),
      Vector(DefinitionArguments.bodyTerm(term("1")))
    )
    val underlying =
      DefinitionConstructionError.MissingTermBinding("definitionArgument0")
    val (failure, parseCalls, completionCalls) = injectedFailure(
      assembly,
      (located, _, typeBindings) =>
        located
          .complete(Map.empty, typeBindings)
          .left
          .toOption
          .get
    )
    val projected = failure.diagnostic.asInstanceOf[CompletionFailure]
    val location = failure.location.getOrElse(fail("Expected exact location"))

    assertEquals(projected.underlying, underlying)
    assertEquals(projected.argumentIndex, Some(0))
    assertEquals(projected.role, Some("body term"))
    assertEquals(location.precision, DiagnosticPrecision.ExactOccurrence)
    assertEquals(
      location.origins.collect {
        case origin: SourceOrigin.InterpolationArgument => origin.argumentIndex
      },
      Vector(0)
    )
    assert(projected.message.contains("argument 0 (body term)"), projected.message)
    assert(!projected.message.contains("definitionArgument0"), projected.message)
    assert(!projected.message.contains("__qq_dt_"), projected.message)
    assertEquals(parseCalls, 1)
    assertEquals(completionCalls, 1)
  }

  test("ordinary literal definitionArgument0 spelling cannot select interpolation zero") {
    val assembly = assemble(
      Vector("def definitionArgument0: Int = 1"),
      Vector.empty
    )
    val underlying = DefinitionConstructionError.BodyConstructionFailure(
      "ordinary literal identifier definitionArgument0 failed"
    )
    val (failure, parseCalls, completionCalls) = injectedFailure(
      assembly,
      (located, _, _) =>
        LocatedDiagnostic(underlying, wholeDefinitionLocation(located))
    )
    val projected = failure.diagnostic.asInstanceOf[CompletionFailure]
    val location = failure.location.getOrElse(fail("Expected whole-source location"))

    assertEquals(projected.argumentIndex, None)
    assertEquals(projected.role, None)
    assertEquals(location.precision, DiagnosticPrecision.WholeSource)
    assertEquals(
      location.origins.collect {
        case origin: SourceOrigin.InterpolationArgument => origin.argumentIndex
      },
      Vector.empty
    )
    assert(
      !projected.message.contains("definition interpolation argument 0"),
      projected.message
    )
    assertEquals(parseCalls, 1)
    assertEquals(completionCalls, 1)
  }

  test("production assembler contains no message-derived semantic attribution") {
    val source = Files.readString(
      Path.of(
        "frontend",
        "src",
        "main",
        "scala",
        "quasiquotes",
        "definitions",
        "parser",
        "DefinitionInterpolationSourceAssembler.scala"
      ),
      StandardCharsets.UTF_8
    )

    assert(!source.contains("assemblyIdentityIn"))
    assert(!source.contains("definitionArgument[0-9]+"))
  }

  private def injectedFailure(
      assembly: DefinitionQuasiquoteAssembly,
      failure: (
          LocatedDefinitionTemplate,
          Map[String, quasiquotes.terms.ConstructedTerm],
          Map[String, quasiquotes.types.TypeNormalForm]
      ) => LocatedDiagnostic[DefinitionConstructionError]
  ): (
      LocatedDiagnostic[DefinitionQuasiquoteError],
      Int,
      Int
  ) =
    var parseCalls = 0
    var completionCalls = 0
    val result = DefinitionInterpolationSourceAssembler.constructUsing(assembly)(
      (source, occurrences, initialMap) =>
        parseCalls += 1
        DefinitionTemplateSourceAdapter.parseLocatedMappedUsing(
          source,
          occurrences,
          initialMap
        )(RawDefinitionParser.parseEnvelopeStandalone),
      (located, termBindings, typeBindings) =>
        completionCalls += 1
        Left(failure(located, termBindings, typeBindings))
    )
    (
      result.left.toOption.get,
      parseCalls,
      completionCalls
    )

  private def wholeDefinitionLocation(
      located: LocatedDefinitionTemplate
  ): Option[DiagnosticLocation] =
    DiagnosticLocation.fromGeneratedMap(
      located.sourceMap,
      located.components.definition,
      DiagnosticPrecision.WholeSource
    )

  private def assemble(
      parts: Vector[String],
      arguments: Vector[DefinitionQuasiquoteArgument]
  ): DefinitionQuasiquoteAssembly =
    DefinitionQuasiquoteAssembly
      .create(parts, arguments)
      .fold(error => fail(error.diagnostic.message), identity)
