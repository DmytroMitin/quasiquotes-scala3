package quasiquotes.construct

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class DefinitionQuasiquoteMacroBoundaryTest extends munit.FunSuite:
  private val constructRoot =
    Path.of("src", "main", "scala", "quasiquotes", "construct")

  test("caller delegates to dqr and contains no parser backend or placement route") {
    val source = read("DefinitionQuasiquoteMacroCaller.scala")

    assert(source.contains("private[quasiquotes] object DefinitionQuasiquoteMacroCaller"))
    assert(source.contains("StringContext(parts*).dqr(arguments*)"))
    Vector(
      "DefinitionQuasiquoteAssembly",
      "DefinitionInterpolationSourceAssembler",
      "ConstructedDefinitionUntypedBackend",
      "ConstructedDefinitionGeneratedOriginAdapter",
      "dotty.tools.dotc",
      "untpd",
      "tpd",
      "Macro-Paradise",
      "macroparadise",
      "owner",
      "symbol"
    ).foreach(value => assert(!source.contains(value), value))
  }

  test("macro reporter projects structured location without parsing messages") {
    val source = read("DefinitionQuasiquoteMacroDiagnosticReporter.scala")

    assert(
      source.contains(
        "DefinitionQuasiquoteMacroAnchorSelector.select(failure.location)"
      )
    )
    assert(source.contains("failure.diagnostic.message"))
    assert(source.contains("report.errorAndAbort"))
    Vector(
      "definitionArgument",
      "__qq_dt_",
      ".r.find",
      "regex",
      "semanticIdentity",
      "ConstructedDefinition"
    ).foreach(value => assert(!source.contains(value), value))
  }

  test("Quotes coupling is confined to the macro-facing construct layer") {
    val definitionRoot =
      Path.of("src", "main", "scala", "quasiquotes", "definitions")
    val compilerFreeFiles = Vector(
      "DefinitionQuasiquoteArgument.scala",
      "DefinitionQuasiquoteAssembly.scala",
      "DefinitionQuasiquoteError.scala",
      "DefinitionQuasiquoteResult.scala",
      "DefinitionQuasiquoteSourceEvidence.scala",
      "DefinitionQuasiquotes.scala",
      "ConstructedDefinition.scala"
    )

    compilerFreeFiles.foreach { file =>
      val source =
        Files.readString(definitionRoot.resolve(file), StandardCharsets.UTF_8)
      assert(!source.contains("scala.quoted"), file)
      assert(!source.contains("quotes.reflect"), file)
      assert(!source.contains("dotty.tools.dotc"), file)
    }
  }

  private def read(file: String): String =
    Files.readString(constructRoot.resolve(file), StandardCharsets.UTF_8)
