package quasiquotes.definitions

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class CompilerFreeDefinitionBoundaryTest extends munit.FunSuite:
  private val definitionSources = Vector(
    "DefinitionName.scala",
    "DefinitionShape.scala",
    "DefinitionSourceMetadata.scala",
    "DefinitionError.scala"
  )

  test("definition representation production sources have no compiler or Macro-Paradise dependency") {
    val root = Path.of("src", "main", "scala", "quasiquotes", "definitions")
    val forbidden = Vector(
      "dotty.",
      "scala.quoted",
      "quotes.reflect",
      "macroparadise"
    )

    definitionSources.foreach { file =>
      val source = Files.readString(root.resolve(file), StandardCharsets.UTF_8)
      forbidden.foreach(value => assert(!source.contains(value), clues(file, value)))
    }
  }

  test("validated constructors are not exposed as public case-class apply or copy paths") {
    val root = Path.of("src", "main", "scala", "quasiquotes", "definitions")
    val shapeSource =
      Files.readString(root.resolve("DefinitionShape.scala"), StandardCharsets.UTF_8)
    val metadataSource =
      Files.readString(root.resolve("DefinitionSourceMetadata.scala"), StandardCharsets.UTF_8)

    assert(!shapeSource.contains("case class ParameterlessDef"))
    assert(!shapeSource.contains("case class ImmutableVal"))
    assert(metadataSource.contains("final class DefinitionComponentSpans private ("))
    assert(metadataSource.contains("final class LocatedDefinitionShape private ("))
  }

  test("definition compiler coupling is confined to the exact-version dotty package") {
    val root = Path.of("src", "main", "scala", "quasiquotes", "definitions")
    val stream = Files.walk(root)
    try
      val compilerFreeSources =
        stream
          .filter(path => path.toString.endsWith(".scala"))
          .filter(path => !path.startsWith(root.resolve("dotty")))
          .toList

      compilerFreeSources.forEach { path =>
        val source = Files.readString(path, StandardCharsets.UTF_8)
        assert(!source.contains("dotty."), clues(path))
        assert(!source.contains("scala.quoted"), clues(path))
        assert(!source.contains("quotes.reflect"), clues(path))
        assert(!source.contains("macroparadise"), clues(path))
      }
    finally stream.close()
  }
