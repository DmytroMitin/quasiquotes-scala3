package quasiquotes.definitions

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class DefinitionQuasiquoteCompilerFreeBoundaryTest extends munit.FunSuite:
  private val root =
    Path.of("src", "main", "scala", "quasiquotes", "definitions")

  private val valueFiles = Vector(
    "DefinitionQuasiquoteArgument.scala",
    "DefinitionQuasiquoteError.scala",
    "DefinitionQuasiquoteAssembly.scala",
    "DefinitionQuasiquoteSourceEvidence.scala",
    "DefinitionQuasiquoteResult.scala",
    "DefinitionQuasiquotes.scala"
  )

  test("definition quasiquote value and facade files contain no compiler or peer dependency") {
    val forbidden = Vector(
      "dotty.tools.dotc",
      "scala.quoted",
      "quotes.reflect",
      "TypeRepr",
      "untpd",
      "tpd",
      "SourceFile",
      "Macro-Paradise",
      "macroparadise"
    )
    val forbiddenWords = Vector("Expr", "Context")

    valueFiles.foreach { file =>
      val source = Files.readString(root.resolve(file), StandardCharsets.UTF_8)
      forbidden.foreach(value => assert(!source.contains(value), clues(file, value)))
      forbiddenWords.foreach { value =>
        assert(
          !s"\\b$value\\b".r.findFirstIn(source).nonEmpty,
          clues(file, value)
        )
      }
    }
  }

  test("facade exposes the exact package-internal compiler-free dqr signature") {
    val source = Files.readString(
      root.resolve("DefinitionQuasiquotes.scala"),
      StandardCharsets.UTF_8
    )

    assert(source.contains("private[quasiquotes] object DefinitionQuasiquotes:"))
    assert(source.contains("extension (context: StringContext)"))
    assert(source.contains("arguments: DefinitionQuasiquoteArgument*"))
    assert(source.contains("LocatedDiagnostic[DefinitionQuasiquoteError]"))
    assert(source.contains("DefinitionQuasiquoteResult"))
    assert(!source.contains("inline def dqr"))
    assert(!source.contains("using Quotes"))
    assert(!source.contains("implicit"))
  }

  test("completed result retains only constructed definition and dedicated evidence") {
    val result = Files.readString(
      root.resolve("DefinitionQuasiquoteResult.scala"),
      StandardCharsets.UTF_8
    )
    val evidence = Files.readString(
      root.resolve("DefinitionQuasiquoteSourceEvidence.scala"),
      StandardCharsets.UTF_8
    )

    assert(result.contains("val constructed: ConstructedDefinition"))
    assert(result.contains("val sourceEvidence: DefinitionQuasiquoteSourceEvidence"))
    Vector(
      "DefinitionTemplate",
      "LocatedDefinitionTemplate",
      "termBindings",
      "typeBindings",
      "def complete",
      "Backend"
    ).foreach(value => assert(!result.contains(value), value))
    assert(!evidence.contains("DefinitionTemplate"))
    assert(!evidence.contains("ConstructedTerm"))
    assert(!evidence.contains("TypeNormalForm"))
  }
