package quasiquotes.terms.parser

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class TermTemplateSourceAdapterBoundaryTest extends munit.FunSuite:
  private val coreRoot =
    Path.of("core", "src", "main", "scala", "quasiquotes")
  private val frontendRoot =
    Path.of("frontend", "src", "main", "scala", "quasiquotes")
  private val adapterRoot =
    frontendRoot.resolve(Path.of("terms", "parser"))
  private val backendRoot =
    Path.of("dotty-internal", "src", "main", "scala", "quasiquotes", "terms", "dotty")

  test("compiler-free term core files remain free of Dotty and Quotes") {
    val coreFiles = Vector(
      "TermTemplate.scala",
      "ConstructedTerm.scala",
      "TermConstructionError.scala",
      "TermTemplateSourceMetadata.scala"
    )
    val forbidden = Vector(
      "dotty.tools.dotc",
      "scala.quoted",
      "quotes.reflect",
      "Expr[",
      "TypeRepr",
      "macroparadise"
    )
    coreFiles.foreach { name =>
      val path = coreRoot.resolve(Path.of("terms", name))
      val source = Files.readString(path, StandardCharsets.UTF_8)
      forbidden.foreach(value =>
        assert(!source.contains(value), clues(path, value))
      )
    }
  }

  test("adapter compiler coupling is confined and contains no backend or Quotes route") {
    val stream = Files.walk(adapterRoot)
    try
      stream
        .filter(_.toString.endsWith(".scala"))
        .forEach { path =>
          val source = Files.readString(path, StandardCharsets.UTF_8)
          Vector(
            "scala.quoted",
            "quotes.reflect",
            "Expr[",
            "TypeRepr",
            "ConstructedTermUntypedBackend",
            "macroparadise"
          ).foreach(value =>
            assert(!source.contains(value), clues(path, value))
          )
        }
    finally stream.close()
  }

  test("adapter adds no public API or generic frontend trait") {
    val stream = Files.walk(adapterRoot)
    try
      stream
        .filter(_.toString.endsWith(".scala"))
        .forEach { path =>
          val source = Files.readString(path, StandardCharsets.UTF_8)
          assert(!source.contains("trait TermTemplateFrontend"))
          assert(!source.contains("trait TermTemplateParser"))
          assert(!source.contains("public"))
          assert(
            !source.linesIterator.exists(line =>
              line.startsWith("object TermTemplateSourceAdapter")
            )
          )
        }
    finally stream.close()
  }

  test("exact term backend remains parser-free") {
    val stream = Files.walk(backendRoot)
    try
      stream
        .filter(_.toString.endsWith(".scala"))
        .forEach { path =>
          val source = Files.readString(path, StandardCharsets.UTF_8)
          Vector(
            "TinyTermParser",
            "TinyTypeParser",
            "Scala3ParserBridge",
            "dotty.tools.dotc.parsing"
          ).foreach(value =>
            assert(!source.contains(value), clues(path, value))
          )
        }
    finally stream.close()
  }

  test("current qr signature and builder path remain unchanged") {
    val quasiquotes = Files.readString(
      frontendRoot.resolve(Path.of("construct", "Quasiquotes.scala")),
      StandardCharsets.UTF_8
    )
    val builder = Files.readString(
      frontendRoot.resolve(Path.of("construct", "QuasiquoteBuilder.scala")),
      StandardCharsets.UTF_8
    )
    assert(
      quasiquotes.contains(
        "def qr(using q: Quotes)(args: (q.reflect.Term | QuasiTypeSplice)*): q.reflect.Term"
      )
    )
    assert(builder.contains("PlaceholderSource.synthesizeCategorized"))
    assert(builder.contains("ParsedTermLowerer"))
    assert(!builder.contains("TermTemplateSourceAdapter"))
  }
