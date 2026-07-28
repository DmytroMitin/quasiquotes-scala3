package quasiquotes.terms

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class CompilerFreeTermBoundaryTest extends munit.FunSuite:
  private val root =
    Path.of("src", "main", "scala", "quasiquotes", "terms")

  test("term core production sources contain no compiler Quotes Expr or Macro-Paradise dependency") {
    val forbidden = Vector(
      "dotty.tools.dotc",
      "scala.quoted",
      "quotes.reflect",
      "Expr[",
      "macroparadise"
    )
    val stream = Files.walk(root)
    try
      stream
        .filter(path => path.toString.endsWith(".scala"))
        .forEach { path =>
          val source = Files.readString(path, StandardCharsets.UTF_8)
          forbidden.foreach(value =>
            assert(!source.contains(value), clues(path, value))
          )
        }
    finally stream.close()
  }

  test("validated wrappers expose no public case-class apply or copy bypass") {
    val constructed =
      Files.readString(
        root.resolve("ConstructedTerm.scala"),
        StandardCharsets.UTF_8
      )
    val template =
      Files.readString(
        root.resolve("TermTemplate.scala"),
        StandardCharsets.UTF_8
      )
    val located =
      Files.readString(
        root.resolve("TermTemplateSourceMetadata.scala"),
        StandardCharsets.UTF_8
      )

    assert(constructed.contains("final class ConstructedTerm private ("))
    assert(template.contains("final class TermTemplate private ("))
    assert(located.contains("final class LocatedTermTemplate private ("))
    assert(!constructed.contains("case class ConstructedTerm"))
    assert(!template.contains("case class TermTemplate("))
    assert(!located.contains("case class LocatedTermTemplate"))
  }

  test("completion core imports neither matching nor normalization") {
    val template =
      Files.readString(
        root.resolve("TermTemplate.scala"),
        StandardCharsets.UTF_8
      )

    assert(!template.contains("quasiquotes.matching"))
    assert(!template.contains("normalization"))
    assert(!template.contains("TermPattern"))
    assert(!template.contains("TargetTermView"))
  }
