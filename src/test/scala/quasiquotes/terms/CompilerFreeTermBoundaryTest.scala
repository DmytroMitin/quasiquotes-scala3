package quasiquotes.terms

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class CompilerFreeTermBoundaryTest extends munit.FunSuite:
  private val root =
    Path.of("src", "main", "scala", "quasiquotes", "terms")
  private val parserAdapter = root.resolve("parser")

  test("compiler-free term core sources contain no compiler Quotes Expr or Macro-Paradise dependency") {
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
        .filter(path =>
          path.toString.endsWith(".scala") &&
            !path.startsWith(root.resolve("dotty")) &&
            !path.startsWith(parserAdapter)
        )
        .forEach { path =>
          val source = Files.readString(path, StandardCharsets.UTF_8)
          forbidden.foreach(value =>
            assert(!source.contains(value), clues(path, value))
          )
        }
    finally stream.close()
  }

  test("Dotty compiler coupling is isolated to the exact-version backend package") {
    val repositoryRoot = Path.of("src", "main", "scala", "quasiquotes")
    val dottyBackend = root.resolve("dotty")
    val stream = Files.walk(repositoryRoot)
    try
      stream
        .filter(path => path.toString.endsWith(".scala"))
        .forEach { path =>
          val source = Files.readString(path, StandardCharsets.UTF_8)
          if source.contains("dotty.tools.dotc.ast.untpd") then
            assert(
              path.startsWith(dottyBackend) ||
                path.startsWith(repositoryRoot.resolve("parser")) ||
                path.startsWith(repositoryRoot.resolve("matching")) ||
                path.startsWith(repositoryRoot.resolve("construct")) ||
                path.startsWith(repositoryRoot.resolve("definitions")) ||
                path.startsWith(parserAdapter),
              clues(path)
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
