package external.consumer

import java.nio.file.{Files, Path}
import dotty.tools.dotc.{Compiler, Driver}

import quasiquotes.construct.{Quasiquotes as StandardQuasiquotes, TermSequenceSplice}
import quasiquotes.scalameta.ScalametaQuasiquotes

final class Q048TermSequenceNegativeTest extends munit.FunSuite:
  private val scenarios = Vector(
    0 -> "carrier without rank marker",
    1 -> "rank marker on scalar Term",
    2 -> "sequence outside Apply/New arguments",
    3 -> "two repeated holes in one list",
    4 -> "unsupported rank three",
    5 -> "split rank dots",
    6 -> "additional argument list",
    7 -> "null carrier",
    8 -> "null contained Term",
    9 -> "named argument position"
  )

  scenarios.foreach { case (scenario, label) =>
    val claim = if scenario == 1 then "has stable scalar diagnostic-detail parity" else "has exact standard and opt-in diagnostic parity"
    test(s"$label $claim"):
      val standard = diagnostic(s"external.consumer.Q048TermSequenceNegativeMacros.standardRejected$scenario")
      val optIn = diagnostic(s"external.consumer.Q048TermSequenceNegativeMacros.optInRejected$scenario")
      if scenario == 1 then
        assertEquals(optIn, s"TERM_TEMPLATE_FAILURE[0..0]: $standard")
      else assertEquals(optIn, standard)
      assert(
        !List("NullPointerException", "MatchError", "ClassCastException", "compiler exception").exists(optIn.contains),
        optIn
      )
  }

  test("the pre-existing contextual sequence topology remains unchanged in both facades"):
    Q048TermSequenceNegativeMacros.standardContextual
    Q048TermSequenceNegativeMacros.optInContextual

  private def diagnostic(expression: String): String =
    val temporary = Files.createTempDirectory("q048-term-sequence-diagnostic-")
    try
      val source = temporary.resolve("Rejected.scala")
      val output = Files.createDirectory(temporary.resolve("classes"))
      Files.writeString(
        source,
        s"package external.q048consumer\nobject Rejected { val result = $expression }\n"
      )
      val reporter = new Driver().process(
        Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      )
      val diagnostics = reporter.allErrors.map(_.message)
      assertEquals(diagnostics.size, 1, diagnostics.mkString("\n"))
      diagnostics.head
    finally
      val stream = Files.walk(temporary)
      try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally stream.close()

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[Compiler],
      classOf[TermSequenceSplice[?]],
      StandardQuasiquotes.getClass,
      ScalametaQuasiquotes.getClass,
      Q048TermSequenceNegativeMacros.getClass,
      getClass
    ).flatMap(clazz =>
      Option(clazz.getProtectionDomain)
        .flatMap(domain => Option(domain.getCodeSource))
        .map(_.getLocation.toURI)
    ).map(Path.of(_).toString).distinct.mkString(java.io.File.pathSeparator)
