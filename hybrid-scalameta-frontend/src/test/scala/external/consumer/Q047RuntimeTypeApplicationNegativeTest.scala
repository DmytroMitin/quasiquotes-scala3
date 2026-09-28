package external.consumer

import java.nio.file.{Files, Path}
import dotty.tools.dotc.{Compiler, Driver}

import quasiquotes.scalameta.ScalametaQuasiquotes
import quasiquotes.types.{QuasiTypequotes, RuntimeTypeApplicationMacros, TypeNormalForm}

final class Q047RuntimeTypeApplicationNegativeTest extends munit.FunSuite:
  private val scenarios = Vector(
    0 -> (0, "TYPE_SEQUENCE_RANK_MISMATCH"),
    1 -> (11, "UNSUPPORTED_TYPE_SEQUENCE_POSITION"),
    2 -> (7, "MULTIPLE_TYPE_SEQUENCE_SPLICES"),
    3 -> (4, "INVALID_TYPE_CONSTRUCTOR_KIND"),
    4 -> (1, "WRONG_TYPE_ARGUMENT_COUNT"),
    5 -> (5, "WRONG_TYPE_ARGUMENT_KIND"),
    6 -> (14, "TYPE_SEQUENCE_RANK_MISMATCH"),
    7 -> (15, "WRONG_TYPE_ARGUMENT_KIND")
  )

  scenarios.foreach { case (optInScenario, (standardScenario, category)) =>
    test(s"scenario $optInScenario has exact standard diagnostic parity for $category"):
      val standard = diagnostic(
        s"quasiquotes.types.RuntimeTypeApplicationMacros.rejected$standardScenario"
      )
      val optIn = diagnostic(
        s"external.consumer.Q047RuntimeTypeApplicationNegativeMacros.rejected$optInScenario"
      )
      assertEquals(optIn, standard)
      assert(optIn.startsWith(s"Invalid tqr type template: $category:"), optIn)
      assert(!List("NullPointerException", "MatchError", "ClassCastException").exists(optIn.contains))
  }

  private def diagnostic(expression: String): String =
    val temporary = Files.createTempDirectory("q047-runtime-type-diagnostic-")
    try
      val source = temporary.resolve("Rejected.scala")
      val output = Files.createDirectory(temporary.resolve("classes"))
      Files.writeString(
        source,
        s"package external.runtimeconsumer\nobject Rejected { val result = $expression }\n"
      )
      val reporter = new Driver().process(
        Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      )
      val diagnostics = reporter.allErrors.map(_.message).filter(_.startsWith("Invalid tqr type template:"))
      assertEquals(diagnostics.size, 1, reporter.allErrors.map(_.message).mkString("\n"))
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
      classOf[TypeNormalForm],
      QuasiTypequotes.getClass,
      RuntimeTypeApplicationMacros.getClass,
      ScalametaQuasiquotes.getClass,
      Q047RuntimeTypeApplicationNegativeMacros.getClass,
      getClass
    ).flatMap(clazz =>
      Option(clazz.getProtectionDomain)
        .flatMap(domain => Option(domain.getCodeSource))
        .map(_.getLocation.toURI)
    ).map(Path.of(_).toString).distinct.mkString(java.io.File.pathSeparator)
