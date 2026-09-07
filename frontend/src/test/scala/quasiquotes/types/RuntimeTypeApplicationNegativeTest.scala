package quasiquotes.types

import java.nio.file.{Files, Path}
import dotty.tools.dotc.{Compiler, Driver}

/** Compile consumers of already compiled macros, outside nested compiletime testing. */
class RuntimeTypeApplicationNegativeTest extends munit.FunSuite:
  private val scenarios = Vector(
    0 -> "TYPE_SEQUENCE_RANK_MISMATCH",
    1 -> "WRONG_TYPE_ARGUMENT_COUNT",
    2 -> "WRONG_TYPE_ARGUMENT_COUNT",
    3 -> "WRONG_TYPE_ARGUMENT_COUNT",
    4 -> "INVALID_TYPE_CONSTRUCTOR_KIND",
    5 -> "WRONG_TYPE_ARGUMENT_KIND",
    6 -> "WRONG_TYPE_ARGUMENT_KIND",
    7 -> "MULTIPLE_TYPE_SEQUENCE_SPLICES",
    8 -> "TYPE_SEQUENCE_RANK_MISMATCH",
    9 -> "TYPE_SEQUENCE_RANK_MISMATCH",
    10 -> "TYPE_SEQUENCE_RANK_MISMATCH",
    11 -> "UNSUPPORTED_TYPE_SEQUENCE_POSITION",
    12 -> "UNSUPPORTED_TYPE_SEQUENCE_POSITION",
    13 -> "INVALID_TYPE_CONSTRUCTOR_KIND",
    14 -> "TYPE_SEQUENCE_RANK_MISMATCH",
    15 -> "WRONG_TYPE_ARGUMENT_KIND",
    16 -> "UNSUPPORTED_TYPE_SEQUENCE_POSITION",
    17 -> "UNSUPPORTED_TYPE_SEQUENCE_POSITION",
    18 -> "UNSUPPORTED_TYPE_SEQUENCE_POSITION",
    19 -> "UNSUPPORTED_TYPE_SEQUENCE_POSITION",
    20 -> "UNSUPPORTED_TYPE_SEQUENCE_POSITION",
    21 -> "UNSUPPORTED_TYPE_SEQUENCE_POSITION",
    22 -> "UNSUPPORTED_TYPE_SEQUENCE_POSITION",
    23 -> "TYPE_SEQUENCE_RANK_MISMATCH",
    24 -> "TYPE_SEQUENCE_RANK_MISMATCH",
    25 -> "TYPE_SEQUENCE_RANK_MISMATCH",
    26 -> "TYPE_SEQUENCE_RANK_MISMATCH",
    27 -> "TYPE_SEQUENCE_RANK_MISMATCH",
    28 -> "INVALID_TYPE_CONSTRUCTOR_KIND",
    29 -> "WRONG_TYPE_ARGUMENT_KIND",
    30 -> "INVALID_TYPE_CONSTRUCTOR_KIND",
    31 -> "WRONG_TYPE_ARGUMENT_KIND",
    32 -> "WRONG_TYPE_ARGUMENT_KIND",
    33 -> "INVALID_TYPE_CONSTRUCTOR_KIND",
    34 -> "TYPE_SEQUENCE_RANK_MISMATCH",
    35 -> "INVALID_TYPE_CONSTRUCTOR_KIND"
  )

  scenarios.foreach { (scenario, code) =>
    test(s"scenario $scenario: $code"):
      val temporary = Files.createTempDirectory("runtime-type-diagnostic-")
      try
        val source = temporary.resolve("Rejected.scala")
        val output = Files.createDirectory(temporary.resolve("classes"))
        Files.writeString(source,
          s"package external.runtimeconsumer\nobject Rejected { val result = quasiquotes.types.RuntimeTypeApplicationMacros.rejected$scenario }\n")
        val reporter = new Driver().process(Array("-classpath", compilationClasspath, "-d", output.toString, source.toString))
        val messages = reporter.allErrors.map(_.message)
        assert(messages.exists(_.startsWith(s"Invalid tqr type template: $code:")), messages.mkString("\n"))
        assert(!messages.exists(e => List("NullPointerException", "MatchError", "ClassCastException").exists(e.contains)))
      finally
        val stream = Files.walk(temporary)
        try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
        finally stream.close()
  }

  private def compilationClasspath: String =
    Vector(classOf[scala.Option[?]], classOf[scala.deriving.Mirror], classOf[Compiler],
      classOf[TypeNormalForm], QuasiTypequotes.getClass, getClass)
      .flatMap(c => Option(c.getProtectionDomain).flatMap(d => Option(d.getCodeSource)).map(_.getLocation.toURI))
      .map(Path.of(_).toString).distinct.mkString(java.io.File.pathSeparator)
