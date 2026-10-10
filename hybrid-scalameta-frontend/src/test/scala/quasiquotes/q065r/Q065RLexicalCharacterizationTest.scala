package quasiquotes.q065r

import java.nio.file.{Files, Path}

import dotty.tools.dotc.{Compiler, Driver}

import _root_.quasiquotes.construct.hybrid.ScalametaTermFrontend
import _root_.quasiquotes.hybrid.TermQ3DialectPolicy
import _root_.quasiquotes.matching.{PatternCompiler, PatternSource, TermPattern}
import _root_.quasiquotes.parser.TinyTermParser
import _root_.quasiquotes.scalameta.TermFrontend

final class Q065RLexicalCharacterizationTest extends munit.FunSuite:
  private final case class Candidate(
      label: String,
      source: String,
      expected: (Boolean, Boolean, Boolean, Boolean)
  )

  private val candidates = List(
    Candidate("ordinary", "{ def id(value: Int): Int = value; id($argument) }", (true, true, true, true)),
    Candidate("underscore-prefix", "{ def _id(_value: Int): Int = _value; _id($argument) }", (true, true, true, true)),
    Candidate("dollar-prefix-treated-as-holes", "{ def $id($value: Int): Int = $value; $id($argument) }", (true, true, false, false)),
    Candidate("quoted-method-keyword", "{ def `type`(value: Int): Int = value; `type`($argument) }", (true, true, true, false)),
    Candidate("quoted-parameter-keyword", "{ def id(`match`: Int): Int = `match`; id($argument) }", (true, true, true, false)),
    Candidate("underscore-method", "{ def _(value: Int): Int = value; _($argument) }", (false, false, false, false)),
    Candidate("symbolic-method-plus", "{ def +(value: Int): Int = value; +($argument) }", (true, true, false, false)),
    Candidate("symbolic-method-plus-plus", "{ def ++(value: Int): Int = value; ++($argument) }", (true, true, false, false)),
    Candidate("symbolic-method-pipe-plus", "{ def |+(value: Int): Int = value; |+($argument) }", (true, true, false, false)),
    Candidate("symbolic-parameter-plus", "{ def id(+: Int): Int = +; id($argument) }", (false, false, false, false)),
    Candidate("unicode-method", "{ def λ(value: Int): Int = value; λ($argument) }", (true, true, false, false)),
    Candidate("unicode-parameter", "{ def id(λ: Int): Int = λ; id($argument) }", (true, true, false, false))
  )

  test("record selected-dialect parse exact compiler standard and hybrid lexical gates"):
    val rows = candidates.map { candidate =>
      val mapped = PatternSource.synthesizeMapped(candidate.source).fold(
        error => fail(error.message),
        identity
      )
      val scalameta = ScalametaTermFrontend.parse(
        mapped.patternSource.source,
        TermQ3DialectPolicy.selected
      )
      val exact = ScalametaTermFrontend.validateExactCompiler(mapped.patternSource.source)
      val standard = TinyTermParser
        .parseRaw(mapped.patternSource.source)
        .flatMap(PatternCompiler.compile)
      val hybrid = TermFrontend.compile(candidate.source)
      val row = List(
        candidate.label,
        s"scalameta=${scalameta.fold(_.category, _ => "ADMIT")}",
        s"exact=${exact.fold(_.category, _ => "ADMIT")}",
        s"standard=${standard.fold(error => error.toString, _ => "ADMIT")}",
        s"hybrid=${hybrid.fold(failure => s"${failure.category}:${failure.detail}", _ => "ADMIT")}"
      ).mkString(" | ")
      println(s"Q065R_LEXICAL_MATRIX $row")
      assertEquals(
        (scalameta.isRight, exact.isRight, standard.isRight, hybrid.isRight),
        candidate.expected,
        candidate.label
      )
      row
    }
    assertEquals(rows.size, candidates.size)

  test("real public typed-Scalameta qq rejects a pattern binder outside the accepted lexical policy"):
    val temporary = Files.createTempDirectory("q065r-public-lexical-")
    try
      val source = temporary.resolve("Rejected.scala")
      val consumer = temporary.resolve("Consumer.scala")
      val output = Files.createDirectory(temporary.resolve("classes"))
      Files.writeString(
        source,
        List(
          "package external.q065r",
          "import scala.quoted.*",
          "import quasiquotes.scalameta.ScalametaQuasiPattern.qq",
          "object Rejected:",
          "  inline def matches(inline expression: Int): Boolean = ${ matchesImpl('expression) }",
          "  def matchesImpl(expression: Expr[Int])(using q: Quotes): Expr[Boolean] =",
          "    import q.reflect.*",
          "    expression.asTerm match",
          "      case qq\"\"\"{ def id(λ: Int): Int = λ; id($argument) }\"\"\" => Expr(true)",
          "      case _ => Expr(false)"
        ).mkString("\n")
      )
      val fixtureReporter = new Driver().process(
        Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      )
      assertEquals(fixtureReporter.allErrors.map(_.message), Nil)

      Files.writeString(
        consumer,
        List(
          "package external.q065r",
          "object Consumer:",
          "  val result = Rejected.matches {",
          "    def renamed(value: Int): Int = value",
          "    renamed(41)",
          "  }"
        ).mkString("\n")
      )
      val consumerClasspath = output.toString + java.io.File.pathSeparator + compilationClasspath
      val reporter = new Driver().process(
        Array("-classpath", consumerClasspath, "-d", output.toString, consumer.toString)
      )
      val messages = reporter.allErrors.map(_.message)
      assert(
        messages.exists(_.contains("P3 requires a simple parameter binder")),
        messages.mkString("\n")
      )
    finally deleteRecursively(temporary)

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[scala.meta.Tree],
      classOf[Compiler],
      classOf[TermPattern],
      TermFrontend.getClass,
      getClass
    ).flatMap { cls =>
      Option(cls.getProtectionDomain)
        .flatMap(domain => Option(domain.getCodeSource))
        .map(source => Path.of(source.getLocation.toURI).toString)
    }.distinct.mkString(java.io.File.pathSeparator)

  private def deleteRecursively(path: Path): Unit =
    val stream = Files.walk(path)
    try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
    finally stream.close()
