package external.consumer

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*
import scala.meta.*
import scala.meta.dialects.Scala3

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.parsing.Parser

import _root_.quasiquotes.terms.dotty.ScalametaTermGeneratedOriginBridge

final class ScalametaTermGeneratedOriginBridgeTyperRuntimeTest
    extends munit.FunSuite:
  test("foreign-package bridge terms survive pre-Typer insertion TASTy and runtime"):
    val temporary = Files.createTempDirectory("term-generated-origin-runtime-")
    try
      val source = temporary.resolve("TermGeneratedOriginRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """object TermGeneratedOriginRuntime:
          |  def lambdaResult: Int => Int = identity
          |  def localValResult: Int = 0
          |  def localDefResult: Int => Int = identity
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val fixtures = Vector(
        "lambdaResult" -> (
          parsed("(x: Int) => x + 1"),
          "<generated:term-runtime-lambda>"
        ),
        "localValResult" -> (
          parsed("{ val x: Int = 41; x + 1 }"),
          "<generated:term-runtime-p2>"
        ),
        "localDefResult" -> (
          parsed("{ def id(x: Int): Int = x; id }"),
          "<generated:term-runtime-p3>"
        )
      )
      val driver = new BridgeDriver(fixtures)
      val reporter = driver.process(
        Array(
          "-classpath",
          compilationClasspath,
          "-d",
          output.toString,
          source.toString
        )
      )

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(
        driver.generatedSources,
        Vector(
          "(x: Int) => x + 1",
          "{ val x: Int = 41; x + 1 }",
          "{ def id(x: Int): Int = x; id }"
        )
      )
      assertEquals(driver.positionedBeforeTyper, Vector(true, true, true))

      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith(".class")), clues(emitted))
      assert(emitted.exists(_.toString.endsWith(".tasty")), clues(emitted))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("TermGeneratedOriginRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        val function = moduleClass
          .getMethod("lambdaResult")
          .invoke(module)
          .asInstanceOf[Function1[Int, Int]]
        assertEquals(function(41), 42)
        assertEquals(
          moduleClass.getMethod("localValResult").invoke(module),
          Integer.valueOf(42)
        )
        val localDef = moduleClass
          .getMethod("localDefResult")
          .invoke(module)
          .asInstanceOf[Function1[Int, Int]]
        assertEquals(localDef(42), 42)
      finally loader.close()
    finally deleteRecursively(temporary)

  private final class BridgeDriver(
      fixtures: Vector[(String, (Term, String))]
  ) extends Driver:
    @volatile var generatedSources: Vector[String] = Vector.empty
    @volatile var positionedBeforeTyper: Vector[Boolean] = Vector.empty

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new InsertBridgeTerms(fixtures, BridgeDriver.this)) ::
            super.frontendPhases.tail

  private final class InsertBridgeTerms(
      fixtures: Vector[(String, (Term, String))],
      evidence: BridgeDriver
  ) extends Phase:
    def phaseName: String = "termGeneratedOriginBridgeInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val lowered = fixtures.foldLeft[
        Either[
          ScalametaTermGeneratedOriginBridge.Failure,
          Vector[(String, ScalametaTermGeneratedOriginBridge.Lowered)]
        ]
      ](Right(Vector.empty)): (accumulator, next) =>
        for
          accumulated <- accumulator
          result <- ScalametaTermGeneratedOriginBridge.lower(next._2._1, next._2._2)
        yield accumulated :+ (next._1 -> result)

      lowered match
        case Left(problem) => report.error(s"${problem.code}: ${problem.detail}")
        case Right(results) =>
          val replacements = results.toMap
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              tree match
                case definition @ untpd.DefDef(name, paramss, tpt, _)
                    if replacements.contains(name.toString) =>
                  untpd.cpy.DefDef(definition)(
                    name,
                    paramss,
                    tpt,
                    replacements(name.toString).tree
                  )
                case _ => super.transform(tree)

          summon[Context].compilationUnit.untpdTree =
            transformer.transform(summon[Context].compilationUnit.untpdTree)
          evidence.generatedSources = results.map(_._2.generatedSource)
          evidence.positionedBeforeTyper = results.map: (_, result) =>
            result.tree.source.exists &&
              result.tree.source.path == result.virtualSourceName &&
              result.tree.span.exists &&
              result.tree.span.start == 0 &&
              result.tree.span.end == result.generatedSource.length

  private def parsed(source: String): Term =
    Scala3(source).parse[Term].get

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[scala.meta.Tree],
      ScalametaTermGeneratedOriginBridge.getClass,
      getClass
    )
      .flatMap(value =>
        Option(value.getProtectionDomain)
          .flatMap(domain => Option(domain.getCodeSource))
          .map(_.getLocation.toURI)
      )
      .map(Path.of(_).toString)
      .distinct
      .mkString(java.io.File.pathSeparator)

  private def deleteRecursively(root: Path): Unit =
    if Files.exists(root) then
      val stream = Files.walk(root)
      try
        stream
          .sorted(java.util.Comparator.reverseOrder())
          .iterator()
          .asScala
          .foreach(Files.deleteIfExists)
      finally stream.close()
