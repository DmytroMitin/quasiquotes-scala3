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

import _root_.quasiquotes.definitions.dotty.ScalametaDefinitionGeneratedOriginBridge

final class ScalametaDefinitionBridgeTyperRuntimeTest extends munit.FunSuite:
  test("foreign-package bridge members survive pre-Typer insertion TASTy and runtime"):
    val temporary = Files.createTempDirectory("c020-definition-runtime-")
    try
      val source = temporary.resolve("C020DefinitionRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """object C020DefinitionRuntime:
          |  def methodResult: String = foo(7)
          |  def valueResult: Int = answer
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new BridgeDriver(
        Vector(
          parsed("def foo(x: Int): String = x.toString") ->
            "<generated:c020-runtime-method>",
          parsed("val answer: Int = 42") ->
            "<generated:c020-runtime-value>"
        )
      )
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
          "def foo(x: Int): String = x.toString",
          "val answer: Int = 42"
        )
      )
      assertEquals(driver.positionedBeforeTyper, Vector(true, true))

      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith(".class")), clues(emitted))
      assert(emitted.exists(_.toString.endsWith(".tasty")), clues(emitted))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("C020DefinitionRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("methodResult").invoke(module), "7")
        assertEquals(
          moduleClass.getMethod("valueResult").invoke(module),
          Integer.valueOf(42)
        )
      finally loader.close()
    finally deleteRecursively(temporary)

  test("a rejected authored definition prevents the complete member batch"):
    val temporary = Files.createTempDirectory("c020-definition-atomic-")
    try
      val source = temporary.resolve("C020DefinitionAtomic.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        "object C020DefinitionAtomic\n",
        StandardCharsets.UTF_8
      )

      val driver = new BridgeDriver(
        Vector(
          parsed("val answer: Int = 42") -> "<generated:c020-atomic-value>",
          parsed("type Unsupported = Int") -> "<generated:c020-atomic-alias>"
        )
      )
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
      assertEquals(driver.rejectionCode, Some("GENERATED_ORIGIN_FAMILY_UNSUPPORTED"))
      assertEquals(driver.generatedSources, Vector.empty)
      assertEquals(driver.insertedMemberCount, 0)
    finally deleteRecursively(temporary)

  private final class BridgeDriver(
      definitions: Vector[(Defn, String)]
  ) extends Driver:
    @volatile var generatedSources: Vector[String] = Vector.empty
    @volatile var positionedBeforeTyper: Vector[Boolean] = Vector.empty
    @volatile var rejectionCode: Option[String] = None
    @volatile var insertedMemberCount: Int = 0

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new InsertBridgeDefinitions(definitions, BridgeDriver.this)) ::
            super.frontendPhases.tail

  private final class InsertBridgeDefinitions(
      definitions: Vector[(Defn, String)],
      evidence: BridgeDriver
  ) extends Phase:
    def phaseName: String = "c020DefinitionBridgeInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val lowered = definitions.foldLeft[
        Either[
          ScalametaDefinitionGeneratedOriginBridge.Failure,
          Vector[ScalametaDefinitionGeneratedOriginBridge.Lowered]
        ]
      ](Right(Vector.empty)): (accumulator, next) =>
        for
          accumulated <- accumulator
          result <- ScalametaDefinitionGeneratedOriginBridge.lower(next._1, next._2)
        yield accumulated :+ result

      lowered match
        case Left(problem) =>
          evidence.rejectionCode = Some(problem.code)
        case Right(results) =>
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              tree match
                case template: untpd.Template =>
                  untpd.cpy.Template(template)(
                    template.constr,
                    template.parentsOrDerived,
                    template.derived,
                    template.self,
                    template.body ++ results.map(_.tree)
                  )
                case _ => super.transform(tree)

          summon[Context].compilationUnit.untpdTree =
            transformer.transform(summon[Context].compilationUnit.untpdTree)
          evidence.generatedSources = results.map(_.generatedSource)
          evidence.positionedBeforeTyper = results.map: result =>
            result.tree.source.exists &&
              result.tree.source.path == result.virtualSourceName &&
              result.tree.span.exists &&
              result.tree.span.start == 0 &&
              result.tree.span.end == result.generatedSource.length
          evidence.insertedMemberCount = results.size

  private def parsed(source: String): Defn =
    Scala3(source).parse[Stat].get match
      case definition: Defn => definition
      case other => fail(s"expected Defn, found ${other.productPrefix}")

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[scala.meta.Tree],
      ScalametaDefinitionGeneratedOriginBridge.getClass,
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
