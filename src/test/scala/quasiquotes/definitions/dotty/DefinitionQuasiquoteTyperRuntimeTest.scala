package quasiquotes.definitions.dotty

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.parsing.Parser

import quasiquotes.definitions.*
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

class DefinitionQuasiquoteTyperRuntimeTest extends munit.FunSuite:
  import DefinitionQuasiquotes.*

  test("dqr output survives explicit generated-origin insertion typer TASTy class emission and runtime use") {
    val constructed =
      dqr"""def generatedMethod: String = "phase52:" + "runtime""""
        .fold(error => fail(error.diagnostic.message), identity)
        .constructed
    val temporary = Files.createTempDirectory("phase52-dqr-runtime-")
    try
      val source = temporary.resolve("Phase52DqrRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """object Phase52DqrRuntime:
          |  def result: String = generatedMethod
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new GeneratedDefinitionDriver(constructed)
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
        driver.insertedSource,
        Some("def generatedMethod: String = \"phase52:\" + \"runtime\"")
      )
      assertEquals(driver.beforeTyperSourceTruth, Some(true))

      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith(".class")))
      assert(emitted.exists(_.toString.endsWith(".tasty")))

      val loader =
        new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("Phase52DqrRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(
          moduleClass.getMethod("result").invoke(module),
          "phase52:runtime"
        )
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class GeneratedDefinitionDriver(
      constructed: ConstructedDefinition
  ) extends Driver:
    @volatile var insertedSource: Option[String] = None
    @volatile var beforeTyperSourceTruth: Option[Boolean] = None

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(
              new InsertGeneratedDefinition(
                constructed,
                GeneratedDefinitionDriver.this
              )
            ) ::
            super.frontendPhases.tail

  private final class InsertGeneratedDefinition(
      constructed: ConstructedDefinition,
      evidence: GeneratedDefinitionDriver
  ) extends Phase:
    def phaseName: String = "phase52DqrDefinitionInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      ConstructedDefinitionGeneratedOriginAdapter
        .lower(constructed, "<quasiquotes-generated:phase52-dqr>") match
        case Left(error) =>
          report.error(error.message)
        case Right(result) =>
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              tree match
                case template: untpd.Template
                    if template.body.exists {
                      case method: untpd.DefDef =>
                        method.name.toString == "result"
                      case _ => false
                    } =>
                  untpd.cpy.Template(template)(
                    template.constr,
                    template.parentsOrDerived,
                    template.derived,
                    template.self,
                    template.body :+ result.tree
                  )
                case _ => super.transform(tree)
          summon[Context].compilationUnit.untpdTree =
            transformer.transform(summon[Context].compilationUnit.untpdTree)
          evidence.insertedSource = Some(result.generatedSource)
          evidence.beforeTyperSourceTruth = Some(
            GeneratedOriginFragmentSupport
              .allTrees(result.tree)
              .forall(tree =>
                tree.source.exists &&
                  tree.source.path == result.virtualSourceName &&
                  tree.span.exists &&
                  tree.span.start >= 0 &&
                  tree.span.end <= result.generatedSource.length
              ) &&
              result.tree.span.start == 0 &&
              result.tree.span.end == result.generatedSource.length
          )

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[_root_.dotty.tools.dotc.Compiler],
      classOf[ConstructedDefinition],
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
          .forEach(Files.deleteIfExists(_))
      finally stream.close()
