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

import quasiquotes.definitions.{ConstructedDefinition, DefinitionName}
import quasiquotes.parser.TermShape
import quasiquotes.terms.ConstructedTerm
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport
import quasiquotes.types.TypeNormalForm

class ConstructedDefinitionGeneratedOriginTyperRuntimeTest
    extends munit.FunSuite:
  import TypeNormalForm.*

  test("positioned method and value members survive typer TASTy class emission and ordinary runtime use") {
    val temporary =
      Files.createTempDirectory("phase49-generated-definition-")
    try
      val source =
        temporary.resolve("Phase49GeneratedDefinitionRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """object Phase49GeneratedDefinitionRuntime:
          |  def methodResult: String = generatedMethod
          |  def valueResult: Int = generatedValue
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver =
        new GeneratedDefinitionDriver(methodFixture, valueFixture)
      val reporter =
        driver.process(
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
        driver.insertedSources,
        Vector(
          "def generatedMethod: String = \"phase49:\" + \"method\"",
          "val generatedValue: Int = 1"
        )
      )
      assertEquals(driver.beforeTyperSourceTruth, Vector(true, true))

      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith(".class")))
      assert(emitted.exists(_.toString.endsWith(".tasty")))

      val loader =
        new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass =
          loader.loadClass("Phase49GeneratedDefinitionRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(
          moduleClass.getMethod("methodResult").invoke(module),
          "phase49:method"
        )
        assertEquals(
          moduleClass.getMethod("valueResult").invoke(module),
          Integer.valueOf(1)
        )
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class GeneratedDefinitionDriver(
      method: ConstructedDefinition,
      value: ConstructedDefinition
  ) extends Driver:
    @volatile var insertedSources: Vector[String] = Vector.empty
    @volatile var beforeTyperSourceTruth: Vector[Boolean] = Vector.empty

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(
              new InsertGeneratedDefinitions(
                method,
                value,
                GeneratedDefinitionDriver.this
              )
            ) ::
            super.frontendPhases.tail

  private final class InsertGeneratedDefinitions(
      method: ConstructedDefinition,
      value: ConstructedDefinition,
      evidence: GeneratedDefinitionDriver
  ) extends Phase:
    def phaseName: String = "phase49GeneratedDefinitionInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val lowered =
        for
          methodResult <- ConstructedDefinitionGeneratedOriginAdapter.lower(
            method,
            "<quasiquotes-generated:phase49-method>"
          )
          valueResult <- ConstructedDefinitionGeneratedOriginAdapter.lower(
            value,
            "<quasiquotes-generated:phase49-value>"
          )
        yield Vector(methodResult, valueResult)

      lowered match
        case Left(error) =>
          report.error(error.message)
        case Right(results) =>
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              tree match
                case template: untpd.Template
                    if template.body.exists {
                      case method: untpd.DefDef =>
                        method.name.toString == "methodResult"
                      case _ => false
                    } =>
                  untpd.cpy.Template(template)(
                    template.constr,
                    template.parentsOrDerived,
                    template.derived,
                    template.self,
                    template.body ++ results.map(_.tree)
                  )
                case _ =>
                  super.transform(tree)
          summon[Context].compilationUnit.untpdTree =
            transformer.transform(summon[Context].compilationUnit.untpdTree)
          evidence.insertedSources = results.map(_.generatedSource)
          evidence.beforeTyperSourceTruth =
            results.map(result =>
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

  private def methodFixture: ConstructedDefinition =
    ConstructedDefinition
      .parameterlessDef(
        DefinitionName.plain("generatedMethod").toOption.get,
        STypeIdent("String"),
        ConstructedTerm
          .fromShape(
            TermShape.Infix(
              TermShape.Literal("\"phase49:\""),
              "+",
              TermShape.Literal("\"method\"")
            )
          )
          .toOption
          .get
      )
      .toOption
      .get

  private def valueFixture: ConstructedDefinition =
    ConstructedDefinition
      .immutableVal(
        DefinitionName.plain("generatedValue").toOption.get,
        STypeIdent("Int"),
        ConstructedTerm
          .fromShape(TermShape.Literal("1"))
          .toOption
          .get
      )
      .toOption
      .get

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
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
