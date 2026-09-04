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

import quasiquotes.definitions.{DefinitionName, DefinitionShape}
import quasiquotes.parser.{BinderId, TermShape, TypeShape}

class DefinitionShapeUntypedLowererTyperRuntimeTest extends munit.FunSuite:
  test("all five production-lowered families survive pre-Typer insertion, TASTy emission, and real use") {
    val temporary = Files.createTempDirectory("u022-definition-shape-")
    try
      val source = temporary.resolve("U022DefinitionShapeRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """object U022DefinitionShapeRuntime:
          |  def valueResult: Int = generatedValue
          |  def methodResult: Int = generatedMethod
          |  def singleResult: Int = generatedSingle(21)
          |  def twoResult: Int = generatedTwo(20, 22)
          |  def aliasResult(value: GeneratedAlias): Int = value
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new GeneratedDefinitionsDriver(fixtures)
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
      assertEquals(driver.beforeTyperRawInvariantValid, Some(true))
      assertEquals(driver.insertedFamilies, Vector("ValDef", "DefDef", "DefDef", "DefDef", "TypeDef"))

      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith("U022DefinitionShapeRuntime$.class")))
      assert(emitted.exists(_.toString.endsWith("U022DefinitionShapeRuntime.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U022DefinitionShapeRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("valueResult").invoke(module), Integer.valueOf(1))
        assertEquals(moduleClass.getMethod("methodResult").invoke(module), Integer.valueOf(1))
        assertEquals(moduleClass.getMethod("singleResult").invoke(module), Integer.valueOf(21))
        assertEquals(moduleClass.getMethod("twoResult").invoke(module), Integer.valueOf(20))
        assertEquals(
          moduleClass.getMethod("aliasResult", Integer.TYPE).invoke(module, Integer.valueOf(42)),
          Integer.valueOf(42)
        )
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class GeneratedDefinitionsDriver(shapes: Vector[DefinitionShape])
      extends Driver:
    @volatile var beforeTyperRawInvariantValid: Option[Boolean] = None
    @volatile var insertedFamilies: Vector[String] = Vector.empty

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new InsertGeneratedDefinitions(shapes, GeneratedDefinitionsDriver.this)) ::
            super.frontendPhases.tail

  private final class InsertGeneratedDefinitions(
      shapes: Vector[DefinitionShape],
      evidence: GeneratedDefinitionsDriver
  ) extends Phase:
    def phaseName: String = "u022DefinitionShapeInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val lowered = shapes.foldLeft[Either[DefinitionShapeUntypedLowererError, Vector[untpd.Tree]]](Right(Vector.empty)) {
        case (accumulated, shape) =>
          for
            prior <- accumulated
            raw <- DefinitionShapeUntypedLowerer.lower(shape)
          yield prior :+ raw
      }

      lowered match
        case Left(problem) => report.error(problem.message)
        case Right(results) =>
          evidence.beforeTyperRawInvariantValid = Some(
            results.forall(raw =>
              DefinitionShapeUntypedLowerer
                .validateRawInvariant(raw, "pre-Typer production output")
                .isRight
            )
          )
          evidence.insertedFamilies = results.map(_.getClass.getSimpleName)
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              tree match
                case template: untpd.Template
                    if template.body.exists {
                      case definition: untpd.DefDef =>
                        definition.name.toString == "aliasResult"
                      case _ => false
                    } =>
                  val placementSite = template.body.collectFirst {
                    case definition: untpd.DefDef
                        if definition.name.toString == "aliasResult" =>
                      definition
                  }.get
                  val positioned = results.map(positionForTyper(_, placementSite))
                  untpd.cpy.Template(template)(
                    template.constr,
                    template.parentsOrDerived,
                    template.derived,
                    template.self,
                    template.body ++ positioned
                  )
                case _ => super.transform(tree)
          summon[Context].compilationUnit.untpdTree =
            transformer.transform(summon[Context].compilationUnit.untpdTree)

    private def positionForTyper(
        raw: untpd.Tree,
        site: untpd.Tree
    )(using Context): untpd.Tree =
      val placement = new untpd.UntypedTreeMap:
        override def transform(current: untpd.Tree)(using Context): untpd.Tree =
          val transformed = super.transform(current)
          if transformed.isEmpty then transformed
          else transformed.cloneIn(site.source).withSpan(site.span)
      placement.transform(raw)

  private def fixtures: Vector[DefinitionShape] =
    val first = BinderId(1)
    val second = BinderId(2)
    val intType = TypeShape.Identifier("Int")
    Vector(
      DefinitionShape
        .immutableVal(name("generatedValue"), intType, TermShape.Literal("1"))
        .toOption
        .get,
      DefinitionShape
        .parameterlessDef(name("generatedMethod"), intType, TermShape.Literal("1"))
        .toOption
        .get,
      DefinitionShape
        .singleParameterDef(
          name("generatedSingle"),
          first,
          name("x"),
          intType,
          intType,
          TermShape.BoundReference(first, "x")
        )
        .toOption
        .get,
      DefinitionShape
        .twoParameterDef(
          name("generatedTwo"),
          first,
          name("x"),
          intType,
          second,
          name("y"),
          intType,
          intType,
          TermShape.BoundReference(first, "x")
        )
        .toOption
        .get,
      DefinitionShape
        .simpleTypeAlias(name("GeneratedAlias"), intType)
        .toOption
        .get
    )

  private def name(source: String): DefinitionName =
    DefinitionName.fromSource(source).fold(error => fail(error.message), identity)

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[DefinitionShape],
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
