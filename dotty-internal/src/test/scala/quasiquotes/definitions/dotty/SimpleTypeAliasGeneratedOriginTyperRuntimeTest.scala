package quasiquotes.definitions.dotty

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser

import quasiquotes.definitions.{DefinitionName, DefinitionShape}
import quasiquotes.parser.TypeShape
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

class SimpleTypeAliasGeneratedOriginTyperRuntimeTest extends munit.FunSuite:
  test("positioned simple alias survives object-template insertion, Typer, TASTy, and ordinary runtime use") {
    val temporary = Files.createTempDirectory("u026-simple-alias-")
    try
      val source = temporary.resolve("U026SimpleAliasRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """object U026SimpleAliasRuntime:
          |  def sum(values: GeneratedInts): Int = values.sum
          |  def run: Int = sum(List(20, 22))
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new GeneratedAliasDriver(alias)
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
      assertEquals(driver.insertedOwner, Some("U026SimpleAliasRuntime"))
      assertEquals(driver.insertedSource, Some("type GeneratedInts = List[Int]"))
      assertEquals(driver.beforeTyperInvariantValid, Some(true))

      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith("U026SimpleAliasRuntime$.class")))
      assert(emitted.exists(_.toString.endsWith("U026SimpleAliasRuntime.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U026SimpleAliasRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("run").invoke(module), Integer.valueOf(42))
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class GeneratedAliasDriver(shape: DefinitionShape.SimpleTypeAlias)
      extends Driver:
    @volatile var insertedOwner: Option[String] = None
    @volatile var insertedSource: Option[String] = None
    @volatile var beforeTyperInvariantValid: Option[Boolean] = None

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new InsertGeneratedAlias(shape, GeneratedAliasDriver.this)) ::
            super.frontendPhases.tail

  private final class InsertGeneratedAlias(
      shape: DefinitionShape.SimpleTypeAlias,
      evidence: GeneratedAliasDriver
  ) extends Phase:
    def phaseName: String = "u026SimpleAliasInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      SimpleTypeAliasGeneratedOriginAdapter
        .lower(shape, "<quasiquotes-generated:u026-simple-alias>") match
        case Left(problem) => report.error(problem.message)
        case Right(result) =>
          val inserted = result.tree.asInstanceOf[untpd.TypeDef]
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              tree match
                case module: untpd.ModuleDef
                    if module.name.toString == "U026SimpleAliasRuntime" =>
                  evidence.insertedOwner = Some(module.name.toString)
                  val template = module.impl
                  val updated = untpd.cpy.Template(template)(
                    template.constr,
                    template.parentsOrDerived,
                    template.derived,
                    template.self,
                    template.body :+ inserted
                  )
                  untpd.cpy.ModuleDef(module)(module.name, updated)
                case _ => super.transform(tree)
          summon[Context].compilationUnit.untpdTree =
            transformer.transform(summon[Context].compilationUnit.untpdTree)
          evidence.insertedSource = Some(result.generatedSource)
          evidence.beforeTyperInvariantValid = Some(
            (inserted +: GeneratedOriginFragmentSupport.allTrees(inserted.rhs)).forall {
              tree =>
                tree.source.exists &&
                  tree.source.path == result.virtualSourceName &&
                  tree.source.content.mkString == result.generatedSource &&
                  tree.span.exists &&
                  tree.span.start >= 0 &&
                  tree.span.end <= result.generatedSource.length &&
                  tree.symbol == NoSymbol &&
                  !tree.isInstanceOf[untpd.TypedSplice]
            }
          )

  private def alias: DefinitionShape.SimpleTypeAlias =
    DefinitionShape
      .simpleTypeAlias(
        DefinitionName
          .fromSource("GeneratedInts")
          .fold(problem => fail(problem.message), identity),
        TypeShape.Apply(
          TypeShape.Identifier("List"),
          List(TypeShape.Identifier("Int"))
        )
      )
      .fold(problem => fail(problem.message), identity)

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
