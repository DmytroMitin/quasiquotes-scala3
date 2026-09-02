package quasiquotes.terms.dotty

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

import quasiquotes.parser.TermShape
import quasiquotes.terms.ConstructedTerm

class GeneratedOriginConstructorTyperRuntimeTest extends munit.FunSuite:
  private val GeneratedSource =
    "new java.lang.StringBuilder(\"u010\").append(\"-generated\").toString()"

  test("richer generated constructor survives pre-Typer TASTy class emission and runtime") {
    val temporary = Files.createTempDirectory("u010-generated-constructor-")
    try
      val source = temporary.resolve("U010GeneratedConstructorRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """object U010GeneratedConstructorRuntime:
          |  def result: String = "placeholder"
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new GeneratedConstructorDriver
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
      assertEquals(driver.insertedSource, Some(GeneratedSource))
      assertEquals(driver.insertedNameRoles, Some(Vector(false, false, true)))
      assertEquals(driver.insertedAllNoSymbol, Some(true))
      assertEquals(driver.insertedAllPositioned, Some(true))
      assertEquals(driver.insertedNoTypedSplice, Some(true))

      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith(".class")))
      assert(emitted.exists(_.toString.endsWith(".tasty")))

      val loader =
        new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U010GeneratedConstructorRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        val value = moduleClass.getMethod("result").invoke(module)
        assertEquals(value, "u010-generated")
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class GeneratedConstructorDriver extends Driver:
    @volatile var insertedSource: Option[String] = None
    @volatile var insertedNameRoles: Option[Vector[Boolean]] = None
    @volatile var insertedAllNoSymbol: Option[Boolean] = None
    @volatile var insertedAllPositioned: Option[Boolean] = None
    @volatile var insertedNoTypedSplice: Option[Boolean] = None

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new InsertGeneratedConstructor(GeneratedConstructorDriver.this)) ::
            super.frontendPhases.tail

  private final class InsertGeneratedConstructor(
      evidence: GeneratedConstructorDriver
  ) extends Phase:
    def phaseName: String = "u010GeneratedConstructorInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      ConstructedTermGeneratedOriginAdapter
        .lower(runtimeFixture, "<u010-generated-constructor>") match
        case Left(error) => report.error(error.message)
        case Right(result) =>
          val inserted = GeneratedOriginFragmentSupport.allTrees(result.tree)
          evidence.insertedSource = Some(result.generatedSource)
          evidence.insertedNameRoles = Some(constructorNameRoles(result.tree))
          evidence.insertedAllNoSymbol = Some(inserted.forall(_.symbol == NoSymbol))
          evidence.insertedAllPositioned = Some(
            inserted.forall(tree =>
              tree.source.path == result.virtualSourceName &&
                tree.span.exists &&
                tree.span.start >= 0 &&
                tree.span.end <= result.generatedSource.length
            )
          )
          evidence.insertedNoTypedSplice =
            Some(inserted.forall(!_.isInstanceOf[untpd.TypedSplice]))

          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              tree match
                case definition @ untpd.DefDef(name, paramss, tpt, _)
                    if name.toString == "result" =>
                  untpd.cpy.DefDef(definition)(name, paramss, tpt, result.tree)
                case _ => super.transform(tree)
          summon[Context].compilationUnit.untpdTree =
            transformer.transform(summon[Context].compilationUnit.untpdTree)

    private def constructorNameRoles(
        tree: untpd.Tree
    )(using Context): Vector[Boolean] =
      GeneratedOriginFragmentSupport
        .allTrees(tree)
        .collectFirst { case fresh: untpd.New => fresh.tpt } match
        case Some(constructor) => typePathNames(constructor).map(_.isTypeName)
        case None =>
          report.error("U010 generated constructor tree has no New node")
          Vector.empty

    private def typePathNames(
        tree: untpd.Tree
    )(using Context): Vector[dotty.tools.dotc.core.Names.Name] =
      tree match
        case identifier: untpd.Ident => Vector(identifier.name)
        case selected: untpd.Select =>
          typePathNames(selected.qualifier) :+ selected.name
        case other =>
          report.error(
            s"U010 unexpected constructor type path: ${other.getClass.getSimpleName}"
          )
          Vector.empty

  private def runtimeFixture: ConstructedTerm =
    ConstructedTerm.fromShape(
      TermShape.Apply(
        TermShape.Select(
          TermShape.Apply(
            TermShape.Select(
              TermShape.New(
                "java.lang.StringBuilder",
                List(TermShape.Literal("\"u010\""))
              ),
              "append"
            ),
            List(TermShape.Literal("\"-generated\""))
          ),
          "toString"
        ),
        Nil
      )
    ).toOption.get

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ConstructedTerm],
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
