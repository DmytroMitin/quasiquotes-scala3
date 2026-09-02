package quasiquotes.terms.dotty

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.parsing.Parser

import quasiquotes.parser.TermShape
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

class ConstructedTermGeneratedOriginTyperRuntimeTest extends munit.FunSuite:
  private val Expected = "phase44:QuasiquotesBackendUser"
  private val SourceName =
    "<macroparadise-generated:externalQuasiquotesTerm:QuasiquotesBackendUser>"

  test("positions through the friend adapter then survives typer TASTy class emission and runtime") {
    val temporary = Files.createTempDirectory("phase47-generated-origin-")
    try
      val source =
        temporary.resolve("Phase47GeneratedRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """object Phase47GeneratedRuntime:
          |  def result: String = "adapter-placeholder"
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new GeneratedOriginDriver(peerFixture, SourceName)
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
      assertEquals(driver.insertedSource, Some(PeerSource))
      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith(".class")))
      assert(emitted.exists(_.toString.endsWith(".tasty")))

      val loader =
        new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("Phase47GeneratedRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        val value = moduleClass.getMethod("result").invoke(module)
        assertEquals(value, Expected)
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  test("P1 Block generated origin survives typer TASTy class emission and runtime") {
    val temporary = Files.createTempDirectory("u006-p1-generated-origin-")
    try
      val source = temporary.resolve("U006P1GeneratedRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """object U006P1GeneratedRuntime:
          |  def result: String = "adapter-placeholder"
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new GeneratedOriginDriver(p1BlockFixture, "<u006-p1-generated-origin>")
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
      assertEquals(driver.insertedSource, Some(P1BlockSource))
      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith(".class")))
      assert(emitted.exists(_.toString.endsWith(".tasty")))

      val loader =
        new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U006P1GeneratedRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        val value = moduleClass.getMethod("result").invoke(module)
        assertEquals(value, "u006-result")
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class GeneratedOriginDriver(
      constructed: ConstructedTerm,
      sourceName: String
  )
      extends Driver:
    @volatile var insertedSource: Option[String] = None

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(
              new InsertGeneratedOrigin(
                constructed,
                sourceName,
                GeneratedOriginDriver.this
              )
            ) ::
            super.frontendPhases.tail

  private final class InsertGeneratedOrigin(
      constructed: ConstructedTerm,
      sourceName: String,
      evidence: GeneratedOriginDriver
  ) extends Phase:
    def phaseName: String = "phase47GeneratedOriginInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      ConstructedTermGeneratedOriginAdapter.lower(constructed, sourceName) match
        case Left(error) =>
          report.error(error.message)
        case Right(result) =>
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              tree match
                case definition @ untpd.DefDef(name, paramss, tpt, _)
                    if name.toString == "result" =>
                  untpd.cpy.DefDef(definition)(
                    name,
                    paramss,
                    tpt,
                    result.tree
                  )
                case _ =>
                  super.transform(tree)
          summon[Context].compilationUnit.untpdTree =
            transformer.transform(summon[Context].compilationUnit.untpdTree)
          evidence.insertedSource = Some(result.generatedSource)

  private def peerFixture: ConstructedTerm =
    val root =
      TermShape.If(
        TermShape.Literal("true"),
        TermShape.Typed(
          TermShape.Infix(
            TermShape.Literal("\"phase44:\""),
            "+",
            TermShape.Literal("\"QuasiquotesBackendUser\"")
          ),
          "String"
        ),
        TermShape.Literal("\"unreachable\"")
      )
    ConstructedTerm
      .create(root, Vector(TypeNormalForm.STypeIdent("String")))
      .toOption
      .get

  private val PeerSource =
    """if true then ("phase44:" + "QuasiquotesBackendUser"): String else "unreachable""""

  private def p1BlockFixture: ConstructedTerm =
    val root =
      TermShape.Block(
        List(
          TermShape.Typed(TermShape.Literal("\"discarded\""), "String"),
          TermShape.Block(
            List(TermShape.Literal("\"nested\"")),
            TermShape.Literal("\"inner\"")
          )
        ),
        TermShape.Typed(TermShape.Literal("\"u006-result\""), "String")
      )
    ConstructedTerm
      .create(
        root,
        Vector(
          TypeNormalForm.STypeIdent("String"),
          TypeNormalForm.STypeIdent("String")
        )
      )
      .toOption
      .get

  private val P1BlockSource =
    """{ "discarded": String; { "nested"; "inner" }; "u006-result": String }"""

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

class ConstructedTermGeneratedOriginPrefixTyperRuntimeTest
    extends munit.FunSuite:
  test("corrected prefix-negative composition survives typer TASTy class emission and runtime") {
    val temporary = Files.createTempDirectory("phase47r-prefix-negative-")
    try
      val source =
        temporary.resolve("Phase47RPrefixNegativeRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """object Phase47RPrefixNegativeRuntime:
          |  def result: Int = 0
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver =
        new PrefixGeneratedOriginDriver(prefixNegativeFixture)
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
      assertEquals(driver.insertedSource, Some("-(-1)"))
      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith(".class")))
      assert(emitted.exists(_.toString.endsWith(".tasty")))

      val loader =
        new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("Phase47RPrefixNegativeRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        val value = moduleClass.getMethod("result").invoke(module)
        assertEquals(value, Integer.valueOf(1))
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class PrefixGeneratedOriginDriver(
      constructed: ConstructedTerm
  ) extends Driver:
    @volatile var insertedSource: Option[String] = None

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(
              new InsertPrefixGeneratedOrigin(
                constructed,
                PrefixGeneratedOriginDriver.this
              )
            ) ::
            super.frontendPhases.tail

  private final class InsertPrefixGeneratedOrigin(
      constructed: ConstructedTerm,
      evidence: PrefixGeneratedOriginDriver
  ) extends Phase:
    def phaseName: String = "phase47rPrefixGeneratedOriginInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      ConstructedTermGeneratedOriginAdapter
        .lower(
          constructed,
          "<quasiquotes-generated:phase47r-prefix-negative>"
        ) match
        case Left(error) =>
          report.error(error.message)
        case Right(result) =>
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              tree match
                case definition @ untpd.DefDef(name, paramss, tpt, _)
                    if name.toString == "result" =>
                  untpd.cpy.DefDef(definition)(
                    name,
                    paramss,
                    tpt,
                    result.tree
                  )
                case _ =>
                  super.transform(tree)
          summon[Context].compilationUnit.untpdTree =
            transformer.transform(summon[Context].compilationUnit.untpdTree)
          evidence.insertedSource = Some(result.generatedSource)

  private def prefixNegativeFixture: ConstructedTerm =
    ConstructedTerm
      .fromShape(
        TermShape.Unary("-", TermShape.Literal("-1"))
      )
      .toOption
      .get

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
