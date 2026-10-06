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

final class CompletedMulticlauseApplyExactBackendTyperRuntimeTest
    extends munit.FunSuite:
  private val expectedGeneratedSource = "(f(20)(22), empty()(41))"

  test("generated-origin completed nested Apply including an empty clause survives Typer and runtime"):
    compileAndRun()

  private def compileAndRun(): Unit =
    val temporary = Files.createTempDirectory("u050-multiclause-generated-origin-")
    try
      val source = temporary.resolve("U050CompletedMulticlauseRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """object U050CompletedMulticlauseRuntime:
          |  def f(a: Int)(b: Int): Int = a + b
          |  def empty()(b: Int): Int = b + 1
          |  def result: (Int, Int) = (0, 0)
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new RuntimeDriver
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
      assertEquals(driver.insertedTopology, Some(expectedTopology))
      assertEquals(driver.insertedNoSymbol, Some(true))
      assertEquals(driver.insertedNoTypedSplice, Some(true))
      assertEquals(driver.insertedAllPositioned, Some(true))
      assertEquals(driver.insertedGeneratedSource, Some(expectedGeneratedSource))

      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith(".class")), clues(emitted))
      assert(emitted.exists(_.toString.endsWith(".tasty")), clues(emitted))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U050CompletedMulticlauseRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        val value = moduleClass.getMethod("result").invoke(module).asInstanceOf[Product]
        assertEquals(value.productElement(0), Integer.valueOf(42))
        assertEquals(value.productElement(1), Integer.valueOf(42))
      finally loader.close()
    finally deleteRecursively(temporary)

  private final class RuntimeDriver extends Driver:
    @volatile var insertedTopology: Option[String] = None
    @volatile var insertedNoSymbol: Option[Boolean] = None
    @volatile var insertedNoTypedSplice: Option[Boolean] = None
    @volatile var insertedAllPositioned: Option[Boolean] = None
    @volatile var insertedGeneratedSource: Option[String] = None

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new InsertCompletedMulticlause(RuntimeDriver.this)) :: super.frontendPhases.tail

  private final class InsertCompletedMulticlause(evidence: RuntimeDriver)
      extends Phase:
    def phaseName: String = "u050CompletedMulticlauseInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      ConstructedTerm.fromShape(runtimeShape) match
        case Left(problem) => report.error(problem.message)
        case Right(completed) =>
          ConstructedTermGeneratedOriginAdapter
            .lower(completed, "generated/U050CompletedMulticlause.scala")
            .fold(
              problem => report.error(problem.message),
              result =>
                val nodes = GeneratedOriginFragmentSupport.allTrees(result.tree)
                evidence.insertedGeneratedSource = Some(result.generatedSource)
                evidence.insertedTopology = Some(topology(result.tree))
                evidence.insertedNoSymbol = Some(nodes.forall(_.symbol == NoSymbol))
                evidence.insertedNoTypedSplice = Some(!nodes.exists(_.isInstanceOf[untpd.TypedSplice]))
                evidence.insertedAllPositioned = Some(
                  nodes.forall(node =>
                    node.source.path == result.virtualSourceName &&
                      node.span.exists &&
                      node.span.start >= 0 &&
                      node.span.end <= result.generatedSource.length
                  )
                )

                val transformer = new untpd.UntypedTreeMap:
                  override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match
                    case definition @ untpd.DefDef(name, paramss, tpt, _)
                        if name.toString == "result" =>
                      untpd.cpy.DefDef(definition)(name, paramss, tpt, result.tree)
                    case _ => super.transform(tree)
                summon[Context].compilationUnit.untpdTree =
                  transformer.transform(summon[Context].compilationUnit.untpdTree)
            )

  private def runtimeShape: TermShape =
    TermShape.Tuple(
      List(
        apply(apply(ident("f"), literal("20")), literal("22")),
        apply(apply(ident("empty")), literal("41"))
      )
    )

  private val expectedTopology =
    "Tuple([Apply(Apply(Ident(f),[Number(20)]),[Number(22)]),Apply(Apply(Ident(empty),[]),[Number(41)])])"

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def literal(value: String): TermShape =
    TermShape.Literal(value)

  private def apply(function: TermShape, arguments: TermShape*): TermShape =
    TermShape.Apply(function, arguments.toList)

  private def topology(tree: untpd.Tree): String = tree match
    case untpd.Ident(name) => s"Ident($name)"
    case untpd.Number(value, untpd.NumberKind.Whole(10)) => s"Number($value)"
    case value: untpd.Apply =>
      s"Apply(${topology(value.fun)},[${value.args.map(topology).mkString(",")}])"
    case value: untpd.Tuple => s"Tuple([${value.trees.map(topology).mkString(",")}])"
    case other => s"Unexpected(${other.getClass.getSimpleName})"

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
