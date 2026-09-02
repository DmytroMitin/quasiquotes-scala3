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

import quasiquotes.neutral.ScalametaTermProjection
import quasiquotes.terms.ConstructedTerm

import scala.meta.*
import scala.meta.dialects.Scala3

class P3LocalDefTyperRuntimeTest extends munit.FunSuite:
  private val LocalDefSource = "{ def id(x: Int): Int = x; id }"

  test("production N007 LocalDef survives source-free audit pre-Typer TASTy and runtime") {
    val projected = ScalametaTermProjection
      .project(Input.String(LocalDefSource).parse[Term].get)
      .toOption.get.shape
    val constructed = ConstructedTerm.fromShape(projected).toOption.get
    val temporary = Files.createTempDirectory("u012-p3-generated-origin-")
    try
      val source = temporary.resolve("U012P3GeneratedRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """object U012P3GeneratedRuntime:
          |  def generated: Int => Int = (_: Int) => 0
          |  def result: Int = generated(42)
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new P3Driver(constructed)
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
      assertEquals(driver.sourceFreeTopology, Some(true))
      assertEquals(driver.sourceFreeInvariants, Some(true))
      assertEquals(driver.generatedSource, Some(LocalDefSource))
      assertEquals(driver.generatedTopology, Some(true))
      assertEquals(driver.generatedInvariants, Some(true))

      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith(".class")))
      assert(emitted.exists(_.toString.endsWith(".tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U012P3GeneratedRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("result").invoke(module), Integer.valueOf(42))
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class P3Driver(constructed: ConstructedTerm) extends Driver:
    @volatile var sourceFreeTopology: Option[Boolean] = None
    @volatile var sourceFreeInvariants: Option[Boolean] = None
    @volatile var generatedSource: Option[String] = None
    @volatile var generatedTopology: Option[Boolean] = None
    @volatile var generatedInvariants: Option[Boolean] = None

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new InsertP3(constructed, P3Driver.this)) ::
            super.frontendPhases.tail

  private final class InsertP3(
      constructed: ConstructedTerm,
      evidence: P3Driver
  ) extends Phase:
    def phaseName: String = "u012P3GeneratedOriginInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      ConstructedTermUntypedBackend.lower(constructed) match
        case Left(error) => report.error(error.message)
        case Right(sourceFree) =>
          val sourceFreeTrees = GeneratedOriginFragmentSupport.allTrees(sourceFree)
          evidence.sourceFreeTopology = Some(isP3(sourceFree))
          evidence.sourceFreeInvariants = Some(
            sourceFreeTrees.forall(tree =>
              !tree.source.exists &&
                !tree.span.exists &&
                tree.symbol == NoSymbol &&
                !tree.isInstanceOf[untpd.TypedSplice]
            )
          )
          ConstructedTermGeneratedOriginAdapter
            .lower(constructed, "<u012-p3-generated-origin>") match
            case Left(error) => report.error(error.message)
            case Right(generated) =>
              val generatedTrees = GeneratedOriginFragmentSupport.allTrees(generated.tree)
              evidence.generatedSource = Some(generated.generatedSource)
              evidence.generatedTopology = Some(isP3(generated.tree))
              evidence.generatedInvariants = Some(
                generatedTrees.forall(tree =>
                  tree.source.path == generated.virtualSourceName &&
                    tree.span.exists &&
                    tree.span.start >= 0 &&
                    tree.span.end <= generated.generatedSource.length &&
                    tree.symbol == NoSymbol &&
                    !tree.isInstanceOf[untpd.TypedSplice]
                )
              )
              val transformer = new untpd.UntypedTreeMap:
                override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
                  tree match
                    case definition @ untpd.DefDef(name, paramss, tpt, _)
                        if name.toString == "generated" =>
                      untpd.cpy.DefDef(definition)(name, paramss, tpt, generated.tree)
                    case _ => super.transform(tree)
              summon[Context].compilationUnit.untpdTree =
                transformer.transform(summon[Context].compilationUnit.untpdTree)

  private def isP3(tree: untpd.Tree)(using Context): Boolean =
    tree match
      case untpd.Block(
            (method: untpd.DefDef) :: Nil,
            result: untpd.Ident
          ) =>
        method.name.toString == "id" &&
          (method.paramss match
            case List(List(parameter: untpd.ValDef)) =>
              parameter.name.toString == "x" &&
                method.rhs.isInstanceOf[untpd.Ident] &&
                result.name.toString == "id"
            case _ => false)
      case _ => false

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
