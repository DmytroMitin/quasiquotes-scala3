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

import quasiquotes.definitions.ParameterlessDelegatedForwardingPlan
import quasiquotes.definitions.ParameterlessDelegatedForwardingPlan.Plan

class ParameterlessDelegatedForwardingMethodGeneratedOriginTyperRuntimeTest
    extends munit.FunSuite:
  private val canonical = createPlan("empty", "A", "inst", "Empty")
  private val renamed = createPlan("obtain", "Element", "evidence", "Provider")

  test("generated parameterless forwarders survive Typer TASTy emission and runtime") {
    val temporary = Files.createTempDirectory("u048-parameterless-forwarder-")
    try
      val source = temporary.resolve("U048ParameterlessForwarderRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """trait Empty[A]:
          |  def empty: A
          |
          |trait Provider[A]:
          |  def obtain: A
          |
          |object U048ParameterlessForwarderRuntime:
          |  given Empty[Int] with
          |    def empty: Int = 42
          |  given Provider[String] with
          |    def obtain: String = "ready"
          |
          |  def canonicalResult: Int = empty[Int]
          |  def renamedResult: String = obtain[String]
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new ForwarderDriver(Vector(canonical, renamed))
      val reporter = driver.process(Array(
        "-classpath",
        compilationClasspath,
        "-d",
        output.toString,
        source.toString
      ))

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(driver.sourceFreeExact, Vector(true, true))
      assertEquals(driver.generatedExact, Vector(true, true))
      assertEquals(
        driver.generatedSources,
        Vector(
          "def empty[A](using inst: Empty[A]): A = inst.empty",
          "def obtain[Element](using evidence: Provider[Element]): Element = evidence.obtain"
        )
      )

      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith("U048ParameterlessForwarderRuntime$.class")))
      assert(emitted.exists(_.toString.endsWith("U048ParameterlessForwarderRuntime.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U048ParameterlessForwarderRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(
          moduleClass.getMethod("canonicalResult").invoke(module),
          Integer.valueOf(42)
        )
        assertEquals(moduleClass.getMethod("renamedResult").invoke(module), "ready")
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class ForwarderDriver(plans: Vector[Plan]) extends Driver:
    @volatile var sourceFreeExact: Vector[Boolean] = Vector.empty
    @volatile var generatedExact: Vector[Boolean] = Vector.empty
    @volatile var generatedSources: Vector[String] = Vector.empty

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new InsertForwarders(plans, ForwarderDriver.this)) ::
            super.frontendPhases.tail

  private final class InsertForwarders(
      plans: Vector[Plan],
      evidence: ForwarderDriver
  ) extends Phase:
    def phaseName: String = "u048ParameterlessForwarderInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val sourceFree = plans.map { plan =>
        ParameterlessDelegatedForwardingMethodUntypedLowerer.lower(plan) match
          case Left(problem) =>
            report.error(problem.message)
            untpd.EmptyTree.asInstanceOf[untpd.DefDef]
          case Right(value) => value
      }
      evidence.sourceFreeExact = sourceFree.map(isExactSourceFree)
      val generatedResults = plans.zipWithIndex.map { case (plan, index) =>
        ParameterlessDelegatedForwardingMethodGeneratedOriginAdapter
          .lower(plan, s"<quasiquotes-generated:u048-parameterless-forwarder-$index>") match
          case Left(problem) =>
            report.error(problem.message)
            null
          case Right(value) => value
      }
      evidence.generatedSources = generatedResults.map(_.generatedSource)
      val generated = generatedResults.map(_.tree.asInstanceOf[untpd.DefDef])
      evidence.generatedExact = generated.map(isExactPositioned)

      val transformer = new untpd.UntypedTreeMap:
        override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
          tree match
            case template: untpd.Template
                if template.body.exists {
                  case definition: untpd.DefDef =>
                    definition.name.toString == "canonicalResult"
                  case _ => false
                } =>
              untpd.cpy.Template(template)(body = template.body ++ generated)
            case _ => super.transform(tree)
      summon[Context].compilationUnit.untpdTree =
        transformer.transform(summon[Context].compilationUnit.untpdTree)

  private def isExactSourceFree(tree: untpd.DefDef)(using Context): Boolean =
    val trees = ParameterlessDelegatedForwardingMethodUntypedLowerer.allTrees(tree)
    trees.size == 10 && trees.forall(current =>
      !current.source.exists && !current.span.exists && current.symbol == NoSymbol &&
        !current.isInstanceOf[untpd.TypedSplice]
    )

  private def isExactPositioned(tree: untpd.DefDef)(using Context): Boolean =
    val trees = ParameterlessDelegatedForwardingMethodUntypedLowerer.allTrees(tree)
    trees.size == 10 && trees.forall(current =>
      current.source.exists && current.span.exists && current.symbol == NoSymbol &&
        !current.isInstanceOf[untpd.TypedSplice]
    )

  private def createPlan(
      method: String,
      typeParameter: String,
      contextualParameter: String,
      contextualConstructor: String
  ): Plan =
    ParameterlessDelegatedForwardingPlan
      .create(method, typeParameter, contextualParameter, contextualConstructor)
      .fold(problem => fail(problem.message), identity)

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ParameterlessDelegatedForwardingPlan.type],
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
      try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally stream.close()
