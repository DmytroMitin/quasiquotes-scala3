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

import quasiquotes.definitions.ValueInstanceFactoryPlan
import quasiquotes.definitions.ValueInstanceFactoryPlan.Plan

class ValueInstanceFactoryGeneratedOriginTyperRuntimeTest extends munit.FunSuite:
  private val canonical = createPlan("instance", "A", "valueValue", "HasValue", "value")
  private val renamed = createPlan("make", "Element", "elementCarrier", "Container", "element")

  test("source-free and generated-origin factories survive pre-Typer insertion TASTy and runtime") {
    val temporary = Files.createTempDirectory("u046-value-instance-factory-")
    try
      val source = temporary.resolve("U046ValueInstanceFactoryRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """trait HasValue[A]:
          |  val value: A
          |
          |trait Container[Element]:
          |  val element: Element
          |
          |object U046ValueInstanceFactoryRuntime:
          |  def canonicalResult: Int = instance[Int](42).value
          |  def renamedResult: Int = make[Int](42).element
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new FactoryDriver(Vector(canonical, renamed))
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

      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith("U046ValueInstanceFactoryRuntime$.class")))
      assert(emitted.exists(_.toString.endsWith("U046ValueInstanceFactoryRuntime.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U046ValueInstanceFactoryRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("canonicalResult").invoke(module), Integer.valueOf(42))
        assertEquals(moduleClass.getMethod("renamedResult").invoke(module), Integer.valueOf(42))
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class FactoryDriver(plans: Vector[Plan]) extends Driver:
    @volatile var sourceFreeExact: Vector[Boolean] = Vector.empty
    @volatile var generatedExact: Vector[Boolean] = Vector.empty

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new InsertFactories(plans, FactoryDriver.this)) ::
            super.frontendPhases.tail

  private final class InsertFactories(
      plans: Vector[Plan],
      evidence: FactoryDriver
  ) extends Phase:
    def phaseName: String = "u046ValueInstanceFactoryInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val sourceFree = plans.map { plan =>
        ValueInstanceFactoryPlanUntypedLowerer.lower(plan) match
          case Left(problem) =>
            report.error(problem.message)
            untpd.EmptyTree.asInstanceOf[untpd.DefDef]
          case Right(value) => value
      }
      evidence.sourceFreeExact = sourceFree.map(isExactSourceFree)
      val generated = plans.zipWithIndex.map { case (plan, index) =>
        ValueInstanceFactoryGeneratedOriginAdapter
          .lower(plan, s"<quasiquotes-generated:u046-value-instance-factory-$index>") match
          case Left(problem) =>
            report.error(problem.message)
            untpd.EmptyTree.asInstanceOf[untpd.DefDef]
          case Right(value) => value.tree.asInstanceOf[untpd.DefDef]
      }
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
    val trees = ValueInstanceFactoryPlanUntypedLowerer.allTrees(tree)
    trees.size == 17 && trees.forall(current =>
      !current.source.exists && !current.span.exists && current.symbol == NoSymbol &&
        !current.isInstanceOf[untpd.TypedSplice]
    )

  private def isExactPositioned(tree: untpd.DefDef)(using Context): Boolean =
    val trees = ValueInstanceFactoryPlanUntypedLowerer.allTrees(tree)
    trees.size == 17 && trees.forall(current =>
      current.source.exists && current.span.exists && current.symbol == NoSymbol &&
        !current.isInstanceOf[untpd.TypedSplice]
    )

  private def createPlan(
      factory: String,
      typeParameter: String,
      carrier: String,
      target: String,
      member: String
  ): Plan =
    ValueInstanceFactoryPlan
      .create(factory, typeParameter, carrier, target, member)
      .fold(problem => fail(problem.message), identity)

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ValueInstanceFactoryPlan.type],
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
