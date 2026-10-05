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

import quasiquotes.definitions.TypeMemberInstanceFactoryPlan
import quasiquotes.definitions.TypeMemberInstanceFactoryPlan.Plan

class TypeMemberInstanceFactoryGeneratedOriginTyperRuntimeTest extends munit.FunSuite:
  private val canonical = createPlan("instance", "A", "Out0", "HasOut", "Out")
  private val renamed = createPlan("make", "Element", "Result0", "Container", "Result")

  test("generated factories preserve concrete Type equality through Typer TASTy and runtime") {
    val temporary = Files.createTempDirectory("u047-type-member-instance-factory-")
    try
      val source = temporary.resolve("U047TypeMemberInstanceFactoryRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """trait HasOut[A]:
          |  type Out
          |
          |trait Container[Element]:
          |  type Result
          |
          |object U047TypeMemberInstanceFactoryRuntime:
          |  val canonical: HasOut[Int] { type Out = String } = instance[Int, String]
          |  val renamed: Container[Int] { type Result = String } = make[Int, String]
          |  def canonicalExists: Boolean = canonical != null
          |  def renamedExists: Boolean = renamed != null
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
      assert(emitted.exists(_.toString.endsWith("U047TypeMemberInstanceFactoryRuntime$.class")))
      assert(emitted.exists(_.toString.endsWith("U047TypeMemberInstanceFactoryRuntime.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U047TypeMemberInstanceFactoryRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("canonicalExists").invoke(module), java.lang.Boolean.TRUE)
        assertEquals(moduleClass.getMethod("renamedExists").invoke(module), java.lang.Boolean.TRUE)
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
    def phaseName: String = "u047TypeMemberInstanceFactoryInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val sourceFree = plans.map { plan =>
        TypeMemberInstanceFactoryPlanUntypedLowerer.lower(plan) match
          case Left(problem) =>
            report.error(problem.message)
            untpd.EmptyTree.asInstanceOf[untpd.DefDef]
          case Right(value) => value
      }
      evidence.sourceFreeExact = sourceFree.map(isExactSourceFree)
      val generated = plans.zipWithIndex.map { case (plan, index) =>
        TypeMemberInstanceFactoryGeneratedOriginAdapter
          .lower(plan, s"<quasiquotes-generated:u047-type-member-instance-factory-$index>") match
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
                  case definition: untpd.ValDef =>
                    definition.name.toString == "canonical"
                  case _ => false
                } =>
              untpd.cpy.Template(template)(body = template.body ++ generated)
            case _ => super.transform(tree)
      summon[Context].compilationUnit.untpdTree =
        transformer.transform(summon[Context].compilationUnit.untpdTree)

  private def isExactSourceFree(tree: untpd.DefDef)(using Context): Boolean =
    val trees = TypeMemberInstanceFactoryPlanUntypedLowerer.allTrees(tree)
    trees.size == 19 && trees.forall(current =>
      !current.source.exists && !current.span.exists && current.symbol == NoSymbol &&
        !current.isInstanceOf[untpd.TypedSplice]
    )

  private def isExactPositioned(tree: untpd.DefDef)(using Context): Boolean =
    val trees = TypeMemberInstanceFactoryPlanUntypedLowerer.allTrees(tree)
    trees.size == 19 && trees.forall(current =>
      current.source.exists && current.span.exists && current.symbol == NoSymbol &&
        !current.isInstanceOf[untpd.TypedSplice]
    )

  private def createPlan(
      factory: String,
      firstTypeParameter: String,
      secondTypeParameter: String,
      target: String,
      member: String
  ): Plan =
    TypeMemberInstanceFactoryPlan
      .create(factory, firstTypeParameter, secondTypeParameter, target, member)
      .fold(problem => fail(problem.message), identity)

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[TypeMemberInstanceFactoryPlan.type],
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
