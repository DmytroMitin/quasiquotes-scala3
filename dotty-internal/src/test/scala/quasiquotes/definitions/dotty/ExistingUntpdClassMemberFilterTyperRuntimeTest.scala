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

class ExistingUntpdClassMemberFilterTyperRuntimeTest extends munit.FunSuite:
  test("production member filter removes an untypable method before Typer and executes retained behavior") {
    val temporary = Files.createTempDirectory("u023-production-filter-")
    try
      val source = temporary.resolve("U023ProductionFilter.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """final class VersionAdapter:
          |  def kept(): Int = 42
          |  def removedForThisLine(): Int = apiThatDoesNotExistHere()
          |
          |object U023ProductionFilterUse:
          |  def result: Int = new VersionAdapter().kept()
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new FilterDriver
      val reporter = driver.process(
        Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      )

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assert(driver.contractValid)
      assert(emitted(output).exists(_.endsWith("VersionAdapter.tasty")))
      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U023ProductionFilterUse$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("result").invoke(module), Integer.valueOf(42))
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class FilterDriver extends Driver:
    @volatile var contractValid: Boolean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new FilterBeforeTyper(FilterDriver.this)) :: super.frontendPhases.tail

  private final class FilterBeforeTyper(evidence: FilterDriver) extends Phase:
    def phaseName: String = "u023ProductionClassMemberFilter"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val filtered = for
        originalRoot <- allTrees(unitTree).collectFirst {
          case value: untpd.TypeDef if value.name.toString == "VersionAdapter" => value
        }.toRight("VersionAdapter was not found")
        captured <- ExistingUntpdClassMemberFilter.capture(originalRoot).left.map(_.message)
        result <- ExistingUntpdClassMemberFilter.retain(captured, Vector(0)).left.map(_.message)
      yield (originalRoot, captured, result)

      filtered match
        case Left(problem) => report.error(problem)
        case Right((originalRoot, captured, result)) =>
          evidence.contractValid =
            captured.members.size == 2 &&
              result.rebuiltTemplate.body.size == 1 &&
              result.rebuiltTemplate.body.head.eq(captured.members.head.tree) &&
              !result.rebuiltTemplate.body.exists(_.eq(captured.members(1).tree)) &&
              !result.rebuiltTemplate.eq(captured.originalTemplate) &&
              !result.rebuiltRoot.eq(originalRoot)
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              if tree.eq(originalRoot) then result.rebuiltRoot else super.transform(tree)
          summon[Context].compilationUnit.untpdTree = transformer.transform(unitTree)

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val collector = Vector.newBuilder[untpd.Tree]
    val traverser = new untpd.UntypedTreeTraverser:
      override def traverse(current: untpd.Tree)(using Context): Unit =
        collector += current
        traverseChildren(current)
    traverser.traverse(tree)
    collector.result()

  private def emitted(output: Path): Vector[String] =
    val stream = Files.walk(output)
    try stream.filter(Files.isRegularFile(_)).iterator().asScala.map(_.toString).toVector
    finally stream.close()

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ExistingUntpdClassMemberFilter.type],
      getClass
    ).flatMap(value =>
      Option(value.getProtectionDomain)
        .flatMap(domain => Option(domain.getCodeSource))
        .map(_.getLocation.toURI)
    ).map(Path.of(_).toString).distinct.mkString(java.io.File.pathSeparator)

  private def deleteRecursively(root: Path): Unit =
    if Files.exists(root) then
      val stream = Files.walk(root)
      try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally stream.close()
