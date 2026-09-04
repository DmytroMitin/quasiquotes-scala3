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
import dotty.tools.dotc.util.{NoSource, SourceFile}

class ExistingUntpdClassMemberFilterOriginStrategyTest extends munit.FunSuite:
  test("raw fresh shells at the original class and Template sites are insertion-ready") {
    withCompilation(transform = true) { (driver, reporter, output) =>
      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assert(driver.contractValid)
      assert(emitted(output).exists(_.endsWith("VersionAdapter.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U023OriginUse$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("result").invoke(module), Integer.valueOf(42))
      finally loader.close()
    }
  }

  test("the same source fails before Typer when the incompatible member is retained") {
    withCompilation(transform = false) { (_, reporter, output) =>
      assert(reporter.hasErrors)
      assert(reporter.allErrors.exists(_.message.contains("apiThatDoesNotExistHere")))
      assert(!emitted(output).exists(_.endsWith("VersionAdapter.tasty")))
    }
  }

  private def withCompilation(
      transform: Boolean
  )(
      check: (OriginDriver, dotty.tools.dotc.reporting.Reporter, Path) => Unit
  ): Unit =
    val temporary = Files.createTempDirectory("u023-origin-strategy-")
    try
      val source = temporary.resolve("U023Origin.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """final class VersionAdapter:
          |  def kept(): Int = 42
          |  def removedForThisLine(): Int = apiThatDoesNotExistHere()
          |
          |object U023OriginUse:
          |  def result: Int = new VersionAdapter().kept()
          |""".stripMargin,
        StandardCharsets.UTF_8
      )
      val driver = new OriginDriver(transform)
      val reporter = driver.process(
        Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      )
      check(driver, reporter, output)
    finally deleteRecursively(temporary)

  private final class OriginDriver(transformEnabled: Boolean) extends Driver:
    @volatile var contractValid: Boolean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          if transformEnabled then
            List(new Parser) :: List(new FilterBeforeTyper(OriginDriver.this)) :: super.frontendPhases.tail
          else super.frontendPhases

  private final class FilterBeforeTyper(evidence: OriginDriver) extends Phase:
    def phaseName: String = "u023RawOriginStrategy"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared = for
        originalRoot <- allTrees(unitTree).collectFirst {
          case value: untpd.TypeDef if value.name.toString == "VersionAdapter" => value
        }.toRight("VersionAdapter was not found")
        originalTemplate <- originalRoot.rhs match
          case value: untpd.Template => Right(value)
          case other => Left(s"VersionAdapter rhs was ${other.getClass.getSimpleName}")
        removed <- originalTemplate.body.collectFirst {
          case value: untpd.DefDef if value.name.toString == "removedForThisLine" => value
        }.toRight("removed member was not found")
      yield
        val retained = originalTemplate.body.filterNot(_.eq(removed))
        given SourceFile = NoSource
        val rebuiltTemplate = untpd.Template(
          originalTemplate.constr,
          originalTemplate.parentsOrDerived,
          originalTemplate.derived,
          originalTemplate.self,
          retained
        )
        val rebuiltRoot = untpd.TypeDef(originalRoot.name, rebuiltTemplate).withMods(originalRoot.mods)
        val positionedTemplate = untpd.cpy.Template(rebuiltTemplate)(
          rebuiltTemplate.constr,
          rebuiltTemplate.parentsOrDerived,
          rebuiltTemplate.derived,
          rebuiltTemplate.self,
          retained
        ).cloneIn(originalTemplate.source).withSpan(originalTemplate.span)
        val positionedRoot = untpd.cpy.TypeDef(rebuiltRoot)(
          rebuiltRoot.name,
          positionedTemplate
        ).cloneIn(originalRoot.source).withSpan(originalRoot.span)
        (originalRoot, originalTemplate, removed, retained, rebuiltRoot, rebuiltTemplate, positionedRoot, positionedTemplate)

      prepared match
        case Left(problem) => report.error(problem)
        case Right((originalRoot, originalTemplate, removed, retained, rebuiltRoot, rebuiltTemplate, positionedRoot, positionedTemplate)) =>
          evidence.contractValid =
            !rebuiltRoot.source.exists && !rebuiltRoot.span.exists &&
              !rebuiltTemplate.source.exists && !rebuiltTemplate.span.exists &&
              !positionedRoot.eq(originalRoot) && !positionedRoot.eq(rebuiltRoot) &&
              !positionedTemplate.eq(originalTemplate) && !positionedTemplate.eq(rebuiltTemplate) &&
              positionedRoot.source == originalRoot.source && positionedRoot.span == originalRoot.span &&
              positionedTemplate.source == originalTemplate.source && positionedTemplate.span == originalTemplate.span &&
              retained.size == 1 && retained.head.eq(originalTemplate.body.head) &&
              !positionedTemplate.body.exists(_.eq(removed)) &&
              positionedTemplate.body.size == retained.size &&
              positionedTemplate.body.zip(retained).forall((actual, expected) => actual.eq(expected)) &&
              positionedRoot.mods.eq(originalRoot.mods) &&
              positionedTemplate.constr.eq(originalTemplate.constr) &&
              positionedTemplate.parentsOrDerived.eq(originalTemplate.parentsOrDerived) &&
              positionedTemplate.derived.eq(originalTemplate.derived) &&
              positionedTemplate.self.eq(originalTemplate.self) &&
              allTrees(positionedRoot).forall(tree => tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice])

          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              if tree.eq(originalRoot) then positionedRoot else super.transform(tree)
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
