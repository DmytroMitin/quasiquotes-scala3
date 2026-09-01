package quasiquotes.definitions.dotty

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.util.{NoSource, SourceFile}

class ExistingUntpdMethodBodyRewriteOriginAdapterTyperRuntimeTest
    extends munit.FunSuite:
  test("production original-site adapter survives Typer, emits TASTy, and executes") {
    withCompilation(invalid = false) { (driver, reporter, output, _) =>
      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assert(driver.beforeTyperContractValid)
      assert(emitted(output).exists(_.endsWith("U003OriginAdapter.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U003OriginAdapterUse$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("value").invoke(module), Integer.valueOf(20))
      finally loader.close()
    }
  }

  test("production adapter attributes an invalid replacement to the replaced RHS site") {
    withCompilation(invalid = true) { (driver, reporter, output, source) =>
      assert(reporter.hasErrors)
      assertEquals(reporter.allErrors.size, 1)
      val problem = reporter.allErrors.head
      assert(problem.message.contains("missingU003Replacement"), clues(problem))
      assertEquals(problem.pos.source.path, source.toString)
      assertEquals(problem.pos.start, driver.originalRhsStart)
      assert(problem.pos.start > 0)
      assert(driver.beforeTyperContractValid)
      assert(!emitted(output).exists(_.endsWith("U003OriginAdapter.tasty")))
    }
  }

  private def withCompilation(
      invalid: Boolean
  )(
      check: (OriginDriver, dotty.tools.dotc.reporting.Reporter, Path, Path) => Unit
  ): Unit =
    val temporary = Files.createTempDirectory("u003-origin-adapter-")
    try
      val source = temporary.resolve("U003OriginAdapter.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """@deprecated("fixture", "1")
          |class U003OriginAdapter:
          |  def keep: Int = 1
          |  def change: Int = 2
          |  val opaque: String = "opaque"
          |  def call: Int = change
          |
          |object U003OriginAdapterUse:
          |  def value: Int = new U003OriginAdapter().change
          |""".stripMargin,
        StandardCharsets.UTF_8
      )
      val driver = new OriginDriver(invalid)
      val reporter = driver.process(
        Array(
          "-classpath",
          compilationClasspath,
          "-d",
          output.toString,
          source.toString
        )
      )
      check(driver, reporter, output, source)
    finally deleteRecursively(temporary)

  private final class OriginDriver(invalid: Boolean) extends Driver:
    @volatile var beforeTyperContractValid: Boolean = false
    @volatile var originalRhsStart: Int = -1

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new AdaptBeforeTyper(invalid, OriginDriver.this)) ::
            super.frontendPhases.tail

  private final class AdaptBeforeTyper(
      invalid: Boolean,
      evidence: OriginDriver
  ) extends Phase:
    def phaseName: String = "u003ExistingMethodBodyOriginAdaptation"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared =
        for
          originalRoot <- allTrees(unitTree).collectFirst {
            case value: untpd.TypeDef
                if value.name.toString == "U003OriginAdapter" => value
          }.toRight("U003 runtime fixture class was not found")
          originalTemplate <- originalRoot.rhs match
            case value: untpd.Template => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          originalTarget <- originalTemplate.body.collectFirst {
            case value: untpd.DefDef if value.name.toString == "change" => value
          }.toRight("U003 runtime fixture target was not found")
          replacement =
            given SourceFile = NoSource
            if invalid then untpd.Ident(termName("missingU003Replacement"))
            else untpd.Number("20", untpd.NumberKind.Whole(10))
          structural <- ExistingUntpdMethodBodyRewriter
            .rewrite(originalRoot, originalTarget, replacement)
            .left
            .map(_.message)
          adapted <- ExistingUntpdMethodBodyRewriteOriginAdapter
            .adapt(structural)
            .left
            .map(_.message)
        yield (originalRoot, originalTemplate, originalTarget, structural, adapted)

      prepared match
        case Left(problem) => report.error(problem)
        case Right((originalRoot, originalTemplate, originalTarget, structural, adapted)) =>
          evidence.originalRhsStart = originalTarget.rhs.span.start
          val originalUntouched =
            originalTemplate.body.filterNot(_.eq(originalTarget))
          val positionedUntouched =
            adapted.positionedTemplate.body.filterNot(_.eq(adapted.positionedTarget))
          evidence.beforeTyperContractValid =
            Vector[untpd.Tree](
              structural.rebuiltRoot,
              structural.rebuiltTemplate,
              structural.rebuiltTarget,
              structural.replacementBody
            ).forall(tree =>
              !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol
            ) &&
              !adapted.positionedRoot.eq(originalRoot) &&
              !adapted.positionedRoot.eq(structural.rebuiltRoot) &&
              !adapted.positionedTemplate.eq(originalTemplate) &&
              !adapted.positionedTarget.eq(originalTarget) &&
              adapted.positionedReplacement.source == originalTarget.rhs.source &&
              adapted.positionedReplacement.span == originalTarget.rhs.span &&
              originalUntouched.size == positionedUntouched.size &&
              originalUntouched.zip(positionedUntouched).forall((left, right) =>
                left.eq(right)
              ) &&
              allTrees(adapted.positionedRoot).forall(tree =>
                tree.symbol == NoSymbol &&
                  !tree.isInstanceOf[untpd.TypedSplice]
              )

          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              if tree.eq(originalRoot) then adapted.positionedRoot
              else super.transform(tree)
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
    try
      stream
        .filter(Files.isRegularFile(_))
        .iterator()
        .asScala
        .map(_.toString)
        .toVector
    finally stream.close()

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ExistingUntpdMethodBodyRewriteOriginAdapter.type],
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
