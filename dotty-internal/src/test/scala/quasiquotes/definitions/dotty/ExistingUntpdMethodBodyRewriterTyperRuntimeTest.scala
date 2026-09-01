package quasiquotes.definitions.dotty

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

class ExistingUntpdMethodBodyRewriterTyperRuntimeTest extends munit.FunSuite:
  test("classifies source-free reconstructed containers as blocked at the pre-Typer boundary") {
    val temporary = Files.createTempDirectory("u002-existing-rewrite-")
    try
      val source = temporary.resolve("U002ExistingRewriteRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """@deprecated("fixture", "1")
          |class U002ExistingRewriteRuntime:
          |  def keep: Int = 1
          |  def change: Int = 2
          |  val opaque: String = "opaque"
          |  def call: Int = change
          |
          |object U002ExistingRewriteRuntimeUse:
          |  def value: Int = new U002ExistingRewriteRuntime().change
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new RewriteDriver
      val failure = intercept[AssertionError] {
        driver.process(
          Array(
            "-classpath",
            compilationClasspath,
            "-d",
            output.toString,
            source.toString
          )
        )
      }

      assert(failure.getMessage.contains("position not set"), clues(failure))
      assert(
        failure.getMessage.contains("U002ExistingRewriteRuntime"),
        clues(failure)
      )
      assertEquals(driver.beforeTyperContractValid, Some(true))
      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(
        !emitted.exists(_.toString.endsWith("U002ExistingRewriteRuntime.tasty"))
      )
    finally deleteRecursively(temporary)
  }

  private final class RewriteDriver extends Driver:
    @volatile var beforeTyperContractValid: Option[Boolean] = None

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new RewriteBeforeTyper(RewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(evidence: RewriteDriver) extends Phase:
    def phaseName: String = "u002ExistingMethodBodyRewrite"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared =
        for
          originalRoot <- collectTrees(unitTree).collectFirst {
            case value: untpd.TypeDef
                if value.name.toString == "U002ExistingRewriteRuntime" => value
          }.toRight("U002 runtime fixture class was not found before Typer")
          originalTemplate <- originalRoot.rhs match
            case value: untpd.Template => Right(value)
            case other =>
              Left(
                s"U002 runtime fixture rhs was ${other.getClass.getSimpleName}, not Template"
              )
          originalTarget <- originalTemplate.body.collectFirst {
            case value: untpd.DefDef if value.name.toString == "change" => value
          }.toRight("U002 runtime fixture target was not found")
          replacement =
            given SourceFile = NoSource
            untpd.Number("20", untpd.NumberKind.Whole(10))
          result <- ExistingUntpdMethodBodyRewriter
            .rewrite(originalRoot, originalTarget, replacement)
            .left
            .map(_.message)
        yield (originalRoot, originalTemplate, originalTarget, replacement, result)
      prepared match
        case Left(problem) => report.error(problem)
        case Right(
              (originalRoot, originalTemplate, originalTarget, replacement, result)
            ) =>
          val originalUntouched =
            originalTemplate.body.filterNot(_.eq(originalTarget))
          val rebuiltUntouched =
            result.rebuiltTemplate.body.filterNot(_.eq(result.rebuiltTarget))
          evidence.beforeTyperContractValid = Some(
            !result.rebuiltRoot.eq(originalRoot) &&
              !result.rebuiltTemplate.eq(originalTemplate) &&
              !result.rebuiltTarget.eq(originalTarget) &&
              result.rebuiltRoot.mods.eq(originalRoot.mods) &&
              result.rebuiltTemplate.constr.eq(originalTemplate.constr) &&
              result.rebuiltTemplate.parentsOrDerived.eq(
                originalTemplate.parentsOrDerived
              ) &&
              result.rebuiltTemplate.derived.eq(originalTemplate.derived) &&
              result.rebuiltTemplate.self.eq(originalTemplate.self) &&
              result.rebuiltTarget.mods.eq(originalTarget.mods) &&
              result.rebuiltTarget.tpt.eq(originalTarget.tpt) &&
              result.rebuiltTarget.rhs.eq(replacement) &&
              originalUntouched.size == rebuiltUntouched.size &&
              originalUntouched.zip(rebuiltUntouched).forall((left, right) =>
                left.eq(right)
              ) &&
              Vector[untpd.Tree](
                result.rebuiltRoot,
                result.rebuiltTemplate,
                result.rebuiltTarget,
                replacement
              ).forall(tree =>
                !tree.source.exists &&
                  !tree.span.exists &&
                  tree.symbol == NoSymbol &&
                  !tree.isInstanceOf[untpd.TypedSplice]
              )
          )
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              if tree.eq(originalRoot) then result.rebuiltRoot
              else super.transform(tree)
          summon[Context].compilationUnit.untpdTree = transformer.transform(unitTree)

  private def collectTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val collector = Vector.newBuilder[untpd.Tree]
    val traverser = new untpd.UntypedTreeTraverser:
      override def traverse(current: untpd.Tree)(using Context): Unit =
        collector += current
        traverseChildren(current)
    traverser.traverse(tree)
    collector.result()

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ExistingUntpdMethodBodyRewriter.type],
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
