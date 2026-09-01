package quasiquotes.definitions.dotty

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.util.{NoSource, SourceFile}

class ExistingUntpdMethodBodyRewriteApplyOriginAdapterTyperRuntimeTest
    extends munit.FunSuite:
  private enum Mode:
    case Valid, MissingFunction, MissingArgument

  test("uniform Apply origin survives Typer, emits TASTy, and executes") {
    withCompilation(Mode.Valid) { (driver, reporter, output, _) =>
      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assert(driver.beforeTyperContractValid)
      assert(emitted(output).exists(_.endsWith("U005ApplyOrigin.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U005ApplyOriginUse$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("value").invoke(module), Integer.valueOf(8))
      finally loader.close()
    }
  }

  test("missing function Ident is diagnosed at the replaced RHS site") {
    assertInvalid(Mode.MissingFunction, "missingU005Function")
  }

  test("missing argument Ident is diagnosed at the replaced RHS site") {
    assertInvalid(Mode.MissingArgument, "missingU005Argument")
  }

  private def assertInvalid(mode: Mode, expectedName: String): Unit =
    withCompilation(mode) { (driver, reporter, output, source) =>
      assert(reporter.hasErrors)
      assertEquals(reporter.allErrors.size, 1)
      val problem = reporter.allErrors.head
      assert(problem.message.contains(expectedName), clues(problem))
      assertEquals(problem.pos.source.path, source.toString)
      assertEquals(problem.pos.start, driver.originalRhsStart)
      assert(problem.pos.start > 0)
      assert(driver.beforeTyperContractValid)
      assert(!emitted(output).exists(_.endsWith("U005ApplyOrigin.tasty")))
    }

  private def withCompilation(
      mode: Mode
  )(
      check: (OriginDriver, dotty.tools.dotc.reporting.Reporter, Path, Path) => Unit
  ): Unit =
    val temporary = Files.createTempDirectory("u005-apply-origin-")
    try
      val source = temporary.resolve("U005ApplyOrigin.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """@deprecated("fixture", "1")
          |class U005ApplyOrigin:
          |  def keep: Int = 1
          |  def valueArg: Int = 3
          |  def combine(flag: Boolean, label: String, value: Int): Int =
          |    if flag then value + label.length else 0
          |  def change: Int = 2
          |  val opaque: String = "opaque"
          |  def call: Int = change
          |
          |object U005ApplyOriginUse:
          |  def value: Int = new U005ApplyOrigin().change
          |""".stripMargin,
        StandardCharsets.UTF_8
      )
      val driver = new OriginDriver(mode)
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

  private final class OriginDriver(mode: Mode) extends Driver:
    @volatile var beforeTyperContractValid: Boolean = false
    @volatile var originalRhsStart: Int = -1

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new AdaptBeforeTyper(mode, OriginDriver.this)) ::
            super.frontendPhases.tail

  private final class AdaptBeforeTyper(mode: Mode, evidence: OriginDriver)
      extends Phase:
    def phaseName: String = "u005ExistingMethodBodyApplyOriginAdaptation"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared =
        for
          originalRoot <- allTrees(unitTree).collectFirst {
            case value: untpd.TypeDef if value.name.toString == "U005ApplyOrigin" =>
              value
          }.toRight("U005 runtime fixture class was not found")
          originalTemplate <- originalRoot.rhs match
            case value: untpd.Template => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          originalTarget <- originalTemplate.body.collectFirst {
            case value: untpd.DefDef if value.name.toString == "change" => value
          }.toRight("U005 runtime fixture target was not found")
          replacement =
            given SourceFile = NoSource
            mode match
              case Mode.Valid =>
                untpd.Apply(
                  untpd.Ident(termName("combine")),
                  List(
                    untpd.Literal(Constant(true)),
                    untpd.Literal(Constant("value")),
                    untpd.Ident(termName("valueArg"))
                  )
                )
              case Mode.MissingFunction =>
                untpd.Apply(
                  untpd.Ident(termName("missingU005Function")),
                  untpd.Number("20", untpd.NumberKind.Whole(10)) :: Nil
                )
              case Mode.MissingArgument =>
                untpd.Apply(
                  untpd.Ident(termName("identity")),
                  untpd.Ident(termName("missingU005Argument")) :: Nil
                )
          originalState = allTrees(originalRoot).map(tree => (tree, tree.source, tree.span))
          structural <- ExistingUntpdMethodBodyRewriter
            .rewrite(originalRoot, originalTarget, replacement)
            .left
            .map(_.message)
          adapted <- ExistingUntpdMethodBodyRewriteOriginAdapter
            .adaptApply(structural)
            .left
            .map(_.message)
        yield (
          originalRoot,
          originalTemplate,
          originalTarget,
          originalState,
          structural,
          adapted
        )

      prepared match
        case Left(problem) => report.error(problem)
        case Right(
              (
                originalRoot,
                originalTemplate,
                originalTarget,
                originalState,
                structural,
                adapted
              )
            ) =>
          evidence.originalRhsStart = originalTarget.rhs.span.start
          val originalUntouched = originalTemplate.body.filterNot(_.eq(originalTarget))
          val positionedUntouched =
            adapted.positionedTemplate.body.filterNot(_.eq(adapted.positionedTarget))
          val structuralReplacementNodes = allTrees(structural.replacementBody)
          val positionedReplacementNodes = allTrees(adapted.positionedReplacement)
          evidence.beforeTyperContractValid =
            (Vector[untpd.Tree](
              structural.rebuiltRoot,
              structural.rebuiltTemplate,
              structural.rebuiltTarget
            ) ++ structuralReplacementNodes).forall(tree =>
              !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol &&
                !tree.isInstanceOf[untpd.TypedSplice]
            ) &&
              originalState.forall { case (tree, source, span) =>
                tree.source == source && tree.span == span
              } &&
              structuralReplacementNodes.size == positionedReplacementNodes.size &&
              structuralReplacementNodes.zip(positionedReplacementNodes).forall {
                case (left, right) => !left.eq(right)
              } &&
              positionedReplacementNodes.forall(tree =>
                tree.source == originalTarget.rhs.source &&
                  tree.span == originalTarget.rhs.span
              ) &&
              !adapted.positionedRoot.eq(originalRoot) &&
              !adapted.positionedRoot.eq(structural.rebuiltRoot) &&
              !adapted.positionedTemplate.eq(originalTemplate) &&
              !adapted.positionedTarget.eq(originalTarget) &&
              originalUntouched.size == positionedUntouched.size &&
              originalUntouched.zip(positionedUntouched).forall((left, right) =>
                left.eq(right)
              ) &&
              allTrees(adapted.positionedRoot).forall(tree =>
                tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
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
