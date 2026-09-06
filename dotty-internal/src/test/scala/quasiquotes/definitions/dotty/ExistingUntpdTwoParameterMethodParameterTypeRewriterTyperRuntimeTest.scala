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

import quasiquotes.types.TypeNormalForm

class ExistingUntpdTwoParameterMethodParameterTypeRewriterTyperRuntimeTest
    extends munit.FunSuite:
  test("U036 no-rewrite baseline fails for both AnyVal parameter positions") {
    val temporary = Files.createTempDirectory("u036-parameter-type-baseline-")
    try
      val source = writeFixture(temporary)
      val output = temporary.resolve("classes")
      Files.createDirectories(output)

      val reporter = new Driver().process(
        Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      )

      assert(reporter.hasErrors)
      assert(reporter.allErrors.count(_.message.contains("+")) >= 2, clues(reporter.allErrors))
    finally deleteRecursively(temporary)
  }

  test("U036 rewrites both selectable positions before Typer emits TASTy and runs at 42") {
    val temporary = Files.createTempDirectory("u036-parameter-type-rewrite-")
    try
      val source = writeFixture(temporary)
      val output = temporary.resolve("classes")
      Files.createDirectories(output)

      val driver = new RewriteDriver
      val reporter = driver.process(
        Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      )

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(driver.validRewrites, Set(0, 1))
      val emittedFiles = emitted(output)
      assert(emittedFiles.exists(_.endsWith("U036FirstParameterRuntime.tasty")))
      assert(emittedFiles.exists(_.endsWith("U036SecondParameterRuntime.tasty")))
      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U036ParameterTypeRuntimeUse$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("first").invoke(module), Integer.valueOf(42))
        assertEquals(moduleClass.getMethod("second").invoke(module), Integer.valueOf(42))
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class RewriteDriver extends Driver:
    @volatile var validRewrites: Set[Int] = Set.empty

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(evidence: RewriteDriver) extends Phase:
    def phaseName: String = "u036TwoParameterMethodParameterTypeRewrite"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val targets = Vector(
        ("U036FirstParameterRuntime", 0),
        ("U036SecondParameterRuntime", 1)
      )
      val prepared = targets.foldLeft[
        Either[String, Vector[(untpd.TypeDef, untpd.TypeDef, Int)]]
      ](Right(Vector.empty)) { case (accumulated, (className, parameterIndex)) =>
        for
          completed <- accumulated
          originalRoot <- allTrees(unitTree).collectFirst {
            case value: untpd.TypeDef if value.name.toString == className => value
          }.toRight(s"U036 runtime fixture class $className was not found")
          originalTemplate <- originalRoot.rhs match
            case value: untpd.Template => Right(value)
            case other => Left(s"fixture rhs was ${other.getClass.getSimpleName}")
          captured <- ExistingUntpdClassMemberFilter.capture(originalRoot).left.map(_.message)
          view <- ExistingUntpdTwoParameterMethodView.capture(captured, 1).left.map(_.message)
          originalState = allTrees(originalRoot).map(tree => (tree, tree.source, tree.span))
          rewritten <- ExistingUntpdTwoParameterMethodParameterTypeRewriter
            .rewrite(view, parameterIndex, TypeNormalForm.STypeIdent("Int"))
            .left.map(_.message)
          _ <- Either.cond(
            contractValid(
              originalRoot,
              originalTemplate,
              view,
              originalState,
              parameterIndex,
              rewritten
            ),
            (),
            s"U036 before-Typer contract failed for parameter index $parameterIndex"
          )
        yield completed :+ (originalRoot, rewritten.positionedRoot, parameterIndex)
      }

      prepared match
        case Left(problem) => report.error(problem)
        case Right(replacements) =>
          evidence.validRewrites = replacements.map(_._3).toSet
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              replacements.collectFirst {
                case (original, rewritten, _) if tree.eq(original) => rewritten
              }.getOrElse(super.transform(tree))
          summon[Context].compilationUnit.untpdTree = transformer.transform(unitTree)

  private def contractValid(
      originalRoot: untpd.TypeDef,
      originalTemplate: untpd.Template,
      view: ExistingUntpdTwoParameterMethodView.View,
      originalState: Vector[(untpd.Tree, dotty.tools.dotc.util.SourceFile, dotty.tools.dotc.util.Spans.Span)],
      parameterIndex: Int,
      rewritten: ExistingUntpdTwoParameterMethodParameterTypeRewriter.Result
  )(using Context): Boolean =
    val parameters = rewritten.positionedMethod.paramss.head.map(_.asInstanceOf[untpd.ValDef])
    val selected = if parameterIndex == 0 then view.firstParameter else view.secondParameter
    val selectedType = if parameterIndex == 0 then view.firstParameterType else view.secondParameterType
    val untouched = if parameterIndex == 0 then view.secondParameter else view.firstParameter
    val untouchedType = if parameterIndex == 0 then view.secondParameterType else view.firstParameterType
    val originalUntouched = originalTemplate.body.filterNot(_.eq(view.method))
    val rewrittenUntouched =
      rewritten.positionedTemplate.body.filterNot(_.eq(rewritten.positionedMethod))
    !rewritten.loweredParameterType.source.exists &&
      !rewritten.loweredParameterType.span.exists &&
      rewritten.loweredParameterType.symbol == NoSymbol &&
      rewritten.positionedParameterType.source == selectedType.source &&
      rewritten.positionedParameterType.span == selectedType.span &&
      !rewritten.positionedParameterType.eq(selectedType) &&
      !rewritten.positionedParameter.eq(selected) &&
      rewritten.positionedParameter.tpt.eq(rewritten.positionedParameterType) &&
      rewritten.positionedParameter.rhs.isEmpty &&
      parameters(parameterIndex).eq(rewritten.positionedParameter) &&
      parameters(1 - parameterIndex).eq(untouched) &&
      parameters(1 - parameterIndex).tpt.eq(untouchedType) &&
      rewritten.positionedMethod.tpt.eq(view.resultType) &&
      rewritten.positionedMethod.rhs.eq(view.rhs) &&
      !rewritten.positionedMethod.eq(view.method) &&
      !rewritten.positionedTemplate.eq(originalTemplate) &&
      !rewritten.positionedRoot.eq(originalRoot) &&
      originalUntouched.zip(rewrittenUntouched).forall((left, right) => left.eq(right)) &&
      originalState.forall { case (tree, source, span) =>
        tree.source == source && tree.span == span
      } &&
      allTrees(rewritten.positionedRoot).forall(tree =>
        tree != null && tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
      )

  private def writeFixture(temporary: Path): Path =
    val source = temporary.resolve("U036ParameterTypeRewriteRuntime.scala")
    Files.writeString(
      source,
      """class U036FirstParameterRuntime:
        |  val before: Int = 1
        |  def sum(x: AnyVal, y: Int): Int = x + y
        |  type After = String
        |
        |class U036SecondParameterRuntime:
        |  val before: Int = 1
        |  def sum(x: Int, y: AnyVal): Int = x + y
        |  type After = String
        |
        |object U036ParameterTypeRuntimeUse:
        |  def first: Int = new U036FirstParameterRuntime().sum(20, 22)
        |  def second: Int = new U036SecondParameterRuntime().sum(20, 22)
        |""".stripMargin,
      StandardCharsets.UTF_8
    )
    source

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
      classOf[ExistingUntpdTwoParameterMethodParameterTypeRewriter.type],
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
