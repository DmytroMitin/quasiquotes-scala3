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

import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.Result
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class ExistingRepeatedParameterPublicBoundaryTyperRuntimeTest
    extends munit.FunSuite:
  test("body and result edits preserve vararg calls through Typer TASTy and runtime") {
    val temporary = Files.createTempDirectory("u057-existing-repeated-safe-")
    try
      val source = temporary.resolve("U057ExistingRepeatedSafe.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(source, SafeProgram, StandardCharsets.UTF_8)

      val baseline = new Driver().process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))
      assert(baseline.hasErrors)
      deleteRecursively(output)
      Files.createDirectories(output)

      val driver = new SafeRewriteDriver
      val reporter = driver.process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(driver.rewritten, Set("U057BodyOnly", "U057ResultOnly"))
      assert(driver.bodyRepeatedPreserved)
      assert(driver.resultRepeatedPreserved)
      assertEquals(driver.untouchedMembersPreserved, 2)
      assert(driver.preTyperClean)

      val files = emitted(output)
      assert(files.exists(_.endsWith("U057BodyOnly.tasty")))
      assert(files.exists(_.endsWith("U057ResultOnly.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        assertEquals(invoke(loader, "U057SafeUse", "bodyOnly"), 7)
        assertEquals(invoke(loader, "U057SafeUse", "resultOnly"), 6)
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  test("parameter-Type replacement preserves unary and repeated-last vararg calls through Typer TASTy and runtime") {
    val temporary = Files.createTempDirectory("u059-existing-repeated-safe-")
    try
      val source = temporary.resolve("U059ExistingRepeatedSafe.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(source, RepeatedProgram, StandardCharsets.UTF_8)

      val baseline = new Driver().process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))
      assert(!baseline.hasErrors, clues(baseline.allErrors))
      val baselineFiles = emitted(output)
      assert(baselineFiles.exists(_.endsWith("U057ParameterType.tasty")))
      assert(baselineFiles.exists(_.endsWith("U057TwoParameter.tasty")))
      deleteRecursively(output)
      Files.createDirectories(output)

      val driver = new ParameterRewriteDriver
      val reporter = driver.process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(
        driver.rewritten,
        Set("U057ParameterType", "U057TwoParameter")
      )
      assert(driver.unaryRepeatedPreserved)
      assert(driver.twoParameterRepeatedPreserved)
      assert(driver.strictNeighborPreserved)
      assertEquals(driver.untouchedMembersPreserved, 2)
      assert(driver.preTyperClean)

      val files = emitted(output)
      assert(files.exists(_.endsWith("U057ParameterType.tasty")))
      assert(files.exists(_.endsWith("U057TwoParameter.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        assertEquals(invoke(loader, "U059RepeatedUse", "unary"), 3)
        assertEquals(invoke(loader, "U059RepeatedUse", "twoParameter"), 4)
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class SafeRewriteDriver extends Driver:
    @volatile var rewritten = Set.empty[String]
    @volatile var bodyRepeatedPreserved = false
    @volatile var resultRepeatedPreserved = false
    @volatile var untouchedMembersPreserved = 0
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new SafeRewriteBeforeTyper(SafeRewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class SafeRewriteBeforeTyper(evidence: SafeRewriteDriver)
      extends Phase:
    def phaseName: String = "u057ExistingRepeatedSafeBoundary"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString == "U057BodyOnly" =>
              rewriteBody(root) match
                case Left(problem) => report.error(problem); root
                case Right(proof) =>
                  evidence.rewritten += root.name.toString
                  evidence.bodyRepeatedPreserved = proof.repeatedPreserved
                  if proof.untouchedPreserved then evidence.untouchedMembersPreserved += 1
                  proof.result.tree
            case root: untpd.TypeDef if root.name.toString == "U057ResultOnly" =>
              rewriteResult(root) match
                case Left(problem) => report.error(problem); root
                case Right(proof) =>
                  evidence.rewritten += root.name.toString
                  evidence.resultRepeatedPreserved = proof.repeatedPreserved
                  if proof.untouchedPreserved then evidence.untouchedMembersPreserved += 1
                  proof.result.tree
            case other => other
          }
          summon[Context].compilationUnit.untpdTree =
            untpd.cpy.PackageDef(packageDef)(packageDef.pid, rewritten)
          evidence.preTyperClean = rewritten.forall(tree =>
            allTrees(tree).forall(node =>
              node.symbol == NoSymbol && !node.isInstanceOf[untpd.TypedSplice]
            )
          )
        case other =>
          report.error(
            "expected PackageDef before Typer, found " + other.getClass.getSimpleName
          )

  private final class ParameterRewriteDriver extends Driver:
    @volatile var rewritten = Set.empty[String]
    @volatile var unaryRepeatedPreserved = false
    @volatile var twoParameterRepeatedPreserved = false
    @volatile var strictNeighborPreserved = false
    @volatile var untouchedMembersPreserved = 0
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new ParameterRewriteBeforeTyper(ParameterRewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class ParameterRewriteBeforeTyper(evidence: ParameterRewriteDriver)
      extends Phase:
    def phaseName: String = "u059ExistingRepeatedSafeBoundary"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString == "U057ParameterType" =>
              rewriteParameter(root, parameterIndex = 0) match
                case Left(problem) => report.error(problem); root
                case Right(proof) =>
                  evidence.rewritten += root.name.toString
                  evidence.unaryRepeatedPreserved = proof.repeatedPreserved
                  if proof.untouchedPreserved then evidence.untouchedMembersPreserved += 1
                  proof.result.tree
            case root: untpd.TypeDef if root.name.toString == "U057TwoParameter" =>
              rewriteParameter(root, parameterIndex = 1) match
                case Left(problem) => report.error(problem); root
                case Right(proof) =>
                  evidence.rewritten += root.name.toString
                  evidence.twoParameterRepeatedPreserved = proof.repeatedPreserved
                  evidence.strictNeighborPreserved = proof.strictNeighborPreserved
                  if proof.untouchedPreserved then evidence.untouchedMembersPreserved += 1
                  proof.result.tree
            case other => other
          }
          summon[Context].compilationUnit.untpdTree =
            untpd.cpy.PackageDef(packageDef)(packageDef.pid, rewritten)
          evidence.preTyperClean = rewritten.forall(tree =>
            allTrees(tree).forall(node =>
              node.symbol == NoSymbol && !node.isInstanceOf[untpd.TypedSplice]
            )
          )
        case other =>
          report.error(
            "expected PackageDef before Typer, found " + other.getClass.getSimpleName
          )

  private final case class SafeProof(
      result: Result,
      repeatedPreserved: Boolean,
      untouchedPreserved: Boolean
  )

  private final case class ParameterProof(
      result: Result,
      repeatedPreserved: Boolean,
      strictNeighborPreserved: Boolean,
      untouchedPreserved: Boolean
  )

  private def rewriteBody(root: untpd.TypeDef)(using Context): Either[String, SafeProof] =
    val originalMethod = firstMethod(root)
    val originalParameter = originalMethod.paramss.head.head.asInstanceOf[untpd.ValDef]
    val originalRepeated = originalParameter.tpt
    val originalUntouched = template(root).body(1)
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      method <- captured.method(captured.members.head.ref).left.map(_.message)
      plan <- captured.emptyPlan
        .replaceBody(method.ref, TermShape.Literal("7"))
        .left.map(_.message)
      result <- ExistingClassUntypedRewrite(captured, plan).left.map(_.message)
      rewritten = firstMethod(result.tree)
      rewrittenParameter = rewritten.paramss.head.head.asInstanceOf[untpd.ValDef]
    yield SafeProof(
      result,
      rewrittenParameter.eq(originalParameter) &&
        rewrittenParameter.tpt.eq(originalRepeated) &&
        rewrittenParameter.tpt.isInstanceOf[untpd.PostfixOp],
      template(result.tree).body(1).eq(originalUntouched)
    )

  private def rewriteResult(root: untpd.TypeDef)(using Context): Either[String, SafeProof] =
    val originalMethod = firstMethod(root)
    val originalParameter = originalMethod.paramss.head.head.asInstanceOf[untpd.ValDef]
    val originalRepeated = originalParameter.tpt
    val originalUntouched = template(root).body(1)
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      method <- captured.method(captured.members.head.ref).left.map(_.message)
      plan <- captured.emptyPlan.replaceResultType(method.ref, IntType).left.map(_.message)
      result <- ExistingClassUntypedRewrite(captured, plan).left.map(_.message)
      rewritten = firstMethod(result.tree)
      rewrittenParameter = rewritten.paramss.head.head.asInstanceOf[untpd.ValDef]
    yield SafeProof(
      result,
      rewrittenParameter.eq(originalParameter) &&
        rewrittenParameter.tpt.eq(originalRepeated) &&
        rewrittenParameter.tpt.isInstanceOf[untpd.PostfixOp],
      template(result.tree).body(1).eq(originalUntouched)
    )

  private def rewriteParameter(
      root: untpd.TypeDef,
      parameterIndex: Int
  )(using Context): Either[String, ParameterProof] =
    val originalMethod = firstMethod(root)
    val originalParameters = originalMethod.paramss.head
    val originalParameter =
      originalParameters(parameterIndex).asInstanceOf[untpd.ValDef]
    val originalRepeated = originalParameter.tpt.asInstanceOf[untpd.PostfixOp]
    val originalMarker = originalRepeated match
      case untpd.PostfixOp(_, marker) => marker
    val originalStrictNeighbor =
      if parameterIndex == 0 then None
      else Some(originalParameters.head.asInstanceOf[untpd.ValDef])
    val originalUntouched = template(root).body(1)
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      method <- captured.method(captured.members.head.ref).left.map(_.message)
      parameter = method.parameterClauses.head.parameters(parameterIndex)
      plan <- captured.emptyPlan.replaceParameterType(parameter.ref, IntType)
        .left.map(_.message)
      result <- ExistingClassUntypedRewrite(captured, plan).left.map(_.message)
      rewritten = firstMethod(result.tree)
      rewrittenParameter =
        rewritten.paramss.head(parameterIndex).asInstanceOf[untpd.ValDef]
      repeatedPreserved = rewrittenParameter.tpt match
        case wrapper: untpd.PostfixOp =>
          val (element, marker) = wrapper match
            case untpd.PostfixOp(currentElement, currentMarker) =>
              (currentElement, currentMarker)
          element match
            case ident: untpd.Ident =>
              ident.name.toString == "Int" &&
                !wrapper.eq(originalRepeated) &&
                marker.eq(originalMarker) &&
                !rewrittenParameter.eq(originalParameter)
            case _ => false
        case _ => false
      strictNeighborPreserved = originalStrictNeighbor.forall(neighbor =>
        rewritten.paramss.head.head.eq(neighbor)
      )
    yield ParameterProof(
      result,
      repeatedPreserved,
      strictNeighborPreserved,
      template(result.tree).body(1).eq(originalUntouched)
    )

  private def template(root: untpd.TypeDef): untpd.Template =
    root.rhs.asInstanceOf[untpd.Template]

  private def firstMethod(root: untpd.TypeDef)(using Context): untpd.DefDef =
    template(root).body.head.asInstanceOf[untpd.DefDef]

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val builder = Vector.newBuilder[untpd.Tree]
    val traverser = new untpd.UntypedTreeTraverser:
      override def traverse(current: untpd.Tree)(using Context): Unit =
        builder += current
        traverseChildren(current)
    traverser.traverse(tree)
    builder.result()

  private def emitted(output: Path): Vector[String] =
    val stream = Files.walk(output)
    try stream.filter(Files.isRegularFile(_)).iterator().asScala.map(_.toString).toVector
    finally stream.close()

  private def invoke(loader: ClassLoader, moduleName: String, methodName: String): Int =
    val moduleClass = loader.loadClass(moduleName + "$")
    val module = moduleClass.getField("MODULE$").get(null)
    moduleClass.getMethod(methodName).invoke(module).asInstanceOf[Integer].intValue

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ExistingClassUntypedRewrite.type],
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

  private val IntType = TypeNormalForm.STypeIdent("Int")

  private val SafeProgram =
    """class U057BodyOnly:
      |  def sum(xs: Int*): Int = 0
      |  val untouched: Int = 7
      |
      |class U057ResultOnly:
      |  def sum(xs: Int*): AnyVal = xs.sum
      |  val untouched: Int = 9
      |
      |object U057SafeUse:
      |  def bodyOnly: Int = new U057BodyOnly().sum(1, 2, 3)
      |  def resultOnly: Int = new U057ResultOnly().sum(1, 2, 3)
      |""".stripMargin

  private val RepeatedProgram =
    """class U057ParameterType:
      |  def sum(xs: AnyVal*): Int = xs.size
      |  val untouched: Int = 11
      |
      |class U057TwoParameter:
      |  def combine(prefix: Int, xs: AnyVal*): Int = prefix + xs.size
      |  val untouched: Int = 13
      |
      |object U059RepeatedUse:
      |  def unary: Int = new U057ParameterType().sum(1, 2, 3)
      |  def twoParameter: Int = new U057TwoParameter().combine(1, 2, 3, 4)
      |""".stripMargin
