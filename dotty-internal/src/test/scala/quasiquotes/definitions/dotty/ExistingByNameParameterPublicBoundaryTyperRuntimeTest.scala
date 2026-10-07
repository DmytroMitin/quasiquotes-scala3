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

import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.{Failure, Result}
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class ExistingByNameParameterPublicBoundaryTyperRuntimeTest
    extends munit.FunSuite:
  test("public edits expose preserved and lost by-name evaluation through Typer TASTy and runtime") {
    val temporary = Files.createTempDirectory("u056-existing-by-name-boundary-")
    try
      val source = temporary.resolve("U056ExistingByNameRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(source, Program, StandardCharsets.UTF_8)

      val baseline = new Driver().process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))
      assert(baseline.hasErrors)
      deleteRecursively(output)
      Files.createDirectories(output)

      val driver = new RewriteDriver
      val reporter = driver.process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(
        driver.rewritten,
        Set(
          "U056BodyOnly",
          "U056ResultOnly",
          "U056ParameterType",
          "U056TwoParameter"
        )
      )
      assert(driver.bodyWrapperPreserved)
      assert(driver.resultWrapperPreserved)
      assert(driver.unaryParameterBecameStrict)
      assert(driver.twoParameterBecameStrict)
      assertEquals(driver.untouchedMembersPreserved, 4)
      assert(driver.preTyperClean)

      val files = emitted(output)
      assert(files.exists(_.endsWith("U056BodyOnly.tasty")))
      assert(files.exists(_.endsWith("U056ResultOnly.tasty")))
      assert(files.exists(_.endsWith("U056ParameterType.tasty")))
      assert(files.exists(_.endsWith("U056TwoParameter.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        assertEquals(invoke(loader, "bodyOnly"), 32)
        assertEquals(invoke(loader, "resultOnly"), 32)
        assertEquals(invoke(loader, "parameterType"), 1)
        assertEquals(invoke(loader, "twoParameter"), 41)
        assertEquals(invoke(loader, "strictControl"), 21)
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class RewriteDriver extends Driver:
    @volatile var rewritten = Set.empty[String]
    @volatile var bodyWrapperPreserved = false
    @volatile var resultWrapperPreserved = false
    @volatile var unaryParameterBecameStrict = false
    @volatile var twoParameterBecameStrict = false
    @volatile var untouchedMembersPreserved = 0
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(evidence: RewriteDriver) extends Phase:
    def phaseName: String = "u056ExistingByNameBoundary"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString == "U056BodyOnly" =>
              rewriteBody(root) match
                case Left(problem) => report.error(problem); root
                case Right(proof) =>
                  evidence.rewritten += root.name.toString
                  evidence.bodyWrapperPreserved = proof.wrapperPreserved
                  if proof.untouchedPreserved then evidence.untouchedMembersPreserved += 1
                  proof.result.tree
            case root: untpd.TypeDef if root.name.toString == "U056ResultOnly" =>
              rewriteResult(root) match
                case Left(problem) => report.error(problem); root
                case Right(proof) =>
                  evidence.rewritten += root.name.toString
                  evidence.resultWrapperPreserved = proof.wrapperPreserved
                  if proof.untouchedPreserved then evidence.untouchedMembersPreserved += 1
                  proof.result.tree
            case root: untpd.TypeDef if root.name.toString == "U056ParameterType" =>
              rewriteParameter(root, firstParameterIndex = 0) match
                case Left(problem) => report.error(problem); root
                case Right(proof) =>
                  evidence.rewritten += root.name.toString
                  evidence.unaryParameterBecameStrict = proof.parameterBecameStrict
                  if proof.untouchedPreserved then evidence.untouchedMembersPreserved += 1
                  proof.result.tree
            case root: untpd.TypeDef if root.name.toString == "U056TwoParameter" =>
              rewriteParameter(root, firstParameterIndex = 0) match
                case Left(problem) => report.error(problem); root
                case Right(proof) =>
                  evidence.rewritten += root.name.toString
                  evidence.twoParameterBecameStrict = proof.parameterBecameStrict
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
      wrapperPreserved: Boolean,
      untouchedPreserved: Boolean
  )

  private final case class UnsafeProof(
      result: Result,
      parameterBecameStrict: Boolean,
      untouchedPreserved: Boolean
  )

  private def rewriteBody(root: untpd.TypeDef)(using Context): Either[String, SafeProof] =
    val originalMethod = firstMethod(root)
    val originalParameter = originalMethod.paramss.head.head.asInstanceOf[untpd.ValDef]
    val originalWrapper = originalParameter.tpt
    val originalUntouched = template(root).body(1)
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      method <- captured.method(captured.members.head.ref).left.map(_.message)
      parameter = method.parameterClauses.head.parameters.head
      reference <- method.parameterScope.reference(parameter.ref).left.map(_.message)
      body = TermShape.Apply(
        TermShape.Select(TermShape.Identifier("Math", false), "addExact"),
        List(reference, reference)
      )
      plan <- captured.emptyPlan.replaceBody(method.ref, body)
        .left.map(_.message)
      result <- ExistingClassUntypedRewrite(captured, plan).left.map(_.message)
      rewritten = firstMethod(result.tree)
      rewrittenParameter = rewritten.paramss.head.head.asInstanceOf[untpd.ValDef]
    yield SafeProof(
      result,
      rewrittenParameter.eq(originalParameter) && rewrittenParameter.tpt.eq(originalWrapper) &&
        rewrittenParameter.tpt.isInstanceOf[untpd.ByNameTypeTree],
      template(result.tree).body(1).eq(originalUntouched)
    )

  private def rewriteResult(root: untpd.TypeDef)(using Context): Either[String, SafeProof] =
    val originalMethod = firstMethod(root)
    val originalParameter = originalMethod.paramss.head.head.asInstanceOf[untpd.ValDef]
    val originalWrapper = originalParameter.tpt
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
      rewrittenParameter.eq(originalParameter) && rewrittenParameter.tpt.eq(originalWrapper) &&
        rewrittenParameter.tpt.isInstanceOf[untpd.ByNameTypeTree],
      template(result.tree).body(1).eq(originalUntouched)
    )

  private def rewriteParameter(
      root: untpd.TypeDef,
      firstParameterIndex: Int
  )(using Context): Either[String, UnsafeProof] =
    val originalMethod = firstMethod(root)
    val originalParameter =
      originalMethod.paramss.head(firstParameterIndex).asInstanceOf[untpd.ValDef]
    val originalUntouched = template(root).body(1)
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      method <- captured.method(captured.members.head.ref).left.map(_.message)
      parameter = method.parameterClauses.head.parameters(firstParameterIndex)
      plan <- captured.emptyPlan.replaceParameterType(parameter.ref, IntType).left.map(_.message)
      result <- ExistingClassUntypedRewrite(captured, plan).left.map(_.message)
      rewritten = firstMethod(result.tree)
      rewrittenParameter =
        rewritten.paramss.head(firstParameterIndex).asInstanceOf[untpd.ValDef]
      strict = rewrittenParameter.tpt match
        case untpd.Ident(name) =>
          name.toString == "Int" && !rewrittenParameter.eq(originalParameter)
        case _ => false
    yield UnsafeProof(
      result,
      strict,
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

  private def invoke(loader: ClassLoader, methodName: String): Int =
    val moduleClass = loader.loadClass("U056Use$")
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

  private val Program =
    """class U056BodyOnly:
      |  def eval(x: => Int): Int = 0
      |  val untouched: Int = 7
      |
      |class U056ResultOnly:
      |  def eval(x: => Int): AnyVal = x + x
      |  val untouched: Int = 9
      |
      |class U056ParameterType:
      |  def eval(x: => AnyVal): Int = 0
      |  val untouched: Int = 11
      |
      |class U056TwoParameter:
      |  def combine(x: => AnyVal, y: Int): Int = y
      |  val untouched: Int = 13
      |
      |class U056Strict:
      |  def eval(x: Int): Int = x + x
      |
      |object U056Use:
      |  def bodyOnly: Int =
      |    var counter = 0
      |    val result = new U056BodyOnly().eval({ counter += 1; counter })
      |    result * 10 + counter
      |
      |  def resultOnly: Int =
      |    var counter = 0
      |    val result: Int = new U056ResultOnly().eval({ counter += 1; counter })
      |    result * 10 + counter
      |
      |  def parameterType: Int =
      |    var counter = 0
      |    val result = new U056ParameterType().eval({ counter += 1; counter })
      |    result * 10 + counter
      |
      |  def twoParameter: Int =
      |    var counter = 0
      |    val result = new U056TwoParameter().combine({ counter += 1; counter }, 4)
      |    result * 10 + counter
      |
      |  def strictControl: Int =
      |    var counter = 0
      |    val result = new U056Strict().eval({ counter += 1; counter })
      |    result * 10 + counter
      |""".stripMargin
