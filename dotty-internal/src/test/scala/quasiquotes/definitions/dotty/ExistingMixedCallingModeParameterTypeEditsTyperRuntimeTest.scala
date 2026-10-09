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
import quasiquotes.types.TypeNormalForm

final class ExistingMixedCallingModeParameterTypeEditsTyperRuntimeTest
    extends munit.FunSuite:
  test("dual Type edits preserve lazy evaluation and repeated positional calls through Typer TASTy and runtime") {
    val temporary = Files.createTempDirectory("u060-mixed-calling-modes-")
    try
      val source = temporary.resolve("U060MixedCallingModes.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(source, Program, StandardCharsets.UTF_8)

      val baseline = new Driver().process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))
      assert(!baseline.hasErrors, clues(baseline.allErrors))
      deleteRecursively(output)
      Files.createDirectories(output)

      val driver = new RewriteDriver
      val reporter = driver.process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(driver.rewritten, Set("U060MixedIgnored", "U060MixedUsed"))
      assert(driver.byNameFirstPreserved)
      assert(driver.repeatedFirstPreserved)
      assertEquals(driver.untouchedMembersPreserved, 2)
      assert(driver.preTyperClean)

      val files = emitted(output)
      assert(files.exists(_.endsWith("U060MixedIgnored.tasty")))
      assert(files.exists(_.endsWith("U060MixedUsed.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        assertEquals(invoke(loader, "ignored"), 30)
        assertEquals(invoke(loader, "usedTwice"), 62)
        assertEquals(invoke(loader, "strictControl"), 51)
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class RewriteDriver extends Driver:
    @volatile var rewritten = Set.empty[String]
    @volatile var byNameFirstPreserved = false
    @volatile var repeatedFirstPreserved = false
    @volatile var untouchedMembersPreserved = 0
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(evidence: RewriteDriver) extends Phase:
    def phaseName: String = "u060MixedCallingModeAtomicTypeEdits"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString == "U060MixedIgnored" =>
              rewrite(root, byNameFirst = true) match
                case Left(problem) => report.error(problem); root
                case Right(proof) =>
                  evidence.rewritten += root.name.toString
                  evidence.byNameFirstPreserved = proof.bothModesPreserved
                  if proof.untouchedPreserved then evidence.untouchedMembersPreserved += 1
                  proof.result.tree
            case root: untpd.TypeDef if root.name.toString == "U060MixedUsed" =>
              rewrite(root, byNameFirst = false) match
                case Left(problem) => report.error(problem); root
                case Right(proof) =>
                  evidence.rewritten += root.name.toString
                  evidence.repeatedFirstPreserved = proof.bothModesPreserved
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

  private final case class Proof(
      result: Result,
      bothModesPreserved: Boolean,
      untouchedPreserved: Boolean
  )

  private def rewrite(
      root: untpd.TypeDef,
      byNameFirst: Boolean
  )(using Context): Either[String, Proof] =
    val originalMethod = firstMethod(root)
    val originalParameters = originalMethod.paramss.head.map(_.asInstanceOf[untpd.ValDef])
    val originalByName = originalParameters.head.tpt.asInstanceOf[untpd.ByNameTypeTree]
    val originalRepeated = originalParameters(1).tpt.asInstanceOf[untpd.PostfixOp]
    val (originalElement, originalMarker) = originalRepeated match
      case untpd.PostfixOp(element, marker) => (element, marker)
    val originalUntouched = template(root).body(1)
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      method <- captured.method(captured.members.head.ref).left.map(_.message)
      parameters = method.parameterClauses.head.parameters
      plan <- (
        if byNameFirst then
          for
            first <- captured.emptyPlan.replaceParameterType(parameters.head.ref, IntType)
            both <- first.replaceParameterType(parameters(1).ref, IntType)
          yield both
        else
          for
            second <- captured.emptyPlan.replaceParameterType(parameters(1).ref, IntType)
            both <- second.replaceParameterType(parameters.head.ref, IntType)
          yield both
      ).left.map(_.message)
      result <- ExistingClassUntypedRewrite(captured, plan).left.map(_.message)
      rewritten = firstMethod(result.tree)
      rewrittenParameters = rewritten.paramss.head.map(_.asInstanceOf[untpd.ValDef])
      byNamePreserved = rewrittenParameters.head.tpt match
        case wrapper: untpd.ByNameTypeTree =>
          wrapper.result match
            case ident: untpd.Ident =>
              ident.name.toString == "Int" &&
                !rewrittenParameters.head.eq(originalParameters.head) &&
                !wrapper.eq(originalByName) &&
                wrapper.source == originalByName.source &&
                wrapper.span == originalByName.span &&
                ident.source == originalByName.result.source &&
                ident.span == originalByName.result.span
            case _ => false
        case _ => false
      repeatedPreserved = rewrittenParameters(1).tpt match
        case wrapper: untpd.PostfixOp =>
          wrapper match
            case untpd.PostfixOp(element: untpd.Ident, marker) =>
              element.name.toString == "Int" &&
                !rewrittenParameters(1).eq(originalParameters(1)) &&
                !wrapper.eq(originalRepeated) &&
                wrapper.source == originalRepeated.source &&
                wrapper.span == originalRepeated.span &&
                !element.eq(originalElement) &&
                element.source == originalElement.source &&
                element.span == originalElement.span &&
                marker.eq(originalMarker)
            case _ => false
        case _ => false
    yield Proof(
      result,
      rewritten.paramss.map(_.size) == List(2) &&
        byNamePreserved && repeatedPreserved &&
        rewritten.tpt.eq(originalMethod.tpt) && rewritten.rhs.eq(originalMethod.rhs),
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
    val moduleClass = loader.loadClass("U060MixedUse$")
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
    """class U060MixedIgnored:
      |  def compute(lazyValue: => AnyVal, values: AnyVal*): Int = values.size
      |  val untouched: Int = 9
      |
      |class U060MixedUsed:
      |  def compute(lazyValue: => AnyVal, values: AnyVal*): Int =
      |    lazyValue.hashCode + lazyValue.hashCode + values.size
      |  val untouched: Int = 10
      |
      |class U060StrictControl:
      |  def compute(value: Int, values: Int*): Int =
      |    value.hashCode + value.hashCode + values.size
      |
      |object U060MixedUse:
      |  def ignored: Int =
      |    var counter = 0
      |    val result = new U060MixedIgnored().compute({ counter += 1; counter }, 10, 20, 30)
      |    result * 10 + counter
      |
      |  def usedTwice: Int =
      |    var counter = 0
      |    val result = new U060MixedUsed().compute({ counter += 1; counter }, 10, 20, 30)
      |    result * 10 + counter
      |
      |  def strictControl: Int =
      |    var counter = 0
      |    val result = new U060StrictControl().compute({ counter += 1; counter }, 10, 20, 30)
      |    result * 10 + counter
      |""".stripMargin
