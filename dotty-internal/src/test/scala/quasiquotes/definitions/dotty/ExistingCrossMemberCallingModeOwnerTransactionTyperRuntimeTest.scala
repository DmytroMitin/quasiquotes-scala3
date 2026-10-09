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

final class ExistingCrossMemberCallingModeOwnerTransactionTyperRuntimeTest
    extends munit.FunSuite:
  test("one owner plan edits distinct by-name and repeated methods through Typer TASTy and runtime") {
    val temporary = Files.createTempDirectory("u061-cross-member-modes-")
    try
      val source = temporary.resolve("U061CrossMemberModes.scala")
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
      assertEquals(driver.rewritten, Set("U061CrossIgnored", "U061CrossUsed"))
      assertEquals(driver.freshOwners, 2)
      assertEquals(driver.bothModesPreserved, 2)
      assertEquals(driver.untouchedMembersPreserved, 2)
      assert(driver.preTyperClean)

      val files = emitted(output)
      assert(files.exists(_.endsWith("U061CrossIgnored.tasty")))
      assert(files.exists(_.endsWith("U061CrossUsed.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        assertEquals(invoke(loader, "ignoredAndRepeated"), 37)
        assertEquals(invoke(loader, "usedTwiceAndRepeated"), 3040)
        assertEquals(invoke(loader, "strictControl"), 2040)
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class RewriteDriver extends Driver:
    @volatile var rewritten = Set.empty[String]
    @volatile var freshOwners = 0
    @volatile var bothModesPreserved = 0
    @volatile var untouchedMembersPreserved = 0
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(evidence: RewriteDriver) extends Phase:
    def phaseName: String = "u061CrossMemberCallingModeOwnerTransaction"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString == "U061CrossIgnored" =>
              rewrite(root, byNameFirst = true) match
                case Left(problem) => report.error(problem); root
                case Right(proof) =>
                  record(evidence, root, proof)
                  proof.result.tree
            case root: untpd.TypeDef if root.name.toString == "U061CrossUsed" =>
              rewrite(root, byNameFirst = false) match
                case Left(problem) => report.error(problem); root
                case Right(proof) =>
                  record(evidence, root, proof)
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
          report.error("expected PackageDef before Typer, found " + other.getClass.getSimpleName)

  private final case class Proof(
      result: Result,
      freshOwner: Boolean,
      modesPreserved: Boolean,
      untouchedPreserved: Boolean
  )

  private def record(
      evidence: RewriteDriver,
      root: untpd.TypeDef,
      proof: Proof
  ): Unit =
    evidence.rewritten += root.name.toString
    if proof.freshOwner then evidence.freshOwners += 1
    if proof.modesPreserved then evidence.bothModesPreserved += 1
    if proof.untouchedPreserved then evidence.untouchedMembersPreserved += 1

  private def rewrite(
      root: untpd.TypeDef,
      byNameFirst: Boolean
  )(using Context): Either[String, Proof] =
    val originalMembers = template(root).body
    val originalEval = originalMembers(0).asInstanceOf[untpd.DefDef]
    val originalSum = originalMembers(1).asInstanceOf[untpd.DefDef]
    val originalEvalParameter = parameter(originalEval)
    val originalSumParameter = parameter(originalSum)
    val originalByName = originalEvalParameter.tpt.asInstanceOf[untpd.ByNameTypeTree]
    val originalRepeated = originalSumParameter.tpt.asInstanceOf[untpd.PostfixOp]
    val (originalElement, originalMarker) = originalRepeated match
      case untpd.PostfixOp(element, marker) => (element, marker)
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      eval <- captured.method(captured.members(0).ref).left.map(_.message)
      sum <- captured.method(captured.members(1).ref).left.map(_.message)
      evalParameter = eval.parameterClauses.head.parameters.head
      sumParameter = sum.parameterClauses.head.parameters.head
      plan <- (
        if byNameFirst then
          for
            first <- captured.emptyPlan.replaceParameterType(evalParameter.ref, IntType)
            both <- first.replaceParameterType(sumParameter.ref, IntType)
          yield both
        else
          for
            first <- captured.emptyPlan.replaceParameterType(sumParameter.ref, IntType)
            both <- first.replaceParameterType(evalParameter.ref, IntType)
          yield both
      ).left.map(_.message)
      result <- ExistingClassUntypedRewrite(captured, plan).left.map(_.message)
      rewrittenMembers = template(result.tree).body
      rewrittenEval = rewrittenMembers(0).asInstanceOf[untpd.DefDef]
      rewrittenSum = rewrittenMembers(1).asInstanceOf[untpd.DefDef]
      rewrittenEvalParameter = parameter(rewrittenEval)
      rewrittenSumParameter = parameter(rewrittenSum)
      byNamePreserved = rewrittenEvalParameter.tpt match
        case wrapper: untpd.ByNameTypeTree =>
          wrapper.result match
            case ident: untpd.Ident =>
              ident.name.toString == "Int" &&
                !rewrittenEval.eq(originalEval) &&
                !rewrittenEvalParameter.eq(originalEvalParameter) &&
                !wrapper.eq(originalByName) &&
                wrapper.source == originalByName.source &&
                wrapper.span == originalByName.span &&
                ident.source == originalByName.result.source &&
                ident.span == originalByName.result.span
            case _ => false
        case _ => false
      repeatedPreserved = rewrittenSumParameter.tpt match
        case wrapper: untpd.PostfixOp =>
          wrapper match
            case untpd.PostfixOp(element: untpd.Ident, marker) =>
              element.name.toString == "Int" &&
                !rewrittenSum.eq(originalSum) &&
                !rewrittenSumParameter.eq(originalSumParameter) &&
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
      !result.tree.eq(root),
      byNamePreserved && repeatedPreserved &&
        rewrittenEval.tpt.eq(originalEval.tpt) && rewrittenEval.rhs.eq(originalEval.rhs) &&
        rewrittenSum.tpt.eq(originalSum.tpt) && rewrittenSum.rhs.eq(originalSum.rhs),
      rewrittenMembers(2).eq(originalMembers(2))
    )

  private def template(root: untpd.TypeDef)(using Context): untpd.Template =
    root.rhs.asInstanceOf[untpd.Template]

  private def parameter(method: untpd.DefDef): untpd.ValDef =
    method.paramss.head.head.asInstanceOf[untpd.ValDef]

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
    val moduleClass = loader.loadClass("U061CrossMemberUse$")
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
    """class U061CrossIgnored:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val untouched: Int = 7
      |
      |class U061CrossUsed:
      |  def eval(lazyValue: => AnyVal): Int = lazyValue.hashCode + lazyValue.hashCode
      |  def sum(values: AnyVal*): Int = values.size
      |  val untouched: Int = 8
      |
      |class U061StrictControl:
      |  def eval(value: Int): Int = value.hashCode + value.hashCode
      |  def sum(values: Int*): Int = values.size
      |  val untouched: Int = 9
      |
      |object U061CrossMemberUse:
      |  def ignoredAndRepeated: Int =
      |    var counter = 0
      |    val instance = new U061CrossIgnored()
      |    val evaluated = instance.eval({ counter += 1; counter })
      |    evaluated * 1000 + instance.sum(10, 20, 30) * 10 + instance.untouched + counter
      |
      |  def usedTwiceAndRepeated: Int =
      |    var counter = 0
      |    val instance = new U061CrossUsed()
      |    val evaluated = instance.eval({ counter += 1; counter })
      |    evaluated * 1000 + instance.sum(10, 20, 30) * 10 + instance.untouched + counter
      |
      |  def strictControl: Int =
      |    var counter = 0
      |    val instance = new U061StrictControl()
      |    val evaluated = instance.eval({ counter += 1; counter })
      |    evaluated * 1000 + instance.sum(10, 20, 30) * 10 + instance.untouched + counter
      |""".stripMargin
