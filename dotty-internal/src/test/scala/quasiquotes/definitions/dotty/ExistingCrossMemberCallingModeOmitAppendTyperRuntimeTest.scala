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

import quasiquotes.definitions.{DefinitionName, DefinitionSemanticError, SemanticDefinition}
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.{Failure, Result}
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class ExistingCrossMemberCallingModeOmitAppendTyperRuntimeTest
    extends munit.FunSuite:
  test("one public plan edits both modes omits and appends through Typer TASTy and runtime") {
    val temporary = Files.createTempDirectory("u062-cross-member-omit-append-")
    try
      val source = temporary.resolve("U062CrossMemberOmitAppend.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)

      Files.writeString(source, BaselineProgram, StandardCharsets.UTF_8)
      val baseline = new Driver().process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))
      assert(!baseline.hasErrors, clues(baseline.allErrors))
      deleteRecursively(output)
      Files.createDirectories(output)

      Files.writeString(source, RewrittenProgram, StandardCharsets.UTF_8)
      val driver = new RewriteDriver
      val reporter = driver.process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(driver.rewritten, Set("U062Ignored", "U062Used"))
      assertEquals(driver.freshOwners, 2)
      assertEquals(driver.bothModesPreserved, 2)
      assertEquals(driver.omittedMembersAbsent, 2)
      assertEquals(driver.untouchedMembersPreserved, 2)
      assertEquals(driver.generatedMembersValid, 2)
      assert(driver.preTyperClean)

      val files = emitted(output)
      assert(files.exists(_.endsWith("U062Ignored.tasty")))
      assert(files.exists(_.endsWith("U062Used.tasty")))
      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        assertEquals(invoke(loader, "ignoredOmittedAppended"), 42)
        assertEquals(invoke(loader, "usedOmittedAppended"), 3045)
        assertEquals(invoke(loader, "strictControl"), 2040)
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class RewriteDriver extends Driver:
    @volatile var rewritten = Set.empty[String]
    @volatile var freshOwners = 0
    @volatile var bothModesPreserved = 0
    @volatile var omittedMembersAbsent = 0
    @volatile var untouchedMembersPreserved = 0
    @volatile var generatedMembersValid = 0
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(evidence: RewriteDriver) extends Phase:
    def phaseName: String = "u062CrossMemberCallingModeOmitAppend"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString == "U062Ignored" =>
              replace(evidence, root, editsFirst = true)
            case root: untpd.TypeDef if root.name.toString == "U062Used" =>
              replace(evidence, root, editsFirst = false)
            case other => other
          }
          summon[Context].compilationUnit.untpdTree =
            untpd.cpy.PackageDef(packageDef)(packageDef.pid, rewritten)
          evidence.preTyperClean = rewritten.forall(tree => allTrees(tree).forall(node =>
            node.symbol == NoSymbol && !node.isInstanceOf[untpd.TypedSplice]
          ))
        case other =>
          report.error("expected PackageDef before Typer, found " + other.getClass.getSimpleName)

    private def replace(
        evidence: RewriteDriver,
        root: untpd.TypeDef,
        editsFirst: Boolean
    )(using Context): untpd.TypeDef =
      rewrite(root, editsFirst) match
        case Left(problem) =>
          report.error(problem)
          root
        case Right(proof) =>
          evidence.rewritten += root.name.toString
          if proof.freshOwner then evidence.freshOwners += 1
          if proof.modesPreserved then evidence.bothModesPreserved += 1
          if proof.omittedAbsent then evidence.omittedMembersAbsent += 1
          if proof.untouchedPreserved then evidence.untouchedMembersPreserved += 1
          if proof.generatedValid then evidence.generatedMembersValid += 1
          proof.result.tree

  private final case class Proof(
      result: Result,
      freshOwner: Boolean,
      modesPreserved: Boolean,
      omittedAbsent: Boolean,
      untouchedPreserved: Boolean,
      generatedValid: Boolean
  )

  private def rewrite(
      root: untpd.TypeDef,
      editsFirst: Boolean
  )(using Context): Either[String, Proof] =
    val originalMembers = template(root).body.toVector
    val originalEval = originalMembers(0).asInstanceOf[untpd.DefDef]
    val originalSum = originalMembers(1).asInstanceOf[untpd.DefDef]
    val originalEvalParameter = parameter(originalEval)
    val originalSumParameter = parameter(originalSum)
    val originalByName = originalEvalParameter.tpt.asInstanceOf[untpd.ByNameTypeTree]
    val originalRepeated = originalSumParameter.tpt.asInstanceOf[untpd.PostfixOp]
    val (originalElement, originalMarker) = originalRepeated match
      case untpd.PostfixOp(element, marker) => (element, marker)
    val path = s"generated/${root.name.toString}Bonus.scala"

    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      eval <- captured.method(captured.members(0).ref).left.map(_.message)
      sum <- captured.method(captured.members(1).ref).left.map(_.message)
      evalParameter = eval.parameterClauses.head.parameters.head
      sumParameter = sum.parameterClauses.head.parameters.head
      plan <- (
        if editsFirst then
          for
            p1 <- captured.emptyPlan.replaceParameterType(evalParameter.ref, IntType)
            p2 <- p1.replaceParameterType(sumParameter.ref, IntType)
            p3 <- p2.omit(captured.members(2).ref)
            p4 <- p3.append(generatedMethod("bonus", "5"), path)
          yield p4
        else
          for
            p1 <- captured.emptyPlan.omit(captured.members(2).ref)
            p2 <- p1.append(generatedMethod("bonus", "5"), path)
            p3 <- p2.replaceParameterType(sumParameter.ref, IntType)
            p4 <- p3.replaceParameterType(evalParameter.ref, IntType)
          yield p4
      ).left.map(_.message)
      result <- ExistingClassUntypedRewrite(captured, plan).left.map(_.message)
      members = template(result.tree).body.toVector
      rewrittenEval = members(0).asInstanceOf[untpd.DefDef]
      rewrittenSum = members(1).asInstanceOf[untpd.DefDef]
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
      names = members.map(_.asInstanceOf[untpd.MemberDef].name.toString)
      generated = members.last.asInstanceOf[untpd.DefDef]
    yield Proof(
      result,
      !result.tree.eq(root) && !result.tree.rhs.eq(root.rhs),
      byNamePreserved && repeatedPreserved &&
        rewrittenEval.tpt.eq(originalEval.tpt) && rewrittenEval.rhs.eq(originalEval.rhs) &&
        rewrittenSum.tpt.eq(originalSum.tpt) && rewrittenSum.rhs.eq(originalSum.rhs),
      names == Vector("eval", "sum", "untouched", "bonus") &&
        !members.exists(_.eq(originalMembers(2))),
      members(2).eq(originalMembers(3)) &&
        result.directMemberIdentities(2).sameObjectAs(captured.members(3).identity),
      generated.source.path == path &&
        generated.source.content.mkString == "def bonus: Int = 5" &&
        generated.span.start == 0 && generated.span.end == generated.source.content.length &&
        result.directMemberIdentities.size == 4
    )

  private def generatedMethod(label: String, literal: String): SemanticDefinition =
    semantic(SemanticDefinition.concreteMethod(
      semantic(DefinitionName.fromSource(label)),
      Vector.empty,
      IntType
    )(_ => Right(TermShape.Literal(literal))))

  private def semantic[A](value: Either[DefinitionSemanticError, A]): A =
    value.fold(problem => throw new IllegalArgumentException(problem.message), identity)

  private def template(root: untpd.TypeDef)(using Context): untpd.Template =
    root.rhs.asInstanceOf[untpd.Template]

  private def parameter(method: untpd.DefDef): untpd.ValDef =
    method.paramss.head.head.asInstanceOf[untpd.ValDef]

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ExistingUntpdClassMemberFilter.allTrees(tree)

  private def emitted(output: Path): Vector[String] =
    val stream = Files.walk(output)
    try stream.filter(Files.isRegularFile(_)).iterator().asScala.map(_.toString).toVector
    finally stream.close()

  private def invoke(loader: ClassLoader, methodName: String): Int =
    val moduleClass = loader.loadClass("U062CrossMemberUse$")
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

  private val Owners =
    """class U062Ignored:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |
      |class U062Used:
      |  def eval(lazyValue: => AnyVal): Int = lazyValue.hashCode + lazyValue.hashCode
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 8
      |
      |class U062StrictControl:
      |  def eval(value: Int): Int = value.hashCode + value.hashCode
      |  def sum(values: Int*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 9
      |""".stripMargin

  private val BaselineProgram = Owners +
    """object U062BaselineUse:
      |  def value: Int =
      |    val instance = new U062Ignored()
      |    instance.eval(1) + instance.sum(1, 2, 3) + instance.obsolete + instance.untouched
      |""".stripMargin

  private val RewrittenProgram = Owners +
    """object U062CrossMemberUse:
      |  def ignoredOmittedAppended: Int =
      |    var counter = 0
      |    val instance = new U062Ignored()
      |    val evaluated = instance.eval({ counter += 1; counter })
      |    evaluated * 1000 + instance.sum(10, 20, 30) * 10 + instance.untouched + instance.bonus + counter
      |
      |  def usedOmittedAppended: Int =
      |    var counter = 0
      |    val instance = new U062Used()
      |    val evaluated = instance.eval({ counter += 1; counter })
      |    evaluated * 1000 + instance.sum(10, 20, 30) * 10 + instance.untouched + instance.bonus + counter
      |
      |  def strictControl: Int =
      |    var counter = 0
      |    val instance = new U062StrictControl()
      |    val evaluated = instance.eval({ counter += 1; counter })
      |    evaluated * 1000 + instance.sum(10, 20, 30) * 10 + instance.untouched + counter
      |""".stripMargin
