package quasiquotes.definitions.dotty

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser

import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.*
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class ExistingContextualMethodCaptureRewriteModelFitTyperRuntimeTest
    extends munit.FunSuite:
  test("U067 public C1 and C2 rewrites survive ordinary Typer TASTy and runtime") {
    val temporary = Files.createTempDirectory("u054-existing-contextual-model-fit-")
    try
      val source = temporary.resolve("U054ExistingContextualRuntime.scala")
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
        Set("U054ContextualOnly", "U054OrdinaryThenContextual")
      )
      assert(driver.contextualOnlyPreserved)
      assert(driver.mixedPreserved)
      assertEquals(driver.untouchedMembersPreserved, 2)
      assert(driver.preTyperClean)
      val files = emitted(output)
      assert(files.exists(_.endsWith("U054ContextualOnly.tasty")))
      assert(files.exists(_.endsWith("U054OrdinaryThenContextual.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try assertEquals(invokeValue(loader, "U054Use$"), 64)
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class RewriteDriver extends Driver:
    @volatile var rewritten = Set.empty[String]
    @volatile var contextualOnlyPreserved = false
    @volatile var mixedPreserved = false
    @volatile var untouchedMembersPreserved = 0
    @volatile var preTyperClean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(evidence: RewriteDriver) extends Phase:
    def phaseName: String = "u054ExistingContextualModelFit"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString == "U054ContextualOnly" =>
              rewrite(root, Vector(0 -> 0)) match
                case Left(problem) =>
                  report.error(problem)
                  root
                case Right(result) =>
                  evidence.rewritten += root.name.toString
                  evidence.contextualOnlyPreserved =
                    result.method.paramss.map(_.size) == List(1) &&
                      result.method.paramss.head.head
                        .asInstanceOf[untpd.ValDef].mods.is(Flags.Given)
                  if result.untouchedMemberPreserved then
                    evidence.untouchedMembersPreserved += 1
                  result.root
            case root: untpd.TypeDef
                if root.name.toString == "U054OrdinaryThenContextual" =>
              rewrite(root, Vector(0 -> 0, 1 -> 0)) match
                case Left(problem) =>
                  report.error(problem)
                  root
                case Right(result) =>
                  evidence.rewritten += root.name.toString
                  val first =
                    result.method.paramss(0).head.asInstanceOf[untpd.ValDef]
                  val second =
                    result.method.paramss(1).head.asInstanceOf[untpd.ValDef]
                  evidence.mixedPreserved =
                    result.method.paramss.map(_.size) == List(1, 1) &&
                      !first.mods.is(Flags.Given) &&
                      second.mods.is(Flags.Given)
                  if result.untouchedMemberPreserved then
                    evidence.untouchedMembersPreserved += 1
                  result.root
            case other => other
          }
          summon[Context].compilationUnit.untpdTree =
            untpd.cpy.PackageDef(packageDef)(packageDef.pid, rewritten)
          evidence.preTyperClean = rewritten.forall(tree =>
            ExistingUntpdClassMemberFilter.allTrees(tree).forall(node =>
              node.symbol == NoSymbol && !node.isInstanceOf[untpd.TypedSplice]
            )
          )
        case other =>
          report.error(
            "expected PackageDef before Typer, found " + other.getClass.getSimpleName
          )

  private final case class RewriteResult(
      root: untpd.TypeDef,
      method: untpd.DefDef,
      untouchedMemberPreserved: Boolean
  )

  private def rewrite(
      root: untpd.TypeDef,
      bodyReferences: Vector[(Int, Int)]
  )(using Context): Either[String, RewriteResult] =
    for
      captured <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      member <- captured.member(0).left.map(_.message)
      method <- captured.method(member.ref).left.map(_.message)
      selected <- bodyReferences.foldLeft[Either[String, Vector[ParameterView]]](Right(Vector.empty)) {
        case (result, (clauseIndex, parameterIndex)) =>
          for
            parameters <- result
            clause <- method.parameterClauses.lift(clauseIndex)
              .toRight(s"missing clause $clauseIndex")
            parameter <- clause.parameters.lift(parameterIndex)
              .toRight(s"missing parameter $clauseIndex/$parameterIndex")
          yield parameters :+ parameter
      }
      references <- selected.foldLeft[Either[String, Vector[TermShape]]](Right(Vector.empty)) {
        case (result, parameter) =>
          for
            values <- result
            reference <- method.parameterScope.reference(parameter.ref).left.map(_.message)
          yield values :+ reference
      }
      body <- references match
        case Vector(reference) => Right(reference)
        case Vector(first, second) => Right(TermShape.Apply(
          TermShape.Select(TermShape.Identifier("Math", false), "addExact"),
          List(first, second)
        ))
        case _ => Left("expected one or two selected contextual body references")
      typed <- selected.foldLeft[Either[String, EditPlan]](Right(captured.emptyPlan)) {
        case (result, parameter) =>
          result.flatMap(_.replaceParameterType(parameter.ref, TypeNormalForm.STypeIdent("Int"))
            .left.map(_.message))
      }
      resultTyped <- typed.replaceResultType(method.ref, TypeNormalForm.STypeIdent("Int"))
        .left.map(_.message)
      complete <- resultTyped.replaceBody(method.ref, body).left.map(_.message)
      applied <- ExistingClassUntypedRewrite(captured, complete).left.map(_.message)
      rewrittenMethod <- applied.tree.rhs.asInstanceOf[untpd.Template].body.headOption
        .collect { case value: untpd.DefDef => value }
        .toRight("rewritten first direct member was not a method")
    yield RewriteResult(
      applied.tree,
      rewrittenMethod,
      applied.directMemberIdentities(1).sameObjectAs(captured.members(1).identity)
    )

  private def emitted(output: Path): Vector[String] =
    val stream = Files.walk(output)
    try stream.filter(Files.isRegularFile(_)).iterator().asScala.map(_.toString).toVector
    finally stream.close()

  private def invokeValue(loader: ClassLoader, moduleName: String): Int =
    val moduleClass = loader.loadClass(moduleName)
    val module = moduleClass.getField("MODULE$").get(null)
    moduleClass.getMethod("value").invoke(module).asInstanceOf[Integer].intValue

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ExistingUntpdClassMemberFilter.type],
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

  private val Program =
    """class U054ContextualOnly:
      |  def value(using x: AnyVal): AnyVal = x
      |  val untouched: Int = 7
      |
      |class U054OrdinaryThenContextual:
      |  def combine(x: AnyVal)(using y: AnyVal): AnyVal = x
      |  val untouched: Int = 9
      |
      |object U054Use:
      |  given Int = 22
      |  def contextualValue: Int = new U054ContextualOnly().value
      |  def mixedValue: Int = new U054OrdinaryThenContextual().combine(20)
      |  def value: Int = contextualValue + mixedValue
      |""".stripMargin
