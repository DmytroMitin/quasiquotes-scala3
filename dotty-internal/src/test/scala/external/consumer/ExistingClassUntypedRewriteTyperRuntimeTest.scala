package external.consumer

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

import quasiquotes.definitions.*
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.*
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class ExistingClassUntypedRewriteTyperRuntimeTest extends munit.FunSuite:
  test("U1 U2 U3 no-op omission and generated alias survive ordinary Typer TASTy and runtime"):
    val temporary = Files.createTempDirectory("c056-existing-class-rewrite-")
    try
      val source = temporary.resolve("C056Runtime.scala")
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
        driver.completed.toSet,
        Set("C056U1", "C056U2", "C056U3", "C056Noop", "C056Composition", "C056Alias")
      )
      assert(driver.preTyperClean)
      assert(emitted(output).exists(_.endsWith("C056U3.tasty")))
      assert(emitted(output).exists(_.endsWith("C056Alias.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        assertEquals(invoke(loader, "C056UseU1$"), 5)
        assertEquals(invoke(loader, "C056UseU2$"), 16)
        assertEquals(invoke(loader, "C056UseU3$"), 12)
        assertEquals(invoke(loader, "C056UseNoop$"), 3)
        assertEquals(invoke(loader, "C056UseComposition$"), 14)
        assertEquals(invoke(loader, "C056UseAlias$"), 42)
      finally loader.close()
    finally deleteRecursively(temporary)

  private final class RewriteDriver extends Driver:
    @volatile var completed: Vector[String] = Vector.empty
    @volatile var preTyperClean: Boolean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(evidence: RewriteDriver) extends Phase:
    def phaseName: String = "c056ExistingClassRewrite"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      summon[Context].compilationUnit.untpdTree match
        case packageDef: untpd.PackageDef =>
          val rewritten = packageDef.stats.map {
            case root: untpd.TypeDef if root.name.toString.startsWith("C056") &&
                !root.name.toString.startsWith("C056Use") =>
              rewrite(root, evidence)
            case other => other
          }
          summon[Context].compilationUnit.untpdTree =
            untpd.cpy.PackageDef(packageDef)(packageDef.pid, rewritten)
          evidence.preTyperClean = rewritten.forall(tree => allTrees(tree).forall(node =>
            node.symbol == NoSymbol && !node.isInstanceOf[untpd.TypedSplice]
          ))
        case other => report.error(s"expected PackageDef before Typer, found ${other.getClass.getSimpleName}")

    private def rewrite(root: untpd.TypeDef, evidence: RewriteDriver)(using Context): untpd.TypeDef =
      val name = root.name.toString
      val result = name match
        case "C056U1" => appendOnly(root, "bonus", "5")
        case "C056U2" => rewriteMethod(root, append = None, omit = None)
        case "C056U3" => rewriteMethod(root, append = Some("bonus" -> "5"), omit = None)
        case "C056Noop" =>
          for
            captured <- ExistingClassUntypedRewrite.capture(root)
            applied <- ExistingClassUntypedRewrite(captured, captured.emptyPlan)
            _ <- Either.cond(
              !applied.changed && applied.tree.eq(root),
              (),
              Failure("INTERNAL_INVARIANT_FAILED", "the public no-op contract was not exact.")
            )
          yield applied
        case "C056Composition" => rewriteMethod(root, append = Some("bonus" -> "5"), omit = Some(0))
        case "C056Alias" => appendAlias(root)
      result match
        case Left(problem) =>
          report.error(problem.message)
          root
        case Right(value) =>
          evidence.completed :+= name
          value.tree

    private def appendOnly(
        root: untpd.TypeDef,
        label: String,
        literal: String
    )(using Context): Either[Failure, Result] =
      for
        captured <- ExistingClassUntypedRewrite.capture(root)
        selected <- captured.member(0)
        _ <- captured.method(selected.ref)
        plan <- captured.emptyPlan.append(methodDefinition(label, literal), s"generated/$label.scala")
        result <- ExistingClassUntypedRewrite(captured, plan)
        _ <- Either.cond(
          result.changed && result.directMemberIdentities.head.sameObjectAs(selected.identity),
          (),
          Failure("INTERNAL_INVARIANT_FAILED", "U1 did not preserve the exact original method.")
        )
      yield result

    private def rewriteMethod(
        root: untpd.TypeDef,
        append: Option[(String, String)],
        omit: Option[Int]
    )(using Context): Either[Failure, Result] =
      for
        captured <- ExistingClassUntypedRewrite.capture(root)
        member <- captured.member(if omit.isDefined then 1 else 0)
        method <- captured.method(member.ref)
        parameter = method.parameterClauses.head.parameters.head
        reference <- method.parameterScope.reference(parameter.ref)
        body = TermShape.Apply(
          TermShape.Select(TermShape.Identifier("Math", false), "abs"),
          List(reference)
        )
        p0 <- omit.fold[Either[Failure, EditPlan]](Right(captured.emptyPlan))(index =>
          captured.member(index).flatMap(member => captured.emptyPlan.omit(member.ref))
        )
        p1 <- p0.replaceParameterType(parameter.ref, IntType)
        p2 <- p1.replaceResultType(method.ref, IntType)
        p3 <- p2.replaceBody(method.ref, body)
        p4 <- append.fold[Either[Failure, EditPlan]](Right(p3)) { case (label, literal) =>
          p3.append(methodDefinition(label, literal), s"generated/$label.scala")
        }
        result <- ExistingClassUntypedRewrite(captured, p4)
        _ <- Either.cond(
          result.changed && !result.tree.eq(root),
          (),
          Failure("INTERNAL_INVARIANT_FAILED", "a changed public plan did not allocate one fresh owner.")
        )
      yield result

    private def appendAlias(root: untpd.TypeDef)(using Context): Either[Failure, Result] =
      for
        captured <- ExistingClassUntypedRewrite.capture(root)
        plan <- captured.emptyPlan.append(aliasDefinition("AddedAlias"), "generated/C056Alias.scala")
        result <- ExistingClassUntypedRewrite(captured, plan)
      yield result

  private def methodDefinition(label: String, literal: String): SemanticDefinition =
    semantic(SemanticDefinition.concreteMethod(
      semantic(DefinitionName.fromSource(label)),
      Vector.empty,
      IntType
    )(_ => Right(TermShape.Literal(literal))))

  private def aliasDefinition(label: String): SemanticDefinition =
    semantic(SemanticDefinition.typeAlias(semantic(DefinitionName.fromSource(label)), IntType))

  private def semantic[A](value: Either[DefinitionSemanticError, A]): A =
    value.fold(problem => throw new IllegalArgumentException(problem.message), identity)

  private val IntType = TypeNormalForm.STypeIdent("Int")

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

  private def invoke(loader: ClassLoader, moduleName: String): Int =
    val moduleClass = loader.loadClass(moduleName)
    val module = moduleClass.getField("MODULE$").get(null)
    moduleClass.getMethod("value").invoke(module).asInstanceOf[Integer].intValue

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[_root_.dotty.tools.dotc.Compiler],
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

  private val Program =
    """class C056U1:
      |  def keep(x: Int): Int = x
      |
      |object C056UseU1:
      |  def value: Int = new C056U1().bonus
      |
      |class C056U2:
      |  def change(x: AnyVal): AnyVal = x
      |  val keep: Int = 9
      |
      |object C056UseU2:
      |  def value: Int = new C056U2().change(-7) + new C056U2().keep
      |
      |class C056U3:
      |  def change(x: AnyVal): AnyVal = x
      |
      |object C056UseU3:
      |  def value: Int = new C056U3().change(-7) + new C056U3().bonus
      |
      |class C056Noop:
      |  def keep: Int = 3
      |
      |object C056UseNoop:
      |  def value: Int = new C056Noop().keep
      |
      |class C056Composition:
      |  val removeMe: Int = 100
      |  def change(x: AnyVal): AnyVal = x
      |  val keepMe: Int = 2
      |
      |object C056UseComposition:
      |  def value: Int = new C056Composition().change(-7) + new C056Composition().keepMe + new C056Composition().bonus
      |
      |class C056Alias:
      |  def keep: Int = 1
      |
      |object C056UseAlias:
      |  def value: Int =
      |    val instance = new C056Alias()
      |    val answer: instance.AddedAlias = 42
      |    answer
      |""".stripMargin
