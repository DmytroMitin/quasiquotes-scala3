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

import quasiquotes.definitions.*
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

class ExistingUntpdClassOwnerTransactionTyperRuntimeTest extends munit.FunSuite:
  test("U3 method edit plus generated sibling is insertion-ready before Typer") {
    val temporary = Files.createTempDirectory("u044-owner-transaction-")
    try
      val source = temporary.resolve("U044OwnerTransactionRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """class U044Runtime:
          |  def bump(x: AnyVal): AnyVal = x
          |
          |object U044RuntimeUse:
          |  def value: Int = new U044Runtime().bump(-7) + new U044Runtime().bonus
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val baseline = new Driver().process(
        Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      )
      assert(baseline.hasErrors)
      deleteRecursively(output)
      Files.createDirectories(output)

      val driver = new RewriteDriver
      val reporter = driver.process(
        Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      )

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assert(driver.beforeTyperContractValid)
      assert(emitted(output).exists(_.endsWith("U044Runtime.tasty")))
      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try assertEquals(invokeValue(loader, "U044RuntimeUse$"), Integer.valueOf(12))
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class RewriteDriver extends Driver:
    @volatile var beforeTyperContractValid: Boolean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(evidence: RewriteDriver) extends Phase:
    def phaseName: String = "u044OwnerTransaction"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared = for
        root <- classNamed(unitTree, "U044Runtime")
        captured <- ExistingUntpdClassMemberFilter.capture(root).left.map(_.message)
        descriptor <- ExistingUntpdOrdinaryMethodDescriptor.capture(captured, 0).left.map(_.message)
        parameter <- ExistingUntpdOrdinaryMethodTypeEditPreparation
          .prepareParameterType(descriptor, descriptor.parameterClauses.head.head, TypeNormalForm.STypeIdent("Int"))
          .left.map(_.message)
        resultType <- ExistingUntpdOrdinaryMethodTypeEditPreparation
          .prepareResultType(descriptor, TypeNormalForm.STypeIdent("Int"))
          .left.map(_.message)
        body <- ExistingUntpdOrdinaryMethodRhsEditPreparation.prepareBody(descriptor) { scope =>
          scope.reference(descriptor.parameterClauses.head.head).map(reference =>
            TermShape.Apply(
              TermShape.Select(TermShape.Identifier("Math", false), "abs"),
              List(reference)
            )
          )
        }.left.map(_.message)
        method <- ExistingUntpdOrdinaryMethodReconstruction
          .reconstructMethod(descriptor, Vector(parameter), Some(resultType), Some(body))
          .left.map(_.message)
        bonusSemantic <- SemanticDefinition.concreteMethod(
          DefinitionName.fromSource("bonus").toOption.get,
          Vector.empty,
          TypeNormalForm.STypeIdent("Int")
        )(_ => Right(TermShape.Literal("5"))).left.map(_.message)
        bonus <- ExistingUntpdClassOwnerTransaction
          .prepareGenerated(bonusSemantic, "<generated:u044-runtime-bonus>")
          .left.map(_.message)
        result <- ExistingUntpdClassOwnerTransaction
          .apply(captured, Vector.empty, Vector(method), Vector(bonus))
          .left.map(_.message)
      yield (root, captured, method, bonus, result)

      prepared match
        case Left(problem) => report.error(problem)
        case Right((root, captured, method, bonus, result)) =>
          evidence.beforeTyperContractValid =
            result.changed &&
              result.root.ne(root) &&
              result.template.ne(captured.originalTemplate) &&
              result.finalMembers.size == 2 &&
              result.finalMembers.head.eq(method.positionedMethod) &&
              result.finalMembers.last.eq(bonus.member) &&
              result.template.body.head.eq(method.positionedMethod) &&
              result.template.body.last.eq(bonus.member) &&
              bonus.member.source.eq(bonus.sourceFile) &&
              allTrees(result.root).forall(clean)
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              if tree.eq(root) then result.root else super.transform(tree)
          summon[Context].compilationUnit.untpdTree = transformer.transform(unitTree)

  private def classNamed(
      tree: untpd.Tree,
      name: String
  )(using Context): Either[String, untpd.TypeDef] =
    allTrees(tree).collectFirst {
      case value: untpd.TypeDef if value.name.toString == name => value
    }.toRight(s"fixture class $name was not found")

  private def clean(tree: untpd.Tree)(using Context): Boolean =
    tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ExistingUntpdClassMemberFilter.allTrees(tree)

  private def emitted(output: Path): Vector[String] =
    val stream = Files.walk(output)
    try stream.filter(Files.isRegularFile(_)).iterator().asScala.map(_.toString).toVector
    finally stream.close()

  private def invokeValue(loader: URLClassLoader, moduleName: String): AnyRef =
    val moduleClass = loader.loadClass(moduleName)
    val module = moduleClass.getField("MODULE$").get(null)
    moduleClass.getMethod("value").invoke(module)

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[_root_.dotty.tools.dotc.Compiler],
      classOf[ExistingUntpdClassOwnerTransaction.type],
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
