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

import quasiquotes.definitions.ScopedType.*
import quasiquotes.definitions.dotty.BoundedExtensionModulePlan.*
import quasiquotes.parser.BinderId

class BoundedExtensionModuleTyperRuntimeTest extends munit.FunSuite:
  test("canonical and renamed generated modules survive pre-Typer insertion TASTy and runtime") {
    val temporary = Files.createTempDirectory("u024-extension-module-")
    try
      val source = temporary.resolve("U024Runtime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """trait Semigroup[A]:
          |  def combine(left: A, right: A): A
          |
          |trait Choice[A]:
          |  def merge(left: A, right: A): A
          |
          |object CanonicalUsage:
          |  given Semigroup[Int] with
          |    def combine(left: Int, right: Int): Int = left + right
          |  import syntax.*
          |  def result: Int = 20.combine(22)
          |
          |object RenamedUsage:
          |  given Choice[Int] with
          |    def merge(left: Int, right: Int): Int = left * right
          |  import operations.*
          |  def result: Int = 6.merge(7)
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new ExtensionModuleDriver
      val reporter = driver.process(Array(
        "-classpath",
        compilationClasspath,
        "-d",
        output.toString,
        source.toString
      ))

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assert(driver.beforeTyperInsertionReady)
      assertEquals(driver.generatedSources, Vector(
        """object syntax:
          |  extension [A](receiver: A)
          |    def combine(argument: A)(using evidence: Semigroup[A]): A =
          |      evidence.combine(receiver, argument)
          |""".stripMargin,
        """object operations:
          |  extension [Element](left: Element)
          |    def merge(right: Element)(using instance: Choice[Element]): Element =
          |      instance.merge(left, right)
          |""".stripMargin
      ))
      assert(Files.exists(output.resolve("syntax.tasty")), clues(output))
      assert(Files.exists(output.resolve("operations.tasty")), clues(output))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        assertEquals(invokeResult(loader, "CanonicalUsage$"), 42)
        assertEquals(invokeResult(loader, "RenamedUsage$"), 42)
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class ExtensionModuleDriver extends Driver:
    @volatile var generatedSources: Vector[String] = Vector.empty
    @volatile var beforeTyperInsertionReady: Boolean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new InsertExtensionModules(ExtensionModuleDriver.this)) ::
            super.frontendPhases.tail

  private final class InsertExtensionModules(evidence: ExtensionModuleDriver)
      extends Phase:
    def phaseName: String = "u024ExtensionModuleInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val lowered = for
        canonical <- BoundedExtensionModuleGeneratedOriginAdapter.lower(
          validPlan(Names("syntax", "A", "receiver", "combine", "argument", "evidence", "Semigroup")),
          "<quasiquotes-generated:u024-syntax>"
        )
        renamed <- BoundedExtensionModuleGeneratedOriginAdapter.lower(
          validPlan(Names("operations", "Element", "left", "merge", "right", "instance", "Choice")),
          "<quasiquotes-generated:u024-operations>"
        )
      yield Vector(canonical, renamed)

      lowered match
        case Left(problem) => report.error(problem.message)
        case Right(results) =>
          summon[Context].compilationUnit.untpdTree match
            case packageDef: untpd.PackageDef =>
              summon[Context].compilationUnit.untpdTree =
                untpd.cpy.PackageDef(packageDef)(
                  packageDef.pid,
                  packageDef.stats ++ results.map(_.tree)
                )
              evidence.generatedSources = results.map(_.generatedSource)
              evidence.beforeTyperInsertionReady = results.forall(result =>
                BoundedExtensionModuleUntypedLowerer
                  .allTrees(result.tree)
                  .forall(tree =>
                    tree.source.exists &&
                      tree.source.path == result.virtualSourceName &&
                      tree.span.exists && tree.symbol == NoSymbol &&
                      !tree.isInstanceOf[untpd.TypedSplice]
                  )
              )
            case other =>
              report.error(
                s"expected PackageDef before Typer, found ${other.getClass.getSimpleName}"
              )

  private final case class Names(
      module: String,
      typeParameter: String,
      receiver: String,
      method: String,
      argument: String,
      evidence: String,
      evidenceType: String
  )

  private def validPlan(names: Names): Plan =
    val typeBinder = BinderId(0)
    val receiverBinder = BinderId(1)
    val argumentBinder = BinderId(2)
    val evidenceBinder = BinderId(3)
    val reference = TypeParameterReference(typeBinder, names.typeParameter)
    BoundedExtensionModulePlan
      .create(
        names.module,
        names.method,
        TypeParameter(typeBinder, names.typeParameter),
        ReceiverParameter(receiverBinder, names.receiver, reference),
        OrdinaryArgument(argumentBinder, names.argument, reference),
        ContextualParameter(
          evidenceBinder,
          names.evidence,
          Applied(SourceName(names.evidenceType), Vector(reference))
        ),
        reference,
        DelegatedBody(
          BodyTermReference(evidenceBinder),
          names.method,
          Vector(
            BodyTermReference(receiverBinder),
            BodyTermReference(argumentBinder)
          )
        )
      )
      .fold(problem => throw new IllegalArgumentException(problem.message), identity)

  private def invokeResult(loader: ClassLoader, className: String): Any =
    val moduleClass = loader.loadClass(className)
    val module = moduleClass.getField("MODULE$").get(null)
    moduleClass.getMethod("result").invoke(module)

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[BoundedExtensionModuleGeneratedOriginAdapter.type],
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
