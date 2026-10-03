package com.github.dmytromitin.auxify.macros.internal

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*
import scala.meta.*
import scala.meta.dialects.Scala3

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser

import _root_.quasiquotes.definitions.dotty.ExtensionModulePeerBridge

class ExtensionModulePeerBridgeTyperRuntimeTest extends munit.FunSuite:
  test("foreign public bridge output survives pre-Typer insertion TASTy and runtime") {
    val temporary = Files.createTempDirectory("c054-extension-module-")
    try
      val source = temporary.resolve("C054Runtime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """trait Monoid[A]:
          |  def combine(left: A, right: A): A
          |
          |trait Combine[A]:
          |  def merge(left: A, right: A): A
          |
          |object CanonicalUsage:
          |  given Monoid[Int] with
          |    def combine(left: Int, right: Int): Int = left + right
          |  import syntax.*
          |  def result: Int = 20.combine(22)
          |
          |object RenamedUsage:
          |  given Combine[Int] with
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
      assertEquals(driver.generatedSources, Vector(CanonicalSource, RenamedSource))
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
    def phaseName: String = "c054ExtensionModuleInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val definitions = Vector(parse(CanonicalSource), parse(RenamedSource))
      val before = definitions.map(_.structure)
      val lowered = definitions
        .zip(Vector(
          "<quasiquotes-generated:c054-syntax>",
          "<quasiquotes-generated:c054-operations>"
        ))
        .foldLeft[Either[ExtensionModulePeerBridge.Failure, Vector[ExtensionModulePeerBridge.Lowered]]](Right(Vector.empty)) {
          case (accumulated, (definition, sourceName)) =>
            for
              values <- accumulated
              value <- ExtensionModulePeerBridge.lower(definition, sourceName)
            yield values :+ value
        }

      lowered match
        case Left(problem) => report.error(s"${problem.code}: ${problem.detail}")
        case Right(results) =>
          summon[Context].compilationUnit.untpdTree match
            case packageDef: untpd.PackageDef =>
              summon[Context].compilationUnit.untpdTree =
                untpd.cpy.PackageDef(packageDef)(
                  packageDef.pid,
                  packageDef.stats ++ results.map(_.tree)
                )
              evidence.generatedSources = results.map(_.generatedSource)
              evidence.beforeTyperInsertionReady =
                definitions.map(_.structure) == before && results.forall(result =>
                  allTrees(result.tree).size == 21 &&
                    allTrees(result.tree).forall(tree =>
                      tree.source.exists &&
                        tree.source.path == result.virtualSourceName &&
                        tree.span.exists &&
                        tree.symbol == NoSymbol &&
                        !tree.isInstanceOf[untpd.TypedSplice]
                    )
                )
            case other =>
              report.error(
                s"expected PackageDef before Typer, found ${other.getClass.getSimpleName}"
              )

  private val CanonicalSource =
    """object syntax:
      |  extension [A](a: A)
      |    def combine(a1: A)(using inst: Monoid[A]): A =
      |      inst.combine(a, a1)
      |""".stripMargin

  private val RenamedSource =
    """object operations:
      |  extension [Element](left: Element)
      |    def merge(right: Element)(using evidence: Combine[Element]): Element =
      |      evidence.merge(left, right)
      |""".stripMargin

  private def parse(source: String): Defn.Object =
    Scala3(source).parse[Stat].get.asInstanceOf[Defn.Object]

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.ModuleDef => Vector(value.impl)
      case value: untpd.Template =>
        Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body
      case value: untpd.ExtMethods =>
        value.paramss.flatten.toVector ++ value.methods.toVector
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeDef => Vector(value.rhs).filterNot(_.isEmpty)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def invokeResult(loader: ClassLoader, className: String): Any =
    val moduleClass = loader.loadClass(className)
    val module = moduleClass.getField("MODULE$").get(null)
    moduleClass.getMethod("result").invoke(module)

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[scala.meta.Tree],
      classOf[ExtensionModulePeerBridge.type],
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
