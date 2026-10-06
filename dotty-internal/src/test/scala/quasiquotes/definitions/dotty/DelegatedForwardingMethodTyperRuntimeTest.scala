package quasiquotes.definitions.dotty

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

class DelegatedForwardingMethodTyperRuntimeTest extends munit.FunSuite:
  test("both forwarding families survive Typer, TASTy emission, and runtime") {
    val temporary = Files.createTempDirectory("phase144-delegated-forwarding-")
    try
      val source = temporary.resolve("Phase144DelegatedForwardingRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """trait Show[A]:
          |  def show(value: A): String
          |
          |trait Display[A]:
          |  def render(value: A): Text
          |
          |trait Empty[A]:
          |  def empty: A
          |
          |trait Provider[A]:
          |  def obtain: A
          |
          |final class Text(val value: String)
          |
          |object Phase144CanonicalRuntime:
          |  given Show[Int] with
          |    def show(value: Int): String = "show:" + value
          |  def canonicalResult: String = show(7)
          |
          |object Phase144RenamedRuntime:
          |  given Display[Int] with
          |    def render(value: Int): Text = new Text("render:" + value)
          |  def renamedResult: Text = render(9)
          |
          |object C061ParameterlessCanonicalRuntime:
          |  given Empty[Int] with
          |    def empty: Int = 42
          |  def parameterlessCanonicalResult: Int = empty[Int]
          |
          |object C061ParameterlessRenamedRuntime:
          |  given Provider[String] with
          |    def obtain: String = "ready"
          |  def parameterlessRenamedResult: String = obtain[String]
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new ForwardingDriver
      val reporter = driver.process(
        Array(
          "-classpath",
          compilationClasspath,
          "-d",
          output.toString,
          source.toString
        )
      )

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(
        driver.generatedSources,
        Vector(
          "def show[A](a: A)(using inst: Show[A]): String = inst.show(a)",
          "def render[Element](value: Element)(using evidence: Display[Element]): Text = evidence.render(value)",
          "def empty[A](using inst: Empty[A]): A = inst.empty",
          "def obtain[Element](using evidence: Provider[Element]): Element = evidence.obtain"
        )
      )
      assert(driver.beforeTyperInsertionReady)

      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      val emittedNames = emitted.map(_.getFileName.toString).toSet
      assert(emittedNames.contains("C061ParameterlessCanonicalRuntime.tasty"), clues(emittedNames))
      assert(emittedNames.contains("C061ParameterlessRenamedRuntime.tasty"), clues(emittedNames))
      List(
        "Phase144CanonicalRuntime$.class",
        "Phase144RenamedRuntime$.class",
        "C061ParameterlessCanonicalRuntime$.class",
        "C061ParameterlessRenamedRuntime$.class"
      ).foreach(name => assert(emittedNames.contains(name), clues(name, emittedNames)))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val canonicalClass = loader.loadClass("Phase144CanonicalRuntime$")
        val canonical = canonicalClass.getField("MODULE$").get(null)
        assertEquals(
          canonicalClass.getMethod("canonicalResult").invoke(canonical),
          "show:7"
        )

        val renamedClass = loader.loadClass("Phase144RenamedRuntime$")
        val renamed = renamedClass.getField("MODULE$").get(null)
        val text = renamedClass.getMethod("renamedResult").invoke(renamed)
        assertEquals(text.getClass.getMethod("value").invoke(text), "render:9")

        val parameterlessCanonicalClass =
          loader.loadClass("C061ParameterlessCanonicalRuntime$")
        val parameterlessCanonical =
          parameterlessCanonicalClass.getField("MODULE$").get(null)
        assertEquals(
          parameterlessCanonicalClass
            .getMethod("parameterlessCanonicalResult")
            .invoke(parameterlessCanonical),
          Integer.valueOf(42)
        )
        val parameterlessRenamedClass = loader.loadClass("C061ParameterlessRenamedRuntime$")
        val parameterlessRenamed = parameterlessRenamedClass.getField("MODULE$").get(null)
        assertEquals(parameterlessRenamedClass.getMethod("parameterlessRenamedResult").invoke(parameterlessRenamed), "ready")
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class ForwardingDriver extends Driver:
    @volatile var generatedSources: Vector[String] = Vector.empty
    @volatile var beforeTyperInsertionReady: Boolean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new InsertForwarders(ForwardingDriver.this)) ::
            super.frontendPhases.tail

  private final class InsertForwarders(evidence: ForwardingDriver) extends Phase:
    def phaseName: String = "phase144DelegatedForwardingInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val lowered =
        for
          canonical <- DelegatedForwardingMethodPeerBridge.lower(
            parse(
              "def show[A](a: A)(using inst: Show[A]): String = inst.show(a)"
            ),
            "<quasiquotes-generated:phase144-show>"
          )
          renamed <- DelegatedForwardingMethodPeerBridge.lower(
            parse(
              "def render[Element](value: Element)(using evidence: Display[Element]): Text = evidence.render(value)"
            ),
            "<quasiquotes-generated:phase144-render>"
          )
          parameterlessCanonical <- DelegatedForwardingMethodPeerBridge.lower(
            parse("def empty[A](using inst: Empty[A]): A = inst.empty"),
            "<quasiquotes-generated:c061-empty>"
          )
          parameterlessRenamed <- DelegatedForwardingMethodPeerBridge.lower(
            parse(
              "def obtain[Element](using evidence: Provider[Element]): Element = evidence.obtain"
            ),
            "<quasiquotes-generated:c061-obtain>"
          )
        yield (canonical, renamed, parameterlessCanonical, parameterlessRenamed)

      lowered match
        case Left(problem) => report.error(s"${problem.code}: ${problem.detail}")
        case Right((canonical, renamed, parameterlessCanonical, parameterlessRenamed)) =>
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              tree match
                case module: untpd.ModuleDef =>
                  module.impl.body.collectFirst {
                    case method: untpd.DefDef => method.name.toString
                  } match
                    case Some("canonicalResult") =>
                      untpd.cpy.ModuleDef(module)(
                        module.name,
                        untpd.cpy.Template(module.impl)(
                          module.impl.constr,
                          module.impl.parentsOrDerived,
                          module.impl.derived,
                          module.impl.self,
                          module.impl.body :+ canonical.tree
                        )
                      )
                    case Some("renamedResult") =>
                      untpd.cpy.ModuleDef(module)(
                        module.name,
                        untpd.cpy.Template(module.impl)(
                          module.impl.constr,
                          module.impl.parentsOrDerived,
                          module.impl.derived,
                          module.impl.self,
                          module.impl.body :+ renamed.tree
                        )
                      )
                    case Some("parameterlessCanonicalResult") =>
                      untpd.cpy.ModuleDef(module)(
                        module.name,
                        untpd.cpy.Template(module.impl)(
                          module.impl.constr,
                          module.impl.parentsOrDerived,
                          module.impl.derived,
                          module.impl.self,
                          module.impl.body :+ parameterlessCanonical.tree
                        )
                      )
                    case Some("parameterlessRenamedResult") =>
                      untpd.cpy.ModuleDef(module)(
                        module.name,
                        untpd.cpy.Template(module.impl)(
                          module.impl.constr,
                          module.impl.parentsOrDerived,
                          module.impl.derived,
                          module.impl.self,
                          module.impl.body :+ parameterlessRenamed.tree
                        )
                      )
                    case _ => super.transform(tree)
                case _ => super.transform(tree)

          summon[Context].compilationUnit.untpdTree =
            transformer.transform(summon[Context].compilationUnit.untpdTree)
          val results =
            Vector(canonical, renamed, parameterlessCanonical, parameterlessRenamed)
          evidence.generatedSources = results.map(_.generatedSource)
          evidence.beforeTyperInsertionReady = results.forall(result =>
            allTrees(result.tree).forall(tree =>
              tree.source.exists &&
                tree.source.path == result.virtualSourceName &&
                tree.span.exists &&
                tree.symbol == NoSymbol &&
                !tree.isInstanceOf[untpd.TypedSplice]
            )
          )

  private def parse(source: String): Defn.Def =
    Scala3(source).parse[Stat].get.asInstanceOf[Defn.Def]

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs)
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[DelegatedForwardingMethodPeerBridge.type],
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
