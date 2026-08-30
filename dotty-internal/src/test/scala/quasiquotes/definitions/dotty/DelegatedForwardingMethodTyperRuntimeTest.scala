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
  test("positioned canonical and renamed bridges survive ordinary Typer and execute") {
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
          "def render[Element](value: Element)(using evidence: Display[Element]): Text = evidence.render(value)"
        )
      )
      assert(driver.beforeTyperInsertionReady)

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
        yield canonical -> renamed

      lowered match
        case Left(problem) => report.error(s"${problem.code}: ${problem.detail}")
        case Right((canonical, renamed)) =>
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
                    case _ => super.transform(tree)
                case _ => super.transform(tree)

          summon[Context].compilationUnit.untpdTree =
            transformer.transform(summon[Context].compilationUnit.untpdTree)
          val results = Vector(canonical, renamed)
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
