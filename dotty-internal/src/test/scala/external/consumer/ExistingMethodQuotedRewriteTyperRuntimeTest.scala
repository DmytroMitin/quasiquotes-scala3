package external.consumer

import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.concurrent.duration.*
import dotty.tools.dotc.{Compiler, Driver}
import dotty.tools.dotc.ast.{untpd,tpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser

/** All target identity observations below come from ordinary Typer/runtime. */
final class ExistingMethodQuotedRewriteTyperRuntimeTest extends munit.FunSuite:
  override val munitTimeout = 120.seconds
  import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite
  import ExistingClassUntypedRewrite.*

  Vector("unshadowed", "owner", "importAlias", "inherited", "enclosing", "local", "pathSegments").foreach { environment =>
    test(s"$environment lexical and genuinely rooted calls compile emit TASTy and execute"):
      val temp = Files.createTempDirectory("quoted-rooted-atomic-")
      try
        val source = temp.resolve("Contexts.scala")
        val output = Files.createDirectory(temp.resolve("classes"))
        val args = Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
        // Control: unchanged AnyVal methods cannot satisfy the downstream Int use.
        Files.writeString(source, program(environment, "original"))
        val baseline = new Driver().process(args)
        assert(baseline.hasErrors)
        assert(baseline.allErrors.exists(_.message.contains("AnyVal")))
        Vector("lexicalSource", "rootedSource", "publicRewrite").foreach { mode =>
          delete(output); Files.createDirectory(output)
          Files.writeString(source, program(environment, mode))
          val driver = new ContextDriver(mode)
          val reporter = driver.process(args)
          assert(!reporter.hasErrors, clues(environment,mode,reporter.allErrors.map(_.message)))
          if mode == "publicRewrite" then
            assertEquals(driver.rewritten.sorted, Vector("Echo", "Magnitude"))
            assertEquals(driver.contexts.size, 2)
            assert(driver.clean)

          val files = emitted(output)
          Vector("Echo", "Magnitude").foreach { name =>
            assert(files.exists(p => p.contains(name) && p.endsWith(".class")), clues(files))
            // Local classes' TASTy belongs to their enclosing compilation class.
            if environment != "local" && environment != "enclosing" then assert(files.exists(_.endsWith(name+".tasty")), clues(files))
          }
          assert(files.exists(_.endsWith("Use.tasty")), clues(files))
          if environment == "enclosing" then assert(files.exists(_.endsWith("Container.tasty")),clues(files))
          val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
          try
            val cls = loader.loadClass("qualifier.runtime.Use$")
            val module = cls.getField("MODULE$").get(null)
            val canonical = mode.startsWith("rooted") || mode == "publicRewrite" || environment == "unshadowed" || environment == "pathSegments"
            val expected = if canonical then Vector(11,17) else Vector(99,99)
            val actual = Vector("echoValue","magnitudeValue").map(n => cls.getMethod(n).invoke(module).asInstanceOf[Integer].intValue)
            assertEquals(actual,expected,clues(environment,mode))
            val expectedTargets = if canonical then Vector("java.lang.Math","java.lang.StrictMath") else Vector.fill(2)("qualifier.runtime.LocalAbs")
            assertEquals(driver.targets.sorted,expectedTargets.sorted,clues(environment,mode))
            assertEquals(cls.getMethod("siblings").invoke(module), Integer.valueOf(10))
            println(s"PUBLIC_QUOTED_MATRIX environment=$environment mode=$mode runtime=${actual.mkString("/")} contexts=${driver.contexts.mkString(";")} targets=${driver.targets.mkString(",")}")
          finally loader.close()
        }
      finally delete(temp)
  }

  private final class ContextDriver(mode: String) extends Driver:
    var rewritten = Vector.empty[String]
    var contexts = Vector.empty[String]
    var clean = true
    var targets = Vector.empty[String]
    override protected def newCompiler(using Context): Compiler = new Compiler:
      override protected def frontendPhases: List[List[Phase]] =
        val phases = if mode == "publicRewrite" then
          List(new Parser) :: List(new Rewrite(ContextDriver.this)) :: super.frontendPhases.tail
        else super.frontendPhases
        phases :+ List(new ObserveTargets(ContextDriver.this))

  private final class ObserveTargets(driver: ContextDriver) extends Phase:
    def phaseName: String = "observeActualTypedCallTargets"
    override def isCheckable: Boolean = false
    protected def run(using Context): Unit =
      val traverser = new tpd.TreeTraverser:
        def traverse(tree: tpd.Tree)(using Context): Unit =
          tree match
            case a: tpd.Apply if a.fun.symbol != NoSymbol && a.fun.symbol.name.toString == "abs" =>
              driver.targets :+= a.fun.symbol.owner.fullName.toString.stripSuffix("$")
            case _ => ()
          traverseChildren(tree)
      traverser.traverse(summon[Context].compilationUnit.tpdTree)

  private final class Rewrite(driver: ContextDriver) extends Phase:
    def phaseName: String = "quotedQualifierContextProof"
    override def isCheckable: Boolean = false
    protected def run(using Context): Unit =
      val mapper = new untpd.UntypedTreeMap:
        override def transform(tree: untpd.Tree)(using ctx: Context): untpd.Tree = tree match
          case root: untpd.TypeDef if Set("Echo", "Magnitude")(root.name.toString) =>
            val name = if root.name.toString == "Echo" then "echo" else "magnitude"
            val capture = right(ExistingClassUntypedRewrite.capture(root))
            val member = capture.members.find(_.diagnosticName.contains(name)).get
            val result = right(U080PublicRecipe.rewrite(root,name))
            driver.contexts :+= "public-only"
            assert(!result.tree.eq(root))
            capture.members.filterNot(_.index == member.index).foreach { sibling =>
              assert(result.directMemberIdentities(sibling.index).sameObjectAs(sibling.identity))
            }
            driver.clean &&= result.tree.symbol == NoSymbol
            driver.rewritten :+= root.name.toString
            result.tree
          case other => super.transform(other)
      val unit = summon[Context].compilationUnit
      unit.untpdTree = mapper.transform(unit.untpdTree)

  private def program(environment: String, mode: String): String =
    val original = mode == "original" || mode == "publicRewrite"
    val tpe = if original then "AnyVal" else "Int"
    val prefix = if mode == "rootedSource" then "_root_.java.lang." else ""
    val echo = if original then "value" else s"${prefix}Math.abs(value)"
    val magnitude = if original then "number" else s"${prefix}StrictMath.abs(number)"
    val fields = "val Math: LocalAbs = new LocalAbs; val StrictMath: LocalAbs = new LocalAbs;"
    val own = if environment == "owner" then fields else if environment == "pathSegments" then "val java: LocalAbs = new LocalAbs; val lang: LocalAbs = new LocalAbs;" else ""
    val parent = if environment == "inherited" then " extends Parent" else ""
    val classes = s"""class Echo$parent { $own val before: Int = 1;
      |def echo(value: $tpe): $tpe = $echo; val after: Int = 4 }
      |class Magnitude$parent { $own val before: Int = 2;
      |def magnitude(number: $tpe): $tpe = $magnitude; val after: Int = 3 }
      |""".stripMargin
    val methods = """def echoValue: Int = new Echo().echo(-11)
      |def magnitudeValue: Int = new Magnitude().magnitude(-17)
      |def siblings: Int = { val e = new Echo(); val m = new Magnitude(); e.before + e.after + m.before + m.after }
      |""".stripMargin
    val body = environment match
      case "enclosing" => s"object Container { $fields $classes }; object Use { import Container.*; $methods }"
      case "local" => s"""object Use {
        |def values: (Int,Int,Int) = { $fields $classes
        |val e = new Echo(); val m = new Magnitude(); (e.echo(-11),m.magnitude(-17),e.before+e.after+m.before+m.after) }
        |def echoValue: Int = values._1
        |def magnitudeValue: Int = values._2
        |def siblings: Int = values._3 }
        |""".stripMargin
      case _ => s"$classes object Use { $methods }"
    val imports = if environment == "importAlias" then "object Aliases { val math: LocalAbs = new LocalAbs; val strict: LocalAbs = new LocalAbs }; import Aliases.{math => Math, strict => StrictMath};" else ""
    s"package qualifier.runtime\nclass LocalAbs { def abs(value: Int): Int = 99 }; class Parent { $fields }; $imports\n$body"

  private def right[E,A](v: Either[E,A]): A = v.fold(p => fail(p.toString),identity)
  private def compilationClasspath: String =
    Vector(classOf[scala.Option[?]],classOf[scala.deriving.Mirror],classOf[Compiler],classOf[ExistingClassUntypedRewrite.type],getClass)
      .flatMap(c => Option(c.getProtectionDomain).flatMap(d => Option(d.getCodeSource)).map(_.getLocation.toURI))
      .map(Path.of(_).toString).distinct.mkString(java.io.File.pathSeparator)
  private def emitted(root: Path): Vector[String] =
    val stream = Files.walk(root)
    try stream.filter(Files.isRegularFile(_)).iterator.asScala.map(_.toString).toVector
    finally stream.close()
  private def delete(root: Path): Unit =
    if Files.exists(root) then
      val stream = Files.walk(root)
      try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally stream.close()
