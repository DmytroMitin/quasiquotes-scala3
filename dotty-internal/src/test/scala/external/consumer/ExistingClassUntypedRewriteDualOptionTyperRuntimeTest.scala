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
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite
import quasiquotes.types.TypeNormalForm

final class ExistingClassUntypedRewriteDualOptionTyperRuntimeTest extends munit.FunSuite:
  import DualOptionPublicFixture.*

  test("foreign public one-plan rewrites reach ordinary Typer class TASTy and runtime in every meaningful order"):
    withTemporary { (sourceFile, output) =>
      Files.writeString(sourceFile, program, StandardCharsets.UTF_8)
      val args = Array("-classpath", compilationClasspath, "-d", output.toString, sourceFile.toString)
      val baseline = new Driver().process(args)
      assert(baseline.hasErrors, "source calls must require both Option signatures to be rewritten")
      val baselineErrors = baseline.allErrors.toVector.map(_.message)
      assert(baselineErrors.exists(_.contains("Some")), clues(baselineErrors))
      deleteRecursively(output)
      Files.createDirectories(output)
      val driver = new RewriteDriver
      val reporter = driver.process(args)
      assert(!reporter.hasErrors, clues(reporter.allErrors.toVector.map(_.message)))
      assertEquals(driver.transactions.sorted, ownerNames.sorted)
      assert(driver.preTyperClean)
      val files = emitted(output)
      ownerNames.foreach { name =>
        assert(files.exists(_.endsWith(name + ".class")), clues(name, files))
        assert(files.exists(_.endsWith(name + ".tasty")), clues(name, files))
      }
      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("dual.runtime.Use$")
        val module = moduleClass.getField("MODULE$").get(null)
        val expected: Vector[(String, Any)] = Vector(
          "firstOnly" -> (Some("a"), "b"), "secondOnly" -> ("a", Some("b")),
          "mixed" -> (Some("a"), 7), "aliases" -> (Some("a"), Some("b")),
          "localShadows" -> (Some("a"), Some("b"))
        ) ++ orders.indices.map(i => s"both$i" -> (Some("a"), Some("b")))
        expected.foreach { (method, wanted) =>
          assertEquals(moduleClass.getMethod(method).invoke(module), wanted, clues(method))
        }
      finally loader.close()
    }

  test("incompatible unchanged body is rejected by ordinary Typer after a successful public transaction"):
    withTemporary { (sourceFile, output) =>
      val text = """package dual.runtime
        |class Broken:
        |  def first(value: String): String = value.trim
        |  def second(other: String): String = other
        |object BrokenUse:
        |  def value = new Broken().first(Some("a"))
        |""".stripMargin
      Files.writeString(sourceFile, text, StandardCharsets.UTF_8)
      val driver = new RewriteDriver
      val reporter = driver.process(Array("-classpath", compilationClasspath, "-d", output.toString, sourceFile.toString))
      assertEquals(driver.transactions, Vector("Broken"))
      assert(driver.preTyperClean)
      assert(reporter.hasErrors)
      assert(reporter.allErrors.exists(_.message.contains("trim")), clues(reporter.allErrors.map(_.message)))
      assert(!emitted(output).exists(_.endsWith("Broken.tasty")))
    }

  private val ownerNames = Vector("FirstOnly", "SecondOnly", "Mixed", "Aliases", "LocalShadows") ++
    orders.indices.map(i => s"Both$i")

  private final class RewriteDriver extends Driver:
    var transactions = Vector.empty[String]
    var preTyperClean = true
    override protected def newCompiler(using Context): Compiler = new Compiler:
      override protected def frontendPhases: List[List[Phase]] =
        List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) :: super.frontendPhases.tail

  private final class RewriteBeforeTyper(driver: RewriteDriver) extends Phase:
    def phaseName: String = "dualCanonicalOptionPublicOwnerRewrite"
    override def isCheckable: Boolean = false
    protected def run(using Context): Unit =
      val mapper = new untpd.UntypedTreeMap:
        override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match
          case root: untpd.TypeDef if ownerNames.contains(root.name.toString) || root.name.toString == "Broken" =>
            val c = right(ExistingClassUntypedRewrite.capture(root))
            val name = root.name.toString
            val order = name match
              case "FirstOnly" => Vector(0, 1, 2)
              case "SecondOnly" => Vector(3, 4, 5)
              case "Broken" => Vector(0, 1, 3, 4, 5) // Keep incompatible first RHS for Typer.
              case n if n.startsWith("Both") => orders(n.stripPrefix("Both").toInt)
              case _ => orders.head
            val p = plan(c, order, if name == "Mixed" then TypeNormalForm.STypeIdent("Int") else canonical)
            val original = members(root)
            val before = snapshot(root)
            val result = right(ExistingClassUntypedRewrite(c, p)) // One apply, never per-method applies.
            assertEquals(snapshot(root), before)
            assert(!result.tree.eq(root))
            assert(!result.tree.rhs.eq(root.rhs))
            val fresh = members(result.tree)
            assertEquals(fresh.size, original.size)
            c.members.indices.foreach { i =>
              val selected = c.members(i).diagnosticName.exists { n =>
                (n == "first" && order.exists(_ < 3)) || (n == "second" && order.exists(_ >= 3))
              }
              assertEquals(fresh(i).eq(original(i)), !selected)
              assertEquals(result.directMemberIdentities(i).sameObjectAs(c.members(i).identity), !selected)
            }
            val ms = rawMethods(result.tree)
            if order.exists(_ < 3) then
              assert(canonicalShape(param(ms(0)).tpt), clues(param(ms(0)).tpt))
              assert(canonicalShape(ms(0).tpt), clues(ms(0).tpt))
            if order.exists(_ >= 3) && name != "Mixed" then
              assert(canonicalShape(param(ms(1)).tpt), clues(param(ms(1)).tpt))
              assert(canonicalShape(ms(1).tpt), clues(ms(1).tpt))
            driver.transactions :+= name
            driver.preTyperClean &&= nodes(result.tree).forall(t => t.symbol == NoSymbol && !t.isInstanceOf[untpd.TypedSplice])
            result.tree
          case other => super.transform(other)
      val unit = summon[Context].compilationUnit
      unit.untpdTree = mapper.transform(unit.untpdTree)

  private def owner(name: String, extras: String = ""): String =
    s"""class $name {
       |  $extras
       |  val untouchedBefore: Int = 1
       |  def first(value: String): String = value
       |  def second(other: String): String = other
       |  val untouchedAfter: Int = 2
       |}
       |""".stripMargin

  private def program: String =
    val classes = owner("FirstOnly") + owner("SecondOnly") + owner("Mixed") +
      orders.indices.map(i => owner(s"Both$i")).mkString +
      owner("LocalShadows", "type Option[A] = List[A]; type String = Int; object scala { type Option[A] = List[A] }")
    val uses = orders.indices.map(i => s"def both$i = { val s = new Both$i(); (s.first(Some(\"a\")), s.second(Some(\"b\"))) }").mkString("\n  ")
    s"""package dual.runtime {
       |$classes
       |object Use {
       |  def firstOnly = { val s = new FirstOnly(); (s.first(Some("a")), s.second("b")) }
       |  def secondOnly = { val s = new SecondOnly(); (s.first("a"), s.second(Some("b"))) }
       |  def mixed = { val s = new Mixed(); (s.first(Some("a")), s.second(7)) }
       |  def localShadows = { val s = new LocalShadows(); (s.first(Some("a")), s.second(Some("b"))) }
       |  def aliases = { val s = new alias.scope.Aliases(); (s.first(Some("a")), s.second(Some("b"))) }
       |  $uses
       |}
       |}
       |package alias.definitions {
       |object Types { type Option[A] = List[A]; type String = Int }
       |object Names { object scala { type Option[A] = List[A] } }
       |}
       |package alias.scope {
       |import alias.definitions.Types.{Option, String}
       |import alias.definitions.Names.scala
       |${owner("Aliases")}
       |}
       |""".stripMargin

  private def compilationClasspath: String =
    Vector(classOf[scala.Option[?]], classOf[scala.deriving.Mirror], classOf[Compiler],
      classOf[ExistingClassUntypedRewrite.type], getClass).flatMap(c =>
        Option(c.getProtectionDomain).flatMap(d => Option(d.getCodeSource)).map(_.getLocation.toURI)
      ).map(Path.of(_).toString).distinct.mkString(java.io.File.pathSeparator)
  private def emitted(root: Path): Vector[String] =
    val stream = Files.walk(root)
    try stream.filter(Files.isRegularFile(_)).iterator.asScala.map(_.toString).toVector
    finally stream.close()
  private def withTemporary(body: (Path, Path) => Unit): Unit =
    val root = Files.createTempDirectory("dual-option-public-")
    try
      val output = Files.createDirectory(root.resolve("classes"))
      body(root.resolve("DualOption.scala"), output)
    finally deleteRecursively(root)
  private def deleteRecursively(root: Path): Unit =
    if Files.exists(root) then
      val stream = Files.walk(root)
      try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally stream.close()
