package quasiquotes.definitions.dotty

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser

/** Counterexample: bound-argument proof is not a free-qualifier declaration certificate. */
final class QuotedExistingMethodQualifierAuthorityGapTest extends munit.FunSuite:
  import ExistingClassUntypedRewrite.*

  test("own parameter proof does not prevent free Math and StrictMath from resolving to owner shadows"):
    val temporary = Files.createTempDirectory("quoted-existing-method-runtime-")
    try
      val source = temporary.resolve("QuotedServices.scala")
      val output = Files.createDirectory(temporary.resolve("classes"))
      Files.writeString(source, program, StandardCharsets.UTF_8)
      val args = Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      val baseline = new Driver().process(args)
      assert(baseline.hasErrors, "Int result assignments must fail against the original AnyVal signatures")
      val baselineErrors = baseline.allErrors.toVector.map(_.message)
      assert(baselineErrors.exists(_.contains("AnyVal")), clues(baselineErrors))
      deleteRecursively(output)
      Files.createDirectories(output)
      val driver = new QuoteRewriteDriver
      val reporter = driver.process(args)
      assert(!reporter.hasErrors, clues(reporter.allErrors.toVector.map(_.message)))
      assertEquals(driver.rewritten.sorted, Vector("Magnitudes", "Service"))
      assertEquals(driver.transactions, 2) // One oracle/public owner transaction per independent class.
      assert(driver.preTyperClean)
      val files = emitted(output)
      Vector("Service", "Magnitudes").foreach { name =>
        assert(files.exists(_.endsWith(name + ".class")), clues(name, files))
        assert(files.exists(_.endsWith(name + ".tasty")), clues(name, files))
      }
      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("quoted.runtime.Use$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("echoValue").invoke(module), Integer.valueOf(99))
        assertEquals(moduleClass.getMethod("magnitudeValue").invoke(module), Integer.valueOf(99))
        assertEquals(moduleClass.getMethod("siblings").invoke(module), Integer.valueOf(10))
      finally loader.close()
    finally deleteRecursively(temporary)

  private final class QuoteRewriteDriver extends Driver:
    var rewritten = Vector.empty[String]
    var transactions = 0
    var preTyperClean = true
    override protected def newCompiler(using Context): Compiler = new Compiler:
      override protected def frontendPhases: List[List[Phase]] =
        List(new Parser) :: List(new RewriteBeforeTyper(QuoteRewriteDriver.this)) :: super.frontendPhases.tail

  private final class RewriteBeforeTyper(driver: QuoteRewriteDriver) extends Phase:
    def phaseName: String = "quotedExistingMethodAuthorityModel"
    override def isCheckable: Boolean = false
    protected def run(using Context): Unit =
      val mapper = new untpd.UntypedTreeMap:
        override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match
          case root: untpd.TypeDef if Set("Service", "Magnitudes")(root.name.toString) =>
            val selectedName = if root.name.toString == "Service" then "echo" else "magnitude"
            val originalMembers = members(root)
            val capture = right(ExistingClassUntypedRewrite.capture(root))
            val selected = capture.members.filter(_.diagnosticName.contains(selectedName))
            assertEquals(selected.size, 1)
            val index = selected.head.index
            val originalMethod = originalMembers(index).asInstanceOf[untpd.DefDef]
            val originalParameter = originalMethod.paramss.head.head.asInstanceOf[untpd.ValDef]
            val originalSnapshot = snapshot(root)
            // NONPUBLIC feasibility seam. Its implementation builds the real quote,
            // proves the output binder, and applies exactly one unchanged public plan.
            val result = right(QuotedExistingMethodAuthorityModel.rewrite(root, selectedName))
            driver.transactions += 1
            assertEquals(snapshot(root), originalSnapshot)
            assert(result.changed)
            assert(!result.tree.eq(root))
            assert(!result.tree.rhs.eq(root.rhs))
            assertEquals(result.tree.source, root.source)
            assertEquals(result.tree.span, root.span)
            assertEquals(result.tree.rhs.source, root.rhs.source)
            assertEquals(result.tree.rhs.span, root.rhs.span)
            val freshMembers = members(result.tree)
            assertEquals(freshMembers.size, originalMembers.size)
            assertEquals(freshMembers.map(_.asInstanceOf[untpd.MemberDef].name.toString),
              originalMembers.map(_.asInstanceOf[untpd.MemberDef].name.toString))
            originalMembers.indices.foreach { i =>
              assertEquals(freshMembers(i).eq(originalMembers(i)), i != index)
              assertEquals(result.directMemberIdentities(i).sameObjectAs(capture.members(i).identity), i != index)
            }
            val freshMethod = freshMembers(index).asInstanceOf[untpd.DefDef]
            val freshParameter = freshMethod.paramss.head.head.asInstanceOf[untpd.ValDef]
            assert(!freshParameter.eq(originalParameter))
            assertEquals(freshMethod.source, originalMethod.source)
            assertEquals(freshMethod.span, originalMethod.span)
            assertEquals(freshParameter.source, originalParameter.source)
            assertEquals(freshParameter.span, originalParameter.span)
            assertEquals(freshParameter.tpt.source, originalParameter.tpt.source)
            assertEquals(freshParameter.tpt.span, originalParameter.tpt.span)
            assertEquals(freshMethod.tpt.source, originalMethod.tpt.source)
            assertEquals(freshMethod.tpt.span, originalMethod.tpt.span)
            assertEquals(freshMethod.rhs.source, originalMethod.rhs.source)
            assertEquals(freshMethod.rhs.span, originalMethod.rhs.span)
            driver.preTyperClean &&= nodes(result.tree).forall(t => t.symbol == NoSymbol && !t.isInstanceOf[untpd.TypedSplice])
            driver.rewritten :+= root.name.toString
            result.tree
          case other => super.transform(other)
      val unit = summon[Context].compilationUnit
      unit.untpdTree = mapper.transform(unit.untpdTree)

  private val program = """package quoted.runtime
    |class LocalAbs:
    |  def abs(value: Int): Int = 99
    |class Service:
    |  val Math: LocalAbs = new LocalAbs
    |  val before: Int = 1
    |  def echo(value: AnyVal): AnyVal = value
    |  val after: Int = 2
    |class Magnitudes:
    |  val StrictMath: LocalAbs = new LocalAbs
    |  def magnitude(number: AnyVal): AnyVal = number
    |  val untouched: Int = 7
    |object Use:
    |  def echoValue: Int = new Service().echo(-11)
    |  def magnitudeValue: Int = new Magnitudes().magnitude(-17)
    |  def siblings: Int =
    |    val service = new Service()
    |    service.before + service.after + new Magnitudes().untouched
    |""".stripMargin

  private def right[A](result: Either[Failure, A]): A = result.fold(p => fail(p.message), identity)
  private def members(root: untpd.TypeDef)(using Context): List[untpd.Tree] = root.rhs.asInstanceOf[untpd.Template].body
  private def nodes(tree: untpd.Tree): Vector[untpd.Tree] =
    def visit(value: Any): Vector[untpd.Tree] = value match
      case t: untpd.Tree => Vector(t) ++ t.productIterator.toVector.flatMap(visit)
      case xs: List[?] => xs.toVector.flatMap(visit)
      case _ => Vector.empty
    visit(tree)
  private def snapshot(tree: untpd.Tree): Vector[(untpd.Tree, Any, Any)] = nodes(tree).map(t => (t, t.source, t.span))
  private def compilationClasspath: String =
    Vector(classOf[scala.Option[?]], classOf[scala.deriving.Mirror], classOf[Compiler],
      classOf[ExistingClassUntypedRewrite.type], getClass).flatMap(c =>
        Option(c.getProtectionDomain).flatMap(d => Option(d.getCodeSource)).map(_.getLocation.toURI)
      ).map(Path.of(_).toString).distinct.mkString(java.io.File.pathSeparator)
  private def emitted(root: Path): Vector[String] =
    val stream = Files.walk(root)
    try stream.filter(Files.isRegularFile(_)).iterator.asScala.map(_.toString).toVector
    finally stream.close()
  private def deleteRecursively(root: Path): Unit =
    if Files.exists(root) then
      val stream = Files.walk(root)
      try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally stream.close()
