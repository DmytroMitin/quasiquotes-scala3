package external.consumer

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser

import quasiquotes.parser.TermShape
import quasiquotes.terms.dotty.{TermGeneratedOriginLowering, TermUntypedLowering}

final class CompletedMulticlauseApplyPublicFacadesTest extends munit.FunSuite:
  private final case class Fixture(
      source: String,
      shape: TermShape,
      topology: String,
      clauseArities: List[Int]
  )

  private val fixtures = Vector(
    Fixture("renamed", ident("renamed"), "Ident(renamed)", Nil),
    Fixture("renamed()", apply(ident("renamed")), "Apply(Ident(renamed),[])", List(0)),
    Fixture("renamed(left)", apply(ident("renamed"), ident("left")), "Apply(Ident(renamed),[Ident(left)])", List(1)),
    Fixture(
      "combine(left)(right)",
      apply(apply(ident("combine"), ident("left")), ident("right")),
      "Apply(Apply(Ident(combine),[Ident(left)]),[Ident(right)])",
      List(1, 1)
    ),
    Fixture(
      "empty()(right)",
      apply(apply(ident("empty")), ident("right")),
      "Apply(Apply(Ident(empty),[]),[Ident(right)])",
      List(0, 1)
    ),
    Fixture(
      "combine(left)()(middle, right)",
      apply(apply(apply(ident("combine"), ident("left"))), ident("middle"), ident("right")),
      "Apply(Apply(Apply(Ident(combine),[Ident(left)]),[]),[Ident(middle),Ident(right)])",
      List(1, 0, 2)
    )
  )

  test("public source-free lowering preserves 0/1/2/3 clause topology, empties, and freshness"):
    withContext:
      fixtures.foreach: fixture =>
        val first = lowerSourceFree(fixture.shape)
        val second = lowerSourceFree(fixture.shape)
        assertEquals(topology(first), fixture.topology, clues(fixture.source))
        assertEquals(rawClauseArities(first), fixture.clauseArities, clues(fixture.source))
        assertSourceFree(first)
        assertFresh(first, second)

      val flat = lowerSourceFree(apply(ident("combine"), ident("left"), ident("right")))
      val nested = lowerSourceFree(apply(apply(ident("combine"), ident("left")), ident("right")))
      assertEquals(rawClauseArities(flat), List(2))
      assertEquals(rawClauseArities(nested), List(1, 1))
      assertNotEquals(topology(flat), topology(nested))

  test("public source-free lowering admits nested Apply under existing composite contexts"):
    withContext:
      val nested = apply(apply(ident("combine"), ident("left")), ident("right"))
      val contexts = List(
        TermShape.Tuple(List(nested, literal("0"))) ->
          "Tuple([Apply(Apply(Ident(combine),[Ident(left)]),[Ident(right)]),Number(0)])",
        TermShape.If(ident("ready"), nested, literal("0")) ->
          "If(Ident(ready),Apply(Apply(Ident(combine),[Ident(left)]),[Ident(right)]),Number(0))",
        TermShape.Typed(nested, "Int") ->
          "Typed(Apply(Apply(Ident(combine),[Ident(left)]),[Ident(right)]),Ident(Int))",
        TermShape.Block(List(nested), literal("0")) ->
          "Block([Apply(Apply(Ident(combine),[Ident(left)]),[Ident(right)])],Number(0))"
      )
      contexts.foreach {
        case (shape, expected) =>
          val raw = lowerSourceFree(shape)
          assertEquals(topology(raw), expected, clues(shape))
          assertSourceFree(raw)
      }

  test("public generated-origin lowering preserves deterministic source, spans, identity, and freshness"):
    withContext:
      fixtures.zipWithIndex.foreach: (fixture, index) =>
        val path = s"generated/c067-$index.scala"
        val first = lowerGenerated(fixture.shape, path)
        val second = lowerGenerated(fixture.shape, path)
        assertEquals(first.generatedSource, fixture.source, clues(fixture.source))
        assertEquals(first.virtualSourceName, path, clues(fixture.source))
        assertEquals(first.sourceFile.path, path, clues(fixture.source))
        assertEquals(first.sourceFile.content.mkString, fixture.source, clues(fixture.source))
        assertEquals(topology(first.tree), fixture.topology, clues(fixture.source))
        assertEquals(rawClauseArities(first.tree), fixture.clauseArities, clues(fixture.source))
        assertEquals(spanSnapshot(first.tree), spanSnapshot(second.tree), clues(fixture.source))
        assertEquals(first.tree.span.start, 0, clues(fixture.source))
        assertEquals(first.tree.span.end, fixture.source.length, clues(fixture.source))
        allTrees(first.tree).foreach: node =>
          assert(node.source eq first.sourceFile, clues(fixture.source, node))
          assert(node.span.exists, clues(fixture.source, node))
          assert(node.span.start >= 0, clues(fixture.source, node))
          assert(node.span.start <= node.span.point, clues(fixture.source, node))
          assert(node.span.point <= node.span.end, clues(fixture.source, node))
          assert(node.span.end <= fixture.source.length, clues(fixture.source, node))
          assertEquals(node.symbol, NoSymbol, clues(fixture.source, node))
          assert(!node.isInstanceOf[untpd.TypedSplice], clues(fixture.source, node))
        assert(!(first.sourceFile eq second.sourceFile), clues(fixture.source))
        assertFresh(first.tree, second.tree)

  test("newly traversable nested Apply retains existing malformed, policy, and origin failures"):
    withContext:
      val malformed = apply(apply(ident("combine"), ident("left")), null)
      assertEquals(sourceFailure(malformed).code, "MALFORMED_SEMANTIC_VALUE")
      assertEquals(generatedFailure(malformed, "generated/C067.scala").code, "MALFORMED_SEMANTIC_VALUE")

      val unsupportedOperator = apply(
        apply(TermShape.Infix(literal("1"), "custom", literal("2")), literal("3")),
        literal("4")
      )
      assertEquals(sourceFailure(unsupportedOperator).code, "UNSUPPORTED_SEMANTIC_VALUE")
      assertEquals(generatedFailure(unsupportedOperator, "generated/C067.scala").code, "UNSUPPORTED_SEMANTIC_VALUE")

      val unsupportedChild = apply(
        apply(ident("combine"), TermShape.Unsupported("future", "not admitted")),
        literal("4")
      )
      assertEquals(sourceFailure(unsupportedChild).code, "UNSUPPORTED_SEMANTIC_VALUE")

      val invalidOriginName = apply(apply(ident("bad.name"), literal("1")), literal("2"))
      assert(TermUntypedLowering.lower(invalidOriginName).isRight)
      assertEquals(generatedFailure(invalidOriginName, "generated/C067.scala").code, "UNSUPPORTED_SEMANTIC_VALUE")

      val signedReceiver = apply(
        apply(TermShape.Select(literal("-1"), "abs"), literal("1")),
        literal("2")
      )
      assert(TermUntypedLowering.lower(signedReceiver).isRight)
      assertEquals(generatedFailure(signedReceiver, "generated/C067.scala").code, "UNSUPPORTED_SEMANTIC_VALUE")

      assertEquals(generatedFailure(malformed, null).code, "MISSING_INPUT")
      assertEquals(generatedFailure(malformed, " bad.scala").code, "INVALID_VIRTUAL_SOURCE")

  test("public generated-origin nested Apply survives ordinary Typer, class/TASTy emission, and runtime"):
    val temporary = Files.createTempDirectory("c067-public-multiclause-")
    try
      val source = temporary.resolve("C067PublicMulticlause.scala")
      val output = Files.createDirectory(temporary.resolve("classes"))
      Files.writeString(
        source,
        """object C067PublicMulticlause:
          |  def combine(left: Int)(right: Int): Int = left + right
          |  def empty()(right: Int): Int = right + 1
          |  def result: (Int, Int) = (0, 0)
          |""".stripMargin,
        StandardCharsets.UTF_8
      )
      val driver = new RuntimeDriver
      val reporter = driver.process(Array("-classpath", compilationClasspath, "-d", output.toString, source.toString))
      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(driver.generatedSource, Some("(combine(20)(22), empty()(41))"))
      assertEquals(driver.topology, Some(
        "Tuple([Apply(Apply(Ident(combine),[Number(20)]),[Number(22)]),Apply(Apply(Ident(empty),[]),[Number(41)])])"
      ))
      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith(".class")), clues(emitted))
      assert(emitted.exists(_.toString.endsWith(".tasty")), clues(emitted))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val clazz = loader.loadClass("C067PublicMulticlause$")
        val module = clazz.getField("MODULE$").get(null)
        val value = clazz.getMethod("result").invoke(module).asInstanceOf[Product]
        assertEquals(value.productElement(0), Integer.valueOf(42))
        assertEquals(value.productElement(1), Integer.valueOf(42))
      finally loader.close()
    finally deleteRecursively(temporary)

  private final class RuntimeDriver extends Driver:
    @volatile var generatedSource: Option[String] = None
    @volatile var topology: Option[String] = None

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new InsertGenerated(RuntimeDriver.this)) :: super.frontendPhases.tail

  private final class InsertGenerated(evidence: RuntimeDriver) extends Phase:
    def phaseName: String = "c067PublicMulticlauseInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      TermGeneratedOriginLowering.lower(runtimeShape, "generated/C067PublicMulticlause.scala") match
        case Left(problem) => report.error(problem.message)
        case Right(result) =>
          evidence.generatedSource = Some(result.generatedSource)
          evidence.topology = Some(CompletedMulticlauseApplyPublicFacadesTest.this.topology(result.tree))
          val nodes = allTrees(result.tree)
          if !nodes.forall(node => node.source.eq(result.sourceFile) && node.span.exists && node.symbol == NoSymbol) then
            report.error("generated nested Apply lost source, span, or pre-Typer symbol invariants")
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match
              case method: untpd.DefDef if method.name.toString == "result" =>
                untpd.cpy.DefDef(method)(method.name, method.paramss, method.tpt, result.tree)
              case _ => super.transform(tree)
          summon[Context].compilationUnit.untpdTree = transformer.transform(summon[Context].compilationUnit.untpdTree)

  private def runtimeShape: TermShape =
    TermShape.Tuple(List(
      apply(apply(ident("combine"), literal("20")), literal("22")),
      apply(apply(ident("empty")), literal("41"))
    ))

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def literal(value: String): TermShape =
    TermShape.Literal(value)

  private def apply(function: TermShape, arguments: TermShape*): TermShape =
    TermShape.Apply(function, arguments.toList)

  private def lowerSourceFree(term: TermShape)(using Context): untpd.Tree =
    TermUntypedLowering.lower(term).fold(problem => fail(problem.message), identity)

  private def sourceFailure(term: TermShape)(using Context): TermUntypedLowering.Failure =
    TermUntypedLowering.lower(term).swap.toOption.getOrElse(fail(s"unexpected source-free success: $term"))

  private def lowerGenerated(term: TermShape, path: String)(using Context): TermGeneratedOriginLowering.Lowered =
    TermGeneratedOriginLowering.lower(term, path).fold(problem => fail(problem.message), identity)

  private def generatedFailure(term: TermShape, path: String)(using Context): TermGeneratedOriginLowering.Failure =
    TermGeneratedOriginLowering.lower(term, path).swap.toOption.getOrElse(fail(s"unexpected generated-origin success: $term"))

  private def rawClauseArities(tree: untpd.Tree): List[Int] =
    def loop(current: untpd.Tree, reversed: List[Int]): List[Int] = current match
      case value: untpd.Apply => loop(value.fun, value.args.size :: reversed)
      case _: untpd.Ident => reversed
      case other => fail(s"unexpected completed Apply root: ${topology(other)}")
    loop(tree, Nil)

  private def topology(tree: untpd.Tree): String = tree match
    case untpd.EmptyTree => "Empty"
    case untpd.Ident(name) => s"Ident($name)"
    case untpd.Number(value, untpd.NumberKind.Whole(10)) => s"Number($value)"
    case value: untpd.Apply => s"Apply(${topology(value.fun)},[${value.args.map(topology).mkString(",")}])"
    case value: untpd.Tuple => s"Tuple([${value.trees.map(topology).mkString(",")}])"
    case value: untpd.If => s"If(${topology(value.cond)},${topology(value.thenp)},${topology(value.elsep)})"
    case value: untpd.Typed => s"Typed(${topology(value.expr)},${topology(value.tpt)})"
    case value: untpd.Block => s"Block([${value.stats.map(topology).mkString(",")}],${topology(value.expr)})"
    case other => s"Unexpected(${other.getClass.getSimpleName})"

  private def assertSourceFree(tree: untpd.Tree)(using Context): Unit =
    allTrees(tree).foreach: node =>
      assert(!node.source.exists, clues(node))
      assert(!node.span.exists, clues(node))
      assertEquals(node.symbol, NoSymbol, clues(node))
      assert(!node.isInstanceOf[untpd.TypedSplice], clues(node))

  private def assertFresh(first: untpd.Tree, second: untpd.Tree)(using Context): Unit =
    val left = allTrees(first)
    val right = allTrees(second)
    assertEquals(left.size, right.size)
    left.zip(right).foreach { case (a, b) =>
      assert(!(a eq b), clues(a, b))
    }

  private def spanSnapshot(tree: untpd.Tree)(using Context): Vector[(String, Int, Int, Int)] =
    allTrees(tree).map(node => (node.getClass.getSimpleName, node.span.start, node.span.end, node.span.point))

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree +: (tree match
      case value: untpd.Apply => allTrees(value.fun) ++ value.args.toVector.flatMap(allTrees)
      case value: untpd.Select => allTrees(value.qualifier)
      case value: untpd.Tuple => value.trees.toVector.flatMap(allTrees)
      case value: untpd.If => allTrees(value.cond) ++ allTrees(value.thenp) ++ allTrees(value.elsep)
      case value: untpd.Typed => allTrees(value.expr) ++ allTrees(value.tpt)
      case value: untpd.Block => value.stats.toVector.flatMap(allTrees) ++ allTrees(value.expr)
      case value: untpd.Parens => allTrees(value.t)
      case _ => Vector.empty)

  private def compilationClasspath: String =
    Vector(classOf[scala.Option[?]], classOf[scala.deriving.Mirror], classOf[Compiler], classOf[TermShape], getClass)
      .flatMap(value => Option(value.getProtectionDomain).flatMap(domain => Option(domain.getCodeSource)).map(_.getLocation.toURI))
      .map(Path.of(_).toString)
      .distinct
      .mkString(java.io.File.pathSeparator)

  private def deleteRecursively(root: Path): Unit =
    if Files.exists(root) then
      val stream = Files.walk(root)
      try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally stream.close()

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)
