package external.consumer

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser

import quasiquotes.parser.TermShape
import quasiquotes.terms.{TermParameterSpec, TermShapeBindings}
import quasiquotes.terms.dotty.TermGeneratedOriginLowering
import quasiquotes.types.TypeNormalForm

final class TermGeneratedOriginLoweringConsumerTest extends munit.FunSuite:
  test("public semantic terms retain their origin through pre-Typer transfer and emit usable class and TASTy files"):
    val temporary = Files.createTempDirectory("semantic-term-origin-consumer-")
    try
      val source = temporary.resolve("GeneratedTermConsumers.scala")
      val output = Files.createDirectory(temporary.resolve("classes"))
      Files.writeString(source,
        """object GeneratedTermConsumers:
          |  def ordinary: Int = 0
          |  def signedReceiver: Int = 0
          |  def lambda: Int => Int = identity
          |  def localValue: Int = 0
          |  def localMethod: Int = 0
          |  def unusedLocalMethod: Int = 0
          |""".stripMargin, StandardCharsets.UTF_8)
      val driver = new ConsumerDriver(fixtures)
      val reporter = driver.process(Array("-classpath", compilationClasspath, "-d", output.toString, source.toString))
      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(driver.transferred.toSet, fixtures.map(_._1).toSet)
      assert(Files.isRegularFile(output.resolve("GeneratedTermConsumers$.class")))
      assert(Files.isRegularFile(output.resolve("GeneratedTermConsumers.tasty")))
      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val clazz = loader.loadClass("GeneratedTermConsumers$")
        val module = clazz.getField("MODULE$").get(null)
        for name <- List("ordinary", "signedReceiver", "localValue", "localMethod", "unusedLocalMethod") do
          assertEquals(clazz.getMethod(name).invoke(module), Integer.valueOf(42), clues(name))
        val lambda = clazz.getMethod("lambda").invoke(module).asInstanceOf[Int => Int]
        assertEquals(lambda(41), 42)
      finally loader.close()
    finally
      val stream = Files.walk(temporary)
      try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally stream.close()

  private def fixtures: Vector[(String, TermShape, String)] =
    val int = TypeNormalForm.STypeIdent("Int")
    def plusOne(value: TermShape) = TermShape.Infix(value, "+", TermShape.Literal("1"))
    val lambda = TermShapeBindings.lambda(Vector(TermParameterSpec("x", int))) { scope =>
      scope.reference(scope.parameterBinders.head.head).map(plusOne)
    }.toOption.get
    val value = TermShapeBindings.localValue("x", int, TermShape.Literal("41")) { scope =>
      scope.reference(scope.declaredBinder.get).map(plusOne)
    }.toOption.get
    val method = TermShapeBindings.localMethod("increment", Vector(Vector(TermParameterSpec("x", int))), int) { scope =>
      scope.reference(scope.parameterBinders.head.head).map(plusOne)
    } { scope =>
      scope.reference(scope.declaredBinder.get).map(ref => TermShape.Apply(ref, List(TermShape.Literal("41"))))
    }.toOption.get
    val unused = TermShapeBindings.localMethod("unused", Vector(Vector(TermParameterSpec("x", int))), int) { scope =>
      scope.reference(scope.parameterBinders.head.head)
    } { _ => Right(TermShape.Literal("42")) }.toOption.get
    Vector(
      ("ordinary", TermShape.If(TermShape.Literal("true"), TermShape.Literal("42"), TermShape.Literal("0")), "if true then 42 else 0"),
      ("signedReceiver", TermShape.Select(TermShape.Parenthesized(TermShape.Literal("-42")), "abs"), "(-42).abs"),
      ("lambda", lambda, "(x: Int) => x + 1"),
      ("localValue", value, "{ val x: Int = 41; x + 1 }"),
      ("localMethod", method, "{ def increment(x: Int): Int = x + 1; increment(41) }"),
      ("unusedLocalMethod", unused, "{ def unused(x: Int): Int = x; 42 }")
    )

  private final class ConsumerDriver(cases: Vector[(String, TermShape, String)]) extends Driver:
    val transferred = scala.collection.mutable.ArrayBuffer.empty[String]
    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new InsertGenerated(cases, ConsumerDriver.this)) :: super.frontendPhases.tail

  // Test-owned transfer validates the generated-origin boundary; placement is not a public API.
  private final class InsertGenerated(cases: Vector[(String, TermShape, String)], evidence: ConsumerDriver) extends Phase:
    def phaseName: String = "publicSemanticTermOriginInsertion"
    override def isCheckable: Boolean = false
    protected def run(using Context): Unit =
      val byName = cases.map(c => c._1 -> (c._2, c._3)).toMap
      val transformer = new untpd.UntypedTreeMap:
        override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match
          case method: untpd.DefDef if byName.contains(method.name.toString) =>
            val (semantic, expectedSource) = byName(method.name.toString)
            val path = s"generated/${method.name}.scala"
            TermGeneratedOriginLowering.lower(semantic, path) match
              case Left(problem) =>
                problem.code match
                  case "INVALID_VIRTUAL_SOURCE" => report.error(s"invalid generated path: ${problem.detail}")
                  case _ => report.error(problem.message)
                method
              case Right(result) =>
                assertEquals(result.generatedSource, expectedSource)
                assertEquals(result.virtualSourceName, path)
                assertEquals(result.sourceFile.path, path)
                assertEquals(result.sourceFile.content.mkString, expectedSource)
                assertEquals(result.tree.span.start, 0)
                assertEquals(result.tree.span.end, expectedSource.length)
                allTrees(result.tree).foreach { node =>
                  assert(node.source eq result.sourceFile)
                  assert(node.span.exists && node.span.start >= 0 && node.span.start <= node.span.point && node.span.point <= node.span.end && node.span.end <= expectedSource.length)
                  assertEquals(node.symbol, NoSymbol)
                  assert(!node.isInstanceOf[untpd.TypedSplice])
                }
                val updated = untpd.cpy.DefDef(method)(method.name, method.paramss, method.tpt, result.tree)
                assert(updated.rhs eq result.tree)
                evidence.transferred += method.name.toString
                updated
          case _ => super.transform(tree)
      summon[Context].compilationUnit.untpdTree = transformer.transform(summon[Context].compilationUnit.untpdTree)

  private def allTrees(tree: untpd.Tree)(using Context): List[untpd.Tree] =
    if tree.isEmpty then Nil
    else tree :: (tree match
      case t: untpd.DefDef => t.paramss.flatten.flatMap(allTrees) ::: allTrees(t.tpt) ::: allTrees(t.rhs)
      case t: untpd.ValDef => allTrees(t.tpt) ::: allTrees(t.rhs)
      case t: untpd.Select => allTrees(t.qualifier)
      case t: untpd.Apply => allTrees(t.fun) ::: t.args.flatMap(allTrees)
      case t: untpd.InfixOp => allTrees(t.left) ::: allTrees(t.op) ::: allTrees(t.right)
      case t: untpd.Parens => allTrees(t.t)
      case t: untpd.Function => t.args.flatMap(allTrees) ::: allTrees(t.body)
      case t: untpd.If => allTrees(t.cond) ::: allTrees(t.thenp) ::: allTrees(t.elsep)
      case t: untpd.Block => t.stats.flatMap(allTrees) ::: allTrees(t.expr)
      case _ => Nil)

  private def compilationClasspath: String =
    Vector(classOf[scala.Option[?]], classOf[scala.deriving.Mirror], classOf[Compiler], classOf[TermShape], getClass)
      .flatMap(c => Option(c.getProtectionDomain).flatMap(d => Option(d.getCodeSource)).map(_.getLocation.toURI))
      .map(Path.of(_).toString).distinct.mkString(java.io.File.pathSeparator)
