package external.consumer

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
import dotty.tools.dotc.parsing.Parser

import _root_.quasiquotes.definitions.dotty.ScalametaDefinitionClassMemberAppendBridge

final class ScalametaDefinitionClassMemberAppendBridgeTyperRuntimeTest
    extends munit.FunSuite:
  test("hybrid bridge classes survive ordinary Typer TASTy emission and runtime"):
    val temporary = Files.createTempDirectory("c023-hybrid-runtime-")
    try
      val source = temporary.resolve("C023HybridRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """final class GeneratedMethodClass:
          |  def oldMethod(): Int = 7
          |
          |final class GeneratedValueClass:
          |  val oldValue: Int = 7
          |
          |object C023HybridRuntime:
          |  def methodResult: String = new GeneratedMethodClass().foo(7)
          |  def oldMethodResult: Int = new GeneratedMethodClass().oldMethod()
          |  def valueResult: Int = new GeneratedValueClass().foo
          |  def oldValueResult: Int = new GeneratedValueClass().oldValue
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new BridgeDriver
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
      assertEquals(driver.transformedClasses, Vector("GeneratedMethodClass", "GeneratedValueClass"))
      assertEquals(driver.contractChecks, Vector(true, true))

      val emittedFiles = emitted(output)
      assert(emittedFiles.exists(_.endsWith("GeneratedMethodClass.class")), clues(emittedFiles))
      assert(emittedFiles.exists(_.endsWith("GeneratedMethodClass.tasty")), clues(emittedFiles))
      assert(emittedFiles.exists(_.endsWith("GeneratedValueClass.class")), clues(emittedFiles))
      assert(emittedFiles.exists(_.endsWith("GeneratedValueClass.tasty")), clues(emittedFiles))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("C023HybridRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("methodResult").invoke(module), "7")
        assertEquals(
          moduleClass.getMethod("oldMethodResult").invoke(module),
          Integer.valueOf(7)
        )
        assertEquals(
          moduleClass.getMethod("valueResult").invoke(module),
          Integer.valueOf(42)
        )
        assertEquals(
          moduleClass.getMethod("oldValueResult").invoke(module),
          Integer.valueOf(7)
        )
      finally loader.close()
    finally deleteRecursively(temporary)

  private final class BridgeDriver extends Driver:
    @volatile var transformedClasses: Vector[String] = Vector.empty
    @volatile var contractChecks: Vector[Boolean] = Vector.empty

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new AppendBeforeTyper(BridgeDriver.this)) ::
            super.frontendPhases.tail

  private final class AppendBeforeTyper(evidence: BridgeDriver) extends Phase:
    def phaseName: String = "c023HybridAppendBeforeTyper"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val requested = Vector(
        (
          "GeneratedMethodClass",
          parsed("def foo(x: Int): String = x.toString"),
          "<generated:c023-runtime-method>"
        ),
        (
          "GeneratedValueClass",
          parsed("val foo: Int = 42"),
          "<generated:c023-runtime-value>"
        )
      )

      val transformed = requested.foldLeft[
        Either[
          String,
          Vector[(untpd.TypeDef, ScalametaDefinitionClassMemberAppendBridge.Lowered)]
        ]
      ](Right(Vector.empty)): (accumulator, next) =>
        for
          accumulated <- accumulator
          original <- allTrees(unitTree).collectFirst {
            case value: untpd.TypeDef if value.name.toString == next._1 => value
          }.toRight(s"${next._1} was not found")
          lowered <- ScalametaDefinitionClassMemberAppendBridge
            .append(original, next._2, next._3)
            .left
            .map(problem => s"${problem.code}: ${problem.detail}")
        yield accumulated :+ (original -> lowered)

      transformed match
        case Left(problem) => report.error(problem)
        case Right(results) =>
          evidence.transformedClasses = results.map(_._1.name.toString)
          evidence.contractChecks = results.map: (original, lowered) =>
            val oldTemplate = original.rhs.asInstanceOf[untpd.Template]
            val rebuiltTemplate = lowered.tree.rhs.asInstanceOf[untpd.Template]
            rebuiltTemplate.body.size == oldTemplate.body.size + 1 &&
              oldTemplate.body.indices.forall(index =>
                rebuiltTemplate.body(index).eq(oldTemplate.body(index))
              ) &&
              rebuiltTemplate.body.last.eq(lowered.appendedMember) &&
              !lowered.tree.eq(original) &&
              !rebuiltTemplate.eq(oldTemplate) &&
              lowered.appendedMember.source == lowered.generatedSourceFile &&
              lowered.appendedMember.span.exists

          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              results.collectFirst {
                case (original, lowered) if tree.eq(original) => lowered.tree
              }.getOrElse(super.transform(tree))
          summon[Context].compilationUnit.untpdTree = transformer.transform(unitTree)

  private def parsed(source: String): Defn =
    Scala3(source).parse[Stat].get match
      case definition: Defn => definition
      case other => fail(s"expected Defn, found ${other.productPrefix}")

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val builder = Vector.newBuilder[untpd.Tree]
    val traverser = new untpd.UntypedTreeTraverser:
      override def traverse(current: untpd.Tree)(using Context): Unit =
        builder += current
        traverseChildren(current)
    traverser.traverse(tree)
    builder.result()

  private def emitted(output: Path): Vector[String] =
    val stream = Files.walk(output)
    try
      stream
        .filter(Files.isRegularFile(_))
        .iterator()
        .asScala
        .map(_.toString)
        .toVector
    finally stream.close()

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[scala.meta.Tree],
      ScalametaDefinitionClassMemberAppendBridge.getClass,
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
          .iterator()
          .asScala
          .foreach(Files.deleteIfExists)
      finally stream.close()
