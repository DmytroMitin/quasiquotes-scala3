package quasiquotes.definitions.dotty

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.util.SourceFile

import quasiquotes.definitions.*

class SelfAbstractTypeMemberOptionMatrixGeneratedOriginTyperTest
    extends munit.FunSuite:
  private final case class Row(
      marker: String,
      lowerPresent: Boolean,
      fBoundPresent: Boolean,
      nodeCount: Int
  )

  private val rows = Vector(
    Row("bothWitness", lowerPresent = true, fBoundPresent = true, 9),
    Row("lowerWitness", lowerPresent = true, fBoundPresent = false, 5),
    Row("fBoundWitness", lowerPresent = false, fBoundPresent = true, 7),
    Row("neitherWitness", lowerPresent = false, fBoundPresent = false, 3)
  )

  test("all four generated rows survive exact pre-Typer insertion and TASTy emission") {
    val temporary = Files.createTempDirectory("u049-self-option-matrix-")
    try
      val source = temporary.resolve("U049SelfOptionMatrix.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """trait Nat
          |
          |abstract class Both extends Nat:
          |  self =>
          |  def bothWitness(value: self.Self): Nat { type Self = self.Self } = value
          |  def bothLower: self.Self = self
          |
          |abstract class LowerOnly extends Nat:
          |  self =>
          |  def lowerWitness(value: self.Self): Nat = value
          |  def lowerValue: self.Self = self
          |
          |abstract class FBoundOnly extends Nat:
          |  self =>
          |  def fBoundWitness(value: self.Self): Nat { type Self = self.Self } = value
          |
          |abstract class Neither extends Nat:
          |  self =>
          |  def neitherWitness(value: self.Self): Nat = value
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new MatrixDriver(rows.map(validPlan))
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
      assertEquals(driver.insertedCounts, rows.map(_.nodeCount))
      assertEquals(driver.insertedSources, Vector(
        "type Self >: self.type <: Nat { type Self = self.Self }",
        "type Self >: self.type <: Nat",
        "type Self <: Nat { type Self = self.Self }",
        "type Self <: Nat"
      ))
      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      Vector("Both", "LowerOnly", "FBoundOnly", "Neither").foreach { name =>
        assert(emitted.exists(_.toString.endsWith(s"$name.class")), clues(name))
        assert(emitted.exists(_.toString.endsWith(s"$name.tasty")), clues(name))
      }
    finally deleteRecursively(temporary)
  }

  private final class MatrixDriver(plans: Vector[SelfAbstractTypeMemberPlan])
      extends Driver:
    @volatile var insertedCounts: Vector[Int] = Vector.empty
    @volatile var insertedSources: Vector[String] = Vector.empty

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new InsertMatrix(plans, MatrixDriver.this)) ::
            super.frontendPhases.tail

  private final class InsertMatrix(
      plans: Vector[SelfAbstractTypeMemberPlan],
      evidence: MatrixDriver
  ) extends Phase:
    def phaseName: String = "u049SelfOptionMatrixInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val generated = plans.zipWithIndex.map { case (plan, index) =>
        SelfAbstractTypeMemberGeneratedOriginAdapter
          .lower(plan, s"<quasiquotes-generated:u049-self-option-matrix-$index>") match
          case Left(problem) =>
            report.error(problem.message)
            new GeneratedOriginDefinitionResult(
              untpd.EmptyTree,
              "",
              SourceFile.virtual("<invalid>", "")
            )
          case Right(value) => value
      }
      evidence.insertedCounts = generated.map(result =>
        SelfAbstractTypeMemberUntypedLowerer.allTrees(result.tree).size
      )
      evidence.insertedSources = generated.map(_.generatedSource)

      val byMarker = rows.zip(generated).map { case (row, result) =>
        row.marker -> result.tree.asInstanceOf[untpd.TypeDef]
      }.toMap
      val transformer = new untpd.UntypedTreeMap:
        override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
          tree match
            case template: untpd.Template =>
              val marker = template.body.collectFirst {
                case definition: untpd.DefDef if byMarker.contains(definition.name.toString) =>
                  definition.name.toString
              }
              marker match
                case Some(name) =>
                  untpd.cpy.Template(template)(body = template.body :+ byMarker(name))
                case None => super.transform(tree)
            case _ => super.transform(tree)
      summon[Context].compilationUnit.untpdTree =
        transformer.transform(summon[Context].compilationUnit.untpdTree)

  private def validPlan(row: Row): SelfAbstractTypeMemberPlan =
    SelfAbstractTypeMemberPlan
      .createMatrix(
        ObservedSelfAbstractTypeMemberMatrix(
          "Self",
          Option.when(row.lowerPresent)("self"),
          "Nat",
          Option.when(row.fBoundPresent)(
            ObservedSelfAbstractTypeMemberRefinement("Self", "self", "Self")
          )
        ),
        SelfAbstractTypeMemberExpectation("Self", "self", "Nat")
      )
      .fold(problem => fail(problem.message), identity)

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[Compiler],
      classOf[SelfAbstractTypeMemberPlan.type],
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
      try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally stream.close()
