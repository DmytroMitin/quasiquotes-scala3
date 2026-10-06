package com.github.dmytromitin.auxify.macros.internal

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

import _root_.quasiquotes.definitions.dotty.SelfAbstractTypeMemberPeerBridge

class SelfAbstractTypeMemberPeerBridgeTyperTest extends munit.FunSuite:
  private final case class Row(
      marker: String,
      declaration: String,
      nodes: Int,
      member: String = "Self",
      selfAlias: String = "self",
      upperBase: String = "Nat"
  )

  private val rows = Vector(
    Row("bothWitness", "type Self >: self.type <: Nat { type Self = self.Self }", 9),
    Row("lowerWitness", "type Self >: self.type <: Nat", 5),
    Row("fBoundWitness", "type Self <: Nat { type Self = self.Self }", 7),
    Row("neitherWitness", "type Self <: Nat", 3),
    Row(
      "renamedBothWitness",
      "type Element >: owner$2.type <: Domain { type Element = owner$2.Element }",
      9,
      member = "Element",
      selfAlias = "owner$2",
      upperBase = "Domain"
    )
  )

  test("all four public bridge rows and renamed legacy row survive pre-Typer insertion and TASTy emission") {
    val temporary = Files.createTempDirectory("c062-public-self-option-matrix-")
    try
      val source = temporary.resolve("C062SelfOptionMatrix.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """trait Nat
          |trait Domain
          |
          |abstract class Both extends Nat:
          |  self =>
          |  def bothWitness(value: self.Self): Nat { type Self = self.Self } = value
          |  def bothLower: self.Self = self
          |
          |abstract class LowerOnly extends Nat:
          |  self =>
          |  def lowerWitness(value: self.Self): Nat = value
          |  def lowerLower: self.Self = self
          |
          |abstract class FBoundOnly extends Nat:
          |  self =>
          |  def fBoundWitness(value: self.Self): Nat { type Self = self.Self } = value
          |
          |abstract class Neither extends Nat:
          |  self =>
          |  def neitherWitness(value: self.Self): Nat = value
          |
          |abstract class RenamedBoth extends Domain:
          |  owner$2 =>
          |  def renamedBothWitness(value: owner$2.Element): Domain { type Element = owner$2.Element } = value
          |  def renamedBothLower: owner$2.Element = owner$2
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new MatrixDriver
      val reporter = driver.process(
        Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      )

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(driver.insertedCounts, rows.map(_.nodes))
      assertEquals(driver.insertedSources, rows.map(_.declaration))
      assertEquals(driver.loweredThroughPublicBridge, rows.size)
      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      Vector("Both", "LowerOnly", "FBoundOnly", "Neither", "RenamedBoth").foreach { name =>
        assert(emitted.exists(_.toString.endsWith(s"$name.class")), clues(name))
        assert(emitted.exists(_.toString.endsWith(s"$name.tasty")), clues(name))
      }
    finally deleteRecursively(temporary)
  }

  private final class MatrixDriver extends Driver:
    @volatile var insertedCounts: Vector[Int] = Vector.empty
    @volatile var insertedSources: Vector[String] = Vector.empty
    @volatile var loweredThroughPublicBridge: Int = 0

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new InsertMatrix(MatrixDriver.this)) ::
            super.frontendPhases.tail

  private final class InsertMatrix(evidence: MatrixDriver) extends Phase:
    def phaseName: String = "c062PublicSelfOptionMatrixInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val generated = rows.zipWithIndex.flatMap { case (row, index) =>
        val declaration = parseDeclaration(row.declaration)
        SelfAbstractTypeMemberPeerBridge.lower(
          declaration,
          row.member,
          row.selfAlias,
          row.upperBase,
          s"<quasiquotes-generated:c062-self-option-matrix-$index>"
        ) match
          case Left(problem) =>
            report.error(s"${problem.code}: ${problem.detail}")
            None
          case Right(value) => Some(value)
      }
      evidence.loweredThroughPublicBridge = generated.size
      evidence.insertedCounts = generated.map(value => allTrees(value.tree).size)
      evidence.insertedSources = generated.map(_.generatedSource)

      val byMarker = rows.zip(generated).map { case (row, value) =>
        row.marker -> value.tree
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

  private def parseDeclaration(source: String): Decl.Type =
    Scala3(source).parse[Stat].get match
      case declaration: Decl.Type => declaration
      case other => fail(s"expected Decl.Type, found ${other.getClass.getSimpleName}")

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.TypeBoundsTree => Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.SingletonTypeTree => Vector(value.ref)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[Compiler],
      SelfAbstractTypeMemberPeerBridge.getClass,
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
