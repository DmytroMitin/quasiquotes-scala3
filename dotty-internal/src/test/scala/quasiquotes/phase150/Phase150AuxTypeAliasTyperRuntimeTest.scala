package quasiquotes.phase150

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.parsing.Parser

import quasiquotes.parser.BinderId
import quasiquotes.phase150.AuxTypeAliasSemanticPlanProbe.*
import quasiquotes.phase150.AuxTypeAliasSemanticPlanProbe.TypeNode.*

class Phase150AuxTypeAliasTyperRuntimeTest extends munit.FunSuite:
  test("positioned alias survives pre-typer template insertion and TASTy emission") {
    val temporary = Files.createTempDirectory("phase150-aux-alias-")
    try
      val source = temporary.resolve("Phase150AuxAliasRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """trait Nat
          |trait Add[N <: Nat, M <: Nat]:
          |  type Out <: Nat
          |
          |object Phase150AuxAliasRuntime:
          |  final class N extends Nat
          |  final class M extends Nat
          |  final class O extends Nat
          |  type Exact = Add[N, M] { type Out = O }
          |  def coerce(value: Aux[N, M, O]): Exact = value
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new GeneratedAliasDriver(validPlan())
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
        driver.insertedSource,
        Some("type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }")
      )
      assertEquals(driver.beforeTyperProvenanceValid, Some(true))
      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith("Phase150AuxAliasRuntime$.class")))
      assert(emitted.exists(_.toString.endsWith("Phase150AuxAliasRuntime.tasty")))
    finally deleteRecursively(temporary)
  }

  private final class GeneratedAliasDriver(plan: Plan) extends Driver:
    @volatile var insertedSource: Option[String] = None
    @volatile var beforeTyperProvenanceValid: Option[Boolean] = None

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new InsertAlias(plan, GeneratedAliasDriver.this)) :: super.frontendPhases.tail

  private final class InsertAlias(plan: Plan, evidence: GeneratedAliasDriver) extends Phase:
    def phaseName: String = "phase150AuxAliasInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      Phase150AuxTypeAliasUntpdProbe.position(plan, "<quasiquotes-generated:phase150-aux-alias>") match
        case Left(error) => report.error(error.message)
        case Right(result) =>
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              tree match
                case template: untpd.Template
                    if template.body.exists {
                      case definition: untpd.DefDef => definition.name.toString == "coerce"
                      case _ => false
                    } =>
                  untpd.cpy.Template(template)(
                    template.constr,
                    template.parentsOrDerived,
                    template.derived,
                    template.self,
                    template.body :+ result.tree
                  )
                case _ => super.transform(tree)
          summon[Context].compilationUnit.untpdTree =
            transformer.transform(summon[Context].compilationUnit.untpdTree)
          evidence.insertedSource = Some(result.generatedSource)
          evidence.beforeTyperProvenanceValid = Some(
            allTrees(result.tree).size == 18 && allTrees(result.tree).forall(tree =>
              tree.source.exists &&
                tree.source.path == result.virtualSourceName &&
                tree.span.exists &&
                tree.span.start >= 0 &&
                tree.span.end <= result.generatedSource.length
            )
          )

  private def validPlan(): Plan =
    val parameters = Vector(
      TypeParameter(BinderId(1), "N", SourceName("Nat")),
      TypeParameter(BinderId(2), "M", SourceName("Nat")),
      TypeParameter(BinderId(3), "Out0", SourceName("Nat"))
    )
    val applied = Applied(
      SourceName("Add"),
      Vector(BinderReference(BinderId(1), "N"), BinderReference(BinderId(2), "M"))
    )
    AuxTypeAliasSemanticPlanProbe
      .create("Aux", parameters, Refinement(applied, Vector(TypeAlias("Out", BinderReference(BinderId(3), "Out0")))))
      .fold(error => fail(error.message), identity)

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.LambdaTypeTree => value.tparams.toVector :+ value.body
      case value: untpd.TypeBoundsTree => Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case _ => Vector.empty

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[AuxTypeAliasSemanticPlanProbe.type],
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
