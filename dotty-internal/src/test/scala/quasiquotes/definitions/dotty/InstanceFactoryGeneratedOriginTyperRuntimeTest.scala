package quasiquotes.definitions.dotty

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

import quasiquotes.definitions.InstanceFactoryPlan.Plan
import quasiquotes.neutral.ScalametaInstanceFactoryProjection

import scala.meta.*
import scala.meta.dialects.Scala3

class InstanceFactoryGeneratedOriginTyperRuntimeTest extends munit.FunSuite:
  private val FactorySource =
    "def instance[A](emptyValue: => A, combineFunction: (A, A) => A): U017Monoid[A] = new U017Monoid[A] { override def empty: A = emptyValue; override def combine(a: A, a1: A): A = combineFunction(a, a1) }"

  test("normal semantic projection survives exact pre-Typer insertion TASTy emission and runtime") {
    val definition = Input.String(FactorySource).parse[Stat].get.asInstanceOf[Defn.Def]
    val plan = ScalametaInstanceFactoryProjection
      .project(definition)
      .fold(problem => fail(problem.message), _.plan)
    val temporary = Files.createTempDirectory("u017-instance-factory-")
    try
      val source = temporary.resolve("U017InstanceFactoryRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """trait U017Monoid[A]:
          |  def empty: A
          |  def combine(a: A, a1: A): A
          |
          |object U017InstanceFactoryRuntime:
          |  def resultEmpty: Int = instance[Int](7, _ + _).empty
          |  def resultCombine: Int = instance[Int](7, _ + _).combine(20, 22)
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new FactoryDriver(plan)
      val reporter = driver.process(Array(
        "-classpath",
        compilationClasspath,
        "-d",
        output.toString,
        source.toString
      ))

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(driver.sourceFreeTopology, Some(true))
      assertEquals(driver.sourceFreeInvariants, Some(true))
      assertEquals(driver.generatedSource, Some(FactorySource))
      assertEquals(driver.generatedTopology, Some(true))
      assertEquals(driver.generatedInvariants, Some(true))

      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith("U017InstanceFactoryRuntime$.class")))
      assert(emitted.exists(_.toString.endsWith("U017InstanceFactoryRuntime.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U017InstanceFactoryRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(
          moduleClass.getMethod("resultEmpty").invoke(module),
          Integer.valueOf(7)
        )
        assertEquals(
          moduleClass.getMethod("resultCombine").invoke(module),
          Integer.valueOf(42)
        )
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class FactoryDriver(plan: Plan) extends Driver:
    @volatile var sourceFreeTopology: Option[Boolean] = None
    @volatile var sourceFreeInvariants: Option[Boolean] = None
    @volatile var generatedSource: Option[String] = None
    @volatile var generatedTopology: Option[Boolean] = None
    @volatile var generatedInvariants: Option[Boolean] = None

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new InsertFactory(plan, FactoryDriver.this)) ::
            super.frontendPhases.tail

  private final class InsertFactory(
      plan: Plan,
      evidence: FactoryDriver
  ) extends Phase:
    def phaseName: String = "u017InstanceFactoryInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      InstanceFactoryPlanUntypedLowerer.lower(plan) match
        case Left(problem) => report.error(problem.message)
        case Right(sourceFree) =>
          val sourceFreeTrees = allTrees(sourceFree)
          evidence.sourceFreeTopology = Some(isExactFactory(sourceFree))
          evidence.sourceFreeInvariants = Some(
            sourceFreeTrees.size == 33 && sourceFreeTrees.forall(tree =>
              !tree.source.exists && !tree.span.exists &&
                tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
            )
          )
          InstanceFactoryGeneratedOriginAdapter
            .lower(plan, "<quasiquotes-generated:u017-instance-factory>") match
            case Left(problem) => report.error(problem.message)
            case Right(generated) =>
              val inserted = generated.tree.asInstanceOf[untpd.DefDef]
              val generatedTrees = allTrees(inserted)
              evidence.generatedSource = Some(generated.generatedSource)
              evidence.generatedTopology = Some(isExactFactory(inserted))
              evidence.generatedInvariants = Some(
                generatedTrees.size == 33 && generatedTrees.forall(tree =>
                  tree.source.exists && tree.source.path == generated.virtualSourceName &&
                    tree.source.content.mkString == generated.generatedSource &&
                    tree.span.exists && tree.span.start >= 0 &&
                    tree.span.end <= generated.generatedSource.length &&
                    tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
                )
              )
              val transformer = new untpd.UntypedTreeMap:
                override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
                  tree match
                    case template: untpd.Template
                        if template.body.exists {
                          case definition: untpd.DefDef =>
                            definition.name.toString == "resultEmpty"
                          case _ => false
                        } =>
                      untpd.cpy.Template(template)(
                        template.constr,
                        template.parentsOrDerived,
                        template.derived,
                        template.self,
                        template.body :+ inserted
                      )
                    case _ => super.transform(tree)
              summon[Context].compilationUnit.untpdTree =
                transformer.transform(summon[Context].compilationUnit.untpdTree)

  private def isExactFactory(tree: untpd.Tree)(using Context): Boolean =
    tree match
      case definition: untpd.DefDef =>
        definition.paramss match
          case List(
                List(_: untpd.TypeDef),
                List(first: untpd.ValDef, second: untpd.ValDef)
              ) =>
            first.tpt.isInstanceOf[untpd.ByNameTypeTree] &&
              second.tpt.isInstanceOf[untpd.Function] &&
              definition.tpt.isInstanceOf[untpd.AppliedTypeTree] &&
              (definition.rhs match
                case untpd.New(template: untpd.Template) =>
                  template.parentsOrDerived.size == 1 && template.derived.isEmpty &&
                    template.self.isEmpty && template.body.size == 2 &&
                    template.body.forall(_.isInstanceOf[untpd.DefDef])
                case _ => false)
          case _ => false
      case _ => false

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeDef => Vector(value.rhs).filterNot(_.isEmpty)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.ByNameTypeTree => Vector(value.result)
      case value: untpd.Function => value.args.toVector :+ value.body
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.Template =>
        (Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body).filterNot(_.isEmpty)
      case value: untpd.Apply => value.fun +: value.args.toVector
      case _ => Vector.empty

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ScalametaInstanceFactoryProjection.type],
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
        stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally stream.close()
