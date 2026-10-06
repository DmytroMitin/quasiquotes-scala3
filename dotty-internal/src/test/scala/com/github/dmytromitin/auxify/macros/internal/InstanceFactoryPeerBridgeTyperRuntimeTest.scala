package com.github.dmytromitin.auxify.macros.internal

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
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser

import _root_.quasiquotes.definitions.dotty.InstanceFactoryPeerBridge

class InstanceFactoryPeerBridgeTyperRuntimeTest extends munit.FunSuite:
  private val FactorySource =
    "def instance[A](emptyValue: => A, combineFunction: (A, A) => A): Monoid[A] = new Monoid[A] { override def empty: A = emptyValue; override def combine(a: A, a1: A): A = combineFunction(a, a1) }"
  private val AllFamilySources = Vector(
    "def instanceOld[A](emptyValue: => A, combineFunction: (A, A) => A): Monoid[A] = new Monoid[A] { override def empty: A = emptyValue; override def combine(a: A, a1: A): A = combineFunction(a, a1) }",
    "def instanceValue[A](valueValue: A): HasValue[A] = new HasValue[A] { override val value: A = valueValue }",
    "def instanceOut[A, Out0]: HasOut[A] { type Out = Out0 } = new HasOut[A] { type Out = Out0 }"
  )


  test("foreign bridge survives pre-Typer insertion, TASTy emission, and runtime") {
    val temporary = Files.createTempDirectory("c014-instance-factory-")
    try
      val source = temporary.resolve("C014InstanceFactoryRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """trait Monoid[A]:
          |  def empty: A
          |  def combine(a: A, a1: A): A
          |
          |object C014InstanceFactoryRuntime:
          |  def resultEmpty: Int = instance[Int](7, _ + _).empty
          |  def resultCombine: Int = instance[Int](7, _ + _).combine(20, 22)
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new BridgeDriver(parse(FactorySource))
      val reporter = driver.process(Array(
        "-classpath",
        compilationClasspath,
        "-d",
        output.toString,
        source.toString
      ))

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(driver.generatedSource, Some(FactorySource))
      assertEquals(driver.preTyperInvariants, Some(true))

      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith("C014InstanceFactoryRuntime$.class")))
      assert(emitted.exists(_.toString.endsWith("C014InstanceFactoryRuntime.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("C014InstanceFactoryRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("resultEmpty").invoke(module), Integer.valueOf(7))
        assertEquals(moduleClass.getMethod("resultCombine").invoke(module), Integer.valueOf(42))
      finally loader.close()
    finally deleteRecursively(temporary)
  }
  test("one foreign consumer uses all three families through the same public bridge") {
    val temporary = Files.createTempDirectory("c060-instance-factory-families-")
    try
      val source = temporary.resolve("C060InstanceFactoryFamiliesRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """trait Monoid[A]:
          |  def empty: A
          |  def combine(a: A, a1: A): A
          |
          |trait HasValue[A]:
          |  val value: A
          |
          |trait HasOut[A]:
          |  type Out
          |
          |object C060InstanceFactoryFamiliesRuntime:
          |  def oldEmpty: Int = instanceOld[Int](7, _ + _).empty
          |  def oldCombine: Int = instanceOld[Int](7, _ + _).combine(20, 22)
          |  def valueResult: Int = instanceValue[Int](42).value
          |  def typeResult: HasOut[Int] { type Out = String } =
          |    instanceOut[Int, String]
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val definitions = AllFamilySources.zip(
        Vector(
          "<quasiquotes-generated:instance-factory-old>" -> 33,
          "<quasiquotes-generated:instance-factory-value>" -> 17,
          "<quasiquotes-generated:instance-factory-type-member>" -> 19
        )
      ).map { case (factorySource, (virtualSource, expectedNodes)) =>
        (parse(factorySource), virtualSource, expectedNodes)
      }
      val driver = new AllFamiliesDriver(definitions)
      val reporter = driver.process(Array(
        "-classpath",
        compilationClasspath,
        "-d",
        output.toString,
        source.toString
      ))

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(driver.generatedSources, Some(AllFamilySources))
      assertEquals(driver.preTyperInvariants, Some(true))

      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith("C060InstanceFactoryFamiliesRuntime$.class")))
      assert(emitted.exists(_.toString.endsWith("C060InstanceFactoryFamiliesRuntime.tasty")))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("C060InstanceFactoryFamiliesRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        assertEquals(moduleClass.getMethod("oldEmpty").invoke(module), Integer.valueOf(7))
        assertEquals(moduleClass.getMethod("oldCombine").invoke(module), Integer.valueOf(42))
        assertEquals(moduleClass.getMethod("valueResult").invoke(module), Integer.valueOf(42))
        assert(moduleClass.getMethod("typeResult").invoke(module) != null)
      finally loader.close()
    finally deleteRecursively(temporary)
  }


  private final class BridgeDriver(definition: Defn.Def) extends Driver:
    @volatile var generatedSource: Option[String] = None
    @volatile var preTyperInvariants: Option[Boolean] = None

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new InsertFactory(definition, BridgeDriver.this)) ::
            super.frontendPhases.tail

  private final class InsertFactory(
      definition: Defn.Def,
      evidence: BridgeDriver
  ) extends Phase:
    def phaseName: String = "c014InstanceFactoryBridgeInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      InstanceFactoryPeerBridge
        .lower(definition, "<quasiquotes-generated:instance-factory>") match
        case Left(problem) => report.error(s"${problem.code}: ${problem.detail}")
        case Right(lowered) =>
          val inserted = lowered.tree
          val trees = nonEmptyTrees(inserted)
          evidence.generatedSource = Some(lowered.generatedSource)
          evidence.preTyperInvariants = Some(
            trees.size == 33 && trees.forall(tree =>
              tree.source.exists &&
                tree.source.path == lowered.virtualSourceName &&
                tree.source.content.mkString == lowered.generatedSource &&
                tree.span.exists &&
                tree.span.start >= 0 &&
                tree.span.end <= lowered.generatedSource.length &&
                tree.symbol == NoSymbol &&
                !tree.isInstanceOf[untpd.TypedSplice]
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

  private final class AllFamiliesDriver(
      definitions: Vector[(Defn.Def, String, Int)]
  ) extends Driver:
    @volatile var generatedSources: Option[Vector[String]] = None
    @volatile var preTyperInvariants: Option[Boolean] = None

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new InsertAllFamilies(definitions, AllFamiliesDriver.this)) ::
            super.frontendPhases.tail

  private final class InsertAllFamilies(
      definitions: Vector[(Defn.Def, String, Int)],
      evidence: AllFamiliesDriver
  ) extends Phase:
    def phaseName: String = "c060InstanceFactoryFamiliesBridgeInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val lowered = definitions.flatMap { case (definition, virtualSource, _) =>
        InstanceFactoryPeerBridge.lower(definition, virtualSource) match
          case Left(problem) =>
            report.error(s"${problem.code}: ${problem.detail}")
            Vector.empty
          case Right(value) => Vector(value)
      }
      evidence.generatedSources = Some(lowered.map(_.generatedSource))
      evidence.preTyperInvariants = Some(
        lowered.size == definitions.size &&
          lowered.zip(definitions).forall {
            case (result, (_, _, expectedNodes)) =>
              val trees = nonEmptyTrees(result.tree)
              trees.size == expectedNodes && trees.forall(tree =>
                tree.source.exists &&
                  tree.source.path == result.virtualSourceName &&
                  tree.source.content.mkString == result.generatedSource &&
                  tree.span.exists &&
                  tree.span.start >= 0 &&
                  tree.span.end <= result.generatedSource.length &&
                  tree.symbol == NoSymbol &&
                  !tree.isInstanceOf[untpd.TypedSplice]
              )
          }
      )
      if lowered.size == definitions.size then
        val inserted = lowered.map(_.tree)
        val transformer = new untpd.UntypedTreeMap:
          override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
            tree match
              case template: untpd.Template
                  if template.body.exists {
                    case definition: untpd.DefDef =>
                      definition.name.toString == "oldEmpty"
                    case _ => false
                  } =>
                untpd.cpy.Template(template)(
                  template.constr,
                  template.parentsOrDerived,
                  template.derived,
                  template.self,
                  template.body ++ inserted
                )
              case _ => super.transform(tree)
        summon[Context].compilationUnit.untpdTree =
          transformer.transform(summon[Context].compilationUnit.untpdTree)

  private def parse(source: String): Defn.Def =
    Scala3(source).parse[Stat].get.asInstanceOf[Defn.Def]

  private def nonEmptyTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(nonEmptyTrees)

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
      case value: untpd.RefinedTypeTree =>
        value.tpt +: value.refinements.toVector
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
      classOf[scala.meta.Tree],
      classOf[InstanceFactoryPeerBridge.type],
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
          .forEach(Files.deleteIfExists(_))
      finally stream.close()
