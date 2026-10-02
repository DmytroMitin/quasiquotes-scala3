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

import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

class ExistingUntpdOrdinaryMethodReconstructionTyperRuntimeTest extends munit.FunSuite:
  test("U042 shells are insertion-ready for one and two parameter all-field edits") {
    val temporary = Files.createTempDirectory("u042-method-reconstruction-")
    try
      val source = temporary.resolve("U042MethodReconstructionRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """class U042One:
          |  def bump(x: AnyVal): AnyVal = x
          |
          |object U042OneUse:
          |  def value: Int = new U042One().bump(-7) + 1
          |
          |class U042Two:
          |  def combine(x: AnyVal, y: AnyVal): AnyVal = x
          |
          |object U042TwoUse:
          |  def value: Int = new U042Two().combine(20, 42) + 1
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val baseline = new Driver().process(
        Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      )
      assert(baseline.hasErrors)
      deleteRecursively(output)
      Files.createDirectories(output)

      val driver = new RewriteDriver
      val reporter = driver.process(
        Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
      )

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assert(driver.beforeTyperContractValid)
      val files = emitted(output)
      assert(files.exists(_.endsWith("U042One.tasty")))
      assert(files.exists(_.endsWith("U042Two.tasty")))
      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        assertEquals(invokeValue(loader, "U042OneUse$"), Integer.valueOf(8))
        assertEquals(invokeValue(loader, "U042TwoUse$"), Integer.valueOf(43))
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class RewriteDriver extends Driver:
    @volatile var beforeTyperContractValid: Boolean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) ::
            super.frontendPhases.tail

  private final class RewriteBeforeTyper(evidence: RewriteDriver) extends Phase:
    def phaseName: String = "u042PreparedFieldMethodReconstruction"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val prepared =
        for
          oneRoot <- classNamed(unitTree, "U042One")
          twoRoot <- classNamed(unitTree, "U042Two")
          one <- reconstructOne(oneRoot)
          two <- reconstructTwo(twoRoot)
        yield (oneRoot, one, twoRoot, two)

      prepared match
        case Left(problem) => report.error(problem)
        case Right((oneRoot, one, twoRoot, two)) =>
          evidence.beforeTyperContractValid =
            methodContract(one._1, one._2, 1) &&
              methodContract(two._1, two._2, 2) &&
              allTrees(one._2.root).forall(clean) &&
              allTrees(two._2.root).forall(clean)
          val transformer = new untpd.UntypedTreeMap:
            override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
              if tree.eq(oneRoot) then one._2.root
              else if tree.eq(twoRoot) then two._2.root
              else super.transform(tree)
          summon[Context].compilationUnit.untpdTree = transformer.transform(unitTree)

    private def reconstructOne(
        root: untpd.TypeDef
    )(using Context): Either[String, (ExistingUntpdOrdinaryMethodDescriptor.Descriptor, ExistingUntpdClassMemberFilter.Reconstructed)] =
      for
        descriptor <- descriptorFor(root)
        parameter <- ExistingUntpdOrdinaryMethodTypeEditPreparation
          .prepareParameterType(descriptor, descriptor.parameterClauses.head.head, TypeNormalForm.STypeIdent("Int"))
          .left.map(_.message)
        resultType <- ExistingUntpdOrdinaryMethodTypeEditPreparation
          .prepareResultType(descriptor, TypeNormalForm.STypeIdent("Int"))
          .left.map(_.message)
        body <- ExistingUntpdOrdinaryMethodRhsEditPreparation.prepareBody(descriptor) { scope =>
          scope.reference(descriptor.parameterClauses.head.head).map(reference =>
            TermShape.Apply(
              TermShape.Select(TermShape.Identifier("Math", false), "abs"),
              List(reference)
            )
          )
        }.left.map(_.message)
        method <- ExistingUntpdOrdinaryMethodReconstruction
          .reconstructMethod(descriptor, Vector(parameter), Some(resultType), Some(body))
          .left.map(_.message)
        owner <- insert(descriptor, method.positionedMethod)
      yield descriptor -> owner

    private def reconstructTwo(
        root: untpd.TypeDef
    )(using Context): Either[String, (ExistingUntpdOrdinaryMethodDescriptor.Descriptor, ExistingUntpdClassMemberFilter.Reconstructed)] =
      for
        descriptor <- descriptorFor(root)
        first <- ExistingUntpdOrdinaryMethodTypeEditPreparation
          .prepareParameterType(descriptor, descriptor.parameterClauses.head(0), TypeNormalForm.STypeIdent("Int"))
          .left.map(_.message)
        second <- ExistingUntpdOrdinaryMethodTypeEditPreparation
          .prepareParameterType(descriptor, descriptor.parameterClauses.head(1), TypeNormalForm.STypeIdent("Int"))
          .left.map(_.message)
        resultType <- ExistingUntpdOrdinaryMethodTypeEditPreparation
          .prepareResultType(descriptor, TypeNormalForm.STypeIdent("Int"))
          .left.map(_.message)
        body <- ExistingUntpdOrdinaryMethodRhsEditPreparation.prepareBody(descriptor) { scope =>
          for
            x <- scope.reference(descriptor.parameterClauses.head(0))
            y <- scope.reference(descriptor.parameterClauses.head(1))
          yield TermShape.Apply(
            TermShape.Select(TermShape.Identifier("Math", false), "max"),
            List(x, y)
          )
        }.left.map(_.message)
        method <- ExistingUntpdOrdinaryMethodReconstruction
          .reconstructMethod(descriptor, Vector(first, second), Some(resultType), Some(body))
          .left.map(_.message)
        owner <- insert(descriptor, method.positionedMethod)
      yield descriptor -> owner

    private def descriptorFor(
        root: untpd.TypeDef
    )(using Context): Either[String, ExistingUntpdOrdinaryMethodDescriptor.Descriptor] =
      for
        captured <- ExistingUntpdClassMemberFilter.capture(root).left.map(_.message)
        descriptor <- ExistingUntpdOrdinaryMethodDescriptor.capture(captured, 0).left.map(_.message)
      yield descriptor

    private def insert(
        descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor,
        method: untpd.DefDef
    )(using Context): Either[String, ExistingUntpdClassMemberFilter.Reconstructed] =
      ExistingUntpdClassMemberFilter
        .reconstruct(
          descriptor.captured,
          descriptor.captured.originalTemplate.body.toVector.updated(descriptor.memberIndex, method)
        ).left.map(_.message)

    private def methodContract(
        descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor,
        reconstructed: ExistingUntpdClassMemberFilter.Reconstructed,
        arity: Int
    )(using Context): Boolean =
      reconstructed.template.body(descriptor.memberIndex) match
        case method: untpd.DefDef =>
          !method.eq(descriptor.method) &&
            method.paramss.head.size == arity &&
            method.paramss.head.forall(parameter =>
              !descriptor.parameterClauses.head.exists(_.tree.eq(parameter))
            ) &&
            !method.tpt.eq(descriptor.resultType) &&
            !method.rhs.eq(descriptor.rhs)
        case _ => false

  private def classNamed(tree: untpd.Tree, name: String)(using Context): Either[String, untpd.TypeDef] =
    allTrees(tree).collectFirst {
      case value: untpd.TypeDef if value.name.toString == name => value
    }.toRight(s"fixture class $name was not found")

  private def clean(tree: untpd.Tree)(using Context): Boolean =
    tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ExistingUntpdClassMemberFilter.allTrees(tree)

  private def emitted(output: Path): Vector[String] =
    val stream = Files.walk(output)
    try stream.filter(Files.isRegularFile(_)).iterator().asScala.map(_.toString).toVector
    finally stream.close()

  private def invokeValue(loader: URLClassLoader, moduleName: String): AnyRef =
    val moduleClass = loader.loadClass(moduleName)
    val module = moduleClass.getField("MODULE$").get(null)
    moduleClass.getMethod("value").invoke(module)

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ExistingUntpdOrdinaryMethodReconstruction.type],
      getClass
    ).flatMap(value =>
      Option(value.getProtectionDomain)
        .flatMap(domain => Option(domain.getCodeSource))
        .map(_.getLocation.toURI)
    ).map(Path.of(_).toString).distinct.mkString(java.io.File.pathSeparator)

  private def deleteRecursively(root: Path): Unit =
    if Files.exists(root) then
      val stream = Files.walk(root)
      try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally stream.close()
