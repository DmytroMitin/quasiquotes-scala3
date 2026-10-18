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

import quasiquotes.types.{
  ResolvedTypeNameId,
  ResolvedTypeOwnerKind,
  ResolvedTypeOwnerSegment,
  TypeNormalForm
}

final class ExistingRootedOptionRecaptureTyperRuntimeTest extends munit.FunSuite:
  import ExistingClassUntypedRewrite.*

  private enum Mode:
    case BodyOnly, RepeatCanonical, PrimitiveBack

  private val canonicalOptionString: TypeNormalForm =
    val pkg = (name: String) => ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, name)
    TypeNormalForm.STypeApply(
      TypeNormalForm.STypeResolved(ResolvedTypeNameId(Vector(pkg("scala")), "Option")),
      List(
        TypeNormalForm.STypeResolved(
          ResolvedTypeNameId(Vector(pkg("java"), pkg("lang")), "String")
        )
      )
    )

  test("successful second public transactions pass Typer, emit TASTy, and run"):
    val temporary = Files.createTempDirectory("u074-rooted-option-runtime-")
    try
      val source = temporary.resolve("U074RootedOptionRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(source, program, StandardCharsets.UTF_8)

      val baseline = new Driver().process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))
      assert(baseline.hasErrors, "Option call sites must require the two pre-Typer transactions")
      deleteRecursively(output)
      Files.createDirectories(output)

      val driver = new RewriteDriver
      val reporter = driver.process(Array(
        "-classpath", compilationClasspath,
        "-d", output.toString,
        source.toString
      ))

      assert(!reporter.hasErrors, clues(reporter.allErrors.toVector.map(_.message)))
      assertEquals(
        driver.rewritten.sorted,
        Vector("U074BodyOnly", "U074PrimitiveBack", "U074RepeatedCanonical")
      )
      assert(driver.recaptureAccepted)
      assert(driver.semanticProjectionBoundary)
      assert(driver.preTyperClean)
      assert(driver.provenanceAndIdentity)
      val files = emitted(output)
      driver.rewritten.foreach(name => assert(files.exists(_.endsWith(name + ".tasty")), clues(name, files)))

      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        assertEquals(invoke(loader, "u074.runtime.U074Use$", "bodyOnly"), Some("ok"))
        assertEquals(invoke(loader, "u074.runtime.U074Use$", "repeated"), Some("ok"))
        assertEquals(invoke(loader, "u074.runtime.U074Use$", "primitiveBack"), "ok")
      finally loader.close()
    finally deleteRecursively(temporary)

  private final class RewriteDriver extends Driver:
    @volatile var rewritten = Vector.empty[String]
    @volatile var recaptureAccepted = true
    @volatile var semanticProjectionBoundary = true
    @volatile var preTyperClean = true
    @volatile var provenanceAndIdentity = true

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new RewriteBeforeTyper(RewriteDriver.this)) :: super.frontendPhases.tail

  private final class RewriteBeforeTyper(driver: RewriteDriver) extends Phase:
    def phaseName: String = "u074RootedOptionRecapture"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val unitTree = summon[Context].compilationUnit.untpdTree
      val transformed = new untpd.UntypedTreeMap:
        override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match
          case root: untpd.TypeDef if selected(root.name.toString) =>
            rewriteTwice(root, mode(root.name.toString)) match
              case Left(problem) =>
                driver.recaptureAccepted = false
                report.error(problem)
                root
              case Right(proof) =>
                driver.rewritten = driver.rewritten :+ root.name.toString
                driver.semanticProjectionBoundary &&= proof.semanticProjectionBoundary
                driver.preTyperClean &&= allTrees(proof.result.tree).forall(clean)
                driver.provenanceAndIdentity &&= proof.provenanceAndIdentity
                proof.result.tree
          case other => super.transform(other)
      summon[Context].compilationUnit.untpdTree = transformed.transform(unitTree)

    private def selected(name: String): Boolean = name.startsWith("U074") && name != "U074Use"

    private def mode(name: String): Mode = name match
      case "U074BodyOnly" => Mode.BodyOnly
      case "U074RepeatedCanonical" => Mode.RepeatCanonical
      case "U074PrimitiveBack" => Mode.PrimitiveBack

  private final case class RewriteProof(
      result: Result,
      semanticProjectionBoundary: Boolean,
      provenanceAndIdentity: Boolean
  )

  private def rewriteTwice(
      root: untpd.TypeDef,
      mode: Mode
  )(using Context): Either[String, RewriteProof] =
    for
      firstCapture <- ExistingClassUntypedRewrite.capture(root).left.map(_.message)
      firstMethod <- firstCapture.method(firstCapture.members(1).ref).left.map(_.message)
      firstParameter <- firstMethod.parameterClauses.headOption
        .flatMap(_.parameters.headOption).toRight("first parameter was not captured")
      firstBody <- firstMethod.parameterScope.reference(firstParameter.ref).left.map(_.message)
      firstPlan <- (for
        p1 <- firstCapture.emptyPlan.replaceParameterType(firstParameter.ref, canonicalOptionString)
        p2 <- p1.replaceResultType(firstMethod.ref, canonicalOptionString)
        p3 <- p2.replaceBody(firstMethod.ref, firstBody)
      yield p3).left.map(_.message)
      first <- ExistingClassUntypedRewrite(firstCapture, firstPlan).left.map(_.message)
      secondCapture <- ExistingClassUntypedRewrite.capture(first.tree).left.map(_.message)
      secondMethod <- secondCapture.method(secondCapture.members(1).ref).left.map(_.message)
      secondParameter <- secondMethod.parameterClauses.headOption
        .flatMap(_.parameters.headOption).toRight("second parameter was not captured")
      secondBody <- secondMethod.parameterScope.reference(secondParameter.ref).left.map(_.message)
      secondPlan <- mode match
        case Mode.BodyOnly =>
          secondCapture.emptyPlan.replaceBody(secondMethod.ref, secondBody).left.map(_.message)
        case Mode.RepeatCanonical =>
          (for
            p1 <- secondCapture.emptyPlan.replaceParameterType(secondParameter.ref, canonicalOptionString)
            p2 <- p1.replaceResultType(secondMethod.ref, canonicalOptionString)
            p3 <- p2.replaceBody(secondMethod.ref, secondBody)
          yield p3).left.map(_.message)
        case Mode.PrimitiveBack =>
          (for
            p1 <- secondCapture.emptyPlan.replaceParameterType(
              secondParameter.ref,
              TypeNormalForm.STypeIdent("String")
            )
            p2 <- p1.replaceResultType(secondMethod.ref, TypeNormalForm.STypeIdent("String"))
            p3 <- p2.replaceBody(secondMethod.ref, secondBody)
          yield p3).left.map(_.message)
      second <- ExistingClassUntypedRewrite(secondCapture, secondPlan).left.map(_.message)
      firstMethodTree = selectedMethod(first.tree)
      secondMethodTree = selectedMethod(second.tree)
      originalMethodTree = selectedMethod(root)
      semanticBoundary = Vector(
        secondParameter.declaredType.semantic,
        secondMethod.resultType.semantic
      ).forall {
        case Left(problem) =>
          problem.code == "UNSUPPORTED_STRUCTURE" &&
            problem.detail.contains("U038 UNSUPPORTED_SELECTED_TYPE")
        case Right(_) => false
      }
      firstProvenance =
        canonicalAtOriginalSite(selectedParameter(first.tree).tpt, selectedParameter(root).tpt) &&
          canonicalAtOriginalSite(firstMethodTree.tpt, originalMethodTree.tpt)
      provenance = firstProvenance && (mode match
        case Mode.BodyOnly =>
          selectedParameter(second.tree).eq(selectedParameter(first.tree)) &&
            secondMethodTree.tpt.eq(firstMethodTree.tpt)
        case Mode.RepeatCanonical =>
          freshCanonicalAtSameSite(selectedParameter(second.tree).tpt, selectedParameter(first.tree).tpt) &&
            freshCanonicalAtSameSite(secondMethodTree.tpt, firstMethodTree.tpt)
        case Mode.PrimitiveBack =>
          directStringAtOldSite(selectedParameter(second.tree).tpt, selectedParameter(first.tree).tpt) &&
            directStringAtOldSite(secondMethodTree.tpt, firstMethodTree.tpt)
      )
    yield RewriteProof(second, semanticBoundary, provenance)

  private def selectedMethod(root: untpd.TypeDef)(using Context): untpd.DefDef =
    root.rhs.asInstanceOf[untpd.Template].body(1).asInstanceOf[untpd.DefDef]

  private def selectedParameter(root: untpd.TypeDef)(using Context): untpd.ValDef =
    selectedMethod(root).paramss.head.head.asInstanceOf[untpd.ValDef]

  private def freshCanonicalAtSameSite(
      fresh: untpd.Tree,
      previous: untpd.Tree
  )(using Context): Boolean =
    val freshNodes = allTrees(fresh)
    val previousNodes = allTrees(previous)
    canonicalAtOriginalSite(fresh, previous) &&
      fresh.source == previous.source && fresh.span == previous.span &&
      freshNodes.forall(node => previousNodes.forall(previousNode => !node.eq(previousNode))) &&
      (freshNodes(1).source.asInstanceOf[AnyRef] ne previousNodes(1).source.asInstanceOf[AnyRef])

  private def canonicalAtOriginalSite(
      fresh: untpd.Tree,
      oldSite: untpd.Tree
  )(using Context): Boolean =
    val nodes = allTrees(fresh)
    val children = nodes.drop(1)
    val expectedSpans = Vector(
      (0, 19, 13),
      (0, 12, 7),
      (0, 6, 0),
      (20, 43, 37),
      (20, 36, 32),
      (20, 31, 27),
      (20, 26, 20)
    )
    canonical(fresh) && nodes.size == 8 &&
      fresh.source == oldSite.source && fresh.span == oldSite.span &&
      children.nonEmpty &&
      children.forall(_.source.asInstanceOf[AnyRef] eq children.head.source.asInstanceOf[AnyRef]) &&
      children.head.source.path == "generated/CanonicalResolvedOptionStringMethodType.scala" &&
      children.head.source.content.mkString == "_root_.scala.Option[_root_.java.lang.String]" &&
      children.map(node => (node.span.start, node.span.end, node.span.point)) == expectedSpans

  private def directStringAtOldSite(fresh: untpd.Tree, previous: untpd.Tree): Boolean = fresh match
    case ident: untpd.Ident =>
      ident.name.toString == "String" && !ident.eq(previous) &&
        ident.source == previous.source && ident.span == previous.span
    case _ => false

  private def canonical(tree: untpd.Tree): Boolean = tree match
    case untpd.AppliedTypeTree(
          untpd.Select(untpd.Select(untpd.Ident(rootOption), scalaName), optionName),
          List(
            untpd.Select(
              untpd.Select(untpd.Select(untpd.Ident(rootString), javaName), langName),
              stringName
            )
          )
        ) =>
      rootOption.toString == "_root_" && scalaName.toString == "scala" &&
        optionName.toString == "Option" && rootString.toString == "_root_" &&
        javaName.toString == "java" && langName.toString == "lang" &&
        stringName.toString == "String"
    case _ => false

  private def clean(tree: untpd.Tree)(using Context): Boolean =
    tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ExistingUntpdClassMemberFilter.allTrees(tree)

  private def program: String =
    """package u074.runtime
      |
      |class U074BodyOnly:
      |  val before: Int = 1
      |  def echo(value: String): String = value
      |  val after: Int = 2
      |
      |class U074RepeatedCanonical:
      |  val before: Int = 1
      |  def echo(value: String): String = value
      |  val after: Int = 2
      |
      |class U074PrimitiveBack:
      |  val before: Int = 1
      |  def echo(value: String): String = value
      |  val after: Int = 2
      |
      |object U074Use:
      |  def bodyOnly: Option[String] = new U074BodyOnly().echo(Some("ok"))
      |  def repeated: Option[String] = new U074RepeatedCanonical().echo(Some("ok"))
      |  def primitiveBack: String = new U074PrimitiveBack().echo("ok")
      |""".stripMargin

  private def emitted(output: Path): Vector[String] =
    val stream = Files.walk(output)
    try stream.filter(Files.isRegularFile(_)).iterator().asScala.map(_.toString).toVector
    finally stream.close()

  private def invoke(loader: URLClassLoader, moduleName: String, method: String): Any =
    val moduleClass = loader.loadClass(moduleName)
    val module = moduleClass.getField("MODULE$").get(null)
    moduleClass.getMethod(method).invoke(module)

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      classOf[ExistingClassUntypedRewrite.type],
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
