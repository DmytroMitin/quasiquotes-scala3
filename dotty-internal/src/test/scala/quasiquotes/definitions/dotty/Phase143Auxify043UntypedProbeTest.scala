package quasiquotes.definitions.dotty

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

import quasiquotes.definitions.DelegatedForwardingMethodPlan
import quasiquotes.definitions.DelegatedForwardingMethodPlan.*
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

/** Test-only exact-compiler probe. It is not a production lowering path. */
class Phase143Auxify043UntypedProbeTest extends munit.FunSuite:
  private val CanonicalSource =
    "def show[A](a: A)(using inst: Show[A]): String = inst.show(a)"

  test("source-free direct DefDef construction matches the parser raw oracle") {
    withContext {
      val direct = lower(
        "show",
        "A",
        "a",
        "inst",
        "Show",
        "String"
      )
      val parsed = parseOne(CanonicalSource)

      assertEquals(structure(direct), structure(parsed))
      assertExactShape(direct, "show", "A", "a", "inst", "Show", "String")
      nonEmptyTrees(direct).foreach { tree =>
        assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
        assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
        assertEquals(tree.symbol, NoSymbol, clues(tree.getClass.getSimpleName))
        assert(!tree.isInstanceOf[untpd.TypedSplice], clues(tree.getClass.getSimpleName))
      }
    }
  }

  test("dynamic source-free names preserve the same exact raw topology") {
    withContext {
      val source =
        "def render[Element](value: Element)(using evidence: Display[Element]): Text = evidence.render(value)"
      val direct = lower(
        "render",
        "Element",
        "value",
        "evidence",
        "Display",
        "Text"
      )

      assertEquals(structure(direct), structure(parseOne(source)))
      assertExactShape(
        direct,
        "render",
        "Element",
        "value",
        "evidence",
        "Display",
        "Text"
      )
    }
  }

  test("ordinary Typer rejects the unpositioned source-free definition at its position gate") {
    val temporary = Files.createTempDirectory("phase143-auxify043-typer-gate-")
    try
      val source = temporary.resolve("Phase143Auxify043TyperGate.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """trait Show[A]:
          |  def show(a: A): String
          |
          |object Phase143Auxify043TyperGate:
          |  def result: Int = 1
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new SourceFreeInsertionDriver
      val failure = intercept[AssertionError] {
        driver.process(
          Array(
            "-classpath",
            compilationClasspath,
            "-d",
            output.toString,
            source.toString
          )
        )
      }
      assert(driver.wasSourceFreeBeforeTyper)
      assert(
        Option(failure.getMessage).exists(_.contains("position not set")),
        clues(failure)
      )
    finally deleteRecursively(temporary)
  }

  private final class SourceFreeInsertionDriver extends Driver:
    @volatile var wasSourceFreeBeforeTyper: Boolean = false

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new InsertSourceFree043(thisDriver)) ::
            super.frontendPhases.tail

    private def thisDriver: SourceFreeInsertionDriver = this

  private final class InsertSourceFree043(
      evidence: SourceFreeInsertionDriver
  ) extends Phase:
    def phaseName: String = "phase143Auxify043SourceFreeInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      val generated = lower(
        "show",
        "A",
        "a",
        "inst",
        "Show",
        "String"
      )
      evidence.wasSourceFreeBeforeTyper = nonEmptyTrees(generated).forall(tree =>
        !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol
      )

      val transformer = new untpd.UntypedTreeMap:
        override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
          tree match
            case template: untpd.Template
                if template.body.exists {
                  case method: untpd.DefDef => method.name.toString == "result"
                  case _ => false
                } =>
              untpd.cpy.Template(template)(
                template.constr,
                template.parentsOrDerived,
                template.derived,
                template.self,
                template.body :+ generated
              )
            case _ => super.transform(tree)

      summon[Context].compilationUnit.untpdTree =
        transformer.transform(summon[Context].compilationUnit.untpdTree)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

  private def lower(
      methodName: String,
      typeParameterName: String,
      ordinaryName: String,
      contextualName: String,
      constructorName: String,
      resultTypeName: String
  )(using Context): untpd.DefDef =
    val semantic = DelegatedForwardingMethodPlan
      .create(
        methodName,
        TypeParameter(BinderId(0), typeParameterName),
        OrdinaryParameter(
          BinderId(1),
          ordinaryName,
          TypeParameterReference(BinderId(0), typeParameterName)
        ),
        ContextualParameter(
          BinderId(2),
          contextualName,
          Applied(
            SourceName(constructorName),
            Vector(TypeParameterReference(BinderId(0), typeParameterName))
          )
        ),
        SourceName(resultTypeName),
        ForwardingBody(
          ContextualReference(BinderId(2)),
          methodName,
          OrdinaryReference(BinderId(1))
        )
      )
      .fold(problem => fail(problem.message), identity)
    DelegatedForwardingMethodUntypedLowerer
      .lower(semantic)
      .fold(problem => fail(problem.message), identity)

  private def parseOne(source: String)(using outerContext: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed = new Parsers.Parser(SourceFile.virtual("Phase143Parser.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        packageDef.stats.head.asInstanceOf[untpd.DefDef]
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def assertExactShape(
      definition: untpd.DefDef,
      methodName: String,
      typeParameterName: String,
      ordinaryName: String,
      contextualName: String,
      constructorName: String,
      resultTypeName: String
  )(using Context): Unit =
    assertEquals(definition.name.toString, methodName)
    assertEquals(definition.mods.flags, Flags.Method)
    val typeParameter = definition.leadingTypeParams match
      case value :: Nil => value
      case other => fail(s"expected one Type parameter, found $other")
    assertEquals(typeParameter.name.toString, typeParameterName)
    assertEquals(typeParameter.mods.flags, Flags.Param)
    typeParameter.rhs match
      case untpd.TypeBoundsTree(lo, hi, alias) =>
        assert(lo.isEmpty)
        assert(hi.isEmpty)
        assert(alias.isEmpty)
      case other => fail(s"expected unbounded TypeBoundsTree, found $other")
    definition.trailingParamss match
      case List(List(ordinary: untpd.ValDef), List(contextual: untpd.ValDef)) =>
        assertEquals(ordinary.name.toString, ordinaryName)
        assertEquals(ordinary.mods.flags, Flags.Param)
        assertIdent(ordinary.tpt, typeParameterName)
        assertEquals(contextual.name.toString, contextualName)
        assertEquals(contextual.mods.flags, Flags.Param | Flags.Given)
        assertApplied(contextual.tpt, constructorName, typeParameterName)
      case other => fail(s"expected one ordinary and one contextual clause, found $other")
    assertIdent(definition.tpt, resultTypeName)
    definition.rhs match
      case untpd.Apply(
            untpd.Select(untpd.Ident(receiver), selected),
            List(untpd.Ident(argument))
          ) =>
        assertEquals(receiver.toString, contextualName)
        assertEquals(selected.toString, methodName)
        assertEquals(argument.toString, ordinaryName)
      case other => fail(s"expected direct selected application body, found $other")

  private def assertApplied(
      tree: untpd.Tree,
      constructorName: String,
      argumentName: String
  ): Unit =
    tree match
      case untpd.AppliedTypeTree(
            untpd.Ident(constructor),
            List(untpd.Ident(argument))
          ) =>
        assertEquals(constructor.toString, constructorName)
        assertEquals(argument.toString, argumentName)
      case other => fail(s"expected unary AppliedTypeTree, found $other")

  private def assertIdent(tree: untpd.Tree, expected: String): Unit =
    tree match
      case untpd.Ident(name) => assertEquals(name.toString, expected)
      case other => fail(s"expected Ident($expected), found $other")

  private def structure(tree: untpd.Tree)(using Context): String =
    tree match
      case value: untpd.DefDef =>
        s"DefDef(${value.name},${value.mods.flags},${value.paramss.map(_.map(structure))},${structure(value.tpt)},${structure(value.rhs)})"
      case value: untpd.TypeDef =>
        s"TypeDef(${value.name},${value.mods.flags},${structure(value.rhs)})"
      case value: untpd.ValDef =>
        s"ValDef(${value.name},${value.mods.flags},${structure(value.tpt)},${structure(value.rhs)})"
      case value: untpd.TypeBoundsTree =>
        s"TypeBounds(${structure(value.lo)},${structure(value.hi)},${structure(value.alias)})"
      case value: untpd.AppliedTypeTree =>
        s"Applied(${structure(value.tpt)},${value.args.map(structure)})"
      case value: untpd.Apply =>
        s"Apply(${structure(value.fun)},${value.args.map(structure)})"
      case value: untpd.Select => s"Select(${structure(value.qualifier)},${value.name})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName

  private def nonEmptyTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(nonEmptyTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs)
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
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
