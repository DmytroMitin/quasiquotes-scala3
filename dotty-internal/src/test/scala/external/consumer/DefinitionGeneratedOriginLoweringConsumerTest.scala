package external.consumer

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.compiletime.testing.typeCheckErrors
import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser

import quasiquotes.definitions.*
import quasiquotes.definitions.dotty.DefinitionGeneratedOriginLowering
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class DefinitionGeneratedOriginLoweringConsumerTest extends munit.FunSuite:
  test("foreign semantic generated members survive Parser-to-Typer insertion and emit usable class and TASTy files"):
    val temporary = Files.createTempDirectory("semantic-generated-consumer-")
    try
      val source = temporary.resolve("GeneratedConsumers.scala")
      val output = Files.createDirectory(temporary.resolve("classes"))
      val cases = fixtures
      Files.writeString(source, cases.map { fixture =>
        s"object ${fixture.owner}:\n  def run: String = ${fixture.use}\n"
      }.mkString("\n"), StandardCharsets.UTF_8)
      val driver = new ConsumerDriver(cases)
      val reporter = driver.process(Array("-classpath", compilationClasspath, "-d", output.toString, source.toString))
      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(driver.prepared.toMap, cases.map(_.owner -> 1).toMap)
      assertEquals(driver.exactTransfers.toSet, cases.map(_.owner).toSet)
      val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try cases.foreach { fixture =>
        assert(Files.isRegularFile(output.resolve(s"${fixture.owner}$$.class")))
        assert(Files.isRegularFile(output.resolve(s"${fixture.owner}.tasty")))
        val clazz = loader.loadClass(s"${fixture.owner}$$")
        val module = clazz.getField("MODULE$").get(null)
        assertEquals(clazz.getMethod("run").invoke(module), "42", clues(fixture.owner))
      }
      finally loader.close()
    finally
      val stream = Files.walk(temporary)
      try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally stream.close()

  test("foreign callers cannot construct result carriers or access private semantic and backend authorities"):
    assert(typeCheckErrors("new quasiquotes.definitions.dotty.DefinitionGeneratedOriginLowering.Lowered(null, \"\", null)").nonEmpty)
    assert(typeCheckErrors("classOf[quasiquotes.definitions.DefinitionShape]").nonEmpty)
    assert(typeCheckErrors("quasiquotes.definitions.SemanticDefinitionShapeAdapter.adapt(null)").nonEmpty)
    assert(typeCheckErrors("quasiquotes.definitions.dotty.ConstructedDefinitionGeneratedOriginAdapter.lower(null, \"x\")").nonEmpty)
    assert(typeCheckErrors("quasiquotes.definitions.dotty.SimpleTypeAliasGeneratedOriginAdapter.lower(null, \"x\")").nonEmpty)
    assert(typeCheckErrors("classOf[quasiquotes.definitions.dotty.GeneratedOriginDefinitionResult]").nonEmpty)
    assert(typeCheckErrors("quasiquotes.parser.BinderId(0)").nonEmpty)

  private final case class Fixture(owner: String, semantic: SemanticDefinition, source: String, use: String)

  private def fixtures: List[Fixture] =
    val intType = TypeNormalForm.STypeIdent("Int")
    val stringType = TypeNormalForm.STypeIdent("String")
    val ints = TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("List"), List(intType))
    def name(value: String): DefinitionName = right(DefinitionName.fromSource(value))
    def clause(names: String*): DefinitionParameterClause =
      right(DefinitionParameterClause.ordinary(names.toVector.map(n => DefinitionParameter(name(n), intType))))
    def value(n: String): SemanticDefinition = right(SemanticDefinition.immutableValue(name(n), intType, TermShape.Literal("42")))
    def method0(n: String): SemanticDefinition = right(SemanticDefinition.concreteMethod(name(n), Vector.empty, intType)(_ => Right(TermShape.Literal("42"))))
    def method1(n: String, p: String): SemanticDefinition = right(SemanticDefinition.concreteMethod(name(n), Vector(clause(p)), stringType)(_.reference(0, 0).map(ref => TermShape.Select(ref, "toString"))))
    def method2(n: String, p: String, q: String, slot: Int): SemanticDefinition = right(SemanticDefinition.concreteMethod(name(n), Vector(clause(p, q)), intType)(_.reference(0, slot)))
    def alias(n: String): SemanticDefinition = right(SemanticDefinition.typeAlias(name(n), ints))
    List(
      Fixture("GeneratedValue", value("answer"), "val answer: Int = 42", "answer.toString"),
      Fixture("GeneratedRenamedValue", value("result"), "val result: Int = 42", "result.toString"),
      Fixture("GeneratedMethod", method0("answer"), "def answer: Int = 42", "answer.toString"),
      Fixture("GeneratedRenamedMethod", method0("result"), "def result: Int = 42", "result.toString"),
      Fixture("GeneratedOne", method1("foo", "x"), "def foo(x: Int): String = x.toString", "foo(42)"),
      Fixture("GeneratedRenamedOne", method1("render", "item"), "def render(item: Int): String = item.toString", "render(42)"),
      Fixture("GeneratedTwo", method2("choose", "x", "y", 0), "def choose(x: Int, y: Int): Int = x", "choose(42, 7).toString"),
      Fixture("GeneratedRenamedTwo", method2("second", "right", "left", 1), "def second(right: Int, left: Int): Int = left", "second(7, 42).toString"),
      Fixture("GeneratedAlias", alias("T"), "type T = List[Int]", "(List(20, 22): T).sum.toString"),
      Fixture("GeneratedRenamedAlias", alias("Numbers"), "type Numbers = List[Int]", "(List(20, 22): Numbers).sum.toString")
    )

  private def right[A](value: Either[DefinitionSemanticError, A]): A = value.fold(e => fail(e.message), identity)

  private final class ConsumerDriver(cases: List[Fixture]) extends Driver:
    val prepared = scala.collection.mutable.Map.empty[String, Int]
    val exactTransfers = scala.collection.mutable.Set.empty[String]
    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) :: List(new InsertGenerated(cases, ConsumerDriver.this)) :: super.frontendPhases.tail

  // Test-owned insertion demonstrates the future preparation/opaque-transfer seam.
  // It is not an implementation of a public existing-owner transaction.
  private final class InsertGenerated(cases: List[Fixture], evidence: ConsumerDriver) extends Phase:
    def phaseName: String = "semanticGeneratedInsertion"
    override def isCheckable: Boolean = false
    protected def run(using Context): Unit =
      val byOwner = cases.map(f => f.owner -> f).toMap
      val transformer = new untpd.UntypedTreeMap:
        override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match
          case module: untpd.ModuleDef if byOwner.contains(module.name.toString) =>
            val fixture = byOwner(module.name.toString)
            evidence.prepared.update(fixture.owner, evidence.prepared.getOrElse(fixture.owner, 0) + 1)
            val virtualName = s"generated/${fixture.owner}.scala"
            DefinitionGeneratedOriginLowering.lower(fixture.semantic, virtualName) match
              case Left(problem) =>
                // Public consumers branch on the stable code without private error types.
                problem.code match
                  case "INVALID_VIRTUAL_SOURCE" => report.error(s"invalid generated path: ${problem.detail}")
                  case _ => report.error(problem.message)
                module
              case Right(result) =>
                assertEquals(result.generatedSource, fixture.source)
                assertEquals(result.sourceFile.path, virtualName)
                assertEquals(result.virtualSourceName, virtualName)
                assertEquals(result.sourceFile.content.mkString, fixture.source)
                allTrees(result.tree).foreach { node =>
                  assert(node.source eq result.sourceFile)
                  assert(node.span.exists && node.span.start >= 0 && node.span.start <= node.span.point && node.span.point <= node.span.end && node.span.end <= result.generatedSource.length)
                  assertEquals(node.symbol, NoSymbol)
                  assert(!node.isInstanceOf[untpd.TypedSplice])
                }
                val template = module.impl
                val updated = untpd.cpy.Template(template)(template.constr, template.parentsOrDerived, template.derived, template.self, template.body :+ result.tree)
                assert(updated.body.last eq result.tree)
                template.body.zip(updated.body).foreach { case (old, kept) => assert(old eq kept) }
                evidence.exactTransfers += fixture.owner
                untpd.cpy.ModuleDef(module)(module.name, updated)
          case _ => super.transform(tree)
      summon[Context].compilationUnit.untpdTree = transformer.transform(summon[Context].compilationUnit.untpdTree)

  private def allTrees(tree: untpd.Tree)(using Context): List[untpd.Tree] =
    if tree.isEmpty then Nil
    else tree :: (tree match
      case t: untpd.TypeDef => allTrees(t.rhs)
      case t: untpd.DefDef => t.paramss.flatten.flatMap(allTrees) ::: allTrees(t.tpt) ::: allTrees(t.rhs)
      case t: untpd.ValDef => allTrees(t.tpt) ::: allTrees(t.rhs)
      case t: untpd.Select => allTrees(t.qualifier)
      case t: untpd.AppliedTypeTree => allTrees(t.tpt) ::: t.args.flatMap(allTrees)
      case _ => Nil)

  private def compilationClasspath: String =
    Vector(classOf[scala.Option[?]], classOf[scala.deriving.Mirror], classOf[Compiler], classOf[SemanticDefinition], getClass)
      .flatMap(c => Option(c.getProtectionDomain).flatMap(d => Option(d.getCodeSource)).map(_.getLocation.toURI))
      .map(Path.of(_).toString).distinct.mkString(java.io.File.pathSeparator)
