package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.{DefinitionName, DefinitionSemanticError, SemanticDefinition}
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.{Capture, Failure}
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class ExistingGeneratedSiblingReferenceResolutionCharacterizationTest
    extends munit.FunSuite:
  private val IntType = TypeNormalForm.STypeIdent("Int")

  test("public semantic method accepts a free sibling Apply and generated lowering keeps it unresolved") {
    withContext {
      val semanticDefinition = generatedApplyMethod("bonus", "sum", List("10", "20", "30"))
      val semanticBody = semanticDefinition.asMethod.flatMap(_.body).getOrElse(fail("missing method body"))
      semanticBody match
        case TermShape.Apply(TermShape.Identifier(name, false), arguments) =>
          assertEquals(name, "sum")
          assertEquals(arguments, List("10", "20", "30").map(TermShape.Literal(_)))
        case other => fail("unexpected semantic body: " + other)

      val path = "generated/U063RawBonus.scala"
      val lowered = DefinitionGeneratedOriginLowering.lower(semanticDefinition, path)
        .fold(problem => fail(problem.message), identity)
      assertEquals(lowered.generatedSource, "def bonus: Int = sum(10, 20, 30)")
      assertEquals(lowered.sourceFile.path, path)
      assertGeneratedApply(lowered.tree, "bonus", "sum", 3, path)
      assertPreTyperClean(lowered.tree)
    }
  }

  test("public append retains exact originals and installs the requested raw free-name Apply last") {
    withContext {
      val root = parseClass(Canonical.replace("GeneratedReferenceOwner", "U063AppendOnly"))
      val original = directMembers(root)
      val captured = capture(root)
      val path = "generated/U063AppendOnlyBonus.scala"
      val plan = publicRight(captured.emptyPlan.append(
        generatedApplyMethod("bonus", "sum", List("10", "20", "30")),
        path
      ))
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val members = directMembers(result.tree)

      assert(result.changed)
      assertEquals(memberNames(members), Vector("eval", "sum", "obsolete", "untouched", "bonus"))
      original.indices.foreach { index =>
        assert(members(index).eq(original(index)))
        assert(result.directMemberIdentities(index).sameObjectAs(captured.members(index).identity))
      }
      assertGeneratedApply(members.last, "bonus", "sum", 3, path)
      assertPreTyperClean(result.tree)
    }
  }

  test("bounded retained-value control keeps a direct free name for ordinary Typer") {
    withContext {
      val root = parseClass(Canonical.replace("GeneratedReferenceOwner", "U063RetainedValue"))
      val original = directMembers(root)
      val captured = capture(root)
      val path = "generated/U063RetainedValueBonus.scala"
      val plan = publicRight(captured.emptyPlan.append(
        generatedIdentifierMethod("bonus", "untouched"),
        path
      ))
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val members = directMembers(result.tree)
      val generated = members.last.asInstanceOf[untpd.DefDef]

      assertEquals(memberNames(members), Vector("eval", "sum", "obsolete", "untouched", "bonus"))
      assert(members(3).eq(original(3)))
      generated.rhs match
        case identifier: untpd.Ident => assertEquals(identifier.name.toString, "untouched")
        case other => fail("expected free untouched Ident, found " + other)
      assertEquals(generated.source.path, path)
      assertPreTyperClean(result.tree)
    }
  }

  test("invalid virtual source is rejected before generated preparation") {
    withContext {
      val captured = capture(parseClass(Canonical))
      assertPublicCode(
        captured.emptyPlan.append(
          generatedApplyMethod("bonus", "sum", List("10", "20", "30")),
          " invalid/U063.scala"
        ),
        "SEMANTIC_FRAGMENT_INVALID"
      )
    }
  }

  private def generatedApplyMethod(
      label: String,
      sibling: String,
      literals: List[String]
  ): SemanticDefinition =
    semantic(SemanticDefinition.concreteMethod(
      semantic(DefinitionName.fromSource(label)),
      Vector.empty,
      IntType
    )(_ => Right(TermShape.Apply(
      TermShape.Identifier(sibling, false),
      literals.map(TermShape.Literal(_))
    ))))

  private def generatedIdentifierMethod(label: String, sibling: String): SemanticDefinition =
    semantic(SemanticDefinition.concreteMethod(
      semantic(DefinitionName.fromSource(label)),
      Vector.empty,
      IntType
    )(_ => Right(TermShape.Identifier(sibling, false))))

  private def assertGeneratedApply(
      tree: untpd.Tree,
      methodName: String,
      siblingName: String,
      argumentCount: Int,
      path: String
  )(using Context): Unit =
    val generated = tree.asInstanceOf[untpd.DefDef]
    assertEquals(generated.name.toString, methodName)
    assertEquals(generated.source.path, path)
    assertEquals(generated.span.start, 0)
    assertEquals(generated.span.end, generated.source.content.length)
    generated.rhs match
      case untpd.Apply(function: untpd.Ident, arguments) =>
        assertEquals(function.name.toString, siblingName)
        assertEquals(arguments.size, argumentCount)
        assert(function.source.eq(generated.source))
        assert(arguments.forall(_.source.eq(generated.source)))
      case other => fail("expected raw free-name Apply, found " + other)

  private def memberNames(members: Vector[untpd.Tree]): Vector[String] =
    members.map(_.asInstanceOf[untpd.MemberDef].name.toString)

  private def directMembers(root: untpd.TypeDef)(using Context): Vector[untpd.Tree] =
    root.rhs.asInstanceOf[untpd.Template].body.toVector

  private def capture(root: untpd.TypeDef)(using Context): Capture =
    publicRight(ExistingClassUntypedRewrite.capture(root))

  private def assertPreTyperClean(tree: untpd.Tree)(using Context): Unit =
    val trees = ExistingUntpdClassMemberFilter.allTrees(tree)
    assert(trees.forall(_.symbol == NoSymbol))
    assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U063Characterization.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def semantic[A](value: Either[DefinitionSemanticError, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def publicRight[A](value: Either[Failure, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def assertPublicCode[A](value: Either[Failure, A], expected: String): Unit =
    value match
      case Left(problem) => assertEquals(problem.code, expected, clues(problem))
      case Right(success) => fail("expected " + expected + ", found success " + success)

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)

  private val Canonical =
    """class GeneratedReferenceOwner:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |""".stripMargin
