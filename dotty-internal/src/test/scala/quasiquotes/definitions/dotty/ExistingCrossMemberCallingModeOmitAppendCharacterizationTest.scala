package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.{DefinitionName, DefinitionSemanticError, SemanticDefinition}
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.{Capture, Failure, MethodView}
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class ExistingCrossMemberCallingModeOmitAppendCharacterizationTest
    extends munit.FunSuite:
  private val IntType = TypeNormalForm.STypeIdent("Int")

  test("canonical owner pins ordered refs wrappers omission target and untouched identity") {
    withContext {
      val root = parseClass(Canonical)
      val raw = directMembers(root)
      val eval = raw(0).asInstanceOf[untpd.DefDef]
      val sum = raw(1).asInstanceOf[untpd.DefDef]
      val obsolete = raw(2).asInstanceOf[untpd.ValDef]
      val untouched = raw(3).asInstanceOf[untpd.ValDef]
      val evalParameter = parameter(eval)
      val sumParameter = parameter(sum)

      assertEquals(raw.map(_.asInstanceOf[untpd.MemberDef].name.toString),
        Vector("eval", "sum", "obsolete", "untouched"))
      assertEquals(eval.mods.flags, Flags.Method)
      assertEquals(sum.mods.flags, Flags.Method)
      assertEquals(evalParameter.mods.flags, Flags.Param)
      assertEquals(sumParameter.mods.flags, Flags.Param)
      assert(evalParameter.tpt.isInstanceOf[untpd.ByNameTypeTree])
      val repeated = sumParameter.tpt.asInstanceOf[untpd.PostfixOp]
      val (_, star) = repeated match
        case untpd.PostfixOp(element, marker) => (element, marker)
      assertEquals(star.asInstanceOf[untpd.Ident].name.toString, "*")
      assertEquals(obsolete.name.toString, "obsolete")
      assertEquals(untouched.name.toString, "untouched")
      raw.foreach { tree =>
        assert(tree.source.eq(root.source))
        assert(tree.span.exists)
      }

      val captured = capture(root)
      assertEquals(captured.members.map(_.index), Vector(0, 1, 2, 3))
      assertEquals(captured.members.map(_.kind.code), Vector("METHOD", "METHOD", "VALUE", "VALUE"))
      assertEquals(captured.members.map(_.diagnosticName),
        Vector(Some("eval"), Some("sum"), Some("obsolete"), Some("untouched")))
      val evalView = method(captured, 0)
      val sumView = method(captured, 1)
      val evalParameterView = evalView.parameterClauses.head.parameters.head
      val sumParameterView = sumView.parameterClauses.head.parameters.head
      assert(!evalView.ref.eq(sumView.ref))
      assert(!evalParameterView.ref.eq(sumParameterView.ref))
      assert(evalParameterView.identity.sameObjectAs(new ExistingClassUntypedRewrite.ExactIdentity(evalParameter)))
      assert(sumParameterView.identity.sameObjectAs(new ExistingClassUntypedRewrite.ExactIdentity(sumParameter)))
      assert(captured.members(2).identity.sameObjectAs(new ExistingClassUntypedRewrite.ExactIdentity(obsolete)))
      assert(captured.members(3).identity.sameObjectAs(new ExistingClassUntypedRewrite.ExactIdentity(untouched)))
      assertPublicCode(evalView.parameterScope.reference(sumParameterView.ref), "SELECTION_FAILED")
      assertPublicCode(sumView.parameterScope.reference(evalParameterView.ref), "SELECTION_FAILED")
      assertPreTyperClean(root)
    }
  }

  test("public append creates one deterministic generated value after exact originals") {
    withContext {
      val root = parseClass(Canonical.replace("MultipleModesAndMembers", "AppendOnlyModes"))
      val captured = capture(root)
      val path = "generated/U062AppendOnlyBonus.scala"
      val plan = publicRight(captured.emptyPlan.append(generatedValue("bonus", "5"), path))
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val rewritten = directMembers(result.tree)

      assert(result.changed)
      assert(!result.tree.eq(root))
      assertEquals(rewritten.size, 5)
      captured.members.indices.foreach(index =>
        assert(rewritten(index).eq(directMembers(root)(index)))
        assert(result.directMemberIdentities(index).sameObjectAs(captured.members(index).identity))
      )
      val generated = rewritten.last.asInstanceOf[untpd.ValDef]
      assertEquals(generated.name.toString, "bonus")
      assertEquals(generated.source.path, path)
      assert(generated.source.content.mkString.contains("bonus"))
      assertEquals(generated.span.start, 0)
      assertEquals(generated.span.end, generated.source.content.length)
      assert(result.directMemberIdentities.last.sameObjectAs(new ExistingClassUntypedRewrite.ExactIdentity(generated)))
      assertPreTyperClean(result.tree)
    }
  }

  test("no-op remains exact and strict control remains ordinary") {
    withContext {
      val root = parseClass(
        "class U062StrictControl:\n" +
          "  def eval(value: Int): Int = value\n" +
          "  def sum(values: Int*): Int = values.size\n" +
          "  val obsolete: Int = 101\n" +
          "  val untouched: Int = 7\n"
      )
      val captured = capture(root)
      val strict = parameter(directMembers(root).head.asInstanceOf[untpd.DefDef])
      assert(strict.tpt.isInstanceOf[untpd.Ident])
      val result = publicRight(ExistingClassUntypedRewrite(captured, captured.emptyPlan))
      assert(!result.changed)
      assert(result.tree.eq(root))
      assert(result.directMemberIdentities.zip(captured.members).forall { case (actual, expected) =>
        actual.sameObjectAs(expected.identity)
      })
    }
  }

  private def generatedValue(label: String, literal: String): SemanticDefinition =
    semantic(SemanticDefinition.immutableValue(
      semantic(DefinitionName.fromSource(label)),
      IntType,
      TermShape.Literal(literal)
    ))

  private def directMembers(root: untpd.TypeDef)(using Context): Vector[untpd.Tree] =
    root.rhs.asInstanceOf[untpd.Template].body.toVector

  private def parameter(method: untpd.DefDef): untpd.ValDef =
    method.paramss.head.head.asInstanceOf[untpd.ValDef]

  private def capture(root: untpd.TypeDef)(using Context): Capture =
    publicRight(ExistingClassUntypedRewrite.capture(root))

  private def method(captured: Capture, index: Int)(using Context): MethodView =
    publicRight(captured.method(publicRight(captured.member(index)).ref))

  private def assertPreTyperClean(root: untpd.Tree)(using Context): Unit =
    val trees = ExistingUntpdClassMemberFilter.allTrees(root)
    assert(trees.forall(_.symbol == NoSymbol))
    assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U062Characterization.scala", source)
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
      case Left(problem) => assertEquals(problem.code, expected)
      case Right(success) => fail("expected " + expected + ", found success " + success)

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)

  private val Canonical =
    """class MultipleModesAndMembers:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |""".stripMargin
