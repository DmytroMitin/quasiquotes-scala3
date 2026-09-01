package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

class ExistingUntpdMethodBodyRewriteOriginAdapterTest extends munit.FunSuite:
  test("adapts fresh containers and a single-node replacement at original sites without mutation") {
    withContext {
      val originalRoot = parseClass(
        """@deprecated("fixture", "1")
          |class U003Adapter:
          |  def keep: Int = 1
          |  def change: Int = 2
          |  val opaque: String = "opaque"
          |  def call: Int = change
          |""".stripMargin
      )
      val originalTemplate = originalRoot.rhs.asInstanceOf[untpd.Template]
      val originalTarget = originalTemplate.body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing target"))
      val originalTreeState = allTrees(originalRoot).map(tree =>
        (tree, tree.source, tree.span)
      )
      val originalUntouched = originalTemplate.body.filterNot(_.eq(originalTarget))

      given SourceFile = NoSource
      val replacement = untpd.Number("20", untpd.NumberKind.Whole(10))
      val structural = ExistingUntpdMethodBodyRewriter
        .rewrite(originalRoot, originalTarget, replacement)
        .fold(problem => fail(problem.message), identity)

      val adapted = ExistingUntpdMethodBodyRewriteOriginAdapter
        .adapt(structural)
        .fold(problem => fail(problem.message), identity)

      assert(adapted.structuralResult.eq(structural))
      assert(!adapted.positionedRoot.eq(originalRoot))
      assert(!adapted.positionedRoot.eq(structural.rebuiltRoot))
      assert(!adapted.positionedTemplate.eq(originalTemplate))
      assert(!adapted.positionedTemplate.eq(structural.rebuiltTemplate))
      assert(!adapted.positionedTarget.eq(originalTarget))
      assert(!adapted.positionedTarget.eq(structural.rebuiltTarget))
      assert(!adapted.positionedReplacement.eq(replacement))

      assertEquals(adapted.positionedRoot.source, originalRoot.source)
      assertEquals(adapted.positionedRoot.span, originalRoot.span)
      assertEquals(adapted.positionedTemplate.source, originalTemplate.source)
      assertEquals(adapted.positionedTemplate.span, originalTemplate.span)
      assertEquals(adapted.positionedTarget.source, originalTarget.source)
      assertEquals(adapted.positionedTarget.span, originalTarget.span)
      assertEquals(adapted.positionedReplacement.source, originalTarget.rhs.source)
      assertEquals(adapted.positionedReplacement.span, originalTarget.rhs.span)

      val adaptedUntouched =
        adapted.positionedTemplate.body.filterNot(_.eq(adapted.positionedTarget))
      assertEquals(adaptedUntouched.size, originalUntouched.size)
      assert(
        originalUntouched.zip(adaptedUntouched).forall((left, right) =>
          left.eq(right)
        )
      )
      assert(adapted.positionedRoot.mods.eq(originalRoot.mods))
      assert(adapted.positionedTemplate.constr.eq(originalTemplate.constr))
      assert(adapted.positionedTemplate.parentsOrDerived.eq(originalTemplate.parentsOrDerived))
      assert(adapted.positionedTemplate.derived.eq(originalTemplate.derived))
      assert(adapted.positionedTemplate.self.eq(originalTemplate.self))
      assert(adapted.positionedTarget.mods.eq(originalTarget.mods))
      assert(adapted.positionedTarget.tpt.eq(originalTarget.tpt))
      assert(adapted.positionedTarget.rhs.eq(adapted.positionedReplacement))

      assertEquals(
        adapted.originKinds,
        Vector(
          ExistingUntpdMethodBodyRewriteOriginAdapter.OriginKind.PreservedOriginalObject,
          ExistingUntpdMethodBodyRewriteOriginAdapter.OriginKind.ReconstructedAtOriginalSite,
          ExistingUntpdMethodBodyRewriteOriginAdapter.OriginKind.ReplacementAtTransformationSite
        )
      )
      assert(
        originalTreeState.forall { case (tree, source, span) =>
          tree.source == source && tree.span == span
        }
      )
      Vector[untpd.Tree](
        structural.rebuiltRoot,
        structural.rebuiltTemplate,
        structural.rebuiltTarget,
        structural.replacementBody
      ).foreach { tree =>
        assert(!tree.source.exists)
        assert(!tree.span.exists)
        assertEquals(tree.symbol, NoSymbol)
      }
    }
  }

  test("fails closed outside the selected source-free single-node original-site policy") {
    withContext {
      val originalRoot = parseClass(
        """class U003Invalid:
          |  def change: Int = 2
          |""".stripMargin
      )
      val originalTemplate = originalRoot.rhs.asInstanceOf[untpd.Template]
      val originalTarget = originalTemplate.body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing target"))
      given SourceFile = NoSource
      val replacement = untpd.Number("20", untpd.NumberKind.Whole(10))
      val structural = ExistingUntpdMethodBodyRewriter
        .rewrite(originalRoot, originalTarget, replacement)
        .fold(problem => fail(problem.message), identity)

      assertError("RESULT_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adapt(
          null.asInstanceOf[ExistingUntpdMethodBodyRewriter.Result]
        )
      )

      val multiNodeReplacement = untpd.Apply(
        untpd.Ident(dotty.tools.dotc.core.Names.termName("identity")),
        untpd.Number("20", untpd.NumberKind.Whole(10)) :: Nil
      )
      val multiNode = ExistingUntpdMethodBodyRewriter
        .rewrite(originalRoot, originalTarget, multiNodeReplacement)
        .fold(problem => fail(problem.message), identity)
      assertError("REPLACEMENT_CHILDREN_UNSUPPORTED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adapt(multiNode)
      )

      val contaminated = structural.copy(
        rebuiltRoot = structural.rebuiltRoot
          .cloneIn(originalRoot.source)
          .withSpan(originalRoot.span)
      )
      assertError("SOURCE_FREE_INTERMEDIATE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adapt(contaminated)
      )

      val missingSite = structural.copy(originalTarget = structural.rebuiltTarget)
      assertError("ORIGINAL_SITE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adapt(missingSite)
      )
    }
  }

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U003Adapter.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val builder = Vector.newBuilder[untpd.Tree]
    val traverser = new untpd.UntypedTreeTraverser:
      override def traverse(current: untpd.Tree)(using Context): Unit =
        builder += current
        traverseChildren(current)
    traverser.traverse(tree)
    builder.result()

  private def assertError(expectedCode: String)(
      result: Either[ExistingUntpdMethodBodyRewriteOriginError, ?]
  ): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, expectedCode)
      case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
