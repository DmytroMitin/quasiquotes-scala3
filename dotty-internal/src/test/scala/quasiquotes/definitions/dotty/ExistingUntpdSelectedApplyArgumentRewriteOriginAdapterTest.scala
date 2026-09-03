package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

class ExistingUntpdSelectedApplyArgumentRewriteOriginAdapterTest
    extends munit.FunSuite:
  test("positions only reconstructed containers and the replacement at their granular original sites") {
    withContext {
      val root = parseClass(
        """@deprecated("fixture", "1")
          |class U014Origin:
          |  object service:
          |    def invoke(a: Int, b: Int): Int = a + b
          |  val oldArg: Int = 1
          |  val keptArg: Int = 20
          |  def keep: Int = 1
          |  def change: Int = service.invoke(oldArg, keptArg)
          |  val opaque: String = "opaque"
          |""".stripMargin
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val target = methodNamed(template.body, "change")
      val originalApply = target.rhs.asInstanceOf[untpd.Apply]
      val originalFunction = originalApply.fun
      val originalArgument = originalApply.args.head
      val untouchedArgument = originalApply.args(1)
      val originalState = allTrees(root).map(tree => (tree, tree.source, tree.span))

      given SourceFile = NoSource
      val replacement = untpd.Number("2", untpd.NumberKind.Whole(10))
      val structural = ExistingUntpdSelectedApplyArgumentRewriter
        .rewrite(root, target, originalArgument, replacement)
        .fold(problem => fail(problem.message), identity)
      val adapted = ExistingUntpdSelectedApplyArgumentRewriteOriginAdapter
        .adapt(structural)
        .fold(problem => fail(problem.message), identity)

      assert(adapted.structuralResult.eq(structural))
      assert(!adapted.positionedRoot.eq(root))
      assert(!adapted.positionedRoot.eq(structural.rebuiltRoot))
      assert(!adapted.positionedTemplate.eq(template))
      assert(!adapted.positionedTemplate.eq(structural.rebuiltTemplate))
      assert(!adapted.positionedTarget.eq(target))
      assert(!adapted.positionedTarget.eq(structural.rebuiltTarget))
      assert(!adapted.positionedApply.eq(originalApply))
      assert(!adapted.positionedApply.eq(structural.rebuiltApply))
      assert(!adapted.positionedReplacement.eq(originalArgument))
      assert(!adapted.positionedReplacement.eq(replacement))
      assert(adapted.positionedApply.fun.eq(originalFunction))
      assert(adapted.positionedApply.args(1).eq(untouchedArgument))
      assert(adapted.positionedApply.args.head.eq(adapted.positionedReplacement))
      assertEquals(adapted.positionedApply.source, originalApply.source)
      assertEquals(adapted.positionedApply.span, originalApply.span)
      assertEquals(adapted.positionedReplacement.source, originalArgument.source)
      assertEquals(adapted.positionedReplacement.span, originalArgument.span)
      assertEquals(adapted.positionedReplacement.symbol, NoSymbol)
      assert(originalState.forall { case (tree, source, span) =>
        tree.source == source && tree.span == span
      })
      Vector[untpd.Tree](
        structural.rebuiltRoot,
        structural.rebuiltTemplate,
        structural.rebuiltTarget,
        structural.rebuiltApply,
        structural.replacementLeaf
      ).foreach { tree =>
        assert(!tree.source.exists, clues(tree))
        assert(!tree.span.exists, clues(tree))
        assertEquals(tree.symbol, NoSymbol)
      }
    }
  }

  test("fails closed for null, source-bearing, identity-invalid, and missing-site inputs") {
    withContext {
      assertError("RESULT_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriteOriginAdapter.adapt(
          null.asInstanceOf[ExistingUntpdSelectedApplyArgumentRewriter.Result]
        )
      )

      val root = parseClass(
        "class U014OriginFailures:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val target = methodNamed(template.body, "change")
      val apply = target.rhs.asInstanceOf[untpd.Apply]
      val argument = apply.args.head
      given SourceFile = NoSource
      val replacement = untpd.Number("2", untpd.NumberKind.Whole(10))
      val structural = ExistingUntpdSelectedApplyArgumentRewriter
        .rewrite(root, target, argument, replacement)
        .fold(problem => fail(problem.message), identity)

      assertError("SOURCE_FREE_STRUCTURAL_RESULT_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriteOriginAdapter.adapt(
          structural.copy(replacementLeaf = argument)
        )
      )
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriteOriginAdapter.adapt(
          structural.copy(
            rebuiltApply = untpd.Apply(
              untpd.Ident(termName("differentFunction")),
              structural.rebuiltApply.args
            )
          )
        )
      )
      val sourceFreeOriginalRoot = untpd
        .TypeDef(structural.originalRoot.name, structural.originalTemplate)
        .withMods(structural.originalRoot.mods)
      assertError("ORIGINAL_SITE_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriteOriginAdapter.adapt(
          structural.copy(originalRoot = sourceFreeOriginalRoot)
        )
      )

      val wrongRoot = untpd
        .TypeDef(typeName("WrongRootName"), structural.rebuiltTemplate)
        .withMods(structural.originalRoot.mods)
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriteOriginAdapter.adapt(
          structural.copy(rebuiltRoot = wrongRoot)
        )
      )
      val wrongTarget = untpd
        .DefDef(
          termName("wrongTargetName"),
          structural.rebuiltTarget.paramss,
          structural.rebuiltTarget.tpt,
          structural.rebuiltApply
        )
        .withMods(structural.rebuiltTarget.mods)
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriteOriginAdapter.adapt(
          structural.copy(rebuiltTarget = wrongTarget)
        )
      )

      val childBearingReplacement = untpd.Apply(untpd.Ident(termName("nested")), Nil)
      val childBearingApply = untpd.Apply(
        structural.originalApply.fun,
        childBearingReplacement :: structural.originalApply.args.tail
      )
      val childBearingTarget = untpd
        .DefDef(
          structural.originalTarget.name,
          structural.originalTarget.paramss,
          structural.originalTarget.tpt,
          childBearingApply
        )
        .withMods(structural.originalTarget.mods)
      val childBearingTemplate = untpd.Template(
        structural.originalTemplate.constr,
        structural.originalTemplate.parentsOrDerived,
        structural.originalTemplate.derived,
        structural.originalTemplate.self,
        structural.prefix ::: childBearingTarget :: structural.suffix
      )
      val childBearingRoot = untpd
        .TypeDef(structural.originalRoot.name, childBearingTemplate)
        .withMods(structural.originalRoot.mods)
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriteOriginAdapter.adapt(
          structural.copy(
            replacementLeaf = childBearingReplacement,
            rebuiltApply = childBearingApply,
            rebuiltTarget = childBearingTarget,
            rebuiltTemplate = childBearingTemplate,
            rebuiltRoot = childBearingRoot
          )
        )
      )
    }
  }

  test("rewrites and adapts the middle of three arguments while preserving both siblings") {
    withContext {
      val root = parseClass(
        "class U014ThreeArguments:\n  def change: Int = service.invoke(firstArg, oldArg, lastArg)\n"
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val target = methodNamed(template.body, "change")
      val apply = target.rhs.asInstanceOf[untpd.Apply]
      val first = apply.args.head
      val middle = apply.args(1)
      val last = apply.args(2)
      given SourceFile = NoSource
      val replacement = untpd.Number("2", untpd.NumberKind.Whole(10))

      val structural = ExistingUntpdSelectedApplyArgumentRewriter
        .rewrite(root, target, middle, replacement)
        .fold(problem => fail(problem.message), identity)
      val adapted = ExistingUntpdSelectedApplyArgumentRewriteOriginAdapter
        .adapt(structural)
        .fold(problem => fail(problem.message), identity)

      assertEquals(structural.argumentIndex, 1)
      assert(structural.rebuiltApply.args.head.eq(first))
      assert(structural.rebuiltApply.args(1).eq(replacement))
      assert(structural.rebuiltApply.args(2).eq(last))
      assert(adapted.positionedApply.args.head.eq(first))
      assert(adapted.positionedApply.args(1).eq(adapted.positionedReplacement))
      assert(adapted.positionedApply.args(2).eq(last))
      assertEquals(adapted.positionedReplacement.source, middle.source)
      assertEquals(adapted.positionedReplacement.span, middle.span)
    }
  }

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U014Origin.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def methodNamed(body: List[untpd.Tree], name: String): untpd.DefDef =
    body.collectFirst {
      case value: untpd.DefDef if value.name.toString == name => value
    }.getOrElse(fail(s"missing method $name"))

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val builder = Vector.newBuilder[untpd.Tree]
    val traverser = new untpd.UntypedTreeTraverser:
      override def traverse(current: untpd.Tree)(using Context): Unit =
        builder += current
        traverseChildren(current)
    traverser.traverse(tree)
    builder.result()

  private def assertError(expectedCode: String)(
      result: Either[ExistingUntpdSelectedApplyArgumentRewriteOriginError, ?]
  ): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, expectedCode)
      case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
