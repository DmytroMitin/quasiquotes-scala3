package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.Span

class ExistingUntpdMethodBodyRewriteApplyOriginAdapterTest extends munit.FunSuite:
  test("keeps U003 rejection and uniformly attributes one bounded Apply family without mutation") {
    withContext {
      val originalRoot = parseClass(
        """@deprecated("fixture", "1")
          |class U005Adapter:
          |  def keep: Int = 1
          |  def change: Int = 2
          |  val opaque: String = "opaque"
          |  def call: Int = change
          |""".stripMargin
      )
      val originalTemplate = originalRoot.rhs.asInstanceOf[untpd.Template]
      val originalTarget = methodNamed(originalTemplate.body, "change")
      val originalState = allTrees(originalRoot).map(tree => (tree, tree.source, tree.span))
      val originalUntouched = originalTemplate.body.filterNot(_.eq(originalTarget))

      given SourceFile = NoSource
      val function = untpd.Ident(termName("f"))
      val arguments = List[untpd.Tree](
        untpd.Ident(termName("x")),
        untpd.Number("20", untpd.NumberKind.Whole(10)),
        untpd.Literal(Constant(true))
      )
      val replacement = untpd.Apply(function, arguments)
      val structural = ExistingUntpdMethodBodyRewriter
        .rewrite(originalRoot, originalTarget, replacement)
        .fold(problem => fail(problem.message), identity)

      assertError("REPLACEMENT_CHILDREN_UNSUPPORTED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adapt(structural)
      )
      assertError("REPLACEMENT_CHILDREN_UNSUPPORTED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adapt(
          structural.copy(originalTarget = structural.rebuiltTarget)
        )
      )

      val adapted = ExistingUntpdMethodBodyRewriteOriginAdapter
        .adaptApply(structural)
        .fold(problem => fail(problem.message), identity)
      val positionedApply = adapted.positionedReplacement.asInstanceOf[untpd.Apply]
      val positionedFunction = positionedApply.fun.asInstanceOf[untpd.Ident]

      assert(adapted.structuralResult.eq(structural))
      assert(!adapted.positionedRoot.eq(originalRoot))
      assert(!adapted.positionedRoot.eq(structural.rebuiltRoot))
      assert(!adapted.positionedTemplate.eq(originalTemplate))
      assert(!adapted.positionedTemplate.eq(structural.rebuiltTemplate))
      assert(!adapted.positionedTarget.eq(originalTarget))
      assert(!adapted.positionedTarget.eq(structural.rebuiltTarget))
      assert(!positionedApply.eq(replacement))
      assert(!positionedFunction.eq(function))
      assertEquals(positionedApply.args.size, arguments.size)
      assert(positionedApply.args.zip(arguments).forall((left, right) => !left.eq(right)))

      val replacementSite = (originalTarget.rhs.source, originalTarget.rhs.span)
      val positionedReplacementNodes =
        Vector[untpd.Tree](positionedApply, positionedFunction) ++ positionedApply.args
      positionedReplacementNodes.foreach { tree =>
        assertEquals((tree.source, tree.span), replacementSite)
        assertEquals(tree.symbol, NoSymbol)
        assert(!tree.isInstanceOf[untpd.TypedSplice])
      }

      assert(adapted.positionedRoot.mods.eq(originalRoot.mods))
      assert(adapted.positionedTemplate.constr.eq(originalTemplate.constr))
      assert(adapted.positionedTemplate.parentsOrDerived.eq(originalTemplate.parentsOrDerived))
      assert(adapted.positionedTemplate.derived.eq(originalTemplate.derived))
      assert(adapted.positionedTemplate.self.eq(originalTemplate.self))
      assert(adapted.positionedTarget.mods.eq(originalTarget.mods))
      assert(adapted.positionedTarget.tpt.eq(originalTarget.tpt))
      assert(adapted.positionedTarget.rhs.eq(positionedApply))
      val positionedUntouched =
        adapted.positionedTemplate.body.filterNot(_.eq(adapted.positionedTarget))
      assertEquals(positionedUntouched.size, originalUntouched.size)
      assert(originalUntouched.zip(positionedUntouched).forall((left, right) => left.eq(right)))

      assert(originalState.forall { case (tree, source, span) =>
        tree.source == source && tree.span == span
      })
      (Vector[untpd.Tree](
        structural.rebuiltRoot,
        structural.rebuiltTemplate,
        structural.rebuiltTarget
      ) ++ allTrees(structural.replacementBody)).foreach { tree =>
        assert(!tree.source.exists, clues(tree))
        assert(!tree.span.exists, clues(tree))
        assertEquals(tree.symbol, NoSymbol)
      }
    }
  }

  test("fails closed for non-Apply, function topology, argument count, and argument topology") {
    withFixture { (root, target) =>
      given SourceFile = NoSource
      val ident = untpd.Ident(termName("f"))
      val leaf = untpd.Number("20", untpd.NumberKind.Whole(10))

      assertApplyError("APPLY_REPLACEMENT_REQUIRED")(root, target, leaf)
      assertApplyError("APPLY_ARGUMENT_COUNT_REQUIRED")(root, target, untpd.Apply(ident, Nil))
      assertApplyError("APPLY_ARGUMENT_COUNT_REQUIRED")(
        root,
        target,
        untpd.Apply(ident, List.fill(4)(leaf))
      )

      val selectFunction = untpd.Select(untpd.Ident(termName("service")), termName("f"))
      val nestedFunction = untpd.Apply(ident, leaf :: Nil)
      val typeApplyFunction =
        untpd.TypeApply(ident, untpd.Ident(typeName("Int")) :: Nil)
      List[untpd.Tree](selectFunction, nestedFunction, typeApplyFunction).foreach { function =>
        assertApplyError("APPLY_FUNCTION_IDENT_REQUIRED")(
          root,
          target,
          untpd.Apply(function, leaf :: Nil)
        )
      }

      val childBearingArguments = List[untpd.Tree](
        untpd.Apply(ident, leaf :: Nil),
        untpd.Select(untpd.Ident(termName("service")), termName("value")),
        untpd.TypeApply(ident, untpd.Ident(typeName("Int")) :: Nil),
        untpd.If(untpd.Literal(Constant(true)), leaf, leaf),
        untpd.Tuple(leaf :: leaf :: Nil),
        untpd.Block(leaf :: Nil, leaf),
        untpd.Typed(leaf, untpd.Ident(typeName("Int")))
      )
      (childBearingArguments :+ untpd.EmptyTree).foreach { argument =>
        assertApplyError("APPLY_ARGUMENT_LEAF_REQUIRED")(
          root,
          target,
          untpd.Apply(ident, argument :: Nil)
        )
      }
    }
  }

  test("fails closed for null, provenance contamination, TypedSplice, and missing sites") {
    withFixture { (root, target) =>
      given SourceFile = NoSource
      val replacement = untpd.Apply(
        untpd.Ident(termName("f")),
        untpd.Number("20", untpd.NumberKind.Whole(10)) :: Nil
      )
      val structural = ExistingUntpdMethodBodyRewriter
        .rewrite(root, target, replacement)
        .fold(problem => fail(problem.message), identity)

      assertError("RESULT_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptApply(
          null.asInstanceOf[ExistingUntpdMethodBodyRewriter.Result]
        )
      )

      val sourceful = parseTerm("sourceful")
      val sourceContaminated = structural.copy(replacementBody = sourceful)
      assertError("SOURCE_FREE_INTERMEDIATE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptApply(sourceContaminated)
      )

      val spannedReplacement = untpd
        .Apply(
          untpd.Ident(termName("f")),
          untpd.Number("20", untpd.NumberKind.Whole(10)) :: Nil
        )
        .withSpan(Span(0, 1, 0))
      val spanned = structural.copy(replacementBody = spannedReplacement)
      assertError("SOURCE_FREE_INTERMEDIATE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptApply(spanned)
      )

      val symbol = newSymbol(NoSymbol, termName("u005Symbol"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(termName("symbolBearing")).withType(symbol.termRef)
      val symbolContaminated = structural.copy(
        replacementBody = untpd.Apply(untpd.Ident(termName("f")), symbolBearing :: Nil)
      )
      assertError("SOURCE_FREE_INTERMEDIATE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptApply(symbolContaminated)
      )

      val typedSplice = untpd.TypedSplice(symbolBearing)
      val spliceContaminated = structural.copy(
        replacementBody = untpd.Apply(untpd.Ident(termName("f")), typedSplice :: Nil)
      )
      assertError("SOURCE_FREE_INTERMEDIATE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptApply(spliceContaminated)
      )

      val rebuiltContaminated = structural.copy(
        rebuiltRoot = structural.rebuiltRoot.cloneIn(root.source).withSpan(root.span)
      )
      assertError("SOURCE_FREE_INTERMEDIATE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptApply(rebuiltContaminated)
      )

      val missingSite = structural.copy(originalTarget = structural.rebuiltTarget)
      assertError("ORIGINAL_SITE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptApply(missingSite)
      )
    }
  }

  private def withFixture(
      run: Context ?=> (untpd.TypeDef, untpd.DefDef) => Unit
  ): Unit =
    withContext {
      val root = parseClass(
        """class U005Invalid:
          |  def change: Int = 2
          |""".stripMargin
      )
      val target = methodNamed(root.rhs.asInstanceOf[untpd.Template].body, "change")
      run(root, target)
    }

  private def assertApplyError(expectedCode: String)(
      root: untpd.TypeDef,
      target: untpd.DefDef,
      replacement: untpd.Tree
  )(using Context): Unit =
    val structural = ExistingUntpdMethodBodyRewriter
      .rewrite(root, target, replacement)
      .fold(problem => fail(problem.message), identity)
    assertError(expectedCode)(
      ExistingUntpdMethodBodyRewriteOriginAdapter.adaptApply(structural)
    )

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U005ApplyAdapter.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def parseTerm(source: String)(using outerContext: Context): untpd.Tree =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U005Contaminated.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).expr()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed

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
      result: Either[ExistingUntpdMethodBodyRewriteOriginError, ?]
  ): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, expectedCode)
      case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
