package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.{Name, termName, typeName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.{NoSource, SourceFile}

class ExistingUntpdMethodBodyRewriteSelectedApplyOriginAdapterTest
    extends munit.FunSuite:
  test("keeps U003/U005 rejection and uniformly attributes one selected Apply without mutation") {
    withFixture { (originalRoot, originalTarget) =>
      val originalTemplate = originalRoot.rhs.asInstanceOf[untpd.Template]
      val originalState = allTrees(originalRoot).map(tree => (tree, tree.source, tree.span))
      val originalUntouched = originalTemplate.body.filterNot(_.eq(originalTarget))

      given SourceFile = NoSource
      val qualifier = untpd.Ident(termName("service"))
      val selection = untpd.Select(qualifier, termName("invoke"))
      val arguments = List[untpd.Tree](
        untpd.Ident(termName("x")),
        untpd.Number("20", untpd.NumberKind.Whole(10)),
        untpd.Literal(Constant(true))
      )
      val replacement = untpd.Apply(selection, arguments)
      val structural = rewrite(originalRoot, originalTarget, replacement)

      assertError("REPLACEMENT_CHILDREN_UNSUPPORTED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adapt(structural)
      )
      assertError("APPLY_FUNCTION_IDENT_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptApply(structural)
      )

      val adapted = ExistingUntpdMethodBodyRewriteOriginAdapter
        .adaptSelectedApply(structural)
        .fold(problem => fail(problem.message), identity)
      val positionedApply = adapted.positionedReplacement.asInstanceOf[untpd.Apply]
      val positionedSelection = positionedApply.fun.asInstanceOf[untpd.Select]
      val positionedQualifier = positionedSelection.qualifier.asInstanceOf[untpd.Ident]

      assert(adapted.structuralResult.eq(structural))
      assert(!adapted.positionedRoot.eq(originalRoot))
      assert(!adapted.positionedRoot.eq(structural.rebuiltRoot))
      assert(!adapted.positionedTemplate.eq(originalTemplate))
      assert(!adapted.positionedTemplate.eq(structural.rebuiltTemplate))
      assert(!adapted.positionedTarget.eq(originalTarget))
      assert(!adapted.positionedTarget.eq(structural.rebuiltTarget))
      assert(!positionedApply.eq(replacement))
      assert(!positionedSelection.eq(selection))
      assert(!positionedQualifier.eq(qualifier))
      assert(positionedApply.args.zip(arguments).forall((left, right) => !left.eq(right)))
      assertEquals(positionedSelection.name, selection.name)

      val replacementSite = (originalTarget.rhs.source, originalTarget.rhs.span)
      allTrees(positionedApply).foreach { tree =>
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
      assert(originalUntouched.zip(positionedUntouched).forall((left, right) => left.eq(right)))
      assert(originalState.forall { case (tree, source, span) =>
        tree.source == source && tree.span == span
      })
      (Vector[untpd.Tree](
        structural.rebuiltRoot,
        structural.rebuiltTemplate,
        structural.rebuiltTarget
      ) ++ allTrees(structural.replacementBody)).foreach { tree =>
        assert(!tree.source.exists)
        assert(!tree.span.exists)
        assertEquals(tree.symbol, NoSymbol)
      }
    }
  }

  test("adapts the exact two-argument selected Apply boundary in order") {
    withFixture { (root, target) =>
      given SourceFile = NoSource
      val replacement = untpd.Apply(
        untpd.Select(untpd.Ident(termName("service")), termName("invoke")),
        List(
          untpd.Ident(termName("x")),
          untpd.Number("20", untpd.NumberKind.Whole(10))
        )
      )
      val adapted = ExistingUntpdMethodBodyRewriteOriginAdapter
        .adaptSelectedApply(rewrite(root, target, replacement))
        .fold(problem => fail(problem.message), identity)
      val positioned = adapted.positionedReplacement.asInstanceOf[untpd.Apply]

      assertEquals(positioned.args.size, 2)
      assertEquals(positioned.args.head.asInstanceOf[untpd.Ident].name.toString, "x")
      assertEquals(positioned.args(1).asInstanceOf[untpd.Number].digits, "20")
      allTrees(positioned).foreach(tree =>
        assertEquals(tree.source, target.rhs.source)
        assertEquals(tree.span, target.rhs.span)
      )
    }
  }

  test("fails closed for selected Apply topology, count, names, qualifier, and arguments") {
    withFixture { (root, target) =>
      given SourceFile = NoSource
      val qualifier = untpd.Ident(termName("service"))
      val selection = untpd.Select(qualifier, termName("invoke"))
      val leaf = untpd.Number("20", untpd.NumberKind.Whole(10))
      val directFunction = untpd.Ident(termName("f"))

      assertSelectedError("SELECTED_APPLY_REPLACEMENT_REQUIRED")(root, target, leaf)
      assertSelectedError("SELECTED_APPLY_REPLACEMENT_REQUIRED")(
        root,
        target,
        null.asInstanceOf[untpd.Tree]
      )
      List[untpd.Tree](
        directFunction,
        untpd.Apply(directFunction, leaf :: Nil),
        untpd.TypeApply(directFunction, untpd.Ident(typeName("Int")) :: Nil)
      ).foreach(function =>
        assertSelectedError("SELECTED_APPLY_FUNCTION_SELECT_REQUIRED")(
          root,
          target,
          untpd.Apply(function, leaf :: Nil)
        )
      )
      assertSelectedError("SELECTED_APPLY_ARGUMENT_COUNT_REQUIRED")(
        root,
        target,
        untpd.Apply(selection, Nil)
      )
      assertSelectedError("SELECTED_APPLY_ARGUMENT_COUNT_REQUIRED")(
        root,
        target,
        untpd.Apply(selection, List.fill(4)(leaf))
      )

      List[untpd.Tree](
        untpd.Select(qualifier, termName("nested")),
        untpd.Apply(directFunction, leaf :: Nil),
        untpd.TypeApply(directFunction, untpd.Ident(typeName("Int")) :: Nil),
        untpd.Literal(Constant("service"))
      ).foreach(invalidQualifier =>
        assertSelectedError("SELECTED_APPLY_QUALIFIER_IDENT_REQUIRED")(
          root,
          target,
          untpd.Apply(untpd.Select(invalidQualifier, termName("invoke")), leaf :: Nil)
        )
      )
      assertSelectedError("SELECTED_APPLY_QUALIFIER_IDENT_REQUIRED")(
        root,
        target,
        untpd.Apply(
          untpd.Select(null.asInstanceOf[untpd.Tree], termName("invoke")),
          leaf :: Nil
        )
      )
      assertSelectedError("SELECTED_APPLY_NAME_TERM_REQUIRED")(
        root,
        target,
        untpd.Apply(untpd.Select(qualifier, typeName("Invoke")), leaf :: Nil)
      )
      assertSelectedError("SELECTED_APPLY_NAME_TERM_REQUIRED")(
        root,
        target,
        untpd.Apply(
          untpd.Select(qualifier, null.asInstanceOf[Name]),
          leaf :: Nil
        )
      )

      val childBearingArguments = List[untpd.Tree](
        untpd.Apply(directFunction, leaf :: Nil),
        untpd.Select(qualifier, termName("value")),
        untpd.TypeApply(directFunction, untpd.Ident(typeName("Int")) :: Nil),
        untpd.If(untpd.Literal(Constant(true)), leaf, leaf),
        untpd.Tuple(leaf :: leaf :: Nil),
        untpd.Block(leaf :: Nil, leaf),
        untpd.Typed(leaf, untpd.Ident(typeName("Int"))),
        untpd.EmptyTree,
        null.asInstanceOf[untpd.Tree]
      )
      childBearingArguments.foreach(argument =>
        assertSelectedError("SELECTED_APPLY_ARGUMENT_LEAF_REQUIRED")(
          root,
          target,
          untpd.Apply(selection, argument :: Nil)
        )
      )
    }
  }

  test("fails closed for null, provenance contamination, TypedSplice, and missing sites") {
    withFixture { (root, target) =>
      given SourceFile = NoSource
      def replacement(argument: untpd.Tree): untpd.Apply =
        untpd.Apply(
          untpd.Select(untpd.Ident(termName("service")), termName("invoke")),
          argument :: Nil
        )
      val leaf = untpd.Number("20", untpd.NumberKind.Whole(10))
      val structural = rewrite(root, target, replacement(leaf))
      def freshStructural(): ExistingUntpdMethodBodyRewriter.Result =
        rewrite(
          root,
          target,
          replacement(untpd.Number("20", untpd.NumberKind.Whole(10)))
        )

      assertError("RESULT_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptSelectedApply(
          null.asInstanceOf[ExistingUntpdMethodBodyRewriter.Result]
        )
      )
      val parsedReplacement = parseTerm("service.invoke(20)").asInstanceOf[untpd.Apply]
      assertError("SOURCE_FREE_INTERMEDIATE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptSelectedApply(
          structural.copy(replacementBody = parsedReplacement)
        )
      )
      val parsedSelection = parsedReplacement.fun.asInstanceOf[untpd.Select]
      assertError("SOURCE_FREE_INTERMEDIATE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptSelectedApply(
          freshStructural().copy(
            replacementBody = untpd.Apply(
              parsedSelection,
              untpd.Number("20", untpd.NumberKind.Whole(10)) :: Nil
            )
          )
        )
      )
      assertError("SOURCE_FREE_INTERMEDIATE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptSelectedApply(
          freshStructural().copy(
            replacementBody = untpd.Apply(
              untpd.Select(parsedSelection.qualifier, termName("invoke")),
              untpd.Number("20", untpd.NumberKind.Whole(10)) :: Nil
            )
          )
        )
      )
      assertError("SOURCE_FREE_INTERMEDIATE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptSelectedApply(
          freshStructural().copy(
            replacementBody = replacement(parsedReplacement.args.head)
          )
        )
      )
      assertError("SOURCE_FREE_INTERMEDIATE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptSelectedApply(
          structural.copy(
            replacementBody = replacement(
              untpd.Number("20", untpd.NumberKind.Whole(10))
            ).withSpan(Span(0, 1, 0))
          )
        )
      )
      assertError("SOURCE_FREE_INTERMEDIATE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptSelectedApply(
          freshStructural().copy(
            replacementBody = untpd.Apply(
              untpd.Select(
                untpd.Ident(termName("service")).withSpan(Span(0, 1, 0)),
                termName("invoke")
              ),
              untpd.Number("20", untpd.NumberKind.Whole(10)) :: Nil
            )
          )
        )
      )
      val symbol = newSymbol(NoSymbol, termName("u013Symbol"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(termName("symbolBearing")).withType(symbol.termRef)
      assertError("SOURCE_FREE_INTERMEDIATE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptSelectedApply(
          structural.copy(replacementBody = replacement(symbolBearing))
        )
      )
      assertError("SOURCE_FREE_INTERMEDIATE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptSelectedApply(
          structural.copy(replacementBody = replacement(untpd.TypedSplice(symbolBearing)))
        )
      )
      assertError("SOURCE_FREE_INTERMEDIATE_REQUIRED")(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptSelectedApply(
          freshStructural().copy(
            rebuiltRoot = structural.rebuiltRoot.cloneIn(root.source).withSpan(root.span)
          )
        )
      )
      val missingOriginalSiteResults = List(
        freshStructural().copy(originalRoot = structural.rebuiltRoot),
        freshStructural().copy(originalTemplate = structural.rebuiltTemplate),
        freshStructural().copy(originalTarget = structural.rebuiltTarget),
        freshStructural().copy(
          originalTarget = untpd
            .cpy
            .DefDef(target)(target.name, target.paramss, target.tpt, structural.replacementBody)
            .cloneIn(target.source)
            .withSpan(target.span)
        )
      )
      missingOriginalSiteResults.foreach(value =>
        assertError("ORIGINAL_SITE_REQUIRED")(
          ExistingUntpdMethodBodyRewriteOriginAdapter.adaptSelectedApply(value)
        )
      )
    }
  }

  private def withFixture(
      run: Context ?=> (untpd.TypeDef, untpd.DefDef) => Unit
  ): Unit =
    withContext {
      val root = parseClass(
        """@deprecated("fixture", "1")
          |class U013Adapter:
          |  def keep: Int = 1
          |  def change: Int = 2
          |  val opaque: String = "opaque"
          |  def call: Int = change
          |""".stripMargin
      )
      val target = methodNamed(root.rhs.asInstanceOf[untpd.Template].body, "change")
      run(root, target)
    }

  private def rewrite(
      root: untpd.TypeDef,
      target: untpd.DefDef,
      replacement: untpd.Tree
  )(using Context): ExistingUntpdMethodBodyRewriter.Result =
    ExistingUntpdMethodBodyRewriter
      .rewrite(root, target, replacement)
      .fold(problem => fail(problem.message), identity)

  private def assertSelectedError(expectedCode: String)(
      root: untpd.TypeDef,
      target: untpd.DefDef,
      replacement: untpd.Tree
  )(using Context): Unit =
    val structural = rewrite(root, target, validReplacement)
    assertError(expectedCode)(
      ExistingUntpdMethodBodyRewriteOriginAdapter.adaptSelectedApply(
        structural.copy(replacementBody = replacement)
      )
    )

  private def validReplacement(using SourceFile): untpd.Apply =
    untpd.Apply(
      untpd.Select(untpd.Ident(termName("service")), termName("invoke")),
      untpd.Number("20", untpd.NumberKind.Whole(10)) :: Nil
    )

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U013SelectedApplyAdapter.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def parseTerm(source: String)(using outerContext: Context): untpd.Tree =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U013Contaminated.scala", source)
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
