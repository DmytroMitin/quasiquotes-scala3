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

class ExistingUntpdTwoParameterMethodRhsRewriterTest extends munit.FunSuite:
  test("preserves both exact parameters types result and unrelated members for a leaf RHS") {
    withContext {
      val root = parseClass(
        """class RenamedOwner:
          |  val before: Int = 1
          |  def renamed(left: Int, right: String): String = left match
          |    case 0 => right
          |    case _ => right.reverse
          |  type After = String
          |""".stripMargin
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val view = viewFor(root, 1)
      val originalNodes = allTrees(root).map(tree => (tree, tree.source, tree.span))
      given SourceFile = NoSource
      val replacement = untpd.Literal(Constant("rewritten"))

      val result = ExistingUntpdTwoParameterMethodRhsRewriter
        .rewrite(view, replacement)
        .fold(problem => fail(problem.message), identity)

      assertEquals(
        result.replacementFamily,
        ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily.SingleNode
      )
      assertTwoParameterIdentity(view, result.structuralResult.rebuiltTarget)
      assertTwoParameterIdentity(view, result.positionedResult.positionedTarget)
      assert(result.structuralResult.rebuiltTarget.rhs.eq(replacement))
      assert(result.positionedResult.positionedTarget.rhs.eq(
        result.positionedResult.positionedReplacement
      ))
      assert(result.structuralResult.rebuiltTemplate.body.head.eq(template.body.head))
      assert(result.structuralResult.rebuiltTemplate.body(2).eq(template.body(2)))
      assert(result.positionedResult.positionedTemplate.body.head.eq(template.body.head))
      assert(result.positionedResult.positionedTemplate.body(2).eq(template.body(2)))
      assertShellAndReplacementOrigins(view, result)
      assertOriginalUnchanged(root, originalNodes)
    }
  }

  test("uses authoritative overload index and admits direct and selected Apply replacements") {
    withContext {
      val root = parseClass(
        """class Overloaded:
          |  def choose(x: String, y: String): String = x + y
          |  def choose(x: Int, y: Int): Int = x - y
          |""".stripMargin
      )
      val first = viewFor(root, 0)
      val second = viewFor(root, 1)
      given SourceFile = NoSource
      val direct = untpd.Apply(
        untpd.Ident(termName("max")),
        untpd.Ident(termName("x")) :: untpd.Ident(termName("y")) :: Nil
      )
      val selected = untpd.Apply(
        untpd.Select(untpd.Ident(termName("Math")), termName("max")),
        untpd.Ident(termName("x")) :: untpd.Ident(termName("y")) :: Nil
      )

      val directResult = ExistingUntpdTwoParameterMethodRhsRewriter
        .rewrite(second, direct)
        .fold(problem => fail(problem.message), identity)
      val selectedResult = ExistingUntpdTwoParameterMethodRhsRewriter
        .rewrite(second, selected)
        .fold(problem => fail(problem.message), identity)

      assert(directResult.positionedResult.positionedTemplate.body.head.eq(first.method))
      assert(selectedResult.positionedResult.positionedTemplate.body.head.eq(first.method))
      assertEquals(
        directResult.replacementFamily,
        ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily.DirectIdentApply
      )
      assertEquals(
        selectedResult.replacementFamily,
        ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily.DirectIdentQualifiedSelectedApply
      )
      assertTwoParameterIdentity(second, directResult.positionedResult.positionedTarget)
      assertTwoParameterIdentity(second, selectedResult.positionedResult.positionedTarget)
      assertUniformReplacementSite(second, directResult)
      assertUniformReplacementSite(second, selectedResult)
    }
  }

  test("repeated rewriting from one view creates fresh shells and replacement graphs") {
    withContext {
      val root = parseClass("class Repeat:\n  def choose(x: Int, y: Int): Int = x - y\n")
      val view = viewFor(root, 0)
      val originalNodes = allTrees(root).map(tree => (tree, tree.source, tree.span))
      def replacement(using SourceFile): untpd.Tree =
        untpd.Apply(
          untpd.Select(untpd.Ident(termName("Math")), termName("max")),
          untpd.Ident(termName("x")) :: untpd.Ident(termName("y")) :: Nil
        )
      given SourceFile = NoSource

      val first = ExistingUntpdTwoParameterMethodRhsRewriter
        .rewrite(view, replacement)
        .fold(problem => fail(problem.message), identity)
      val second = ExistingUntpdTwoParameterMethodRhsRewriter
        .rewrite(view, replacement)
        .fold(problem => fail(problem.message), identity)

      assert(!first.structuralResult.rebuiltRoot.eq(second.structuralResult.rebuiltRoot))
      assert(!first.structuralResult.rebuiltTemplate.eq(second.structuralResult.rebuiltTemplate))
      assert(!first.structuralResult.rebuiltTarget.eq(second.structuralResult.rebuiltTarget))
      assert(!first.positionedResult.positionedRoot.eq(second.positionedResult.positionedRoot))
      assert(!first.positionedResult.positionedTemplate.eq(second.positionedResult.positionedTemplate))
      assert(!first.positionedResult.positionedTarget.eq(second.positionedResult.positionedTarget))
      allTrees(first.positionedResult.positionedReplacement)
        .zip(allTrees(second.positionedResult.positionedReplacement))
        .foreach((left, right) => assert(!left.eq(right)))
      assertTwoParameterIdentity(view, first.positionedResult.positionedTarget)
      assertTwoParameterIdentity(view, second.positionedResult.positionedTarget)
      assertOriginalUnchanged(root, originalNodes)
    }
  }

  test("fails closed for null empty source span symbol and TypedSplice replacements") {
    withContext {
      val view = viewFor(
        parseClass("class Boundary:\n  def change(x: Int, y: Int): Int = x + y\n"),
        0
      )
      assertCode(ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(view, null), "REPLACEMENT_BODY_REQUIRED")
      assertCode(
        ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(view, untpd.EmptyTree),
        "REPLACEMENT_BODY_REQUIRED"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(view, parseTerm("sourceValue")),
        "REPLACEMENT_SOURCE_PROVENANCE"
      )

      given SourceFile = NoSource
      val spanned = untpd.Number("1", untpd.NumberKind.Whole(10)).withSpan(Span(0, 1, 0))
      assertCode(ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(view, spanned), "REPLACEMENT_SPAN_PROVENANCE")
      val symbol = newSymbol(NoSymbol, termName("u034Symbol"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(termName("x")).withType(symbol.termRef)
      assertCode(
        ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(view, symbolBearing),
        "REPLACEMENT_SYMBOL_PROVENANCE"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(view, untpd.TypedSplice(symbolBearing)),
        "REPLACEMENT_TYPED_SPLICE_UNSUPPORTED"
      )
    }
  }

  test("returns structured errors for malformed replacement descendants") {
    withContext {
      val view = viewFor(
        parseClass("class Malformed:\n  def change(x: Int, y: Int): Int = x + y\n"),
        0
      )
      given SourceFile = NoSource
      val function = untpd.Ident(termName("f"))
      val leaf = untpd.Literal(Constant(1))
      val malformed = List[untpd.Tree](
        untpd.Apply(null.asInstanceOf[untpd.Tree], leaf :: Nil),
        untpd.Apply(untpd.Select(null.asInstanceOf[untpd.Tree], termName("f")), leaf :: Nil),
        untpd.Apply(function, null.asInstanceOf[untpd.Tree] :: Nil),
        untpd.Apply(function, null.asInstanceOf[List[untpd.Tree]])
      )
      malformed.foreach(value =>
        assertCode(
          ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(view, value),
          "REPLACEMENT_GRAPH_MALFORMED"
        )
      )
    }
  }

  test("rejects forged views malformed captures duplicate target identity and index mismatch") {
    withContext {
      val root = parseClass(
        """class Captured:
          |  def before(x: Int, y: Int): Int = x
          |  def change(x: Int, y: Int): Int = y
          |""".stripMargin
      )
      val view = viewFor(root, 1)
      given SourceFile = NoSource
      val replacement = untpd.Number("7", untpd.NumberKind.Whole(10))

      assertCode(ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(null, replacement), "VIEW_REQUIRED")
      assertCode(
        ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(
          view.copy(secondParameterType = view.firstParameterType),
          replacement
        ),
        "VIEW_INVALID"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(
          view.copy(captured = view.captured.copy(members = view.captured.members.reverse)),
          replacement
        ),
        "VIEW_INVALID"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(view.copy(memberIndex = 0), replacement),
        "VIEW_INVALID"
      )

      val duplicateTemplate = untpd.cpy.Template(view.captured.originalTemplate)(
        view.captured.originalTemplate.constr,
        view.captured.originalTemplate.parentsOrDerived,
        view.captured.originalTemplate.derived,
        view.captured.originalTemplate.self,
        List(view.method, view.method)
      )
      val duplicateRoot = untpd.cpy.TypeDef(view.captured.originalRoot)(
        view.captured.originalRoot.name,
        duplicateTemplate
      )
      val duplicateCapture = ExistingUntpdClassMemberFilter.Capture(
        duplicateRoot,
        duplicateTemplate,
        Vector(
          ExistingUntpdClassMemberFilter.Member(0, view.method),
          ExistingUntpdClassMemberFilter.Member(1, view.method)
        )
      )
      assertCode(
        ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(
          view.copy(captured = duplicateCapture, memberIndex = 0),
          replacement
        ),
        "TARGET_IDENTITY_NOT_UNIQUE"
      )
    }
  }

  test("rejects every replacement outside the exact inherited family constraints") {
    withContext {
      val view = viewFor(
        parseClass("class Family:\n  def change(x: Int, y: Int): Int = x + y\n"),
        0
      )
      given SourceFile = NoSource
      val number = untpd.Literal(Constant(1))
      assertCode(
        ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(
          view,
          untpd.Block(number :: Nil, untpd.Literal(Constant(2)))
        ),
        "REPLACEMENT_FAMILY_UNSUPPORTED"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(
          view,
          untpd.Apply(untpd.Ident(termName("f")), Nil)
        ),
        "APPLY_ARGUMENT_COUNT_REQUIRED"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(
          view,
          untpd.Apply(
            untpd.Ident(termName("f")),
            untpd.Apply(untpd.Ident(termName("g")), number :: Nil) :: Nil
          )
        ),
        "APPLY_ARGUMENT_LEAF_REQUIRED"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(
          view,
          untpd.Apply(
            untpd.Select(untpd.Ident(termName("service")), typeName("Member")),
            number :: Nil
          )
        ),
        "SELECTED_APPLY_NAME_TERM_REQUIRED"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(
          view,
          untpd.Apply(
            untpd.Select(untpd.Ident(termName("service")), termName("f")),
            List.fill(4)(number)
          )
        ),
        "SELECTED_APPLY_ARGUMENT_COUNT_REQUIRED"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodRhsRewriter.rewrite(
          view,
          untpd.Apply(
            untpd.Select(
              untpd.Select(untpd.Ident(termName("outer")), termName("service")),
              termName("f")
            ),
            number :: Nil
          )
        ),
        "REPLACEMENT_FAMILY_UNSUPPORTED"
      )
    }
  }

  test("rejects final parameter type result and member identity drift") {
    withContext {
      val root = parseClass(
        """class FinalCheck:
          |  val before: Int = 1
          |  def change(x: Int, y: String): String = y
          |  val after: Int = 2
          |""".stripMargin
      )
      val view = viewFor(root, 1)
      given SourceFile = NoSource
      val replacement = untpd.Literal(Constant("ok"))
      val result = ExistingUntpdTwoParameterMethodRhsRewriter
        .rewrite(view, replacement)
        .fold(problem => fail(problem.message), identity)
      val positioned = result.positionedResult
      val clonedFirst = untpd.cpy.ValDef(view.firstParameter)(
        view.firstParameter.name,
        view.firstParameter.tpt,
        view.firstParameter.rhs
      )
      val wrongParameters = untpd.cpy.DefDef(positioned.positionedTarget)(
        positioned.positionedTarget.name,
        List(List(clonedFirst, view.secondParameter)),
        positioned.positionedTarget.tpt,
        positioned.positionedTarget.rhs
      )
      assertFinalCode(view, result, positioned.copy(positionedTarget = wrongParameters))

      val wrongFirstType = untpd.cpy.ValDef(view.firstParameter)(
        view.firstParameter.name,
        view.secondParameterType,
        view.firstParameter.rhs
      )
      val typeDrift = untpd.cpy.DefDef(positioned.positionedTarget)(
        positioned.positionedTarget.name,
        List(List(wrongFirstType, view.secondParameter)),
        positioned.positionedTarget.tpt,
        positioned.positionedTarget.rhs
      )
      assertFinalCode(view, result, positioned.copy(positionedTarget = typeDrift))

      val resultTypeDrift = untpd.cpy.DefDef(positioned.positionedTarget)(
        positioned.positionedTarget.name,
        positioned.positionedTarget.paramss,
        view.firstParameterType,
        positioned.positionedTarget.rhs
      )
      assertFinalCode(view, result, positioned.copy(positionedTarget = resultTypeDrift))

      val wrongBody = positioned.positionedTemplate.body.updated(0, positioned.positionedTarget)
      val memberDrift = untpd.cpy.Template(positioned.positionedTemplate)(
        positioned.positionedTemplate.constr,
        positioned.positionedTemplate.parentsOrDerived,
        positioned.positionedTemplate.derived,
        positioned.positionedTemplate.self,
        wrongBody
      )
      assertFinalCode(view, result, positioned.copy(positionedTemplate = memberDrift))
    }
  }

  test("retains structured origin errors for malformed structural carriers") {
    withContext {
      val view = viewFor(
        parseClass("class Carrier:\n  def change(x: Int, y: Int): Int = x + y\n"),
        0
      )
      given SourceFile = NoSource
      val replacement = untpd.Apply(
        untpd.Select(untpd.Ident(termName("Math")), termName("max")),
        untpd.Ident(termName("x")) :: untpd.Ident(termName("y")) :: Nil
      )
      val structural = ExistingUntpdMethodBodyRewriter
        .rewriteTwoParameter(view, replacement)
        .fold(problem => fail(problem.message), identity)
      val result = ExistingUntpdMethodBodyRewriteOriginAdapter.adaptSelectedApply(
        structural.copy(rebuiltTarget = null)
      )
      result match
        case Left(problem) => assertEquals(problem.code, "ORIGIN_ADAPTATION_FAILED")
        case Right(value) => fail(s"expected ORIGIN_ADAPTATION_FAILED, found $value")
    }
  }

  private def assertFinalCode(
      view: ExistingUntpdTwoParameterMethodView.View,
      result: ExistingUntpdTwoParameterMethodRhsRewriter.Result,
      positioned: ExistingUntpdMethodBodyRewriteOriginAdapter.Result
  )(using Context): Unit =
    assertCode(
      ExistingUntpdTwoParameterMethodRhsRewriter.validateResult(
        view,
        result.replacementFamily,
        result.structuralResult,
        positioned
      ),
      "FINAL_REWRITE_INVARIANT_FAILED"
    )

  private def assertTwoParameterIdentity(
      view: ExistingUntpdTwoParameterMethodView.View,
      method: untpd.DefDef
  ): Unit =
    assertEquals(method.paramss.size, 1)
    assertEquals(method.paramss.head.size, 2)
    val first = method.paramss.head(0).asInstanceOf[untpd.ValDef]
    val second = method.paramss.head(1).asInstanceOf[untpd.ValDef]
    assert(first.eq(view.firstParameter))
    assert(first.tpt.eq(view.firstParameterType))
    assert(second.eq(view.secondParameter))
    assert(second.tpt.eq(view.secondParameterType))
    assert(method.tpt.eq(view.resultType))

  private def assertShellAndReplacementOrigins(
      view: ExistingUntpdTwoParameterMethodView.View,
      result: ExistingUntpdTwoParameterMethodRhsRewriter.Result
  )(using Context): Unit =
    val structural = result.structuralResult
    val positioned = result.positionedResult
    Vector[untpd.Tree](structural.rebuiltRoot, structural.rebuiltTemplate, structural.rebuiltTarget)
      .foreach(tree =>
        assert(!tree.source.exists)
        assert(!tree.span.exists)
        assertEquals(tree.symbol, NoSymbol)
      )
    assertEquals(positioned.positionedRoot.source, view.captured.originalRoot.source)
    assertEquals(positioned.positionedRoot.span, view.captured.originalRoot.span)
    assertEquals(positioned.positionedTemplate.source, view.captured.originalTemplate.source)
    assertEquals(positioned.positionedTemplate.span, view.captured.originalTemplate.span)
    assertEquals(positioned.positionedTarget.source, view.method.source)
    assertEquals(positioned.positionedTarget.span, view.method.span)
    assertUniformReplacementSite(view, result)

  private def assertUniformReplacementSite(
      view: ExistingUntpdTwoParameterMethodView.View,
      result: ExistingUntpdTwoParameterMethodRhsRewriter.Result
  )(using Context): Unit =
    val structuralNodes = allTrees(result.structuralResult.replacementBody)
    val positionedNodes = allTrees(result.positionedResult.positionedReplacement)
    assertEquals(structuralNodes.size, positionedNodes.size)
    structuralNodes.foreach(tree =>
      assert(!tree.source.exists)
      assert(!tree.span.exists)
      assertEquals(tree.symbol, NoSymbol)
    )
    structuralNodes.zip(positionedNodes).foreach((left, right) => assert(!left.eq(right)))
    positionedNodes.foreach(tree =>
      assertEquals(tree.source, view.rhs.source)
      assertEquals(tree.span, view.rhs.span)
      assertEquals(tree.symbol, NoSymbol)
      assert(!tree.isInstanceOf[untpd.TypedSplice])
    )

  private def assertOriginalUnchanged(
      root: untpd.TypeDef,
      original: Vector[(untpd.Tree, SourceFile, Span)]
  )(using Context): Unit =
    val current = allTrees(root)
    assertEquals(current.size, original.size)
    current.zip(original).foreach { case (tree, (oldTree, source, span)) =>
      assert(tree.eq(oldTree))
      assertEquals(tree.source, source)
      assertEquals(tree.span, span)
    }

  private def viewFor(
      root: untpd.TypeDef,
      memberIndex: Int
  )(using Context): ExistingUntpdTwoParameterMethodView.View =
    val captured = ExistingUntpdClassMemberFilter
      .capture(root)
      .fold(problem => fail(problem.message), identity)
    ExistingUntpdTwoParameterMethodView
      .capture(captured, memberIndex)
      .fold(problem => fail(problem.message), identity)

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U034RhsRewrite.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def parseTerm(source: String)(using outer: Context): untpd.Tree =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U034Replacement.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).expr()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ExistingUntpdClassMemberFilter.allTrees(tree)

  private def assertCode[A](
      result: Either[ExistingUntpdTwoParameterMethodRhsRewriteError, A],
      expected: String
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected)
    case Right(value) => fail(s"expected $expected, found success $value")

  private def withContext(test: Context ?=> Unit): Unit =
    given Context = new ContextBase().initialCtx.fresh
    test
