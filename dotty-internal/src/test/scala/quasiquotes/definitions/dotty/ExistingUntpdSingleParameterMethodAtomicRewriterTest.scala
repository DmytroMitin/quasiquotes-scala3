package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.NoSpan

import quasiquotes.types.TypeNormalForm

class ExistingUntpdSingleParameterMethodAtomicRewriterTest extends munit.FunSuite:
  test("atomically rewrites all three fields and preserves only opaque islands") {
    withContext {
      val root = parseClass(
        """class Counter:
          |  val before: Int = 1
          |  def bump(x: AnyVal): AnyVal = x
          |  type After = String
          |""".stripMargin
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
      val view = ExistingUntpdSingleParameterMethodView.capture(captured, 1).toOption.get
      val snapshot = ExistingUntpdClassMemberFilter.allTrees(root).map(tree =>
        (tree, tree.source, tree.span)
      )

      val result = rewrite(view, "Int", "String", selectedApply("Math", "abs", "x"))
      val parameter = result.parameterRewrite.positionedParameter
      val resultType = result.resultRewrite.positionedResultType
      val rhs = result.rhsRewrite.positionedResult.positionedReplacement

      assert(!parameter.eq(view.parameter))
      assert(!parameter.tpt.eq(view.parameterType))
      assert(!resultType.eq(view.resultType))
      assert(!rhs.eq(view.rhs))
      assert(!result.positionedMethod.eq(view.method))
      assert(!result.positionedMethod.eq(result.parameterRewrite.positionedMethod))
      assert(!result.positionedMethod.eq(result.resultRewrite.positionedMethod))
      assert(!result.positionedMethod.eq(result.rhsRewrite.positionedResult.positionedTarget))
      assert(result.positionedMethod.paramss.head.head.eq(parameter))
      assert(result.positionedMethod.tpt.eq(resultType))
      assert(result.positionedMethod.rhs.eq(rhs))
      assert(result.positionedMethod.mods.eq(view.method.mods))
      assert(parameter.mods.eq(view.parameter.mods))
      assert(parameter.rhs.isEmpty)
      assert(!result.positionedTemplate.eq(template))
      assert(!result.positionedRoot.eq(root))
      assert(result.positionedTemplate.constr.eq(template.constr))
      assert(result.positionedTemplate.parentsOrDerived.eq(template.parentsOrDerived))
      assert(result.positionedTemplate.derived.eq(template.derived))
      assert(result.positionedTemplate.self.eq(template.self))
      assert(result.positionedTemplate.body.head.eq(template.body.head))
      assert(result.positionedTemplate.body(1).eq(result.positionedMethod))
      assert(result.positionedTemplate.body(2).eq(template.body(2)))
      assert(ExistingUntpdClassMemberFilter.allTrees(result.positionedRoot).forall(tree =>
        tree != null && tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
      ))
      ExistingUntpdClassMemberFilter.allTrees(root).zip(snapshot).foreach {
        case (current, (original, source, span)) =>
          assert(current.eq(original))
          assertEquals(current.source, source)
          assertEquals(current.span, span)
      }
    }
  }

  test("reuses the accepted three component authorities from the same original view") {
    withContext {
      val view = viewFor(
        """class Differential:
          |  def same(value: AnyVal): AnyVal = value
          |  def same(value: AnyVal): AnyVal = value
          |""".stripMargin,
        1
      )
      val replacement = directApply("identity", untpd.Literal(Constant(7)))
      val result = rewrite(view, "Boolean", "String", replacement)

      assert(result.parameterRewrite.view.eq(view))
      assert(result.resultRewrite.view.eq(view))
      assert(result.rhsRewrite.view.eq(view))
      assertIdent(result.parameterRewrite.positionedParameterType, "Boolean")
      assertIdent(result.resultRewrite.positionedResultType, "String")
      assertEquals(
        result.rhsRewrite.replacementFamily,
        ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily.DirectIdentApply
      )
      assertEquals(result.parameterRewrite.positionedParameterType.source, view.parameterType.source)
      assertEquals(result.resultRewrite.positionedResultType.source, view.resultType.source)
      assertEquals(
        result.rhsRewrite.positionedResult.positionedReplacement.source,
        view.rhs.source
      )
      assert(result.positionedTemplate.body.head.eq(view.captured.members.head.tree))
      assert(result.positionedTemplate.body(1).eq(result.positionedMethod))
    }
  }

  test("admits all inherited RHS families without widening them") {
    withContext {
      val cases = Vector(
        (
          untpd.Literal(Constant(7)),
          ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily.SingleNode
        ),
        (
          directApply("identity", untpd.Ident(termName("value"))),
          ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily.DirectIdentApply
        ),
        (
          selectedApply("Math", "abs", "value"),
          ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily.DirectIdentQualifiedSelectedApply
        )
      )
      cases.foreach { case (replacement, family) =>
        val view = viewFor("class Families:\n  def change(value: AnyVal): AnyVal = value\n", 0)
        val result = rewrite(view, "Int", "Int", replacement)
        assertEquals(result.rhsRewrite.replacementFamily, family)
        assert(!result.parameterRewrite.loweredParameterType.eq(result.resultRewrite.loweredResultType))
        assert(!result.parameterRewrite.positionedParameterType.eq(result.resultRewrite.positionedResultType))
      }
    }
  }

  test("repeated rewrite creates independently fresh fragments and final shells") {
    withContext {
      val view = viewFor("class Repeat:\n  def change(value: AnyVal): AnyVal = value\n", 0)
      val first = rewrite(view, "Int", "Int", selectedApply("Math", "abs", "value"))
      val second = rewrite(view, "Int", "Int", selectedApply("Math", "abs", "value"))
      assert(!first.parameterRewrite.loweredParameterType.eq(second.parameterRewrite.loweredParameterType))
      assert(!first.resultRewrite.loweredResultType.eq(second.resultRewrite.loweredResultType))
      assert(!first.parameterRewrite.positionedParameter.eq(second.parameterRewrite.positionedParameter))
      assert(!first.rhsRewrite.positionedResult.positionedReplacement.eq(second.rhsRewrite.positionedResult.positionedReplacement))
      assert(!first.positionedMethod.eq(second.positionedMethod))
      assert(!first.positionedTemplate.eq(second.positionedTemplate))
      assert(!first.positionedRoot.eq(second.positionedRoot))
    }
  }

  test("fails closed before delegation for null forged and missing transformation sites") {
    withContext {
      val view = viewFor("class Sites:\n  def change(value: AnyVal): AnyVal = value\n", 0)
      assertCode(
        ExistingUntpdSingleParameterMethodAtomicRewriter.rewrite(
          null,
          TypeNormalForm.STypeIdent("Int"),
          TypeNormalForm.STypeIdent("Int"),
          selectedApply("Math", "abs", "value")
        ),
        "VIEW_REQUIRED"
      )
      assertCode(rewriteEither(view.copy(rhs = view.parameterType), "Int", "Int", untpd.Literal(Constant(1))), "VIEW_INVALID")
      assertCode(rewriteEither(view, null, "Int", untpd.Literal(Constant(1))), "PARAMETER_TYPE_REQUIRED")
      assertCode(rewriteEither(view, "Int", null, untpd.Literal(Constant(1))), "RESULT_TYPE_REQUIRED")
      assertCode(rewriteEither(view, "Int", "Int", null), "RHS_REPLACEMENT_REQUIRED")

      Vector(
        view.copy(parameter = null) -> "OLD_PARAMETER_REQUIRED",
        view.copy(parameterType = null) -> "OLD_PARAMETER_TYPE_REQUIRED",
        view.copy(resultType = null) -> "OLD_RESULT_TYPE_REQUIRED",
        view.copy(rhs = null) -> "OLD_RHS_REQUIRED",
        view.copy(method = null) -> "OLD_METHOD_REQUIRED"
      ).foreach { case (missing, code) =>
        assertCode(
          rewriteEither(missing, "Int", "Int", untpd.Literal(Constant(1))),
          code
        )
      }

      Vector(
        view.copy(parameter = withoutSource(view.parameter)) -> "OLD_PARAMETER_PROVENANCE_REQUIRED",
        view.copy(parameterType = withoutSource(view.parameterType)) -> "OLD_PARAMETER_TYPE_PROVENANCE_REQUIRED",
        view.copy(resultType = withoutSource(view.resultType)) -> "OLD_RESULT_TYPE_PROVENANCE_REQUIRED",
        view.copy(rhs = withoutSource(view.rhs)) -> "OLD_RHS_PROVENANCE_REQUIRED",
        view.copy(method = withoutSource(view.method)) -> "OLD_METHOD_PROVENANCE_REQUIRED",
        view.copy(parameter = withoutSpan(view.parameter)) -> "OLD_PARAMETER_PROVENANCE_REQUIRED",
        view.copy(parameterType = withoutSpan(view.parameterType)) -> "OLD_PARAMETER_TYPE_PROVENANCE_REQUIRED",
        view.copy(resultType = withoutSpan(view.resultType)) -> "OLD_RESULT_TYPE_PROVENANCE_REQUIRED",
        view.copy(rhs = withoutSpan(view.rhs)) -> "OLD_RHS_PROVENANCE_REQUIRED",
        view.copy(method = withoutSpan(view.method)) -> "OLD_METHOD_PROVENANCE_REQUIRED"
      ).foreach { case (missing, code) =>
        assertCode(
          rewriteEither(missing, "Int", "Int", untpd.Literal(Constant(1))),
          code
        )
      }

      val missingType = view.parameterType.cloneIn(view.parameterType.source)
      missingType.span = NoSpan
      assertCode(
        rewriteEither(replaceParameter(view, view.parameter, missingType), "Int", "Int", untpd.Literal(Constant(1))),
        "OLD_PARAMETER_TYPE_PROVENANCE_REQUIRED"
      )
      val missingResult = view.resultType.cloneIn(view.resultType.source)
      missingResult.span = NoSpan
      assertCode(
        rewriteEither(view.copy(resultType = missingResult), "Int", "Int", untpd.Literal(Constant(1))),
        "OLD_RESULT_TYPE_PROVENANCE_REQUIRED"
      )
      val missingRhs = view.rhs.cloneIn(view.rhs.source)
      missingRhs.span = NoSpan
      assertCode(
        rewriteEither(view.copy(rhs = missingRhs), "Int", "Int", untpd.Literal(Constant(1))),
        "OLD_RHS_PROVENANCE_REQUIRED"
      )
    }
  }

  test("maps inherited semantic and RHS grammar failures without adding fallback families") {
    withContext {
      val view = viewFor("class Boundary:\n  def change(value: AnyVal): AnyVal = value\n", 0)
      assertCode(rewriteEither(view, "AnyVal", "Int", untpd.Literal(Constant(1))), "PARAMETER_REWRITE_FAILED")
      assertCode(rewriteEither(view, "Int", "AnyVal", untpd.Literal(Constant(1))), "RESULT_REWRITE_FAILED")
      given SourceFile = NoSource
      val unsupported = untpd.Block(Nil, untpd.Ident(termName("value")))
      assertCode(rewriteEither(view, "Int", "Int", unsupported), "RHS_REWRITE_FAILED")
      val malformedApply = untpd.Apply(
        untpd.Ident(termName("f")),
        null.asInstanceOf[List[untpd.Tree]]
      )
      assertCode(rewriteEither(view, "Int", "Int", malformedApply), "RHS_REWRITE_FAILED")
      val sourceBearing = view.rhs
      assertCode(rewriteEither(view, "Int", "Int", sourceBearing), "RHS_REWRITE_FAILED")
      val symbol = newSymbol(NoSymbol, termName("injected"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(termName("value")).withType(symbol.termRef)
      assertCode(rewriteEither(view, "Int", "Int", symbolBearing), "RHS_REWRITE_FAILED")
    }
  }

  test("rejects corrupted component results and accidental type aliasing") {
    withContext {
      val view = viewFor("class Components:\n  def change(value: AnyVal): AnyVal = value\n", 0)
      val parameter = ExistingUntpdSingleParameterMethodParameterTypeRewriter
        .rewrite(view, TypeNormalForm.STypeIdent("Int")).toOption.get
      val resultType = ExistingUntpdSingleParameterMethodResultTypeRewriter
        .rewrite(view, TypeNormalForm.STypeIdent("Int")).toOption.get
      val rhs = ExistingUntpdSingleParameterMethodRhsRewriter
        .rewrite(view, selectedApply("Math", "abs", "value")).toOption.get

      assertCode(
        ExistingUntpdSingleParameterMethodAtomicRewriter.compose(
          view,
          null,
          resultType,
          rhs
        ),
        "PARAMETER_COMPONENT_INVALID"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodAtomicRewriter.compose(
          view,
          parameter,
          null,
          rhs
        ),
        "RESULT_COMPONENT_INVALID"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodAtomicRewriter.compose(
          view,
          parameter.copy(loweredParameterType = null),
          resultType,
          rhs
        ),
        "PARAMETER_COMPONENT_INVALID"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodAtomicRewriter.compose(
          view,
          parameter,
          resultType.copy(loweredResultType = parameter.loweredParameterType),
          rhs
        ),
        "TYPE_FRAGMENT_ALIAS"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodAtomicRewriter.compose(
          view,
          parameter.copy(positionedParameter = view.parameter),
          resultType,
          rhs
        ),
        "PARAMETER_COMPONENT_INVALID"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodAtomicRewriter.compose(
          view,
          parameter,
          resultType.copy(positionedMethod = view.method),
          rhs
        ),
        "RESULT_COMPONENT_INVALID"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodAtomicRewriter.compose(
          view,
          parameter,
          resultType,
          rhs.copy(positionedResult = null)
        ),
        "RHS_COMPONENT_INVALID"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodAtomicRewriter.compose(view, parameter, resultType, null),
        "RHS_COMPONENT_INVALID"
      )
    }
  }

  test("final validator rejects a forged atomic output") {
    withContext {
      val view = viewFor("class Final:\n  def change(value: AnyVal): AnyVal = value\n", 0)
      val result = rewrite(view, "Int", "Int", selectedApply("Math", "abs", "value"))
      assertCode(
        ExistingUntpdSingleParameterMethodAtomicRewriter.validateResult(
          result.copy(positionedMethod = view.method)
        ),
        "FINAL_REWRITE_INVARIANT_FAILED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodAtomicRewriter.validateResult(
          result.copy(positionedTemplate = view.captured.originalTemplate)
        ),
        "FINAL_REWRITE_INVARIANT_FAILED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodAtomicRewriter.validateResult(
          result.copy(
            parameterRewrite = result.parameterRewrite.copy(
              loweredParameterType = null
            )
          )
        ),
        "FINAL_REWRITE_INVARIANT_FAILED"
      )
    }
  }

  private def rewrite(
      view: ExistingUntpdSingleParameterMethodView.View,
      parameterType: String,
      resultType: String,
      rhs: untpd.Tree
  )(using Context): ExistingUntpdSingleParameterMethodAtomicRewriter.Result =
    rewriteEither(view, parameterType, resultType, rhs)
      .fold(problem => fail(problem.message), identity)

  private def rewriteEither(
      view: ExistingUntpdSingleParameterMethodView.View,
      parameterType: String,
      resultType: String,
      rhs: untpd.Tree
  )(using Context) =
    ExistingUntpdSingleParameterMethodAtomicRewriter.rewrite(
      view,
      Option(parameterType).map(TypeNormalForm.STypeIdent(_)).orNull,
      Option(resultType).map(TypeNormalForm.STypeIdent(_)).orNull,
      rhs
    )

  private def selectedApply(
      qualifier: String,
      name: String,
      argument: String
  ): untpd.Tree =
    given SourceFile = NoSource
    untpd.Apply(
      untpd.Select(untpd.Ident(termName(qualifier)), termName(name)),
      List(untpd.Ident(termName(argument)))
    )

  private def directApply(
      name: String,
      argument: untpd.Tree
  ): untpd.Tree =
    given SourceFile = NoSource
    untpd.Apply(untpd.Ident(termName(name)), List(argument))

  private def assertIdent(tree: untpd.Tree, expected: String): Unit = tree match
    case ident: untpd.Ident => assertEquals(ident.name.toString, expected)
    case other => fail(s"expected Ident($expected), found ${other.getClass.getSimpleName}")

  private def withoutSource[A <: untpd.Tree](tree: A): A =
    tree.cloneIn(NoSource).withSpan(tree.span).asInstanceOf[A]

  private def withoutSpan[A <: untpd.Tree](tree: A): A =
    tree.cloneIn(tree.source).withSpan(NoSpan).asInstanceOf[A]

  private def assertCode[A](
      result: Either[ExistingUntpdSingleParameterMethodAtomicRewriteError, A],
      expected: String
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected)
    case Right(value) => fail(s"expected $expected, found success $value")

  private def viewFor(source: String, index: Int)(using Context) =
    val root = parseClass(source)
    val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
    ExistingUntpdSingleParameterMethodView.capture(captured, index).toOption.get

  private def replaceParameter(
      view: ExistingUntpdSingleParameterMethodView.View,
      parameter: untpd.ValDef,
      parameterType: untpd.Tree
  )(using Context): ExistingUntpdSingleParameterMethodView.View =
    val method = untpd.cpy.DefDef(view.method)(
      view.method.name,
      List(List(parameter)),
      view.resultType,
      view.rhs
    )
    val body = view.captured.originalTemplate.body.updated(view.memberIndex, method)
    val template = untpd.cpy.Template(view.captured.originalTemplate)(
      view.captured.originalTemplate.constr,
      view.captured.originalTemplate.parentsOrDerived,
      view.captured.originalTemplate.derived,
      view.captured.originalTemplate.self,
      body
    )
    val root = untpd.cpy.TypeDef(view.captured.originalRoot)(
      view.captured.originalRoot.name,
      template
    )
    val captured = view.captured.copy(
      originalRoot = root,
      originalTemplate = template,
      members = view.captured.members.updated(
        view.memberIndex,
        view.captured.members(view.memberIndex).copy(tree = method)
      )
    )
    view.copy(
      captured = captured,
      method = method,
      parameter = parameter,
      parameterType = parameterType
    )

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U032Atomic.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (root: untpd.TypeDef) :: Nil => root
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
