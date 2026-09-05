package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.types.TypeNormalForm

class ExistingUntpdSingleParameterMethodResultTypeRewriterTest
    extends munit.FunSuite:
  test("rewrites only the selected result type while preserving exact parameter RHS and surrounding members") {
    withContext {
      val root = parseClass(
        """class Converter:
          |  val before: Int = 1
          |  def convert(x: Int): AnyVal = x
          |  type After = String
          |""".stripMargin
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
      val view = ExistingUntpdSingleParameterMethodView.capture(captured, 1).toOption.get
      val originalNodes = ExistingUntpdClassMemberFilter.allTrees(root).map(tree =>
        (tree, tree.source, tree.span)
      )

      val result = ExistingUntpdSingleParameterMethodResultTypeRewriter
        .rewrite(view, TypeNormalForm.STypeIdent("Int"))
        .fold(problem => fail(problem.message), identity)

      assert(result.view.eq(view))
      assertEquals(result.normalForm, TypeNormalForm.STypeIdent("Int"))
      assert(!result.loweredResultType.eq(view.resultType))
      assert(!result.loweredResultType.source.exists)
      assert(!result.loweredResultType.span.exists)
      assertEquals(result.loweredResultType.symbol, NoSymbol)
      assert(!result.positionedResultType.eq(result.loweredResultType))
      assert(!result.positionedResultType.eq(view.resultType))
      assertEquals(result.positionedResultType.source, view.resultType.source)
      assertEquals(result.positionedResultType.span, view.resultType.span)
      assertIdent(result.positionedResultType, "Int")

      assert(!result.positionedMethod.eq(view.method))
      assert(!result.positionedTemplate.eq(template))
      assert(!result.positionedRoot.eq(root))
      assertEquals(result.positionedMethod.source, view.method.source)
      assertEquals(result.positionedMethod.span, view.method.span)
      assertEquals(result.positionedTemplate.source, template.source)
      assertEquals(result.positionedTemplate.span, template.span)
      assertEquals(result.positionedRoot.source, root.source)
      assertEquals(result.positionedRoot.span, root.span)

      val parameter = result.positionedMethod.paramss.head.head.asInstanceOf[untpd.ValDef]
      assert(parameter.eq(view.parameter))
      assert(parameter.tpt.eq(view.parameterType))
      assert(result.positionedMethod.rhs.eq(view.rhs))
      assert(result.positionedMethod.mods.eq(view.method.mods))
      assert(result.positionedTemplate.constr.eq(template.constr))
      assert(result.positionedTemplate.parentsOrDerived.eq(template.parentsOrDerived))
      assert(result.positionedTemplate.derived.eq(template.derived))
      assert(result.positionedTemplate.self.eq(template.self))
      assert(result.positionedTemplate.body.head.eq(template.body.head))
      assert(result.positionedTemplate.body(1).eq(result.positionedMethod))
      assert(result.positionedTemplate.body(2).eq(template.body(2)))

      val currentNodes = ExistingUntpdClassMemberFilter.allTrees(root)
      assertEquals(currentNodes.size, originalNodes.size)
      currentNodes.zip(originalNodes).foreach { case (tree, (original, source, span)) =>
        assert(tree.eq(original))
        assertEquals(tree.source, source)
        assertEquals(tree.span, span)
      }
    }
  }

  test("admits all three primitive semantic result types with renamed and duplicate methods") {
    withContext {
      val fixtures = Vector(
        (
          "String",
          """class Textual:
            |  def same(value: Int): Int = value
            |  def same(payload: String): Any = payload
            |""".stripMargin,
          1
        ),
        (
          "Boolean",
          """class Predicate:
            |  val prefix: Int = 0
            |  def inspect(flag: Boolean): AnyVal = flag
            |""".stripMargin,
          1
        )
      )

      fixtures.foreach { case (primitive, source, index) =>
        val root = parseClass(source)
        val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
        val view = ExistingUntpdSingleParameterMethodView.capture(captured, index).toOption.get
        val result = ExistingUntpdSingleParameterMethodResultTypeRewriter
          .rewrite(view, TypeNormalForm.STypeIdent(primitive))
          .fold(problem => fail(problem.message), identity)

        assertIdent(result.positionedResultType, primitive)
        assert(result.positionedMethod.paramss.head.head.eq(view.parameter))
        assert(result.positionedMethod.rhs.eq(view.rhs))
        captured.members.indices.foreach { memberIndex =>
          if memberIndex != index then
            assert(
              result.positionedTemplate.body(memberIndex).eq(captured.members(memberIndex).tree)
            )
        }
      }
    }
  }

  test("repeated rewrite creates fresh result and owner shells while preserving the same old handles") {
    withContext {
      val root = parseClass("class Repeat:\n  def change(value: Int): AnyVal = value\n")
      val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
      val view = ExistingUntpdSingleParameterMethodView.capture(captured, 0).toOption.get
      val first = ExistingUntpdSingleParameterMethodResultTypeRewriter
        .rewrite(view, TypeNormalForm.STypeIdent("Int"))
        .toOption
        .get
      val second = ExistingUntpdSingleParameterMethodResultTypeRewriter
        .rewrite(view, TypeNormalForm.STypeIdent("Int"))
        .toOption
        .get

      assert(!first.loweredResultType.eq(second.loweredResultType))
      assert(!first.positionedResultType.eq(second.positionedResultType))
      assert(!first.positionedMethod.eq(second.positionedMethod))
      assert(!first.positionedTemplate.eq(second.positionedTemplate))
      assert(!first.positionedRoot.eq(second.positionedRoot))
      List(first, second).foreach { result =>
        assert(result.positionedMethod.paramss.head.head.eq(view.parameter))
        assert(result.positionedMethod.rhs.eq(view.rhs))
      }
    }
  }

  test("fails closed for null stale and unsupported semantic inputs") {
    withContext {
      val root = parseClass("class Boundary:\n  def change(value: Int): AnyVal = value\n")
      val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
      val view = ExistingUntpdSingleParameterMethodView.capture(captured, 0).toOption.get

      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.rewrite(
          null,
          TypeNormalForm.STypeIdent("Int")
        ),
        "VIEW_REQUIRED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.rewrite(
          view.copy(rhs = view.parameterType),
          TypeNormalForm.STypeIdent("Int")
        ),
        "VIEW_INVALID"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.rewrite(
          view.copy(method = null),
          TypeNormalForm.STypeIdent("Int")
        ),
        "VIEW_INVALID"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.rewrite(
          view.copy(resultType = null),
          TypeNormalForm.STypeIdent("Int")
        ),
        "OLD_RESULT_TYPE_REQUIRED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.rewrite(view, null),
        "RESULT_TYPE_REQUIRED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.rewrite(
          view,
          TypeNormalForm.STypeIdent("AnyVal")
        ),
        "RESULT_TYPE_IDENTIFIER_UNSUPPORTED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.rewrite(
          view,
          TypeNormalForm.STypeApply(
            TypeNormalForm.STypeIdent("List"),
            List(TypeNormalForm.STypeIdent("Int"))
          )
        ),
        "RESULT_TYPE_FAMILY_UNSUPPORTED"
      )
      val malformedFamilies = List[TypeNormalForm](
        TypeNormalForm.STypeResolved(null),
        TypeNormalForm.STypeApply(null, null),
        TypeNormalForm.STypeTuple(null),
        TypeNormalForm.STypeFunction(null, null)
      )
      malformedFamilies.foreach { malformed =>
        assertCode(
          ExistingUntpdSingleParameterMethodResultTypeRewriter.rewrite(view, malformed),
          "RESULT_TYPE_FAMILY_UNSUPPORTED"
        )
      }
    }
  }

  test("requires one nonempty source-attributed old result-type site") {
    withContext {
      val view = viewFor("class Site:\n  def change(value: Int): AnyVal = value\n", 0)
      given SourceFile = NoSource
      val sourceFree = untpd.Ident(typeName("AnyVal"))
      val sourceFreeView = replaceResultTypeInView(view, sourceFree)
      val nullSourceView = replaceResultTypeInView(
        view,
        view.resultType.cloneIn(null)
      )
      val emptyView = view.copy(resultType = untpd.EmptyTree)

      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.rewrite(
          sourceFreeView,
          TypeNormalForm.STypeIdent("Int")
        ),
        "OLD_RESULT_TYPE_PROVENANCE_REQUIRED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.rewrite(
          nullSourceView,
          TypeNormalForm.STypeIdent("Int")
        ),
        "OLD_RESULT_TYPE_PROVENANCE_REQUIRED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.rewrite(
          emptyView,
          TypeNormalForm.STypeIdent("Int")
        ),
        "OLD_RESULT_TYPE_REQUIRED"
      )
    }
  }

  test("requires an original-site method shell and rejects owner repair inputs") {
    withContext {
      val view = viewFor("class MethodSite:\n  def change(value: Int): AnyVal = value\n", 0)
      given SourceFile = NoSource
      val sourceFreeMethod = untpd
        .DefDef(
          view.method.name,
          view.method.paramss,
          view.resultType,
          view.rhs
        )
        .withMods(view.method.mods)
      val sourceFreeView = replaceMethodInView(view, sourceFreeMethod)
      val nullSourceView = replaceMethodInView(view, view.method.cloneIn(null))

      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.rewrite(
          sourceFreeView,
          TypeNormalForm.STypeIdent("Int")
        ),
        "OLD_METHOD_PROVENANCE_REQUIRED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.rewrite(
          nullSourceView,
          TypeNormalForm.STypeIdent("Int")
        ),
        "OLD_METHOD_PROVENANCE_REQUIRED"
      )
    }
  }

  test("rejects contaminated lowered types and forged final reconstruction identities") {
    withContext {
      val view = viewFor("class FinalCheck:\n  def change(value: Int): AnyVal = value\n", 0)
      val result = ExistingUntpdSingleParameterMethodResultTypeRewriter
        .rewrite(view, TypeNormalForm.STypeIdent("Int"))
        .toOption
        .get
      val sourceBearingInt = viewFor(
        "class SourceBearing:\n  def change(value: Int): Int = value\n",
        0
      ).resultType

      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.validateResult(
          result.copy(loweredResultType = sourceBearingInt)
        ),
        "LOWERED_RESULT_TYPE_PROVENANCE"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.validateResult(
          result.copy(loweredResultType = result.loweredResultType.cloneIn(null))
        ),
        "LOWERED_RESULT_TYPE_PROVENANCE"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.validateResult(
          result.copy(positionedResultType = view.rhs)
        ),
        "POSITIONED_RESULT_TYPE_TOPOLOGY"
      )

      val clonedParameter = untpd.cpy.ValDef(view.parameter)(
        view.parameter.name,
        view.parameter.tpt,
        view.parameter.rhs
      )
      val forgedMethod = untpd.cpy.DefDef(result.positionedMethod)(
        result.positionedMethod.name,
        List(List(clonedParameter)),
        result.positionedMethod.tpt,
        result.positionedMethod.rhs
      )
      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.validateResult(
          result.copy(positionedMethod = forgedMethod)
        ),
        "FINAL_REWRITE_INVARIANT_FAILED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodResultTypeRewriter.validateResult(
          result.copy(view = view.copy(captured = null))
        ),
        "FINAL_REWRITE_INVARIANT_FAILED"
      )
    }
  }

  private def assertIdent(tree: untpd.Tree, expected: String): Unit = tree match
    case ident: untpd.Ident => assertEquals(ident.name.toString, expected)
    case other => fail(s"expected Ident($expected), found ${other.getClass.getSimpleName}")

  private def assertCode[A](
      result: Either[ExistingUntpdSingleParameterMethodResultTypeRewriteError, A],
      expected: String
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected)
    case Right(value) => fail(s"expected $expected, found success $value")

  private def viewFor(
      source: String,
      index: Int
  )(using Context): ExistingUntpdSingleParameterMethodView.View =
    val root = parseClass(source)
    val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
    ExistingUntpdSingleParameterMethodView.capture(captured, index).toOption.get

  private def replaceResultTypeInView(
      view: ExistingUntpdSingleParameterMethodView.View,
      resultType: untpd.Tree
  )(using Context): ExistingUntpdSingleParameterMethodView.View =
    val method = untpd.cpy.DefDef(view.method)(
      view.method.name,
      view.method.paramss,
      resultType,
      view.rhs
    )
    val originalTemplate = view.captured.originalTemplate
    val body = originalTemplate.body.updated(view.memberIndex, method)
    val template = untpd.cpy.Template(originalTemplate)(
      originalTemplate.constr,
      originalTemplate.parentsOrDerived,
      originalTemplate.derived,
      originalTemplate.self,
      body
    )
    val originalRoot = view.captured.originalRoot
    val root = untpd.cpy.TypeDef(originalRoot)(originalRoot.name, template)
    val members = view.captured.members.updated(
      view.memberIndex,
      ExistingUntpdClassMemberFilter.Member(view.memberIndex, method)
    )
    val captured = ExistingUntpdClassMemberFilter.Capture(root, template, members)
    view.copy(
      captured = captured,
      method = method,
      resultType = resultType
    )

  private def replaceMethodInView(
      view: ExistingUntpdSingleParameterMethodView.View,
      method: untpd.DefDef
  )(using Context): ExistingUntpdSingleParameterMethodView.View =
    val originalTemplate = view.captured.originalTemplate
    val body = originalTemplate.body.updated(view.memberIndex, method)
    val template = untpd.cpy.Template(originalTemplate)(
      originalTemplate.constr,
      originalTemplate.parentsOrDerived,
      originalTemplate.derived,
      originalTemplate.self,
      body
    )
    val originalRoot = view.captured.originalRoot
    val root = untpd.cpy.TypeDef(originalRoot)(originalRoot.name, template)
    val members = view.captured.members.updated(
      view.memberIndex,
      ExistingUntpdClassMemberFilter.Member(view.memberIndex, method)
    )
    val captured = ExistingUntpdClassMemberFilter.Capture(root, template, members)
    view.copy(captured = captured, method = method)

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U030ResultTypeRewrite.scala", source)
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
