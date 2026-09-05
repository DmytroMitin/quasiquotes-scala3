package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.NoSpan

import quasiquotes.types.TypeNormalForm

class ExistingUntpdSingleParameterMethodParameterTypeRewriterTest
    extends munit.FunSuite:
  test("rewrites only the selected parameter type and preserves exact result RHS and opaque islands") {
    withContext {
      val root = parseClass(
        """class Counter:
          |  val before: Int = 1
          |  def bump(x: AnyVal): Int = x
          |  type After = String
          |""".stripMargin
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
      val view = ExistingUntpdSingleParameterMethodView.capture(captured, 1).toOption.get
      val snapshot = ExistingUntpdClassMemberFilter.allTrees(root).map(tree =>
        (tree, tree.source, tree.span)
      )

      val result = rewrite(view, "Int")
      val parameter = result.positionedParameter

      assert(result.view.eq(view))
      assert(!result.loweredParameterType.source.exists)
      assert(!result.loweredParameterType.span.exists)
      assertEquals(result.loweredParameterType.symbol, NoSymbol)
      assert(!result.positionedParameterType.eq(view.parameterType))
      assertEquals(result.positionedParameterType.source, view.parameterType.source)
      assertEquals(result.positionedParameterType.span, view.parameterType.span)
      assertIdent(result.positionedParameterType, "Int")
      assert(!parameter.eq(view.parameter))
      assert(parameter.tpt.eq(result.positionedParameterType))
      assert(parameter.rhs.isEmpty)
      assertEquals(parameter.name, view.parameter.name)
      assert(parameter.mods.eq(view.parameter.mods))
      assertEquals(parameter.source, view.parameter.source)
      assertEquals(parameter.span, view.parameter.span)
      assert(!result.positionedMethod.eq(view.method))
      assert(result.positionedMethod.paramss.head.head.eq(parameter))
      assert(result.positionedMethod.tpt.eq(view.resultType))
      assert(result.positionedMethod.rhs.eq(view.rhs))
      assert(result.positionedMethod.mods.eq(view.method.mods))
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
        tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
      ))

      ExistingUntpdClassMemberFilter.allTrees(root).zip(snapshot).foreach {
        case (current, (original, source, span)) =>
          assert(current.eq(original))
          assertEquals(current.source, source)
          assertEquals(current.span, span)
      }
    }
  }

  test("admits String and Boolean with renamed and duplicate methods selected by index") {
    withContext {
      val fixtures = Vector(
        (
          "String",
          """class Textual:
            |  def same(first: Int): Int = first
            |  def same(payload: Any): String = payload.toString
            |""".stripMargin,
          1,
          "payload"
        ),
        (
          "Boolean",
          """class Predicate:
            |  val prefix: Int = 0
            |  def inspect(flag: AnyVal): Boolean = true
            |""".stripMargin,
          1,
          "flag"
        )
      )
      fixtures.foreach { case (primitive, source, index, parameterName) =>
        val view = viewFor(source, index)
        val result = rewrite(view, primitive)
        assertEquals(result.positionedParameter.name.toString, parameterName)
        assertIdent(result.positionedParameterType, primitive)
        assert(result.positionedMethod.tpt.eq(view.resultType))
        assert(result.positionedMethod.rhs.eq(view.rhs))
        view.captured.members.indices.filter(_ != index).foreach { memberIndex =>
          assert(result.positionedTemplate.body(memberIndex).eq(view.captured.members(memberIndex).tree))
        }
      }
    }
  }

  test("repeated rewrite creates fresh type parameter method and owner shells") {
    withContext {
      val view = viewFor("class Repeat:\n  def change(value: AnyVal): Int = value\n", 0)
      val first = rewrite(view, "Int")
      val second = rewrite(view, "Int")
      assert(!first.loweredParameterType.eq(second.loweredParameterType))
      assert(!first.positionedParameterType.eq(second.positionedParameterType))
      assert(!first.positionedParameter.eq(second.positionedParameter))
      assert(!first.positionedMethod.eq(second.positionedMethod))
      assert(!first.positionedTemplate.eq(second.positionedTemplate))
      assert(!first.positionedRoot.eq(second.positionedRoot))
      List(first, second).foreach { result =>
        assert(result.positionedMethod.tpt.eq(view.resultType))
        assert(result.positionedMethod.rhs.eq(view.rhs))
      }
    }
  }

  test("fails closed for null stale unsupported and malformed semantic inputs") {
    withContext {
      val view = viewFor("class Boundary:\n  def change(value: AnyVal): Int = value\n", 0)
      assertCode(
        ExistingUntpdSingleParameterMethodParameterTypeRewriter.rewrite(
          null,
          TypeNormalForm.STypeIdent("Int")
        ),
        "VIEW_REQUIRED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodParameterTypeRewriter.rewrite(
          view.copy(rhs = view.parameterType),
          TypeNormalForm.STypeIdent("Int")
        ),
        "VIEW_INVALID"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodParameterTypeRewriter.rewrite(view, null),
        "PARAMETER_TYPE_REQUIRED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodParameterTypeRewriter.rewrite(
          view,
          TypeNormalForm.STypeIdent("AnyVal")
        ),
        "PARAMETER_TYPE_IDENTIFIER_UNSUPPORTED"
      )
      val malformed = List[TypeNormalForm](
        TypeNormalForm.STypeResolved(null),
        TypeNormalForm.STypeApply(null, null),
        TypeNormalForm.STypeTuple(null),
        TypeNormalForm.STypeFunction(null, null)
      )
      malformed.foreach(value =>
        assertCode(
          ExistingUntpdSingleParameterMethodParameterTypeRewriter.rewrite(view, value),
          "PARAMETER_TYPE_FAMILY_UNSUPPORTED"
        )
      )
    }
  }

  test("requires source-attributed old type parameter and method sites") {
    withContext {
      val view = viewFor("class Sites:\n  def change(value: AnyVal): Int = value\n", 0)
      given SourceFile = NoSource
      val sourceFreeType = untpd.Ident(typeName("AnyVal"))
      val sourceFreeTypeView = replaceParameter(view, view.parameter, sourceFreeType)
      val sourceFreeParameter = untpd
        .ValDef(view.parameter.name, view.parameterType, untpd.EmptyTree)
        .withMods(view.parameter.mods)
      val sourceFreeParameterView = replaceParameter(view, sourceFreeParameter, view.parameterType)
      val sourceFreeMethod = untpd
        .DefDef(view.method.name, view.method.paramss, view.resultType, view.rhs)
        .withMods(view.method.mods)
      val sourceFreeMethodView = replaceMethod(view, sourceFreeMethod)

      assertCode(rewriteEither(sourceFreeTypeView, "Int"), "OLD_PARAMETER_TYPE_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(sourceFreeParameterView, "Int"), "OLD_PARAMETER_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(sourceFreeMethodView, "Int"), "OLD_METHOD_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(view.copy(parameterType = null), "Int"), "OLD_PARAMETER_TYPE_REQUIRED")
      assertCode(rewriteEither(view.copy(parameter = null), "Int"), "OLD_PARAMETER_REQUIRED")
    }
  }

  test("contains delegated lowering failures and rejects contaminated or forged results") {
    withContext {
      val view = viewFor("class Lowering:\n  def change(value: AnyVal): Int = value\n", 0)
      assertCode(
        ExistingUntpdSingleParameterMethodParameterTypeRewriter.rewriteWithLowerer(
          view,
          TypeNormalForm.STypeIdent("Int"),
          _ => Left("synthetic lowerer failure")
        ),
        "PARAMETER_TYPE_LOWERING_FAILED"
      )
      given SourceFile = NoSource
      assertCode(
        ExistingUntpdSingleParameterMethodParameterTypeRewriter.rewriteWithLowerer(
          view,
          TypeNormalForm.STypeIdent("Int"),
          _ => Right(untpd.Tuple(List(untpd.Ident(typeName("Int")), untpd.Ident(typeName("Int")))))
        ),
        "LOWERED_PARAMETER_TYPE_TOPOLOGY"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodParameterTypeRewriter.rewriteWithLowerer(
          view,
          TypeNormalForm.STypeIdent("Int"),
          _ => Right(null)
        ),
        "LOWERED_PARAMETER_TYPE_TOPOLOGY"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodParameterTypeRewriter.rewriteWithLowerer(
          view,
          TypeNormalForm.STypeIdent("Int"),
          _ => null
        ),
        "PARAMETER_TYPE_LOWERING_FAILED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodParameterTypeRewriter.rewriteWithLowerer(
          view,
          TypeNormalForm.STypeIdent("Int"),
          _ => Right(untpd.Ident(typeName("String")))
        ),
        "LOWERED_PARAMETER_TYPE_TOPOLOGY"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodParameterTypeRewriter.rewriteWithLowerer(
          view,
          TypeNormalForm.STypeIdent("Int"),
          _ => Right(
            viewFor("class SourceBearing:\n  def change(value: Int): Int = value\n", 0)
              .parameterType
          )
        ),
        "LOWERED_PARAMETER_TYPE_PROVENANCE"
      )
      val symbol = newSymbol(NoSymbol, typeName("InjectedType"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(typeName("Int")).withType(symbol.typeRef)
      assertCode(
        ExistingUntpdSingleParameterMethodParameterTypeRewriter.rewriteWithLowerer(
          view,
          TypeNormalForm.STypeIdent("Int"),
          _ => Right(symbolBearing)
        ),
        "LOWERED_PARAMETER_TYPE_PRE_TYPER_REQUIRED"
      )

      val result = rewrite(view, "Int")
      assertCode(
        ExistingUntpdSingleParameterMethodParameterTypeRewriter.validateResult(
          result.copy(positionedParameter = view.parameter)
        ),
        "FINAL_REWRITE_INVARIANT_FAILED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodParameterTypeRewriter.validateResult(
          result.copy(positionedMethod = view.method)
        ),
        "FINAL_REWRITE_INVARIANT_FAILED"
      )
    }
  }

  test("fails closed for missing individual spans null sources and malformed raw sequences") {
    withContext {
      val view = viewFor("class Provenance:\n  def change(value: AnyVal): Int = value\n", 0)
      given SourceFile = NoSource
      val typeWithoutSpan = untpd
        .Ident(typeName("AnyVal"))
        .cloneIn(view.parameterType.source)
      val typeWithNullSource = view.parameterType.cloneIn(null)
      val parameterWithoutSpan = untpd
        .ValDef(view.parameter.name, view.parameterType, untpd.EmptyTree)
        .withMods(view.parameter.mods)
        .cloneIn(view.parameter.source)
        .asInstanceOf[untpd.ValDef]
      val parameterWithNullSource = view.parameter.cloneIn(null).asInstanceOf[untpd.ValDef]
      val methodWithoutSpan = untpd
        .DefDef(view.method.name, view.method.paramss, view.resultType, view.rhs)
        .withMods(view.method.mods)
        .cloneIn(view.method.source)
        .asInstanceOf[untpd.DefDef]
      val methodWithNullSource = view.method.cloneIn(null).asInstanceOf[untpd.DefDef]

      val typeWithoutSpanView = replaceParameter(view, view.parameter, typeWithoutSpan)
      typeWithoutSpanView.parameterType.span = NoSpan
      val parameterWithoutSpanView = replaceParameter(view, parameterWithoutSpan, view.parameterType)
      parameterWithoutSpanView.parameter.span = NoSpan
      val methodWithoutSpanView = replaceMethod(view, methodWithoutSpan)
      methodWithoutSpanView.method.span = NoSpan

      assertCode(rewriteEither(typeWithoutSpanView, "Int"), "OLD_PARAMETER_TYPE_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(replaceParameter(view, view.parameter, typeWithNullSource), "Int"), "OLD_PARAMETER_TYPE_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(parameterWithoutSpanView, "Int"), "OLD_PARAMETER_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(replaceParameter(view, parameterWithNullSource, view.parameterType), "Int"), "OLD_PARAMETER_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(methodWithoutSpanView, "Int"), "OLD_METHOD_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(replaceMethod(view, methodWithNullSource), "Int"), "OLD_METHOD_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(view.copy(captured = view.captured.copy(members = null)), "Int"), "VIEW_INVALID")

      val nullClauses = untpd.cpy.DefDef(view.method)(
        view.method.name,
        null,
        view.resultType,
        view.rhs
      )
      assertCode(rewriteEither(replaceMethod(view, nullClauses), "Int"), "VIEW_INVALID")
    }
  }

  private def rewrite(
      view: ExistingUntpdSingleParameterMethodView.View,
      primitive: String
  )(using Context): ExistingUntpdSingleParameterMethodParameterTypeRewriter.Result =
    rewriteEither(view, primitive).fold(problem => fail(problem.message), identity)

  private def rewriteEither(
      view: ExistingUntpdSingleParameterMethodView.View,
      primitive: String
  )(using Context) =
    ExistingUntpdSingleParameterMethodParameterTypeRewriter
      .rewrite(view, TypeNormalForm.STypeIdent(primitive))

  private def assertIdent(tree: untpd.Tree, expected: String): Unit = tree match
    case ident: untpd.Ident => assertEquals(ident.name.toString, expected)
    case other => fail(s"expected Ident($expected), found ${other.getClass.getSimpleName}")

  private def assertCode[A](
      result: Either[ExistingUntpdSingleParameterMethodParameterTypeRewriteError, A],
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

  private def replaceParameter(
      view: ExistingUntpdSingleParameterMethodView.View,
      parameter: untpd.ValDef,
      parameterType: untpd.Tree
  )(using Context): ExistingUntpdSingleParameterMethodView.View =
    val adjusted = untpd.cpy.ValDef(parameter)(parameter.name, parameterType, parameter.rhs)
    val method = untpd.cpy.DefDef(view.method)(
      view.method.name,
      List(List(adjusted)),
      view.resultType,
      view.rhs
    )
    replaceMethod(view, method).copy(parameter = adjusted, parameterType = parameterType)

  private def replaceMethod(
      view: ExistingUntpdSingleParameterMethodView.View,
      method: untpd.DefDef
  )(using Context): ExistingUntpdSingleParameterMethodView.View =
    val oldTemplate = view.captured.originalTemplate
    val template = untpd.cpy.Template(oldTemplate)(
      oldTemplate.constr,
      oldTemplate.parentsOrDerived,
      oldTemplate.derived,
      oldTemplate.self,
      oldTemplate.body.updated(view.memberIndex, method)
    )
    val oldRoot = view.captured.originalRoot
    val root = untpd.cpy.TypeDef(oldRoot)(oldRoot.name, template)
    val members = view.captured.members.updated(
      view.memberIndex,
      ExistingUntpdClassMemberFilter.Member(view.memberIndex, method)
    )
    view.copy(
      captured = ExistingUntpdClassMemberFilter.Capture(root, template, members),
      method = method
    )

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U031ParameterType.scala", source)
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
