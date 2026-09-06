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

class ExistingUntpdTwoParameterMethodResultTypeRewriterTest
    extends munit.FunSuite:
  test("rewrites only the selected result type and preserves both parameters RHS and opaque islands") {
    withContext {
      val root = parseClass(
        """class Calculator:
          |  val before: Int = 1
          |  def sum(x: Int, y: Int): AnyVal = x + y
          |  type After = String
          |""".stripMargin
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
      val view = ExistingUntpdTwoParameterMethodView.capture(captured, 1).toOption.get
      val snapshot = ExistingUntpdClassMemberFilter.allTrees(root).map(tree =>
        (tree, tree.source, tree.span)
      )

      val result = rewrite(view, "Int")

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

      val first = result.positionedMethod.paramss.head(0).asInstanceOf[untpd.ValDef]
      val second = result.positionedMethod.paramss.head(1).asInstanceOf[untpd.ValDef]
      assert(first.eq(view.firstParameter))
      assert(first.tpt.eq(view.firstParameterType))
      assert(second.eq(view.secondParameter))
      assert(second.tpt.eq(view.secondParameterType))
      assert(result.positionedMethod.rhs.eq(view.rhs))
      assert(result.positionedMethod.mods.eq(view.method.mods))
      assert(result.positionedTemplate.constr.eq(template.constr))
      assert(result.positionedTemplate.parentsOrDerived.eq(template.parentsOrDerived))
      assert(result.positionedTemplate.derived.eq(template.derived))
      assert(result.positionedTemplate.self.eq(template.self))
      assert(result.positionedTemplate.body.head.eq(template.body.head))
      assert(result.positionedTemplate.body(1).eq(result.positionedMethod))
      assert(result.positionedTemplate.body(2).eq(template.body(2)))
      assert(
        ExistingUntpdClassMemberFilter.allTrees(result.positionedRoot).forall(tree =>
          tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
        )
      )

      ExistingUntpdClassMemberFilter.allTrees(root).zip(snapshot).foreach {
        case (tree, (original, source, span)) =>
          assert(tree.eq(original))
          assertEquals(tree.source, source)
          assertEquals(tree.span, span)
      }
    }
  }

  test("admits all primitive result types independent of names and creates fresh shells repeatedly") {
    withContext {
      val root = parseClass(
        """class Duplicate:
          |  def same(left: Int, right: Int): AnyVal = left + right
          |  def same(first: String, second: String): Any = first + second
          |""".stripMargin
      )
      val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
      val firstView = ExistingUntpdTwoParameterMethodView.capture(captured, 0).toOption.get
      val secondView = ExistingUntpdTwoParameterMethodView.capture(captured, 1).toOption.get

      val intFirst = rewrite(firstView, "Int")
      val intSecond = rewrite(firstView, "Int")
      val stringResult = rewrite(secondView, "String")
      val booleanResult = rewrite(firstView, "Boolean")

      Vector(
        intFirst -> "Int",
        intSecond -> "Int",
        stringResult -> "String",
        booleanResult -> "Boolean"
      ).foreach { case (result, expected) =>
        assertIdent(result.positionedResultType, expected)
        assert(result.positionedMethod.rhs.eq(result.view.rhs))
      }
      assert(!intFirst.loweredResultType.eq(intSecond.loweredResultType))
      assert(!intFirst.positionedResultType.eq(intSecond.positionedResultType))
      assert(!intFirst.positionedMethod.eq(intSecond.positionedMethod))
      assert(!intFirst.positionedTemplate.eq(intSecond.positionedTemplate))
      assert(!intFirst.positionedRoot.eq(intSecond.positionedRoot))
      assert(stringResult.positionedTemplate.body.head.eq(captured.members.head.tree))
    }
  }

  test("fails closed for null stale forged and unsupported inputs") {
    withContext {
      val view = viewFor("class Boundary:\n  def sum(x: Int, y: Int): AnyVal = x + y\n", 0)

      assertCode(rewriteEither(null, "Int"), "VIEW_REQUIRED")
      assertCode(rewriteEither(view.copy(method = null), "Int"), "VIEW_INVALID")
      assertCode(rewriteEither(view.copy(firstParameter = null), "Int"), "VIEW_INVALID")
      assertCode(rewriteEither(view.copy(secondParameter = null), "Int"), "VIEW_INVALID")
      assertCode(rewriteEither(view.copy(firstParameterType = view.secondParameterType), "Int"), "VIEW_INVALID")
      assertCode(rewriteEither(view.copy(secondParameterType = view.firstParameterType), "Int"), "VIEW_INVALID")
      assertCode(rewriteEither(view.copy(rhs = view.resultType), "Int"), "VIEW_INVALID")
      assertCode(rewriteEither(view.copy(captured = null), "Int"), "VIEW_INVALID")
      assertCode(rewriteEither(view.copy(resultType = null), "Int"), "OLD_RESULT_TYPE_REQUIRED")
      assertCode(
        ExistingUntpdTwoParameterMethodResultTypeRewriter.rewrite(view, null),
        "RESULT_TYPE_REQUIRED"
      )
      assertCode(rewriteEither(view, "AnyVal"), "RESULT_TYPE_IDENTIFIER_UNSUPPORTED")
      assertCode(
        ExistingUntpdTwoParameterMethodResultTypeRewriter.rewrite(
          view,
          TypeNormalForm.STypeTuple(List(TypeNormalForm.STypeIdent("Int")))
        ),
        "RESULT_TYPE_FAMILY_UNSUPPORTED"
      )
      List[TypeNormalForm](
        TypeNormalForm.STypeResolved(null),
        TypeNormalForm.STypeApply(null, null),
        TypeNormalForm.STypeTuple(null),
        TypeNormalForm.STypeFunction(null, null)
      ).foreach(value =>
        assertCode(
          ExistingUntpdTwoParameterMethodResultTypeRewriter.rewrite(view, value),
          "RESULT_TYPE_FAMILY_UNSUPPORTED"
        )
      )
    }
  }

  test("requires source-attributed old result and method sites") {
    withContext {
      val view = viewFor("class Sites:\n  def sum(x: Int, y: Int): AnyVal = x + y\n", 0)
      given SourceFile = NoSource
      val sourceFreeType = untpd.Ident(typeName("AnyVal"))
      val nullSourceType = view.resultType.cloneIn(null)
      val missingSpanType = untpd
        .Ident(typeName("AnyVal"))
        .cloneIn(view.resultType.source)
        .withSpan(NoSpan)
      val sourceFreeMethod = untpd
        .DefDef(view.method.name, view.method.paramss, view.resultType, view.rhs)
        .withMods(view.method.mods)
      val nullSourceMethod = view.method.cloneIn(null).asInstanceOf[untpd.DefDef]
      val missingSpanMethod = untpd
        .DefDef(view.method.name, view.method.paramss, view.resultType, view.rhs)
        .withMods(view.method.mods)
        .cloneIn(view.method.source)
        .withSpan(NoSpan)
        .asInstanceOf[untpd.DefDef]

      assertCode(rewriteEither(view.copy(resultType = sourceFreeType), "Int"), "OLD_RESULT_TYPE_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(view.copy(resultType = nullSourceType), "Int"), "OLD_RESULT_TYPE_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(view.copy(resultType = missingSpanType), "Int"), "OLD_RESULT_TYPE_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(view.copy(method = sourceFreeMethod), "Int"), "OLD_METHOD_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(view.copy(method = nullSourceMethod), "Int"), "OLD_METHOD_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(view.copy(method = missingSpanMethod), "Int"), "OLD_METHOD_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(view.copy(resultType = untpd.EmptyTree), "Int"), "OLD_RESULT_TYPE_REQUIRED")
    }
  }

  test("contains delegated lowering and reconstruction failures") {
    withContext {
      val view = viewFor("class Seams:\n  def sum(x: Int, y: Int): AnyVal = x + y\n", 0)
      assertCode(
        ExistingUntpdTwoParameterMethodResultTypeRewriter.rewriteWithDependencies(
          view,
          TypeNormalForm.STypeIdent("Int"),
          _ => Left("synthetic lowering refusal"),
          (capture, body) => ExistingUntpdClassMemberFilter.reconstruct(capture, body)
        ),
        "RESULT_TYPE_LOWERING_FAILED"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodResultTypeRewriter.rewriteWithDependencies(
          view,
          TypeNormalForm.STypeIdent("Int"),
          _ => null,
          (capture, body) => ExistingUntpdClassMemberFilter.reconstruct(capture, body)
        ),
        "RESULT_TYPE_LOWERING_FAILED"
      )
      given SourceFile = NoSource
      assertCode(
        ExistingUntpdTwoParameterMethodResultTypeRewriter.rewriteWithDependencies(
          view,
          TypeNormalForm.STypeIdent("Int"),
          _ => Right(untpd.Tuple(List(untpd.Ident(typeName("Int")), untpd.Ident(typeName("Int"))))),
          (capture, body) => ExistingUntpdClassMemberFilter.reconstruct(capture, body)
        ),
        "LOWERED_RESULT_TYPE_TOPOLOGY"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodResultTypeRewriter.rewriteWithDependencies(
          view,
          TypeNormalForm.STypeIdent("Int"),
          _ => Right(null),
          (capture, body) => ExistingUntpdClassMemberFilter.reconstruct(capture, body)
        ),
        "LOWERED_RESULT_TYPE_TOPOLOGY"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodResultTypeRewriter.rewriteWithDependencies(
          view,
          TypeNormalForm.STypeIdent("Int"),
          _ => Right(untpd.Ident(typeName("String"))),
          (capture, body) => ExistingUntpdClassMemberFilter.reconstruct(capture, body)
        ),
        "LOWERED_RESULT_TYPE_TOPOLOGY"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodResultTypeRewriter.rewriteWithDependencies(
          view,
          TypeNormalForm.STypeIdent("Int"),
          _ => Right(view.resultType),
          (capture, body) => ExistingUntpdClassMemberFilter.reconstruct(capture, body)
        ),
        "LOWERED_RESULT_TYPE_PROVENANCE"
      )
      val symbol = newSymbol(NoSymbol, typeName("InjectedType"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(typeName("Int")).withType(symbol.typeRef)
      assertCode(
        ExistingUntpdTwoParameterMethodResultTypeRewriter.rewriteWithDependencies(
          view,
          TypeNormalForm.STypeIdent("Int"),
          _ => Right(symbolBearing),
          (capture, body) => ExistingUntpdClassMemberFilter.reconstruct(capture, body)
        ),
        "LOWERED_RESULT_TYPE_PRE_TYPER_REQUIRED"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodResultTypeRewriter.rewriteWithDependencies(
          view,
          TypeNormalForm.STypeIdent("Int"),
          _ => Right(untpd.TypedSplice(symbolBearing)),
          (capture, body) => ExistingUntpdClassMemberFilter.reconstruct(capture, body)
        ),
        "LOWERED_RESULT_TYPE_PRE_TYPER_REQUIRED"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodResultTypeRewriter.rewriteWithDependencies(
          view,
          TypeNormalForm.STypeIdent("Int"),
          _ => Right(untpd.Ident(typeName("Int"))),
          (_, _) => Left(ExistingUntpdClassMemberFilterError("SYNTHETIC_REFUSAL", "refused"))
        ),
        "RECONSTRUCTION_FAILED"
      )
    }
  }

  test("rejects forged final parameter result RHS member and graph identities") {
    withContext {
      val view = viewFor(
        "class FinalCheck:\n  val before: Int = 1\n  def sum(x: Int, y: Int): AnyVal = x + y\n",
        1
      )
      val result = rewrite(view, "Int")
      val clonedFirst = untpd.cpy.ValDef(view.firstParameter)(
        view.firstParameter.name,
        view.firstParameter.tpt,
        view.firstParameter.rhs
      )
      val forgedParameters = untpd.cpy.DefDef(result.positionedMethod)(
        result.positionedMethod.name,
        List(List(clonedFirst, view.secondParameter)),
        result.positionedMethod.tpt,
        result.positionedMethod.rhs
      )
      val clonedSecond = untpd.cpy.ValDef(view.secondParameter)(
        view.secondParameter.name,
        view.firstParameterType,
        view.secondParameter.rhs
      )
      val forgedSecondParameter = untpd.cpy.DefDef(result.positionedMethod)(
        result.positionedMethod.name,
        List(List(view.firstParameter, clonedSecond)),
        result.positionedMethod.tpt,
        result.positionedMethod.rhs
      )
      val forgedResultType = untpd.cpy.DefDef(result.positionedMethod)(
        result.positionedMethod.name,
        result.positionedMethod.paramss,
        view.resultType,
        result.positionedMethod.rhs
      )
      val forgedRhs = untpd.cpy.DefDef(result.positionedMethod)(
        result.positionedMethod.name,
        result.positionedMethod.paramss,
        result.positionedMethod.tpt,
        view.firstParameterType
      )

      Vector(
        result.copy(positionedMethod = forgedParameters),
        result.copy(positionedMethod = forgedSecondParameter),
        result.copy(positionedMethod = forgedResultType),
        result.copy(positionedMethod = forgedRhs),
        result.copy(positionedTemplate = view.captured.originalTemplate),
        result.copy(positionedRoot = view.captured.originalRoot),
        result.copy(view = view.copy(captured = null))
      ).foreach(forged =>
        assertCode(
          ExistingUntpdTwoParameterMethodResultTypeRewriter.validateResult(forged),
          "FINAL_REWRITE_INVARIANT_FAILED"
        )
      )

      val symbol = newSymbol(NoSymbol, typeName("InjectedMember"), EmptyFlags, NoType)
      val contaminated = untpd.Ident(typeName("Int")).withType(symbol.typeRef)
      val contaminatedTemplate = untpd.cpy.Template(result.positionedTemplate)(
        result.positionedTemplate.constr,
        result.positionedTemplate.parentsOrDerived,
        result.positionedTemplate.derived,
        result.positionedTemplate.self,
        contaminated :: result.positionedTemplate.body.tail
      )
      val contaminatedRoot = untpd.cpy.TypeDef(result.positionedRoot)(
        result.positionedRoot.name,
        contaminatedTemplate
      )
      assertCode(
        ExistingUntpdTwoParameterMethodResultTypeRewriter.validateResult(
          result.copy(positionedTemplate = contaminatedTemplate, positionedRoot = contaminatedRoot)
        ),
        "FINAL_REWRITE_INVARIANT_FAILED"
      )
      val nullMemberTemplate = untpd.cpy.Template(result.positionedTemplate)(
        result.positionedTemplate.constr,
        result.positionedTemplate.parentsOrDerived,
        result.positionedTemplate.derived,
        result.positionedTemplate.self,
        null.asInstanceOf[untpd.Tree] :: result.positionedTemplate.body.tail
      )
      val nullMemberRoot = untpd.cpy.TypeDef(result.positionedRoot)(
        result.positionedRoot.name,
        nullMemberTemplate
      )
      assertCode(
        ExistingUntpdTwoParameterMethodResultTypeRewriter.validateResult(
          result.copy(positionedTemplate = nullMemberTemplate, positionedRoot = nullMemberRoot)
        ),
        "FINAL_REWRITE_INVARIANT_FAILED"
      )
    }
  }

  private def rewrite(
      view: ExistingUntpdTwoParameterMethodView.View,
      primitive: String
  )(using Context): ExistingUntpdTwoParameterMethodResultTypeRewriter.Result =
    rewriteEither(view, primitive).fold(problem => fail(problem.message), identity)

  private def rewriteEither(
      view: ExistingUntpdTwoParameterMethodView.View,
      primitive: String
  )(using Context): Either[ExistingUntpdTwoParameterMethodResultTypeRewriteError, ExistingUntpdTwoParameterMethodResultTypeRewriter.Result] =
    ExistingUntpdTwoParameterMethodResultTypeRewriter.rewrite(
      view,
      TypeNormalForm.STypeIdent(primitive)
    )

  private def assertIdent(tree: untpd.Tree, expected: String): Unit = tree match
    case ident: untpd.Ident => assertEquals(ident.name.toString, expected)
    case other => fail(s"expected Ident($expected), found ${other.getClass.getSimpleName}")

  private def assertCode[A](
      result: Either[ExistingUntpdTwoParameterMethodResultTypeRewriteError, A],
      expected: String
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected)
    case Right(value) => fail(s"expected $expected, found success $value")

  private def viewFor(
      source: String,
      index: Int
  )(using Context): ExistingUntpdTwoParameterMethodView.View =
    val root = parseClass(source)
    val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
    ExistingUntpdTwoParameterMethodView.capture(captured, index).toOption.get

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U035ResultTypeRewrite.scala", source)
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
