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
import dotty.tools.dotc.util.Spans.{NoSpan, Span}

import quasiquotes.types.TypeNormalForm

class ExistingUntpdTwoParameterMethodParameterTypeRewriterTest extends munit.FunSuite:
  test("index 0 rewrites only the first parameter and preserves an opaque second type result RHS and members") {
    withContext {
      val root = parseClass(
        """class SelectedParameter:
          |  val before: Int = 1
          |  def choose(left: AnyVal, right: List[(String, Int)]): String = right.head._1
          |  type After = Boolean
          |""".stripMargin
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val view = viewFor(root, 1)
      val snapshot = allTrees(root).map(tree => (tree, tree.source, tree.span))
      val result = rewrite(view, 0, "Int")
      val parameters = exactParameters(result.positionedMethod)

      assertEquals(result.parameterIndex, 0)
      assert(result.view.eq(view))
      assert(!result.loweredParameterType.source.exists)
      assert(!result.loweredParameterType.span.exists)
      assertEquals(result.loweredParameterType.symbol, NoSymbol)
      assertIdent(result.positionedParameterType, "Int")
      assert(!result.positionedParameterType.eq(view.firstParameterType))
      assertEquals(result.positionedParameterType.source, view.firstParameterType.source)
      assertEquals(result.positionedParameterType.span, view.firstParameterType.span)
      assert(!result.positionedParameter.eq(view.firstParameter))
      assert(result.positionedParameter.tpt.eq(result.positionedParameterType))
      assert(result.positionedParameter.rhs.isEmpty)
      assertEquals(result.positionedParameter.name, view.firstParameter.name)
      assert(result.positionedParameter.mods.eq(view.firstParameter.mods))
      assertEquals(result.positionedParameter.source, view.firstParameter.source)
      assertEquals(result.positionedParameter.span, view.firstParameter.span)
      assert(parameters(0).eq(result.positionedParameter))
      assert(parameters(1).eq(view.secondParameter))
      assert(parameters(1).tpt.eq(view.secondParameterType))
      assert(result.positionedMethod.tpt.eq(view.resultType))
      assert(result.positionedMethod.rhs.eq(view.rhs))
      assert(!result.positionedMethod.eq(view.method))
      assertEquals(result.positionedMethod.source, view.method.source)
      assertEquals(result.positionedMethod.span, view.method.span)
      assert(!result.positionedTemplate.eq(template))
      assert(!result.positionedRoot.eq(root))
      assert(result.positionedTemplate.constr.eq(template.constr))
      assert(result.positionedTemplate.parentsOrDerived.eq(template.parentsOrDerived))
      assert(result.positionedTemplate.derived.eq(template.derived))
      assert(result.positionedTemplate.self.eq(template.self))
      assert(result.positionedTemplate.body.head.eq(template.body.head))
      assert(result.positionedTemplate.body(1).eq(result.positionedMethod))
      assert(result.positionedTemplate.body(2).eq(template.body(2)))
      assert(allTrees(result.positionedRoot).forall(tree =>
        tree != null && tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
      ))
      assertOriginalUnchanged(root, snapshot)
    }
  }

  test("index 1 rewrites only the second parameter and admits String Boolean renamed owners and duplicate methods") {
    withContext {
      val fixtures = Vector(
        (
          "String",
          """class Textual:
            |  def same(first: Int, second: Int): Int = first
            |  def same(prefix: List[Int], payload: Any): String = payload.toString
            |""".stripMargin,
          1,
          "payload"
        ),
        (
          "Boolean",
          """class Predicate:
            |  val prefix: Int = 0
            |  def inspect(number: Int, flag: AnyVal): Boolean = true
            |""".stripMargin,
          1,
          "flag"
        )
      )
      fixtures.foreach { case (primitive, source, memberIndex, parameterName) =>
        val root = parseClass(source)
        val view = viewFor(root, memberIndex)
        val result = rewrite(view, 1, primitive)
        val parameters = exactParameters(result.positionedMethod)

        assertEquals(result.positionedParameter.name.toString, parameterName)
        assertIdent(result.positionedParameterType, primitive)
        assert(parameters(0).eq(view.firstParameter))
        assert(parameters(0).tpt.eq(view.firstParameterType))
        assert(parameters(1).eq(result.positionedParameter))
        assert(result.positionedMethod.tpt.eq(view.resultType))
        assert(result.positionedMethod.rhs.eq(view.rhs))
        view.captured.members.indices.filter(_ != memberIndex).foreach { index =>
          assert(result.positionedTemplate.body(index).eq(view.captured.members(index).tree))
        }
      }
    }
  }

  test("repeated rewrites are fresh while preserving the same untouched raw handles") {
    withContext {
      val view = viewFor(
        parseClass("class Repeat:\n  def change(left: AnyVal, right: List[Int]): Int = right.head\n"),
        0
      )
      Vector(0, 1).foreach { parameterIndex =>
        val first = rewrite(view, parameterIndex, "Int")
        val second = rewrite(view, parameterIndex, "Int")
        assert(!first.loweredParameterType.eq(second.loweredParameterType))
        assert(!first.positionedParameterType.eq(second.positionedParameterType))
        assert(!first.positionedParameter.eq(second.positionedParameter))
        assert(!first.positionedMethod.eq(second.positionedMethod))
        assert(!first.positionedTemplate.eq(second.positionedTemplate))
        assert(!first.positionedRoot.eq(second.positionedRoot))
        val untouched = if parameterIndex == 0 then view.secondParameter else view.firstParameter
        assert(exactParameters(first.positionedMethod)(1 - parameterIndex).eq(untouched))
        assert(exactParameters(second.positionedMethod)(1 - parameterIndex).eq(untouched))
      }
    }
  }

  test("fails closed for null stale unsupported index and malformed semantic inputs") {
    withContext {
      val view = viewFor(
        parseClass("class Boundary:\n  def sum(left: AnyVal, right: AnyVal): Int = left\n"),
        0
      )
      assertCode(rewriteEither(null, 0, "Int"), "VIEW_REQUIRED")
      Vector(Int.MinValue, -1, 2, Int.MaxValue).foreach(index =>
        assertCode(rewriteEither(view, index, "Int"), "PARAMETER_INDEX_UNSUPPORTED")
      )
      assertCode(rewriteEither(view.copy(firstParameter = null), 0, "Int"), "OLD_PARAMETER_REQUIRED")
      assertCode(rewriteEither(view.copy(secondParameter = null), 1, "Int"), "OLD_PARAMETER_REQUIRED")
      assertCode(rewriteEither(view.copy(firstParameterType = null), 0, "Int"), "OLD_PARAMETER_TYPE_REQUIRED")
      assertCode(rewriteEither(view.copy(secondParameterType = null), 1, "Int"), "OLD_PARAMETER_TYPE_REQUIRED")
      assertCode(rewriteEither(view.copy(firstParameterType = untpd.EmptyTree), 0, "Int"), "OLD_PARAMETER_TYPE_REQUIRED")
      val oneParameterMethod = untpd.cpy.DefDef(view.method)(
        view.method.name,
        List(List(view.firstParameter)),
        view.resultType,
        view.rhs
      )
      val nullClausesMethod = untpd.cpy.DefDef(view.method)(
        view.method.name,
        null,
        view.resultType,
        view.rhs
      )
      assertCode(rewriteEither(replaceMethod(view, oneParameterMethod), 0, "Int"), "VIEW_INVALID")
      assertCode(rewriteEither(replaceMethod(view, nullClausesMethod), 0, "Int"), "VIEW_INVALID")
      assertCode(rewriteEither(view.copy(rhs = view.resultType), 0, "Int"), "VIEW_INVALID")
      assertCode(rewriteEither(view.copy(captured = null), 0, "Int"), "VIEW_INVALID")
      assertCode(
        ExistingUntpdTwoParameterMethodParameterTypeRewriter.rewrite(view, 0, null),
        "PARAMETER_TYPE_REQUIRED"
      )
      assertCode(rewriteEither(view, 0, "AnyVal"), "PARAMETER_TYPE_IDENTIFIER_UNSUPPORTED")
      List[TypeNormalForm](
        TypeNormalForm.STypeResolved(null),
        TypeNormalForm.STypeApply(null, null),
        TypeNormalForm.STypeTuple(null),
        TypeNormalForm.STypeFunction(null, null)
      ).foreach(value =>
        assertCode(
          ExistingUntpdTwoParameterMethodParameterTypeRewriter.rewrite(view, 0, value),
          "PARAMETER_TYPE_FAMILY_UNSUPPORTED"
        )
      )
    }
  }

  test("requires usable selected type selected parameter and method source spans") {
    withContext {
      val original = viewFor(
        parseClass("class Sites:\n  def sum(left: AnyVal, right: AnyVal): Int = left\n"),
        0
      )
      Vector(0, 1).foreach { parameterIndex =>
        given SourceFile = NoSource
        val selectedType = parameterTypeAt(original, parameterIndex)
        val selectedParameter = parameterAt(original, parameterIndex)
        val sourceFreeType = untpd.Ident(typeName("AnyVal"))
        val nullSourceType = selectedType.cloneIn(null)
        val missingSpanType = untpd
          .Ident(typeName("AnyVal"))
          .cloneIn(selectedType.source)
          .withSpan(NoSpan)
        val sourceFreeParameter = untpd
          .ValDef(selectedParameter.name, selectedType, untpd.EmptyTree)
          .withMods(selectedParameter.mods)
        val nullSourceParameter = selectedParameter.cloneIn(null).asInstanceOf[untpd.ValDef]
        val missingSpanParameter = untpd
          .ValDef(selectedParameter.name, selectedType, untpd.EmptyTree)
          .withMods(selectedParameter.mods)
          .cloneIn(selectedParameter.source)
          .withSpan(NoSpan)
          .asInstanceOf[untpd.ValDef]
        val missingSpanTypeView =
          replaceParameter(original, parameterIndex, selectedParameter, missingSpanType)
        parameterTypeAt(missingSpanTypeView, parameterIndex).span = NoSpan
        val missingSpanParameterView =
          replaceParameter(original, parameterIndex, missingSpanParameter, selectedType)
        parameterAt(missingSpanParameterView, parameterIndex).span = NoSpan

        assertCode(rewriteEither(replaceParameter(original, parameterIndex, selectedParameter, sourceFreeType), parameterIndex, "Int"), "OLD_PARAMETER_TYPE_PROVENANCE_REQUIRED")
        assertCode(rewriteEither(replaceParameter(original, parameterIndex, selectedParameter, nullSourceType), parameterIndex, "Int"), "OLD_PARAMETER_TYPE_PROVENANCE_REQUIRED")
        assertCode(rewriteEither(missingSpanTypeView, parameterIndex, "Int"), "OLD_PARAMETER_TYPE_PROVENANCE_REQUIRED")
        assertCode(rewriteEither(replaceParameter(original, parameterIndex, sourceFreeParameter, selectedType), parameterIndex, "Int"), "OLD_PARAMETER_PROVENANCE_REQUIRED")
        assertCode(rewriteEither(replaceParameter(original, parameterIndex, nullSourceParameter, selectedType), parameterIndex, "Int"), "OLD_PARAMETER_PROVENANCE_REQUIRED")
        assertCode(rewriteEither(missingSpanParameterView, parameterIndex, "Int"), "OLD_PARAMETER_PROVENANCE_REQUIRED")
      }

      given SourceFile = NoSource
      val sourceFreeMethod = untpd
        .DefDef(original.method.name, original.method.paramss, original.resultType, original.rhs)
        .withMods(original.method.mods)
      val nullSourceMethod = original.method.cloneIn(null).asInstanceOf[untpd.DefDef]
      val missingSpanMethod = untpd
        .DefDef(original.method.name, original.method.paramss, original.resultType, original.rhs)
        .withMods(original.method.mods)
        .cloneIn(original.method.source)
        .withSpan(NoSpan)
        .asInstanceOf[untpd.DefDef]
      val missingSpanMethodView = replaceMethod(original, missingSpanMethod)
      missingSpanMethodView.method.span = NoSpan
      assertCode(rewriteEither(replaceMethod(original, sourceFreeMethod), 0, "Int"), "OLD_METHOD_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(replaceMethod(original, nullSourceMethod), 0, "Int"), "OLD_METHOD_PROVENANCE_REQUIRED")
      assertCode(rewriteEither(missingSpanMethodView, 0, "Int"), "OLD_METHOD_PROVENANCE_REQUIRED")
    }
  }

  test("contains delegated lowering and owner reconstruction failures") {
    withContext {
      val view = viewFor(
        parseClass("class Seams:\n  def sum(left: AnyVal, right: Int): Int = right\n"),
        0
      )
      def rewriteWith(
          lower: TypeNormalForm => Either[String, untpd.Tree],
          reconstruct: (
              ExistingUntpdClassMemberFilter.Capture,
              Vector[untpd.Tree]
          ) => Either[ExistingUntpdClassMemberFilterError, ExistingUntpdClassMemberFilter.Reconstructed] =
            (capture, body) => ExistingUntpdClassMemberFilter.reconstruct(capture, body)
      ) = ExistingUntpdTwoParameterMethodParameterTypeRewriter.rewriteWithDependencies(
        view,
        0,
        TypeNormalForm.STypeIdent("Int"),
        lower,
        reconstruct
      )

      assertCode(rewriteWith(_ => Left("synthetic lowering refusal")), "PARAMETER_TYPE_LOWERING_FAILED")
      assertCode(rewriteWith(_ => null), "PARAMETER_TYPE_LOWERING_FAILED")
      assertCode(
        ExistingUntpdTwoParameterMethodParameterTypeRewriter.rewriteWithDependencies(
          view,
          0,
          TypeNormalForm.STypeIdent("Int"),
          null,
          (capture, body) => ExistingUntpdClassMemberFilter.reconstruct(capture, body)
        ),
        "PARAMETER_TYPE_LOWERING_FAILED"
      )
      given SourceFile = NoSource
      assertCode(rewriteWith(_ => Right(null)), "LOWERED_PARAMETER_TYPE_TOPOLOGY")
      assertCode(rewriteWith(_ => Right(untpd.Ident(typeName("String")))), "LOWERED_PARAMETER_TYPE_TOPOLOGY")
      assertCode(
        rewriteWith(_ => Right(untpd.Tuple(List(untpd.Ident(typeName("Int")), untpd.Ident(typeName("Int")))))),
        "LOWERED_PARAMETER_TYPE_TOPOLOGY"
      )
      assertCode(rewriteWith(_ => Right(view.firstParameterType)), "LOWERED_PARAMETER_TYPE_PROVENANCE")
      val symbol = newSymbol(NoSymbol, typeName("InjectedType"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(typeName("Int")).withType(symbol.typeRef)
      assertCode(rewriteWith(_ => Right(symbolBearing)), "LOWERED_PARAMETER_TYPE_PRE_TYPER_REQUIRED")
      assertCode(rewriteWith(_ => Right(untpd.TypedSplice(symbolBearing))), "LOWERED_PARAMETER_TYPE_PRE_TYPER_REQUIRED")
      assertCode(
        rewriteWith(
          _ => Right(untpd.Ident(typeName("Int"))),
          (_, _) => Left(ExistingUntpdClassMemberFilterError("SYNTHETIC_REFUSAL", "refused"))
        ),
        "RECONSTRUCTION_FAILED"
      )
      assertCode(
        rewriteWith(_ => Right(untpd.Ident(typeName("Int"))), (_, _) => null),
        "RECONSTRUCTION_FAILED"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodParameterTypeRewriter.rewriteWithDependencies(
          view,
          0,
          TypeNormalForm.STypeIdent("Int"),
          _ => Right(untpd.Ident(typeName("Int"))),
          null
        ),
        "RECONSTRUCTION_FAILED"
      )
    }
  }

  test("final validation rejects selected untouched result RHS member and graph drift") {
    withContext {
      val view = viewFor(
        parseClass("class FinalCheck:\n  val before: Int = 1\n  def sum(left: AnyVal, right: List[Int]): Int = right.head\n"),
        1
      )
      Vector(0, 1).foreach { parameterIndex =>
        val result = rewrite(view, parameterIndex, "Int")
        val parameters = exactParameters(result.positionedMethod)
        val swappedMethod = untpd.cpy.DefDef(result.positionedMethod)(
          result.positionedMethod.name,
          List(List(parameters(1), parameters(0))),
          result.positionedMethod.tpt,
          result.positionedMethod.rhs
        )
        val oldSelected = parameterAt(view, parameterIndex)
        val untouchedIndex = 1 - parameterIndex
        val oldUntouched = parameterAt(view, untouchedIndex)
        val clonedUntouched = untpd.cpy.ValDef(oldUntouched)(
          oldUntouched.name,
          oldUntouched.tpt,
          oldUntouched.rhs
        )
        val untouchedDriftParameters = parameters.updated(untouchedIndex, clonedUntouched).toList
        val untouchedDriftMethod = untpd.cpy.DefDef(result.positionedMethod)(
          result.positionedMethod.name,
          List(untouchedDriftParameters),
          result.positionedMethod.tpt,
          result.positionedMethod.rhs
        )
        val selectedAliasMethod = untpd.cpy.DefDef(result.positionedMethod)(
          result.positionedMethod.name,
          List(List(
            if parameterIndex == 0 then oldSelected else parameters(0),
            if parameterIndex == 1 then oldSelected else parameters(1)
          )),
          result.positionedMethod.tpt,
          result.positionedMethod.rhs
        )
        val resultDriftMethod = untpd.cpy.DefDef(result.positionedMethod)(
          result.positionedMethod.name,
          result.positionedMethod.paramss,
          view.firstParameterType,
          result.positionedMethod.rhs
        )
        val rhsDriftMethod = untpd.cpy.DefDef(result.positionedMethod)(
          result.positionedMethod.name,
          result.positionedMethod.paramss,
          result.positionedMethod.tpt,
          view.secondParameterType
        )
        Vector(
          result.copy(positionedMethod = swappedMethod),
          result.copy(positionedMethod = selectedAliasMethod),
          result.copy(positionedMethod = untouchedDriftMethod),
          result.copy(positionedMethod = resultDriftMethod),
          result.copy(positionedMethod = rhsDriftMethod),
          result.copy(positionedParameterType = parameterTypeAt(view, parameterIndex)),
          result.copy(positionedTemplate = view.captured.originalTemplate),
          result.copy(positionedRoot = view.captured.originalRoot),
          result.copy(parameterIndex = 2),
          result.copy(view = view.copy(captured = null))
        ).foreach(forged =>
          assertCode(
            ExistingUntpdTwoParameterMethodParameterTypeRewriter.validateResult(forged),
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
          ExistingUntpdTwoParameterMethodParameterTypeRewriter.validateResult(
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
          ExistingUntpdTwoParameterMethodParameterTypeRewriter.validateResult(
            result.copy(positionedTemplate = nullMemberTemplate, positionedRoot = nullMemberRoot)
          ),
          "FINAL_REWRITE_INVARIANT_FAILED"
        )
      }
    }
  }

  private def rewrite(
      view: ExistingUntpdTwoParameterMethodView.View,
      parameterIndex: Int,
      primitive: String
  )(using Context): ExistingUntpdTwoParameterMethodParameterTypeRewriter.Result =
    rewriteEither(view, parameterIndex, primitive).fold(problem => fail(problem.message), identity)

  private def rewriteEither(
      view: ExistingUntpdTwoParameterMethodView.View,
      parameterIndex: Int,
      primitive: String
  )(using Context): Either[
    ExistingUntpdTwoParameterMethodParameterTypeRewriteError,
    ExistingUntpdTwoParameterMethodParameterTypeRewriter.Result
  ] = ExistingUntpdTwoParameterMethodParameterTypeRewriter.rewrite(
    view,
    parameterIndex,
    TypeNormalForm.STypeIdent(primitive)
  )

  private def exactParameters(method: untpd.DefDef): Vector[untpd.ValDef] =
    method.paramss.head.map(_.asInstanceOf[untpd.ValDef]).toVector

  private def parameterAt(
      view: ExistingUntpdTwoParameterMethodView.View,
      parameterIndex: Int
  ): untpd.ValDef =
    if parameterIndex == 0 then view.firstParameter else view.secondParameter

  private def parameterTypeAt(
      view: ExistingUntpdTwoParameterMethodView.View,
      parameterIndex: Int
  ): untpd.Tree =
    if parameterIndex == 0 then view.firstParameterType else view.secondParameterType

  private def assertIdent(tree: untpd.Tree, expected: String): Unit = tree match
    case ident: untpd.Ident => assertEquals(ident.name.toString, expected)
    case other => fail(s"expected Ident($expected), found ${other.getClass.getSimpleName}")

  private def assertCode[A](
      result: Either[ExistingUntpdTwoParameterMethodParameterTypeRewriteError, A],
      expected: String
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected)
    case Right(value) => fail(s"expected $expected, found success $value")

  private def viewFor(
      root: untpd.TypeDef,
      memberIndex: Int
  )(using Context): ExistingUntpdTwoParameterMethodView.View =
    val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
    ExistingUntpdTwoParameterMethodView.capture(captured, memberIndex).toOption.get

  private def replaceParameter(
      view: ExistingUntpdTwoParameterMethodView.View,
      parameterIndex: Int,
      parameter: untpd.ValDef,
      parameterType: untpd.Tree
  )(using Context): ExistingUntpdTwoParameterMethodView.View =
    val adjusted = untpd.cpy.ValDef(parameter)(parameter.name, parameterType, parameter.rhs)
    val parameters =
      if parameterIndex == 0 then List(adjusted, view.secondParameter)
      else List(view.firstParameter, adjusted)
    val method = untpd.cpy.DefDef(view.method)(
      view.method.name,
      List(parameters),
      view.resultType,
      view.rhs
    )
    val replaced = replaceMethod(view, method)
    if parameterIndex == 0 then
      replaced.copy(firstParameter = adjusted, firstParameterType = parameterType)
    else replaced.copy(secondParameter = adjusted, secondParameterType = parameterType)

  private def replaceMethod(
      view: ExistingUntpdTwoParameterMethodView.View,
      method: untpd.DefDef
  )(using Context): ExistingUntpdTwoParameterMethodView.View =
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

  private def assertOriginalUnchanged(
      root: untpd.TypeDef,
      snapshot: Vector[(untpd.Tree, SourceFile, Span)]
  )(using Context): Unit =
    allTrees(root).zip(snapshot).foreach { case (current, (original, source, span)) =>
      assert(current.eq(original))
      assertEquals(current.source, source)
      assertEquals(current.span, span)
    }

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ExistingUntpdClassMemberFilter.allTrees(tree)

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U036ParameterType.scala", source)
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
