package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.terms.dotty.CompletedTypeUntypedLowerer
import quasiquotes.types.TypeNormalForm

class ExistingUntpdSingleParameterMethodResultTypeRawCharacterizationTest
    extends munit.FunSuite:
  test("characterizes primitive result-type transformation-site attribution and owner reconstruction") {
    withContext {
      val root = parseClass(
        """class ResultSite:
          |  val before: Int = 1
          |  def convert(x: Int): AnyVal = x
          |  type After = String
          |""".stripMargin
      )
      val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
      val view = ExistingUntpdSingleParameterMethodView.capture(captured, 1).toOption.get
      val originalBody = captured.originalTemplate.body.toVector
      val lowered = CompletedTypeUntypedLowerer
        .lower(TypeNormalForm.STypeIdent("Int"))
        .toOption
        .get

      assert(lowered.isInstanceOf[untpd.Ident])
      assertEquals(lowered.asInstanceOf[untpd.Ident].name.toString, "Int")
      assert(!lowered.source.exists)
      assert(!lowered.span.exists)
      assertEquals(lowered.symbol, NoSymbol)
      assert(view.resultType.source.exists)
      assert(view.resultType.span.exists)

      given SourceFile = NoSource
      val positionedType = lowered
        .cloneIn(view.resultType.source)
        .withSpan(view.resultType.span)
      val sourceFreeMethod = untpd
        .DefDef(
          view.method.name,
          view.method.paramss,
          lowered,
          view.rhs
        )
        .withMods(view.method.mods)
      val positionedMethod = untpd.cpy
        .DefDef(sourceFreeMethod)(
          sourceFreeMethod.name,
          sourceFreeMethod.paramss,
          positionedType,
          sourceFreeMethod.rhs
        )
        .cloneIn(view.method.source)
        .withSpan(view.method.span)
      val replacementBody = originalBody.updated(view.memberIndex, positionedMethod)
      val reconstructed = ExistingUntpdClassMemberFilter
        .reconstruct(captured, replacementBody)
        .toOption
        .get

      val parameter = positionedMethod.paramss.head.head.asInstanceOf[untpd.ValDef]
      assert(parameter.eq(view.parameter))
      assert(parameter.tpt.eq(view.parameterType))
      assert(positionedMethod.rhs.eq(view.rhs))
      assert(!positionedType.eq(view.resultType))
      assertEquals(positionedType.source, view.resultType.source)
      assertEquals(positionedType.span, view.resultType.span)
      assertEquals(positionedMethod.source, view.method.source)
      assertEquals(positionedMethod.span, view.method.span)
      assert(!reconstructed.root.eq(root))
      assert(!reconstructed.template.eq(captured.originalTemplate))
      originalBody.indices.foreach { index =>
        if index == view.memberIndex then
          assert(reconstructed.template.body(index).eq(positionedMethod))
        else assert(reconstructed.template.body(index).eq(originalBody(index)))
      }
    }
  }

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U030RawResultType.scala", source)
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
