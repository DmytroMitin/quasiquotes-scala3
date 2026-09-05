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

class ExistingUntpdSingleParameterMethodParameterTypeRawCharacterizationTest
    extends munit.FunSuite:
  test("characterizes fresh parameter and type shells at the exact old transformation sites") {
    withContext {
      val root = parseClass(
        """class ParameterSite:
          |  val before: Int = 1
          |  def bump(x: AnyVal): Int = x
          |  type After = String
          |""".stripMargin
      )
      val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
      val view = ExistingUntpdSingleParameterMethodView.capture(captured, 1).toOption.get
      val lowered = CompletedTypeUntypedLowerer
        .lower(TypeNormalForm.STypeIdent("Int"))
        .toOption
        .get

      assert(lowered.isInstanceOf[untpd.Ident])
      assert(!lowered.source.exists)
      assert(!lowered.span.exists)
      assertEquals(lowered.symbol, NoSymbol)
      assert(view.parameterType.source.exists && view.parameterType.span.exists)
      assert(view.parameter.source.exists && view.parameter.span.exists)

      given SourceFile = NoSource
      val positionedType = lowered.cloneIn(view.parameterType.source).withSpan(view.parameterType.span)
      val sourceFreeParameter = untpd
        .ValDef(view.parameter.name, lowered, untpd.EmptyTree)
        .withMods(view.parameter.mods)
      val positionedParameter = untpd.cpy
        .ValDef(sourceFreeParameter)(sourceFreeParameter.name, positionedType, sourceFreeParameter.rhs)
        .cloneIn(view.parameter.source)
        .withSpan(view.parameter.span)
      val sourceFreeMethod = untpd
        .DefDef(view.method.name, List(List(sourceFreeParameter)), view.resultType, view.rhs)
        .withMods(view.method.mods)
      val positionedMethod = untpd.cpy
        .DefDef(sourceFreeMethod)(
          sourceFreeMethod.name,
          List(List(positionedParameter)),
          sourceFreeMethod.tpt,
          sourceFreeMethod.rhs
        )
        .cloneIn(view.method.source)
        .withSpan(view.method.span)
      val reconstructed = ExistingUntpdClassMemberFilter
        .reconstruct(
          captured,
          captured.originalTemplate.body.toVector.updated(view.memberIndex, positionedMethod)
        )
        .toOption
        .get

      assert(!positionedType.eq(view.parameterType))
      assert(!positionedParameter.eq(view.parameter))
      assert(positionedParameter.tpt.eq(positionedType))
      assert(positionedParameter.rhs.isEmpty)
      assert(positionedMethod.tpt.eq(view.resultType))
      assert(positionedMethod.rhs.eq(view.rhs))
      assertEquals(positionedType.source, view.parameterType.source)
      assertEquals(positionedType.span, view.parameterType.span)
      assertEquals(positionedParameter.source, view.parameter.source)
      assertEquals(positionedParameter.span, view.parameter.span)
      assertEquals(positionedMethod.source, view.method.source)
      assertEquals(positionedMethod.span, view.method.span)
      assert(!reconstructed.root.eq(root))
      assert(!reconstructed.template.eq(captured.originalTemplate))
    }
  }

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U031RawParameterType.scala", source)
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
