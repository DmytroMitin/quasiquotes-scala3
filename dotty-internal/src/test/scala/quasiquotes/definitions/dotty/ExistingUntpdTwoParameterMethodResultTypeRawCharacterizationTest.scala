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

class ExistingUntpdTwoParameterMethodResultTypeRawCharacterizationTest
    extends munit.FunSuite:
  test("characterizes the exact two-parameter result-type site and owner reconstruction") {
    withContext {
      val root = parseClass(
        """class ResultSite:
          |  val before: Int = 1
          |  def sum(x: Int, y: Int): AnyVal = x + y
          |  type After = String
          |""".stripMargin
      )
      val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
      val view = ExistingUntpdTwoParameterMethodView.capture(captured, 1).toOption.get
      val originalBody = captured.originalTemplate.body.toVector
      val originalState = ExistingUntpdClassMemberFilter.allTrees(root).map(tree =>
        (tree, tree.source, tree.span)
      )
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
        .DefDef(view.method.name, view.method.paramss, lowered, view.rhs)
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
      val reconstructed = ExistingUntpdClassMemberFilter
        .reconstruct(captured, originalBody.updated(view.memberIndex, positionedMethod))
        .toOption
        .get

      val first = positionedMethod.paramss.head(0).asInstanceOf[untpd.ValDef]
      val second = positionedMethod.paramss.head(1).asInstanceOf[untpd.ValDef]
      assert(first.eq(view.firstParameter))
      assert(first.tpt.eq(view.firstParameterType))
      assert(second.eq(view.secondParameter))
      assert(second.tpt.eq(view.secondParameterType))
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
      ExistingUntpdClassMemberFilter.allTrees(root).zip(originalState).foreach {
        case (tree, (original, source, span)) =>
          assert(tree.eq(original))
          assertEquals(tree.source, source)
          assertEquals(tree.span, span)
      }
    }
  }

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U035RawResultType.scala", source)
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
