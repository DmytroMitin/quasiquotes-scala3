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

class ExistingUntpdTwoParameterMethodParameterTypeRawCharacterizationTest
    extends munit.FunSuite:
  test("characterizes each selected parameter type site and exact untouched parameter reuse") {
    withContext {
      Vector(0, 1).foreach { parameterIndex =>
        val root = parseClass(
          """class ParameterSites:
            |  val before: Int = 1
            |  def combine(left: AnyVal, right: AnyVal): Int = left
            |  type After = String
            |""".stripMargin
        )
        val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
        val view = ExistingUntpdTwoParameterMethodView.capture(captured, 1).toOption.get
        val selected = if parameterIndex == 0 then view.firstParameter else view.secondParameter
        val selectedType = if parameterIndex == 0 then view.firstParameterType else view.secondParameterType
        val untouched = if parameterIndex == 0 then view.secondParameter else view.firstParameter
        val untouchedType = if parameterIndex == 0 then view.secondParameterType else view.firstParameterType
        val originalBody = captured.originalTemplate.body.toVector
        val originalState = ExistingUntpdClassMemberFilter.allTrees(root).map(tree =>
          (tree, tree.source, tree.span)
        )
        val lowered = CompletedTypeUntypedLowerer
          .lower(TypeNormalForm.STypeIdent("Int"))
          .toOption
          .get

        assert(lowered.isInstanceOf[untpd.Ident])
        assert(!lowered.source.exists)
        assert(!lowered.span.exists)
        assertEquals(lowered.symbol, NoSymbol)
        assert(selectedType.source.exists && selectedType.span.exists)
        assert(selected.source.exists && selected.span.exists)

        given SourceFile = NoSource
        val positionedType = lowered.cloneIn(selectedType.source).withSpan(selectedType.span)
        val sourceFreeSelected = untpd
          .ValDef(selected.name, lowered, untpd.EmptyTree)
          .withMods(selected.mods)
        val positionedSelected = untpd.cpy
          .ValDef(sourceFreeSelected)(
            sourceFreeSelected.name,
            positionedType,
            sourceFreeSelected.rhs
          )
          .cloneIn(selected.source)
          .withSpan(selected.span)
        val parameters =
          if parameterIndex == 0 then List(positionedSelected, untouched)
          else List(untouched, positionedSelected)
        val sourceFreeMethod = untpd
          .DefDef(view.method.name, List(parameters), view.resultType, view.rhs)
          .withMods(view.method.mods)
        val positionedMethod = untpd.cpy
          .DefDef(sourceFreeMethod)(
            sourceFreeMethod.name,
            List(parameters),
            sourceFreeMethod.tpt,
            sourceFreeMethod.rhs
          )
          .cloneIn(view.method.source)
          .withSpan(view.method.span)
        val reconstructed = ExistingUntpdClassMemberFilter
          .reconstruct(captured, originalBody.updated(view.memberIndex, positionedMethod))
          .toOption
          .get

        val rebuilt = positionedMethod.paramss.head.map(_.asInstanceOf[untpd.ValDef])
        assert(!positionedType.eq(selectedType))
        assert(!positionedSelected.eq(selected))
        assert(positionedSelected.tpt.eq(positionedType))
        assert(positionedSelected.rhs.isEmpty)
        assert(rebuilt(parameterIndex).eq(positionedSelected))
        assert(rebuilt(1 - parameterIndex).eq(untouched))
        assert(rebuilt(1 - parameterIndex).tpt.eq(untouchedType))
        assert(positionedMethod.tpt.eq(view.resultType))
        assert(positionedMethod.rhs.eq(view.rhs))
        assertEquals(positionedType.source, selectedType.source)
        assertEquals(positionedType.span, selectedType.span)
        assertEquals(positionedSelected.source, selected.source)
        assertEquals(positionedSelected.span, selected.span)
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
  }

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U036RawParameterType.scala", source)
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
