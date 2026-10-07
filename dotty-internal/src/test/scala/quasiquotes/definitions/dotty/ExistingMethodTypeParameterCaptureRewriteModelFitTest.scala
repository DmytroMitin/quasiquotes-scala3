package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

class ExistingMethodTypeParameterCaptureRewriteModelFitTest extends munit.FunSuite:
  private final case class Fixture(
      source: String,
      typeParameterName: String,
      termParameterName: String,
      dependent: Boolean
  )

  private val fixtures = Vector(
    Fixture(
      "class PhantomGeneric:\n  def keep[A](x: Int): Int = x\n",
      "A",
      "x",
      dependent = false
    ),
    Fixture(
      "class RenamedPhantom:\n  def retain[Element](value: Int): Int = value\n",
      "Element",
      "value",
      dependent = false
    ),
    Fixture(
      "class GenericIdentity:\n  def id[A](x: A): A = x\n",
      "A",
      "x",
      dependent = true
    ),
    Fixture(
      "class RenamedIdentity:\n  def same[Element](value: Element): Element = value\n",
      "Element",
      "value",
      dependent = true
    )
  )

  test("method Type parameter parser representation") {
    withContext {
      val method = parseClass(fixtures.head.source)
        .rhs.asInstanceOf[untpd.Template]
        .body.head.asInstanceOf[untpd.DefDef]
      val typeParameter = method.paramss.head.head.asInstanceOf[untpd.TypeDef]

      assertEquals(typeParameter.mods.flags, Flags.Param)
    }
  }

  test("G1 and G2 retain exact Type clause order, bounds, references, origins, and pre-Typer cleanliness") {
    withContext {
      fixtures.foreach { fixture =>
        val root = parseClass(fixture.source)
        val method = root.rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]
        assertEquals(method.mods.flags, Flags.Method)
        assertEquals(method.paramss.map(_.size), List(1, 1))
        val typeParameter = method.paramss.head.head.asInstanceOf[untpd.TypeDef]
        val termParameter = method.paramss(1).head.asInstanceOf[untpd.ValDef]
        assertEquals(typeParameter.name.toString, fixture.typeParameterName)
        assertEquals(typeParameter.mods.flags, Flags.Param)
        assertEquals(termParameter.name.toString, fixture.termParameterName)
        assertEquals(termParameter.mods.flags, Flags.Param)
        typeParameter.rhs match
          case bounds: untpd.TypeBoundsTree =>
            assert(bounds.lo.isEmpty)
            assert(bounds.hi.isEmpty)
            assert(bounds.alias.isEmpty)
          case other => fail("expected unbounded TypeBoundsTree, found " + other)

        val expectedTypeName = if fixture.dependent then fixture.typeParameterName else "Int"
        termParameter.tpt match
          case untpd.Ident(name) => assertEquals(name.toString, expectedTypeName)
          case other => fail("expected direct term-parameter Type Ident, found " + other)
        method.tpt match
          case untpd.Ident(name) => assertEquals(name.toString, expectedTypeName)
          case other => fail("expected direct result Type Ident, found " + other)
        method.rhs match
          case untpd.Ident(name) => assertEquals(name.toString, fixture.termParameterName)
          case other => fail("expected direct term-parameter RHS reference, found " + other)

        Vector[untpd.Tree](method, typeParameter, typeParameter.rhs, termParameter,
          termParameter.tpt, method.tpt, method.rhs).foreach { tree =>
          assert(tree.source.exists)
          assert(tree.span.exists)
          assert(tree.source.eq(method.source))
        }
        val trees = ExistingUntpdClassMemberFilter.allTrees(root)
        assert(trees.forall(_.symbol == NoSymbol))
        assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))
      }

      val ordinary = parseClass("class OrdinaryControl:\n  def f(x: Int): Int = x\n")
        .rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]
      assertEquals(ordinary.paramss.map(_.map(_.getClass.getSimpleName)), List(List("ValDef")))
    }
  }

  test("current private and public capture reject G1 and G2 at generic topology") {
    withContext {
      fixtures.take(1).appended(fixtures(2)).foreach { fixture =>
        val root = parseClass(fixture.source)
        val captured = ExistingUntpdClassMemberFilter.capture(root).fold(
          problem => fail(problem.message),
          identity
        )
        val privateFailure = ExistingUntpdOrdinaryMethodDescriptor
          .capture(captured, 0).left.toOption.get
        assertEquals(privateFailure.code, "UNSUPPORTED_PARAMETER_TOPOLOGY")
        assert(privateFailure.detail.contains("one clause"))

        val publicCapture = ExistingClassUntypedRewrite.capture(root).fold(
          problem => fail(problem.message),
          identity
        )
        val publicFailure = publicCapture
          .method(publicCapture.members.head.ref).left.toOption.get
        assertEquals(publicFailure.code, "UNSUPPORTED_STRUCTURE")
        assert(publicFailure.detail.contains("UNSUPPORTED_PARAMETER_TOPOLOGY"))
      }
    }
  }

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U055ExistingGeneric.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
