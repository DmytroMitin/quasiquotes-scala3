package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.{Accessor, Artifact, EmptyFlags, Erased, ExtensionMethod, Given, Implicit, Synthetic}
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.NoSource
import dotty.tools.dotc.util.Spans.Span

class ExistingUntpdTwoParameterMethodViewTest extends munit.FunSuite:
  test("captures exact heterogeneous and opaque raw handles without changing the original graph") {
    withContext {
      val fixtures = Vector(
        (
          """class PairOps:
            |  val before: Int = 1
            |  def combine(x: Int, y: String): Boolean = x.toString == y
            |  type After = String
            |""".stripMargin,
          1,
          "combine",
          "x",
          "y"
        ),
        (
          """class Renamed:
            |  object Before
            |  def merge(entries: Map[String, List[Int]], fallback: Either[Int, String]): (Int, String) =
            |    entries.toList match
            |      case (name, values) :: _ => (values.sum, name)
            |      case Nil => fallback.fold(value => (value, "missing"), value => (0, value))
            |  val after: Int = 2
            |""".stripMargin,
          1,
          "merge",
          "entries",
          "fallback"
        )
      )

      fixtures.foreach { case (source, index, methodName, firstName, secondName) =>
        val root = parseSingleTypeDef(source)
        val captured = capture(root)
        val method = captured.members(index).tree.asInstanceOf[untpd.DefDef]
        val first = method.paramss.head(0).asInstanceOf[untpd.ValDef]
        val second = method.paramss.head(1).asInstanceOf[untpd.ValDef]
        val before = snapshot(root)

        val firstView = viewOrFail(captured, index)
        val secondView = viewOrFail(captured, index)

        assert(!firstView.eq(secondView))
        Vector(firstView, secondView).foreach { view =>
          assert(view.captured.eq(captured))
          assertEquals(view.memberIndex, index)
          assert(view.method.eq(method))
          assertEquals(view.methodName, methodName)
          assert(view.firstParameter.eq(first))
          assertEquals(view.firstParameterName, firstName)
          assert(view.firstParameterType.eq(first.tpt))
          assert(view.secondParameter.eq(second))
          assertEquals(view.secondParameterName, secondName)
          assert(view.secondParameterType.eq(second.tpt))
          assert(view.resultType.eq(method.tpt))
          assert(view.rhs.eq(method.unforcedRhs))
          assertEquals(ExistingUntpdTwoParameterMethodView.validate(view), Right(()))
        }
        assertSnapshotUnchanged(before)
        assertEquals(ExistingUntpdClassMemberFilter.validateCaptured(captured), Right(()))
      }
    }
  }

  test("uses the captured member index when overloaded methods have the same name") {
    withContext {
      val captured = capture(parseSingleTypeDef(
        """class DuplicateNames:
          |  def same(first: Int, second: String): String = second
          |  def same(left: Boolean, right: Int): Boolean = left
          |""".stripMargin
      ))

      val first = viewOrFail(captured, 0)
      val second = viewOrFail(captured, 1)

      assertEquals(first.methodName, "same")
      assertEquals(second.methodName, "same")
      assertEquals(first.firstParameterName, "first")
      assertEquals(second.firstParameterName, "left")
      assert(first.method.eq(captured.members(0).tree))
      assert(second.method.eq(captured.members(1).tree))
      assert(!first.method.eq(second.method))
    }
  }

  test("fails closed for missing stale or malformed capture index and non-method selection") {
    withContext {
      assertCode(ExistingUntpdTwoParameterMethodView.capture(null, 0), "CAPTURE_REQUIRED")

      val root = parseSingleTypeDef(
        """class Mixed:
          |  val value: Int = 1
          |  type Alias = Int
          |  object Nested
          |  def selected(x: Int, y: String): String = y
          |""".stripMargin
      )
      val captured = capture(root)
      assertCode(
        ExistingUntpdTwoParameterMethodView.capture(
          captured.copy(members = captured.members.reverse),
          0
        ),
        "CAPTURE_INVARIANT_FAILED"
      )
      val nullRootRhs = untpd.cpy.TypeDef(captured.originalRoot)(
        captured.originalRoot.name,
        null
      )
      assertCode(
        ExistingUntpdTwoParameterMethodView.capture(
          captured.copy(originalRoot = nullRootRhs),
          0
        ),
        "CAPTURE_INVARIANT_FAILED"
      )
      assertCode(ExistingUntpdTwoParameterMethodView.capture(captured, -1), "MEMBER_INDEX_NOT_CAPTURED")
      assertCode(ExistingUntpdTwoParameterMethodView.capture(captured, 4), "MEMBER_INDEX_NOT_CAPTURED")
      Vector(0, 1, 2).foreach(index =>
        assertCode(
          ExistingUntpdTwoParameterMethodView.capture(captured, index),
          "SELECTED_MEMBER_NOT_METHOD"
        )
      )

      val nullBodyTemplate = untpd.cpy.Template(captured.originalTemplate)(
        captured.originalTemplate.constr,
        captured.originalTemplate.parentsOrDerived,
        captured.originalTemplate.derived,
        captured.originalTemplate.self,
        null
      )
      val nullBodyRoot = untpd.cpy.TypeDef(captured.originalRoot)(captured.originalRoot.name, nullBodyTemplate)
      assertCode(
        ExistingUntpdTwoParameterMethodView.capture(
          captured.copy(originalRoot = nullBodyRoot, originalTemplate = nullBodyTemplate),
          0
        ),
        "CAPTURE_INVARIANT_FAILED"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodView.capture(captured.copy(members = null), 0),
        "CAPTURE_INVARIANT_FAILED"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodView.capture(
          captured.copy(members = captured.members.updated(3, null)),
          3
        ),
        "CAPTURE_INVARIANT_FAILED"
      )
      assertCode(
        ExistingUntpdTwoParameterMethodView.capture(
          captured.copy(
            members = captured.members.updated(
              3,
              ExistingUntpdClassMemberFilter.Member(3, null)
            )
          ),
          3
        ),
        "CAPTURE_INVARIANT_FAILED"
      )
    }
  }

  test("rejects every method and clause topology outside exact two ordinary parameters") {
    withContext {
      val cases = Vector(
        "class Parameterless:\n  def bad: Int = 1\n" -> "ORDINARY_PARAMETER_CLAUSE_COUNT",
        "class EmptyClause:\n  def bad(): Int = 1\n" -> "PARAMETER_COUNT",
        "class OneParameter:\n  def bad(x: Int): Int = x\n" -> "PARAMETER_COUNT",
        "class ThreeParameters:\n  def bad(x: Int, y: Int, z: Int): Int = x\n" -> "PARAMETER_COUNT",
        "class MultipleClauses:\n  def bad(x: Int)(y: Int): Int = x\n" -> "ORDINARY_PARAMETER_CLAUSE_COUNT",
        "class OrdinaryThenUsing:\n  def bad(x: Int, y: Int)(using z: Int): Int = x\n" -> "CONTEXTUAL_PARAMETER_UNSUPPORTED",
        "class Generic:\n  def bad[A](x: A, y: A): A = x\n" -> "TYPE_PARAMETER_CLAUSE_UNSUPPORTED",
        "class Contextual:\n  def bad(using x: Int, y: Int): Int = x\n" -> "CONTEXTUAL_PARAMETER_UNSUPPORTED",
        "class ImplicitRole:\n  implicit def bad(x: Int, y: Int): Int = x\n" -> "UNSUPPORTED_METHOD_ROLE"
      )
      cases.foreach { case (source, expected) =>
        assertCode(
          ExistingUntpdTwoParameterMethodView.capture(capture(parseSingleTypeDef(source)), 0),
          expected
        )
      }

      val parsed = parseSingleTypeDef("class ConstructorOwner\n")
      assertCode(
        ExistingUntpdTwoParameterMethodView.capture(
          captureAround(parsed.rhs.asInstanceOf[untpd.Template].constr),
          0
        ),
        "UNSUPPORTED_METHOD_ROLE"
      )

      val ordinary = methodFrom("class Roles:\n  def good(x: Int, y: Int): Int = x\n")
      assertCode(
        ExistingUntpdTwoParameterMethodView.capture(
          captureAround(ordinary.withMods(null)),
          0
        ),
        "UNSUPPORTED_METHOD_ROLE"
      )
      Vector(Synthetic, Artifact, Accessor, ExtensionMethod, Given).foreach { flag =>
        val unsupported = ordinary.withMods(untpd.Modifiers(ordinary.mods.flags | flag))
        assertCode(
          ExistingUntpdTwoParameterMethodView.capture(captureAround(unsupported), 0),
          "UNSUPPORTED_METHOD_ROLE"
        )
      }
    }
  }

  test("rejects malformed entries unsupported parameter modifiers defaults and missing trees") {
    withContext {
      val method = methodFrom("class Forged:\n  def good(x: Int, y: String): Boolean = true\n")
      val first = method.paramss.head(0).asInstanceOf[untpd.ValDef]
      val second = method.paramss.head(1).asInstanceOf[untpd.ValDef]
      given dotty.tools.dotc.util.SourceFile = NoSource
      val replacementRhs = untpd.Literal(dotty.tools.dotc.core.Constants.Constant(1))
      val emptyType = untpd.TypeTree()

      assertForgedCode(copyMethod(method, null, method.tpt, method.rhs), "ORDINARY_PARAMETER_CLAUSE_COUNT")
      assertForgedCode(copyMethod(method, List(null), method.tpt, method.rhs), "ORDINARY_PARAMETER_CLAUSE_COUNT")
      assertForgedCode(
        copyMethod(method, List(List(null.asInstanceOf[untpd.ValDef], second)), method.tpt, method.rhs),
        "FIRST_PARAMETER_REQUIRED"
      )
      assertForgedCode(
        copyMethod(method, List(List(first, null.asInstanceOf[untpd.ValDef])), method.tpt, method.rhs),
        "SECOND_PARAMETER_REQUIRED"
      )
      Vector(Implicit, Given, Erased).foreach { flag =>
        val unsupported = first.withMods(untpd.Modifiers(first.mods.flags | flag))
        assertForgedCode(
          copyMethod(method, List(List(unsupported, second)), method.tpt, method.rhs),
          "CONTEXTUAL_PARAMETER_UNSUPPORTED"
        )
      }
      val syntheticParameter = first.withMods(untpd.Modifiers(first.mods.flags | Synthetic))
      assertForgedCode(
        copyMethod(method, List(List(syntheticParameter, second)), method.tpt, method.rhs),
        "UNSUPPORTED_PARAMETER_MODIFIERS"
      )
      val nullModifierParameter = first.withMods(null)
      assertForgedCode(
        copyMethod(method, List(List(nullModifierParameter, second)), method.tpt, method.rhs),
        "UNSUPPORTED_PARAMETER_MODIFIERS"
      )
      val firstWithDefault = untpd.cpy.ValDef(first)(first.name, first.tpt, replacementRhs)
      val secondWithDefault = untpd.cpy.ValDef(second)(second.name, second.tpt, replacementRhs)
      assertForgedCode(
        copyMethod(method, List(List(firstWithDefault, second)), method.tpt, method.rhs),
        "PARAMETER_RHS_UNEXPECTED"
      )
      assertForgedCode(
        copyMethod(method, List(List(first, secondWithDefault)), method.tpt, method.rhs),
        "PARAMETER_RHS_UNEXPECTED"
      )

      val firstEmptyType = untpd.cpy.ValDef(first)(first.name, emptyType, first.rhs)
      val secondEmptyType = untpd.cpy.ValDef(second)(second.name, emptyType, second.rhs)
      assertForgedCode(copyMethod(method, List(List(firstEmptyType, second)), method.tpt, method.rhs), "FIRST_PARAMETER_TYPE_REQUIRED")
      assertForgedCode(copyMethod(method, List(List(first, secondEmptyType)), method.tpt, method.rhs), "SECOND_PARAMETER_TYPE_REQUIRED")
      assertForgedCode(
        copyMethod(method, List(List(untpd.cpy.ValDef(first)(first.name, null, first.rhs), second)), method.tpt, method.rhs),
        "FIRST_PARAMETER_TYPE_REQUIRED"
      )
      assertForgedCode(
        copyMethod(method, List(List(first, untpd.cpy.ValDef(second)(second.name, null, second.rhs))), method.tpt, method.rhs),
        "SECOND_PARAMETER_TYPE_REQUIRED"
      )
      assertForgedCode(copyMethod(method, method.paramss, emptyType, method.rhs), "RESULT_TYPE_REQUIRED")
      assertForgedCode(copyMethod(method, method.paramss, null, method.rhs), "RESULT_TYPE_REQUIRED")
      assertForgedCode(copyMethod(method, method.paramss, method.tpt, untpd.EmptyTree), "RHS_REQUIRED")
      assertForgedCode(copyMethod(method, method.paramss, method.tpt, null), "RHS_REQUIRED")
    }
  }

  test("rejects malformed null descendants symbols and TypedSplice in method or owner graphs") {
    withContext {
      val method = methodFrom("class Repair:\n  def good(x: Int, y: String): Boolean = true\n")
      val first = method.paramss.head(0).asInstanceOf[untpd.ValDef]
      val second = method.paramss.head(1).asInstanceOf[untpd.ValDef]
      given dotty.tools.dotc.util.SourceFile = NoSource
      val malformedType = untpd.AppliedTypeTree(first.tpt, List(null))
      val malformedFirst = untpd.cpy.ValDef(first)(first.name, malformedType, first.rhs)
      assertForgedCode(
        copyMethod(method, List(List(malformedFirst, second)), method.tpt, method.rhs),
        "MALFORMED_METHOD_GRAPH"
      )

      val symbol = newSymbol(NoSymbol, termName("u033Symbol"), EmptyFlags, NoType)
      val symbolRhs = untpd.Ident(termName("symbolBearing")).withType(symbol.termRef)
      assertForgedCode(
        copyMethod(method, method.paramss, method.tpt, symbolRhs),
        "SYMBOL_BEARING_METHOD_GRAPH"
      )
      assertForgedCode(
        copyMethod(method, method.paramss, method.tpt, untpd.TypedSplice(symbolRhs)),
        "TYPED_SPLICE_METHOD_GRAPH"
      )
      val symbolType = untpd.Ident(typeName("SymbolType")).withType(symbol.typeRef)
      val symbolFirst = untpd.cpy.ValDef(first)(first.name, symbolType, first.rhs)
      assertForgedCode(
        copyMethod(method, List(List(symbolFirst, second)), method.tpt, method.rhs),
        "SYMBOL_BEARING_METHOD_GRAPH"
      )
      val typedSpliceSecond = untpd.cpy.ValDef(second)(
        second.name,
        untpd.TypedSplice(symbolType),
        second.rhs
      )
      assertForgedCode(
        copyMethod(method, List(List(first, typedSpliceSecond)), method.tpt, method.rhs),
        "TYPED_SPLICE_METHOD_GRAPH"
      )
      assertForgedCode(
        copyMethod(method, method.paramss, symbolType, method.rhs),
        "SYMBOL_BEARING_METHOD_GRAPH"
      )

      val parsed = parseSingleTypeDef(
        """class OwnerRepair:
          |  val before: Int = 1
          |  def good(x: Int, y: String): Boolean = true
          |""".stripMargin
      )
      val template = parsed.rhs.asInstanceOf[untpd.Template]
      val symbolMember = untpd.Ident(termName("ownerRepair")).withType(symbol.termRef)
      val forgedTemplate = untpd.cpy.Template(template)(
        template.constr,
        template.parentsOrDerived,
        template.derived,
        template.self,
        symbolMember :: template.body.tail
      )
      val forgedRoot = untpd.cpy.TypeDef(parsed)(parsed.name, forgedTemplate)
      val forgedCapture = ExistingUntpdClassMemberFilter.Capture(
        forgedRoot,
        forgedTemplate,
        forgedTemplate.body.iterator.zipWithIndex.map((tree, index) =>
          ExistingUntpdClassMemberFilter.Member(index, tree)
        ).toVector
      )
      assertCode(
        ExistingUntpdTwoParameterMethodView.capture(forgedCapture, 1),
        "SYMBOL_BEARING_OWNER_GRAPH"
      )
    }
  }

  test("detects every forged exposed raw identity while diagnostic names remain non-authoritative") {
    withContext {
      val captured = capture(parseSingleTypeDef(
        "class Valid:\n  def good(x: Int, y: String): Boolean = true\n"
      ))
      val view = viewOrFail(captured, 0)

      assertEquals(
        ExistingUntpdTwoParameterMethodView.validate(
          view.copy(methodName = "diagnostic-only", firstParameterName = "a", secondParameterName = "b")
        ),
        Right(())
      )
      val forgeries = Vector(
        view.copy(method = methodFrom("class Other:\n  def other(x: Int, y: String): Boolean = true\n")),
        view.copy(firstParameter = view.secondParameter),
        view.copy(secondParameter = view.firstParameter),
        view.copy(firstParameterType = view.secondParameterType),
        view.copy(secondParameterType = view.firstParameterType),
        view.copy(resultType = view.firstParameterType),
        view.copy(rhs = view.resultType),
        view.copy(memberIndex = 1),
        view.copy(captured = view.captured.copy(members = null))
      )
      forgeries.foreach(forged =>
        assertCode(
          ExistingUntpdTwoParameterMethodView.validate(forged),
          "VIEW_IDENTITY_INVARIANT_FAILED"
        )
      )
    }
  }

  private def assertForgedCode(method: untpd.DefDef, expected: String)(using Context): Unit =
    assertCode(ExistingUntpdTwoParameterMethodView.capture(captureAround(method), 0), expected)

  private def copyMethod(
      method: untpd.DefDef,
      paramss: List[untpd.ParamClause],
      resultType: untpd.Tree,
      rhs: untpd.Tree
  )(using Context): untpd.DefDef =
    untpd.cpy.DefDef(method)(method.name, paramss, resultType, rhs)

  private def methodFrom(source: String)(using Context): untpd.DefDef =
    parseSingleTypeDef(source).rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]

  private def capture(root: untpd.TypeDef)(using Context): ExistingUntpdClassMemberFilter.Capture =
    ExistingUntpdClassMemberFilter.capture(root).fold(problem => fail(problem.message), identity)

  private def captureAround(member: untpd.Tree)(using Context): ExistingUntpdClassMemberFilter.Capture =
    val parsed = parseSingleTypeDef("class CaptureShell\n")
    val oldTemplate = parsed.rhs.asInstanceOf[untpd.Template]
    val template = untpd.cpy.Template(oldTemplate)(
      oldTemplate.constr,
      oldTemplate.parentsOrDerived,
      oldTemplate.derived,
      oldTemplate.self,
      List(member)
    )
    val root = untpd.cpy.TypeDef(parsed)(parsed.name, template)
    ExistingUntpdClassMemberFilter.Capture(
      root,
      template,
      Vector(ExistingUntpdClassMemberFilter.Member(0, member))
    )

  private def viewOrFail(
      captured: ExistingUntpdClassMemberFilter.Capture,
      index: Int
  )(using Context): ExistingUntpdTwoParameterMethodView.View =
    ExistingUntpdTwoParameterMethodView
      .capture(captured, index)
      .fold(problem => fail(problem.message), identity)

  private def assertCode[A](
      result: Either[ExistingUntpdTwoParameterMethodViewError, A],
      expected: String
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected)
    case Right(value) => fail(s"expected $expected, found success $value")

  private def parseSingleTypeDef(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U033MethodView.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (root: untpd.TypeDef) :: Nil => root
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private final case class TreeSnapshot(
      tree: untpd.Tree,
      source: dotty.tools.dotc.util.SourceFile,
      span: Span,
      symbol: dotty.tools.dotc.core.Symbols.Symbol,
      modifiers: Option[dotty.tools.dotc.core.Flags.FlagSet]
  )

  private def snapshot(tree: untpd.Tree)(using Context): Vector[TreeSnapshot] =
    ExistingUntpdClassMemberFilter.allTrees(tree).map { node =>
      val modifiers = node match
        case member: untpd.MemberDef => Some(member.mods.flags)
        case _ => None
      TreeSnapshot(node, node.source, node.span, node.symbol, modifiers)
    }

  private def assertSnapshotUnchanged(before: Vector[TreeSnapshot])(using Context): Unit =
    val after = ExistingUntpdClassMemberFilter.allTrees(before.head.tree)
    assertEquals(after.size, before.size)
    after.zip(before).foreach { case (actual, expected) =>
      assert(actual.eq(expected.tree))
      assertEquals(actual.source, expected.source)
      assertEquals(actual.span, expected.span)
      assertEquals(actual.symbol, expected.symbol)
      expected.modifiers.foreach(flags =>
        assertEquals(actual.asInstanceOf[untpd.MemberDef].mods.flags, flags)
      )
    }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
