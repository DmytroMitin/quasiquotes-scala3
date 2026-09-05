package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.{EmptyFlags, Given, Synthetic}
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.NoSource
import dotty.tools.dotc.util.Spans.Span

class ExistingUntpdSingleParameterMethodViewTest extends munit.FunSuite:
  test("captures canonical and fully renamed methods as exact read-only handles") {
    withContext {
      val fixtures = Vector(
        (
          """class Box:
            |  def convert(x: Int): String = x.toString
            |""".stripMargin,
          0,
          "convert",
          "x"
        ),
        (
          """class Vessel:
            |  val before: Int = 1
            |  def transform(payload: String): Boolean =
            |    payload.nonEmpty && payload.reverse.nonEmpty
            |  type After = String
            |""".stripMargin,
          1,
          "transform",
          "payload"
        )
      )

      fixtures.foreach { case (source, index, methodName, parameterName) =>
        val root = parseSingleTypeDef(source)
        val captured = capture(root)
        val method = captured.members(index).tree.asInstanceOf[untpd.DefDef]
        val parameter = method.paramss.head.head.asInstanceOf[untpd.ValDef]
        val before = snapshot(root)

        val view = viewOrFail(captured, index)

        assert(view.captured.eq(captured))
        assertEquals(view.memberIndex, index)
        assert(view.method.eq(method))
        assertEquals(view.methodName, methodName)
        assert(view.parameter.eq(parameter))
        assertEquals(view.parameterName, parameterName)
        assert(view.parameterType.eq(parameter.tpt))
        assert(view.resultType.eq(method.tpt))
        assert(view.rhs.eq(method.rhs))
        assertSnapshotUnchanged(before)
        assertEquals(ExistingUntpdClassMemberFilter.validateCaptured(captured), Right(()))
      }
    }
  }

  test("uses the captured direct-member index rather than a duplicate method name") {
    withContext {
      val root = parseSingleTypeDef(
        """class DuplicateNames:
          |  def same(first: Int): Int = first
          |  def same(second: String): String = second
          |""".stripMargin
      )
      val captured = capture(root)
      val first = viewOrFail(captured, 0)
      val second = viewOrFail(captured, 1)

      assertEquals(first.methodName, "same")
      assertEquals(second.methodName, "same")
      assertEquals(first.parameterName, "first")
      assertEquals(second.parameterName, "second")
      assert(first.method.eq(captured.members(0).tree))
      assert(second.method.eq(captured.members(1).tree))
      assert(!first.method.eq(second.method))
    }
  }

  test("accepts an opaque existing RHS outside the bounded semantic Term grammar") {
    withContext {
      val root = parseSingleTypeDef(
        """class OpaqueBody:
          |  def classify(input: Int): String = input match
          |    case 0 => "zero"
          |    case _ => "other"
          |""".stripMargin
      )
      val captured = capture(root)
      val method = captured.members.head.tree.asInstanceOf[untpd.DefDef]
      val view = viewOrFail(captured, 0)

      assertEquals(method.rhs.getClass.getSimpleName, "Match")
      assert(view.rhs.eq(method.rhs))
      assertEquals(ExistingUntpdClassMemberFilter.validateCaptured(captured), Right(()))
    }
  }

  test("fails closed for missing or stale capture, invalid index, and non-method selection") {
    withContext {
      assertCode(ExistingUntpdSingleParameterMethodView.capture(null, 0), "CAPTURE_REQUIRED")

      val root = parseSingleTypeDef(
        """class Mixed:
          |  val value: Int = 1
          |  type Alias = Int
          |  object Nested
          |  def selected(x: Int): Int = x
          |""".stripMargin
      )
      val captured = capture(root)
      assertCode(
        ExistingUntpdSingleParameterMethodView.capture(
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
        ExistingUntpdSingleParameterMethodView.capture(
          captured.copy(originalRoot = nullRootRhs),
          0
        ),
        "CAPTURE_INVARIANT_FAILED"
      )
      assertCode(ExistingUntpdSingleParameterMethodView.capture(captured, -1), "MEMBER_INDEX_NOT_CAPTURED")
      assertCode(ExistingUntpdSingleParameterMethodView.capture(captured, 4), "MEMBER_INDEX_NOT_CAPTURED")
      Vector(0, 1, 2).foreach(index =>
        assertCode(
          ExistingUntpdSingleParameterMethodView.capture(captured, index),
          "SELECTED_MEMBER_NOT_METHOD"
        )
      )

      val staleTemplate = untpd.cpy.Template(captured.originalTemplate)(
        captured.originalTemplate.constr,
        captured.originalTemplate.parentsOrDerived,
        captured.originalTemplate.derived,
        captured.originalTemplate.self,
        captured.originalTemplate.body.updated(3, captured.originalTemplate.body.head)
      )
      val staleRoot = untpd.cpy.TypeDef(captured.originalRoot)(captured.originalRoot.name, staleTemplate)
      val stale = captured.copy(originalRoot = staleRoot, originalTemplate = staleTemplate)
      assertCode(
        ExistingUntpdSingleParameterMethodView.capture(stale, 3),
        "CAPTURE_INVARIANT_FAILED"
      )
    }
  }

  test("rejects unsupported method roles and parameter-clause topologies") {
    withContext {
      val cases = Vector(
        "class Parameterless:\n  def bad: Int = 1\n" -> "ORDINARY_PARAMETER_CLAUSE_COUNT",
        "class EmptyClause:\n  def bad(): Int = 1\n" -> "PARAMETER_COUNT",
        "class TwoParameters:\n  def bad(x: Int, y: Int): Int = x\n" -> "PARAMETER_COUNT",
        "class MultipleClauses:\n  def bad(x: Int)(y: Int): Int = x\n" -> "ORDINARY_PARAMETER_CLAUSE_COUNT",
        "class OrdinaryThenUsing:\n  def bad(x: Int)(using y: Int): Int = x\n" -> "CONTEXTUAL_PARAMETER_UNSUPPORTED",
        "class Generic:\n  def bad[A](x: A): A = x\n" -> "TYPE_PARAMETER_CLAUSE_UNSUPPORTED",
        "class Contextual:\n  def bad(using x: Int): Int = x\n" -> "CONTEXTUAL_PARAMETER_UNSUPPORTED",
        "class ImplicitRole:\n  implicit def bad(x: Int): Int = x\n" -> "UNSUPPORTED_METHOD_ROLE"
      )
      cases.foreach { case (source, expected) =>
        val captured = capture(parseSingleTypeDef(source))
        assertCode(ExistingUntpdSingleParameterMethodView.capture(captured, 0), expected)
      }

      val parsed = parseSingleTypeDef("class ConstructorOwner\n")
      val constructor = parsed.rhs.asInstanceOf[untpd.Template].constr
      assertCode(
        ExistingUntpdSingleParameterMethodView.capture(captureAround(constructor), 0),
        "UNSUPPORTED_METHOD_ROLE"
      )

      val ordinary = methodFrom("class SyntheticOwner:\n  def good(x: Int): Int = x\n")
      val synthetic = ordinary.withMods(untpd.Modifiers(ordinary.mods.flags | Synthetic))
      assertCode(
        ExistingUntpdSingleParameterMethodView.capture(captureAround(synthetic), 0),
        "UNSUPPORTED_METHOD_ROLE"
      )
      val givenRole = ordinary.withMods(untpd.Modifiers(ordinary.mods.flags | Given))
      assertCode(
        ExistingUntpdSingleParameterMethodView.capture(captureAround(givenRole), 0),
        "UNSUPPORTED_METHOD_ROLE"
      )
    }
  }

  test("returns structured failures for malformed parameter, type, result, and RHS seams") {
    withContext {
      val method = methodFrom("class Forged:\n  def good(x: Int): Int = x\n")
      val parameter = method.paramss.head.head.asInstanceOf[untpd.ValDef]
      given dotty.tools.dotc.util.SourceFile = NoSource
      val replacementRhs = untpd.Literal(dotty.tools.dotc.core.Constants.Constant(1))
      val emptyType = untpd.TypeTree()

      val emptyParameter = copyMethod(method, List(List(untpd.EmptyValDef)), method.tpt, method.rhs)
      assertForgedCode(emptyParameter, "PARAMETER_REQUIRED")
      val nullParameter = copyMethod(
        method,
        List(List(null.asInstanceOf[untpd.ValDef])),
        method.tpt,
        method.rhs
      )
      assertForgedCode(nullParameter, "PARAMETER_REQUIRED")

      val parameterWithRhs = untpd.cpy.ValDef(parameter)(parameter.name, parameter.tpt, replacementRhs)
      assertForgedCode(
        copyMethod(method, List(List(parameterWithRhs)), method.tpt, method.rhs),
        "PARAMETER_RHS_UNEXPECTED"
      )
      val emptyParameterType = untpd.cpy.ValDef(parameter)(parameter.name, emptyType, parameter.rhs)
      assertForgedCode(
        copyMethod(method, List(List(emptyParameterType)), method.tpt, method.rhs),
        "PARAMETER_TYPE_REQUIRED"
      )
      val nullParameterType = untpd.cpy.ValDef(parameter)(parameter.name, null, parameter.rhs)
      assertForgedCode(
        copyMethod(method, List(List(nullParameterType)), method.tpt, method.rhs),
        "PARAMETER_TYPE_REQUIRED"
      )
      assertForgedCode(copyMethod(method, method.paramss, emptyType, method.rhs), "RESULT_TYPE_REQUIRED")
      assertForgedCode(copyMethod(method, method.paramss, null, method.rhs), "RESULT_TYPE_REQUIRED")
      assertForgedCode(copyMethod(method, method.paramss, method.tpt, untpd.EmptyTree), "RHS_REQUIRED")
      assertForgedCode(copyMethod(method, method.paramss, method.tpt, null), "RHS_REQUIRED")
    }
  }

  test("rejects owner-repair graphs and detects a forged view identity failure") {
    withContext {
      val method = methodFrom("class Repair:\n  def good(x: Int): Int = x\n")
      val symbol = newSymbol(NoSymbol, termName("u028Symbol"), EmptyFlags, NoType)
      val symbolRhs = method.rhs.withType(symbol.termRef)
      assertForgedCode(
        copyMethod(method, method.paramss, method.tpt, symbolRhs),
        "SYMBOL_BEARING_METHOD_GRAPH"
      )
      assertForgedCode(
        copyMethod(method, method.paramss, method.tpt, untpd.TypedSplice(symbolRhs)),
        "TYPED_SPLICE_METHOD_GRAPH"
      )

      val captured = capture(parseSingleTypeDef("class Valid:\n  def good(x: Int): Int = x\n"))
      val view = viewOrFail(captured, 0)
      assertCode(
        ExistingUntpdSingleParameterMethodView.validate(view.copy(rhs = view.parameterType)),
        "VIEW_IDENTITY_INVARIANT_FAILED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodView.validate(
          view.copy(captured = view.captured.copy(members = null))
        ),
        "VIEW_IDENTITY_INVARIANT_FAILED"
      )
      val nullRootRhs = untpd.cpy.TypeDef(view.captured.originalRoot)(
        view.captured.originalRoot.name,
        null
      )
      assertCode(
        ExistingUntpdSingleParameterMethodView.validate(
          view.copy(captured = view.captured.copy(originalRoot = nullRootRhs))
        ),
        "VIEW_IDENTITY_INVARIANT_FAILED"
      )
    }
  }

  private def assertForgedCode(method: untpd.DefDef, expected: String)(using Context): Unit =
    assertCode(ExistingUntpdSingleParameterMethodView.capture(captureAround(method), 0), expected)

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
  )(using Context): ExistingUntpdSingleParameterMethodView.View =
    ExistingUntpdSingleParameterMethodView
      .capture(captured, index)
      .fold(problem => fail(problem.message), identity)

  private def assertCode[A](
      result: Either[ExistingUntpdSingleParameterMethodViewError, A],
      expected: String
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected)
    case Right(value) => fail(s"expected $expected, found success $value")

  private def parseSingleTypeDef(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U028MethodView.scala", source)
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
