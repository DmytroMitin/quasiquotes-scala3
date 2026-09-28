package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.FlagSet
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.parser.TermShape
import quasiquotes.terms.{TermBinder, TermBindingCategory, TermShapeBindingView}

final class ExistingUntpdOrdinaryMethodRhsTermProjectionTest extends munit.FunSuite:
  import ExistingUntpdOrdinaryMethodDescriptor.Descriptor
  import ExistingUntpdOrdinaryMethodRhsTermProjection.*

  test("one parameter reference projects through its public graph-local binder") {
    withContext {
      val value = descriptor("class One:\n  def f(x: Int): Int = x\n")
      val projected = project(value)

      assertEquals(projected.parameterBinders.map(_.size), Vector(1))
      assertEquals(projected.shape.render, "BoundRef(x)")
      assertEquals(boundBinders(projected.shape), Vector(projected.parameterBinders.head.head))
      assertEquals(validateProjection(projected), Right(()))
    }
  }

  test("two-parameter infix and selected Apply references retain their ordered binders") {
    withContext {
      val infix = project(descriptor("class Infix:\n  def f(x: Int, y: Int): Int = x + y\n"))
      val selectedApply = project(descriptor(
        "class SelectedApply:\n  def f(x: Int, y: Int): Int = Math.max(x, y)\n"
      ))

      assertEquals(infix.shape.render, "Infix(BoundRef(x), +, BoundRef(y))")
      assertEquals(boundBinders(infix.shape), infix.parameterBinders.flatten)
      assertNotEquals(infix.parameterBinders.head(0), infix.parameterBinders.head(1))
      assertEquals(
        selectedApply.shape.render,
        "Apply(Select(Ident(Math), max), [BoundRef(x), BoundRef(y)])"
      )
      assertEquals(boundBinders(selectedApply.shape), selectedApply.parameterBinders.flatten)
    }
  }

  test("free identifiers and selected same-spelling member names remain ordinary") {
    withContext {
      val free = project(descriptor("class Free:\n  def f(x: Int): Int = helper(x)\n"))
      val selected = project(descriptor("class Selected:\n  def f(x: Int): Int = holder.x + x\n"))

      assertEquals(free.shape.render, "Apply(Ident(helper), [BoundRef(x)])")
      assertEquals(identifiers(free.shape), Vector("helper"))
      assertEquals(boundBinders(free.shape), free.parameterBinders.flatten)
      assertEquals(selected.shape.render, "Infix(Select(Ident(holder), x), +, BoundRef(x))")
      assertEquals(identifiers(selected.shape), Vector("holder"))
      assertEquals(boundBinders(selected.shape), selected.parameterBinders.flatten)
    }
  }

  test("literals unary tuples If Parens and recursive combinations stay in the bounded family") {
    withContext {
      val cases = Vector(
        "class Integer:\n  def f(x: Int): Int = 1\n" -> "Literal(1)",
        "class BooleanValue:\n  def f(flag: Boolean): Boolean = true\n" -> "Literal(true)",
        "class StringValue:\n  def f(x: Int): String = \"text\"\n" -> "Literal(\"text\")",
        "class UnaryValue:\n  def f(x: Int): Int = -x\n" -> "Unary(-, BoundRef(x))",
        "class TupleValue:\n  def f(x: Int, y: Int): (Int, Int) = (x, y)\n" -> "Tuple([BoundRef(x), BoundRef(y)])",
        "class IfValue:\n  def f(flag: Boolean, x: Int): Int = if flag then x else 0\n" ->
          "If(BoundRef(flag), BoundRef(x), Literal(0))",
        "class ParensValue:\n  def f(x: Int): Int = ((helper(x)))\n" ->
          "Parens(Parens(Apply(Ident(helper), [BoundRef(x)])))"
      )

      cases.foreach { (source, expected) =>
        val projected = project(descriptor(source))
        assertEquals(projected.shape.render, expected)
        assertEquals(validateProjection(projected), Right(()))
      }
    }
  }

  test("fresh projections keep semantic equality but never share public binder identity") {
    withContext {
      val sameDescriptor = descriptor("class Repeat:\n  def f(x: Int): Int = x + 1\n")
      val first = project(sameDescriptor)
      val repeated = project(sameDescriptor)
      val foreign = project(descriptor("class Foreign:\n  def f(x: Int): Int = x + 1\n"))

      assertEquals(first.shape, repeated.shape)
      assertEquals(first.shape, foreign.shape)
      assertNotEquals(first.parameterBinders.head.head, repeated.parameterBinders.head.head)
      assertNotEquals(first.parameterBinders.head.head, foreign.parameterBinders.head.head)
    }
  }

  test("cross-capture and forged descriptor scope shape and binder swaps fail closed") {
    withContext {
      val first = project(descriptor("class First:\n  def f(x: Int): Int = x\n"))
      val second = project(descriptor("class Second:\n  def f(x: Int): Int = x\n"))

      assertCode(validateProjection(first.copy(shape = second.shape)), "FOREIGN_BINDER_GRAPH")
      assertCode(
        validateProjection(first.copy(privateScope = second.privateScope)),
        "FINAL_PROJECTION_INVARIANT_FAILED"
      )
      assertCode(
        validateProjection(first.copy(parameterBinders = second.parameterBinders)),
        "FINAL_PROJECTION_INVARIANT_FAILED"
      )
      assertCode(
        validateProjection(first.copy(descriptor = second.descriptor)),
        "FINAL_PROJECTION_INVARIANT_FAILED"
      )
    }
  }

  test("duplicate declaration spelling is ambiguous and opaque Match remains an exact descriptor") {
    withContext {
      val duplicate = descriptor("class Duplicate:\n  def f(x: Int, x: Int): Int = x\n")
      assertCode(projectRhs(duplicate), "AMBIGUOUS_PARAMETER_NAME")

      val opaque = descriptor(
        "class Opaque:\n  def f(x: Int): Int = x match { case value => value }\n"
      )
      val before = snapshot(opaque.captured.originalRoot)
      val exactRhs = opaque.rhs
      assertEquals(ExistingUntpdOrdinaryMethodDescriptor.validate(opaque), Right(()))
      assertCode(projectRhs(opaque), "UNSUPPORTED_RHS_TOPOLOGY")
      assertEquals(ExistingUntpdOrdinaryMethodDescriptor.validate(opaque), Right(()))
      assert(opaque.rhs.eq(exactRhs))
      assertSnapshotUnchanged(before)
    }
  }

  test("invalid descriptors fail before decoding and success or failure never mutates raw identity") {
    withContext {
      val value = descriptor("class Immutable:\n  def f(x: Int, y: Int): Int = (x + y)\n")
      val before = snapshot(value.captured.originalRoot)
      val forged = value.copy(rhs = value.resultType)

      assertCode(projectRhs(null), "INVALID_DESCRIPTOR")
      assertCode(projectRhs(forged), "INVALID_DESCRIPTOR")
      project(value)
      project(value)
      assertSnapshotUnchanged(before)
    }
  }

  private def project(value: Descriptor)(using Context): Projection =
    projectRhs(value).fold(problem => fail(problem.message), identity)

  private def descriptor(source: String)(using Context): Descriptor =
    val root = parseSingleTypeDef(source)
    val captured = ExistingUntpdClassMemberFilter.capture(root)
      .fold(problem => fail(problem.message), identity)
    ExistingUntpdOrdinaryMethodDescriptor.capture(captured, 0)
      .fold(problem => fail(problem.message), identity)

  private def parseSingleTypeDef(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("RhsTermProjection.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (root: untpd.TypeDef) :: Nil => root
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def allShapes(shape: TermShape): Vector[TermShape] =
    shape +: (shape match
      case TermShape.Select(qualifier, _) => allShapes(qualifier)
      case TermShape.Apply(function, arguments) =>
        allShapes(function) ++ arguments.toVector.flatMap(allShapes)
      case TermShape.Infix(left, _, right) => allShapes(left) ++ allShapes(right)
      case TermShape.Unary(_, operand) => allShapes(operand)
      case TermShape.Tuple(elements) => elements.toVector.flatMap(allShapes)
      case TermShape.If(condition, thenBranch, elseBranch) =>
        allShapes(condition) ++ allShapes(thenBranch) ++ allShapes(elseBranch)
      case TermShape.Parenthesized(expression) => allShapes(expression)
      case _ => Vector.empty)

  private def boundBinders(shape: TermShape): Vector[TermBinder] =
    allShapes(shape).flatMap { current =>
      val view = TermShapeBindingView.inspect(current).fold(problem => fail(problem.message), identity)
      if view.category == TermBindingCategory.BoundReference then
        Vector(view.boundReference.getOrElse(fail("missing bound-reference view")).binder)
      else Vector.empty
    }

  private def identifiers(shape: TermShape): Vector[String] =
    allShapes(shape).collect { case TermShape.Identifier(name, false) => name }

  private def assertCode[A](value: Either[Error, A], expected: String): Unit =
    value match
      case Left(problem) =>
        assertEquals(problem.code, expected, clues(problem))
        assert(problem.detail.nonEmpty, clues(problem))
        assert(problem.message.nonEmpty, clues(problem))
      case Right(actual) => fail(s"expected $expected, obtained $actual")

  private final case class TreeSnapshot(
      tree: untpd.Tree,
      source: dotty.tools.dotc.util.SourceFile,
      span: Span,
      symbol: Symbol,
      modifiers: Option[FlagSet]
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
    after.zip(before).foreach { (actual, expected) =>
      assert(actual.eq(expected.tree))
      assert(actual.source.eq(expected.source))
      assertEquals(actual.span, expected.span)
      assertEquals(actual.symbol, expected.symbol)
      expected.modifiers.foreach(flags =>
        assertEquals(actual.asInstanceOf[untpd.MemberDef].mods.flags, flags)
      )
    }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
