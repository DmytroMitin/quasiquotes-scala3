package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.*
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class ExistingTwoUnaryOrdinaryClauseAdmissionAndRewriteTest extends munit.FunSuite:
  test("descriptor and public facade retain two distinct ordinary unary clauses"):
    withContext:
      val root = parseClass("class Curried:\n  def combine(a: Int)(b: Int): Int = a\n")
      val exact = ExistingUntpdClassMemberFilter.capture(root).fold(problem => fail(problem.message), identity)
      val descriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(exact, 0)
        .fold(problem => fail(problem.message), identity)
      assertEquals(descriptor.parameterClauses.map(_.map(_.diagnosticName)), Vector(Vector("a"), Vector("b")))
      assert(descriptor.parameterClauses(0)(0).tree.eq(descriptor.method.paramss(0).head))
      assert(descriptor.parameterClauses(1)(0).tree.eq(descriptor.method.paramss(1).head))
      assertEquals(ExistingUntpdOrdinaryMethodDescriptor.validate(descriptor), Right(()))

      val captured = ExistingClassUntypedRewrite.capture(root).fold(problem => fail(problem.message), identity)
      val method = captured.method(captured.members.head.ref).fold(problem => fail(problem.message), identity)
      assertEquals(method.parameterClauses.map(_.kind.code), Vector("ORDINARY", "ORDINARY"))
      assertEquals(method.parameterClauses.map(_.parameters.size), Vector(1, 1))
      val first = method.parameterClauses(0).parameters(0)
      val second = method.parameterClauses(1).parameters(0)
      assertEquals((first.clauseIndex, first.parameterIndex), (0, 0))
      assertEquals((second.clauseIndex, second.parameterIndex), (1, 0))
      assertNotEquals(
        method.parameterScope.binder(first.ref).fold(problem => fail(problem.message), identity),
        method.parameterScope.binder(second.ref).fold(problem => fail(problem.message), identity)
      )
      assertEquals(first.declaredType.semantic, Right(TypeNormalForm.STypeIdent("Int")))
      assertEquals(second.declaredType.semantic, Right(TypeNormalForm.STypeIdent("Int")))
      assertEquals(method.resultType.semantic, Right(TypeNormalForm.STypeIdent("Int")))
      assert(method.body.semantic.isRight)

  test("two-clause descriptor recapture rejects topology, identity, modifier, default, and owner tampering"):
    withContext:
      val method = rawMethod(parseClass("class Curried:\n  def combine(a: Int)(b: Int): Int = a\n"))
      val captured = captureAround(method)
      val descriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(captured, 0)
        .fold(problem => fail(problem.message), identity)
      val first = descriptor.parameterClauses(0)(0)
      val second = descriptor.parameterClauses(1)(0)
      Vector(
        descriptor.copy(parameterClauses = descriptor.parameterClauses.reverse),
        descriptor.copy(parameterClauses = Vector(Vector(first, second))),
        descriptor.copy(parameterClauses = Vector(Vector(first), Vector(first))),
        descriptor.copy(parameterClauses = Vector(Vector(first), Vector(second.copy(tree = first.tree)))),
        descriptor.copy(parameterClauses = Vector(Vector(first), Vector(second.copy(tpt = first.tpt))))
      ).foreach { forged =>
        assertEquals(
          ExistingUntpdOrdinaryMethodDescriptor.validate(forged).left.toOption.map(_.code),
          Some("DESCRIPTOR_IDENTITY_INVARIANT_FAILED")
        )
      }

      val firstTree = method.paramss(0).head.asInstanceOf[untpd.ValDef]
      val secondTree = method.paramss(1).head.asInstanceOf[untpd.ValDef]
      val erased = secondTree.withMods(
        secondTree.mods.copy(flags = secondTree.mods.flags | dotty.tools.dotc.core.Flags.Erased)
      )
      val defaulted = untpd.cpy.ValDef(secondTree)(
        secondTree.name,
        secondTree.tpt,
        method.rhs
      )
      Vector(erased, defaulted).foreach { changedSecond =>
        val changedMethod = untpd.cpy.DefDef(method)(
          method.name,
          List(List(firstTree), List(changedSecond)),
          method.tpt,
          method.rhs
        )
        assert(ExistingUntpdOrdinaryMethodDescriptor.capture(captureAround(changedMethod), 0).isLeft)
      }
      assert(ExistingUntpdOrdinaryMethodDescriptor.capture(captured.copy(originalRoot = null), 0).isLeft)

  test("public plans edit either clause, both clauses in either order, result, body, and all fields atomically"):
    withContext:
      val source =
        "class CurriedOrdinary:\n  def combine(a: AnyVal)(b: AnyVal): AnyVal = a\n  val untouched: Int = 7\n"
      def fixture() =
        val root = parseClass(source)
        val captured = ExistingClassUntypedRewrite.capture(root).fold(problem => fail(problem.message), identity)
        val method = captured.method(captured.members.head.ref).fold(problem => fail(problem.message), identity)
        (root, captured, method, method.parameterClauses(0).parameters(0), method.parameterClauses(1).parameters(0))

      val intType = TypeNormalForm.STypeIdent("Int")
      Vector((0, 1), (1, 0)).foreach { (selected, unchanged) =>
        val (root, captured, method, first, second) = fixture()
        val parameters = Vector(first, second)
        val plan = captured.emptyPlan.replaceParameterType(parameters(selected).ref, intType)
          .fold(problem => fail(problem.message), identity)
        val rewritten = applyPlan(captured, plan)
        val original = rawMethod(root)
        val result = rawMethod(rewritten)
        assert(!result.paramss(selected).head.eq(original.paramss(selected).head))
        assert(result.paramss(unchanged).head.eq(original.paramss(unchanged).head))
        assert(result.tpt.eq(original.tpt))
        assert(result.rhs.eq(original.rhs))
        assertEquals(method.parameterClauses.map(_.parameters.size), Vector(1, 1))
      }

      Vector(false, true).foreach { reverse =>
        val (root, captured, _, first, second) = fixture()
        val ordered = if reverse then Vector(second, first) else Vector(first, second)
        val plan = ordered.foldLeft(captured.emptyPlan) { (current, parameter) =>
          current.replaceParameterType(parameter.ref, intType).fold(problem => fail(problem.message), identity)
        }
        val result = rawMethod(applyPlan(captured, plan))
        val original = rawMethod(root)
        assertEquals(result.paramss.map(_.size), List(1, 1))
        assert(!result.paramss(0).head.eq(original.paramss(0).head))
        assert(!result.paramss(1).head.eq(original.paramss(1).head))
      }

      val (resultRoot, resultCapture, resultMethod, _, _) = fixture()
      val resultOnly = resultCapture.emptyPlan.replaceResultType(resultMethod.ref, intType)
        .fold(problem => fail(problem.message), identity)
      val resultEdited = rawMethod(applyPlan(resultCapture, resultOnly))
      assert(resultEdited.paramss(0).head.eq(rawMethod(resultRoot).paramss(0).head))
      assert(resultEdited.paramss(1).head.eq(rawMethod(resultRoot).paramss(1).head))
      assert(!resultEdited.tpt.eq(rawMethod(resultRoot).tpt))

      val bodyRoot = parseClass("class BodyOnly:\n  def combine(a: Int)(b: Int): Int = a\n  val untouched: Int = 7\n")
      val bodyCapture = ExistingClassUntypedRewrite.capture(bodyRoot).fold(problem => fail(problem.message), identity)
      val bodyMethod = bodyCapture.method(bodyCapture.members.head.ref).fold(problem => fail(problem.message), identity)
      val bodyFirst = bodyMethod.parameterClauses(0).parameters(0)
      val bodySecond = bodyMethod.parameterClauses(1).parameters(0)
      val body = addExact(bodyMethod, bodyFirst, bodySecond)
      val bodyPlan = bodyCapture.emptyPlan.replaceBody(bodyMethod.ref, body)
        .fold(problem => fail(problem.message), identity)
      val bodyEdited = rawMethod(applyPlan(bodyCapture, bodyPlan))
      assertEquals(bodyEdited.paramss.map(_.size), List(1, 1))
      assert(!bodyEdited.rhs.eq(rawMethod(bodyRoot).rhs))

      val (allRoot, allCapture, allMethod, allFirst, allSecond) = fixture()
      val allBody = addExact(allMethod, allFirst, allSecond)
      val allPlan = (for
        p1 <- allCapture.emptyPlan.replaceParameterType(allFirst.ref, intType)
        p2 <- p1.replaceParameterType(allSecond.ref, intType)
        p3 <- p2.replaceResultType(allMethod.ref, intType)
        p4 <- p3.replaceBody(allMethod.ref, allBody)
      yield p4).fold(problem => fail(problem.message), identity)
      val allResult = applyPlan(allCapture, allPlan)
      val allRaw = rawMethod(allResult)
      val originalRaw = rawMethod(allRoot)
      val originalTemplate = allRoot.rhs.asInstanceOf[untpd.Template]
      val rewrittenTemplate = allResult.rhs.asInstanceOf[untpd.Template]
      assert(!allResult.eq(allRoot))
      assert(!rewrittenTemplate.eq(originalTemplate))
      assert(!allRaw.eq(originalRaw))
      assertEquals(allRaw.paramss.map(_.size), List(1, 1))
      Vector(0, 1).foreach { clause =>
        val changed = allRaw.paramss(clause).head.asInstanceOf[untpd.ValDef]
        val old = originalRaw.paramss(clause).head.asInstanceOf[untpd.ValDef]
        assert(!changed.eq(old))
        assert(!changed.tpt.eq(old.tpt))
        assertEquals(changed.tpt.source, old.tpt.source)
        assertEquals(changed.tpt.span, old.tpt.span)
        assertEquals(changed.tpt.symbol, NoSymbol)
      }
      assert(!allRaw.tpt.eq(originalRaw.tpt))
      assertEquals(allRaw.tpt.source, originalRaw.tpt.source)
      assertEquals(allRaw.tpt.span, originalRaw.tpt.span)
      assertEquals(allRaw.tpt.symbol, NoSymbol)
      assert(!allRaw.rhs.eq(originalRaw.rhs))
      assertEquals(allRaw.rhs.source, originalRaw.rhs.source)
      assertEquals(allRaw.rhs.span, originalRaw.rhs.span)
      assertEquals(allRaw.rhs.symbol, NoSymbol)
      assertEquals(allRaw.source, originalRaw.source)
      assertEquals(allRaw.span, originalRaw.span)
      assert(allRaw.mods.eq(originalRaw.mods))
      assert(rewrittenTemplate.body(1).eq(originalTemplate.body(1)))
      val graph = ExistingUntpdClassMemberFilter.allTrees(allResult)
      assert(graph.forall(_.symbol == NoSymbol))
      assert(!graph.exists(_.isInstanceOf[untpd.TypedSplice]))

      val noOp = ExistingClassUntypedRewrite(allCapture, allCapture.emptyPlan)
        .fold(problem => fail(problem.message), identity)
      assert(!noOp.changed)
      assert(noOp.tree.eq(allRoot))

  test("duplicate names and adjacent unproved two-clause topologies remain fail closed"):
    withContext:
      val rejected = Vector(
        "class Duplicate:\n  def f(same: Int)(same: Int): Int = same\n",
        "class Contextual:\n  def f(a: Int)(using b: Int): Int = a\n",
        "class ByNameFirst:\n  def f(a: => Int)(b: Int): Int = b\n",
        "class ByNameSecond:\n  def f(a: Int)(b: => Int): Int = a\n",
        "class RepeatedFirst:\n  def f(a: Int*)(b: Int): Int = b\n",
        "class RepeatedSecond:\n  def f(a: Int)(b: Int*): Int = a\n",
        "class Generic:\n  def f[T](a: Int)(b: Int): Int = a\n",
        "class EmptyFirst:\n  def f()(b: Int): Int = b\n",
        "class EmptySecond:\n  def f(a: Int)(): Int = a\n",
        "class WideFirst:\n  def f(a: Int, x: Int)(b: Int): Int = a\n",
        "class WideSecond:\n  def f(a: Int)(b: Int, y: Int): Int = a\n",
        "class ThreeClauses:\n  def f(a: Int)(b: Int)(c: Int): Int = a\n"
      )
      rejected.foreach { source =>
        val root = parseClass(source)
        val captured = ExistingClassUntypedRewrite.capture(root).fold(problem => fail(problem.message), identity)
        assertCode(captured.method(captured.members.head.ref), "UNSUPPORTED_STRUCTURE")
      }
      val defaulted = ExistingClassUntypedRewrite.capture(
        parseClass("class Defaulted:\n  def f(a: Int = 1)(b: Int): Int = b\n")
      ).fold(problem => fail(problem.message), identity)
      assertCode(defaulted.method(defaulted.members.head.ref), "INTERNAL_INVARIANT_FAILED")


  test("two-clause refs and plans reject foreign, stale, duplicate, and conflicting use"):
    withContext:
      def public(source: String) =
        val captured = ExistingClassUntypedRewrite.capture(parseClass(source)).fold(problem => fail(problem.message), identity)
        val method = captured.method(captured.members.head.ref).fold(problem => fail(problem.message), identity)
        (captured, method)
      val source = "class Curried:\n  def f(a: Int)(b: Int): Int = a\n"
      val (left, method) = public(source)
      val (right, foreignMethod) = public(source)
      val originalRoot = left.exact.originalRoot
      val originalMethod = rawMethod(originalRoot)
      val first = method.parameterClauses(0).parameters(0)
      val foreignFirst = foreignMethod.parameterClauses(0).parameters(0)
      val foreignReference = foreignMethod.parameterScope.reference(foreignFirst.ref)
        .fold(problem => fail(problem.message), identity)
      assertCode(method.parameterScope.binder(foreignFirst.ref), "SELECTION_FAILED")
      assertCode(method.parameterScope.reference(foreignFirst.ref), "SELECTION_FAILED")
      assertCode(left.emptyPlan.replaceParameterType(foreignFirst.ref, TypeNormalForm.STypeIdent("Int")), "SELECTION_FAILED")
      assertCode(left.emptyPlan.replaceResultType(foreignMethod.ref, TypeNormalForm.STypeIdent("Int")), "SELECTION_FAILED")
      assertCode(left.emptyPlan.replaceBody(foreignMethod.ref, TermShape.Literal("1")), "SELECTION_FAILED")
      assertCode(left.emptyPlan.replaceBody(method.ref, foreignReference), "SEMANTIC_FRAGMENT_INVALID")
      assertCode(left.emptyPlan.replaceBody(method.ref, null), "SEMANTIC_FRAGMENT_INVALID")
      val parameterPlan = left.emptyPlan.replaceParameterType(first.ref, TypeNormalForm.STypeIdent("Int"))
        .fold(problem => fail(problem.message), identity)
      assertCode(parameterPlan.replaceParameterType(first.ref, TypeNormalForm.STypeIdent("Int")), "EDIT_CONFLICT")
      val omitted = left.emptyPlan.omit(left.members.head.ref).fold(problem => fail(problem.message), identity)
      assertCode(omitted.replaceResultType(method.ref, TypeNormalForm.STypeIdent("Int")), "EDIT_CONFLICT")
      assertCode(ExistingClassUntypedRewrite(left, right.emptyPlan), "SELECTION_FAILED")
      val invalidType = left.emptyPlan.replaceParameterType(first.ref, TypeNormalForm.STypeResolved(null))
        .fold(problem => fail(problem.message), identity)
      assertCode(ExistingClassUntypedRewrite(left, invalidType), "UNSUPPORTED_STRUCTURE")
      assert(left.exact.originalRoot.eq(originalRoot))
      assert(rawMethod(left.exact.originalRoot).eq(originalMethod))

  private def addExact(method: MethodView, first: ParameterView, second: ParameterView): TermShape =
    val firstRef = method.parameterScope.reference(first.ref).fold(problem => fail(problem.message), identity)
    val secondRef = method.parameterScope.reference(second.ref).fold(problem => fail(problem.message), identity)
    TermShape.Apply(TermShape.Select(TermShape.Identifier("Math", false), "addExact"), List(firstRef, secondRef))

  private def applyPlan(captured: Capture, plan: EditPlan)(using Context): untpd.TypeDef =
    ExistingClassUntypedRewrite(captured, plan).fold(problem => fail(problem.message), _.tree)

  private def rawMethod(root: untpd.TypeDef)(using Context): untpd.DefDef =
    root.rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]

  private def captureAround(member: untpd.Tree)(using Context): ExistingUntpdClassMemberFilter.Capture =
    val parsed = parseClass("class CaptureShell\n")
    val oldTemplate = parsed.rhs.asInstanceOf[untpd.Template]
    val template = untpd.cpy.Template(oldTemplate)(
      oldTemplate.constr,
      oldTemplate.parentsOrDerived,
      oldTemplate.derived,
      oldTemplate.self,
      List(member)
    )
    val root = untpd.cpy.TypeDef(parsed)(parsed.name, template)
    ExistingUntpdClassMemberFilter.capture(root).fold(problem => fail(problem.message), identity)

  private def assertCode[A](result: Either[Failure, A], expected: String): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected, clues(problem))
    case Right(value) => fail(s"expected $expected, found success $value")

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U066ExistingTwoUnary.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef => packageDef.stats.head.asInstanceOf[untpd.TypeDef]
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)
