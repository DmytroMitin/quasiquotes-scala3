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

final class ExistingZeroParameterMethodAdmissionAndRewriteTest extends munit.FunSuite:
  test("private descriptor and public view admit Nil and List(Nil) without binders"):
    withContext:
      val fixtures = Vector(
        "class Parameterless:\n  def value: Int = 41\n" -> Vector.empty[Int],
        "class EmptyParen:\n  def value(): Int = 41\n" -> Vector(0)
      )

      fixtures.foreach { (source, expectedClauseSizes) =>
        val root = parseClass(source)
        val exact = ExistingUntpdClassMemberFilter.capture(root).fold(problem => fail(problem.message), identity)
        val descriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(exact, 0)
          .fold(problem => fail(problem.message), identity)
        assertEquals(descriptor.parameterClauses.map(_.size), expectedClauseSizes)
        assert(descriptor.method.eq(exact.members.head.tree))
        assert(descriptor.resultType.eq(descriptor.method.tpt))
        assert(descriptor.rhs.eq(descriptor.method.rhs))
        assertEquals(ExistingUntpdOrdinaryMethodDescriptor.validate(descriptor), Right(()))

        val captured = ExistingClassUntypedRewrite.capture(root).fold(problem => fail(problem.message), identity)
        val method = captured.method(captured.members.head.ref).fold(problem => fail(problem.message), identity)
        assertEquals(method.parameterClauses.map(_.parameters.size), expectedClauseSizes)
        assertEquals(method.parameterClauses.map(_.kind.code), expectedClauseSizes.map(_ => "ORDINARY"))
        assertEquals(method.parameterClauses.flatMap(_.parameters), Vector.empty)
        assertEquals(method.resultType.semantic, Right(TypeNormalForm.STypeIdent("Int")))
        assertEquals(method.body.semantic, Right(TermShape.Literal("41")))
      }

  test("zero-parameter public scopes reject foreign and forged parameters deterministically"):
    withContext:
      val unary = publicMethod("class Unary:\n  def value(x: Int): Int = x\n")
      val foreign = unary.parameterClauses.head.parameters.head.ref
      Vector(
        publicMethod("class Parameterless:\n  def value: Int = 41\n"),
        publicMethod("class EmptyParen:\n  def value(): Int = 41\n")
      ).foreach { zero =>
        assertCode(zero.parameterScope.binder(foreign), "SELECTION_FAILED")
        assertCode(zero.parameterScope.reference(foreign), "SELECTION_FAILED")

        val forged = new ParameterRef(zero.ref, 0, 0, null, null)
        assertCode(zero.parameterScope.binder(forged), "SELECTION_FAILED")
        assertCode(zero.parameterScope.reference(forged), "SELECTION_FAILED")
      }
  test("public result and body edits preserve the exact zero-parameter call convention"):
    withContext:
      val fixtures = Vector(
        ("class Parameterless:\n  def value: AnyVal = 0\n  val untouched: Int = 7\n", List.empty[Int], "20"),
        ("class EmptyParen:\n  def value(): AnyVal = 0\n  val untouched: Int = 9\n", List(0), "22")
      )

      fixtures.foreach { (source, expectedParamss, replacement) =>
        val root = parseClass(source)
        val originalTemplate = root.rhs.asInstanceOf[untpd.Template]
        val originalMethod = originalTemplate.body.head.asInstanceOf[untpd.DefDef]
        val originalUntouched = originalTemplate.body(1)
        val captured = ExistingClassUntypedRewrite.capture(root).fold(problem => fail(problem.message), identity)
        val method = captured.method(captured.members.head.ref).fold(problem => fail(problem.message), identity)
        val plan = (for
          result <- captured.emptyPlan.replaceResultType(method.ref, TypeNormalForm.STypeIdent("Int"))
          body <- result.replaceBody(method.ref, TermShape.Literal(replacement))
        yield body).fold(problem => fail(problem.message), identity)
        val applied = ExistingClassUntypedRewrite(captured, plan).fold(problem => fail(problem.message), identity)
        val rewrittenTemplate = applied.tree.rhs.asInstanceOf[untpd.Template]
        val rewrittenMethod = rewrittenTemplate.body.head.asInstanceOf[untpd.DefDef]

        assert(applied.changed)
        assert(!applied.tree.eq(root))
        assert(!rewrittenMethod.eq(originalMethod))
        assertEquals(rewrittenMethod.paramss.map(_.size), expectedParamss)
        assert(rewrittenMethod.mods.eq(originalMethod.mods))
        assertEquals(rewrittenMethod.source, originalMethod.source)
        assertEquals(rewrittenMethod.span, originalMethod.span)
        assert(!rewrittenMethod.tpt.eq(originalMethod.tpt))
        assertEquals(rewrittenMethod.tpt.source, originalMethod.tpt.source)
        assertEquals(rewrittenMethod.tpt.span, originalMethod.tpt.span)
        assert(!rewrittenMethod.rhs.eq(originalMethod.rhs))
        assertEquals(rewrittenMethod.rhs.source, originalMethod.rhs.source)
        assertEquals(rewrittenMethod.rhs.span, originalMethod.rhs.span)
        assert(rewrittenTemplate.body(1).eq(originalUntouched))
        val graph = ExistingUntpdClassMemberFilter.allTrees(applied.tree)
        assert(graph.forall(_.symbol == NoSymbol))
        assert(!graph.exists(_.isInstanceOf[untpd.TypedSplice]))
      }

  test("zero-parameter plans retain stable foreign stale duplicate and semantic failures"):
    withContext:
      val firstRoot = parseClass("class First:\n  def value: AnyVal = 0\n")
      val first = ExistingClassUntypedRewrite.capture(firstRoot).fold(problem => fail(problem.message), identity)
      val method = first.method(first.members.head.ref).fold(problem => fail(problem.message), identity)
      val stale = ExistingClassUntypedRewrite.capture(parseClass("class First:\n  def value: AnyVal = 0\n"))
        .fold(problem => fail(problem.message), identity)
      val staleMethod = stale.method(stale.members.head.ref).fold(problem => fail(problem.message), identity)
      val unary = publicMethod("class Unary:\n  def value(x: Int): Int = x\n")
      val foreignParameter = unary.parameterClauses.head.parameters.head
      val foreignReference = unary.parameterScope.reference(foreignParameter.ref)
        .fold(problem => fail(problem.message), identity)

      assertCode(first.method(stale.members.head.ref), "SELECTION_FAILED")
      assertCode(first.emptyPlan.omit(stale.members.head.ref), "SELECTION_FAILED")
      assertCode(first.emptyPlan.replaceResultType(staleMethod.ref, TypeNormalForm.STypeIdent("Int")), "SELECTION_FAILED")
      assertCode(first.emptyPlan.replaceBody(staleMethod.ref, TermShape.Literal("1")), "SELECTION_FAILED")
      assertCode(first.emptyPlan.replaceBody(method.ref, foreignReference), "SEMANTIC_FRAGMENT_INVALID")
      val resultPlan = first.emptyPlan.replaceResultType(method.ref, TypeNormalForm.STypeIdent("Int"))
        .fold(problem => fail(problem.message), identity)
      val bodyPlan = first.emptyPlan.replaceBody(method.ref, TermShape.Literal("1"))
        .fold(problem => fail(problem.message), identity)
      assertCode(resultPlan.replaceResultType(method.ref, TypeNormalForm.STypeIdent("Int")), "EDIT_CONFLICT")
      assertCode(bodyPlan.replaceBody(method.ref, TermShape.Literal("2")), "EDIT_CONFLICT")
      val unsupported = first.emptyPlan.replaceResultType(method.ref, TypeNormalForm.STypeResolved(null))
        .fold(problem => fail(problem.message), identity)
      assertCode(ExistingClassUntypedRewrite(first, unsupported), "UNSUPPORTED_STRUCTURE")

  test("adjacent unsupported method topologies remain fail closed"):
    withContext:
      val unsupported = Vector(
        "class Generic:\n  def value[T]: Int = 0\n" -> "INTERNAL_INVARIANT_FAILED",
        "class Curried:\n  def value(x: Int)(y: Int): Int = x + y\n" -> "UNSUPPORTED_STRUCTURE",
        "class Contextual:\n  def value(using x: Int): Int = x\n" -> "UNSUPPORTED_STRUCTURE",
        "class Defaulted:\n  def value(x: Int = 1): Int = x\n" -> "INTERNAL_INVARIANT_FAILED",
        "class Three:\n  def value(a: Int, b: Int, c: Int): Int = a\n" -> "UNSUPPORTED_STRUCTURE"
      )
      unsupported.foreach { (source, expectedCode) =>
        val captured = ExistingClassUntypedRewrite.capture(parseClass(source))
          .fold(problem => fail(problem.message), identity)
        assertCode(captured.method(captured.members.head.ref), expectedCode)
      }

  private def publicMethod(source: String)(using Context): MethodView =
    val captured = ExistingClassUntypedRewrite.capture(parseClass(source))
      .fold(problem => fail(problem.message), identity)
    captured.method(captured.members.head.ref).fold(problem => fail(problem.message), identity)

  private def assertCode[A](result: Either[Failure, A], expected: String): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected, clues(problem))
    case Right(value) => fail(s"expected $expected, found success $value")

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U065ExistingZeroParameter.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef => packageDef.stats.head.asInstanceOf[untpd.TypeDef]
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)
