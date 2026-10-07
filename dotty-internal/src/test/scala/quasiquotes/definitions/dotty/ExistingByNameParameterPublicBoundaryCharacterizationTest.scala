package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.Failure
import quasiquotes.parser.TermShape
import quasiquotes.terms.{TermBindingCategory, TermBindingInternals, TermShapeBindingView}
import quasiquotes.types.TypeNormalForm

class ExistingByNameParameterPublicBoundaryCharacterizationTest extends munit.FunSuite:
  private val fixtures = Vector(
    ("class ByName:\n  def eval(x: => Int): Int = x\n", "eval", "x"),
    ("class RenamedByName:\n  def forceLater(thunk: => Int): Int = thunk\n", "forceLater", "thunk")
  )

  test("BN1 raw parser topology retains an exact ByNameTypeTree wrapper") {
    withContext {
      fixtures.foreach { case (source, methodName, parameterName) =>
        val root = parseClass(source)
        val method = firstMethod(root)
        val parameter = method.paramss.head.head.asInstanceOf[untpd.ValDef]
        assertEquals(method.name.toString, methodName)
        assertEquals(method.mods.flags, Flags.Method)
        assertEquals(method.paramss.map(_.size), List(1))
        assertEquals(parameter.name.toString, parameterName)
        assertEquals(parameter.mods.flags, Flags.Param)
        assert(parameter.rhs.isEmpty)
        parameter.tpt match
          case byName: untpd.ByNameTypeTree =>
            byName.result match
              case untpd.Ident(name) => assertEquals(name.toString, "Int")
              case other => fail("expected inner Int Ident, found " + other)
            assert(parameter.tpt.eq(byName))
            assert(byName.source.eq(method.source))
            assert(byName.result.source.eq(method.source))
            assert(byName.span.exists)
            assert(byName.result.span.exists)
          case other => fail("expected ByNameTypeTree, found " + other)
        method.tpt match
          case untpd.Ident(name) => assertEquals(name.toString, "Int")
          case other => fail("expected result Int Ident, found " + other)
        method.rhs match
          case untpd.Ident(name) => assertEquals(name.toString, parameterName)
          case other => fail("expected parameter RHS reference, found " + other)
        Vector[untpd.Tree](method, parameter, parameter.tpt, method.tpt, method.rhs)
          .foreach { tree =>
            assert(tree.source.exists)
            assert(tree.span.exists)
          }
        val trees = ExistingUntpdClassMemberFilter.allTrees(root)
        assert(trees.forall(_.symbol == NoSymbol))
        assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))
      }

      val strict = firstMethod(parseClass("class Strict:\n  def eval(x: Int): Int = x\n"))
      val strictParameter = strict.paramss.head.head.asInstanceOf[untpd.ValDef]
      assert(strictParameter.tpt.isInstanceOf[untpd.Ident])
      assert(!strictParameter.tpt.isInstanceOf[untpd.ByNameTypeTree])
    }
  }

  test("single view descriptor and public method capture already admit BN1") {
    withContext {
      fixtures.foreach { case (source, methodName, parameterName) =>
        val root = parseClass(source)
        val exact = right(ExistingUntpdClassMemberFilter.capture(root))
        val single = ExistingUntpdSingleParameterMethodView
          .capture(exact, 0).fold(problem => fail(problem.message), identity)
        val descriptor = ExistingUntpdOrdinaryMethodDescriptor
          .capture(exact, 0).fold(problem => fail(problem.message), identity)
        assertEquals(single.methodName, methodName)
        assertEquals(single.parameterName, parameterName)
        assert(single.parameterType.isInstanceOf[untpd.ByNameTypeTree])
        assert(descriptor.method.eq(single.method))
        assert(descriptor.parameterClauses.head.head.tree.eq(single.parameter))
        assert(descriptor.parameterClauses.head.head.tpt.eq(single.parameterType))

        val publicCapture = publicRight(ExistingClassUntypedRewrite.capture(root))
        val method = publicRight(publicCapture.method(publicCapture.members.head.ref))
        assertEquals(method.diagnosticName, methodName)
        assertEquals(method.parameterClauses.size, 1)
        assertEquals(method.parameterClauses.head.kind,
          ExistingClassUntypedRewrite.ParameterClauseKind.Ordinary)
        val parameter = method.parameterClauses.head.parameters.head
        assertEquals(parameter.diagnosticName, parameterName)
        assertEquals(parameter.clauseIndex, 0)
        assertEquals(parameter.parameterIndex, 0)
        assert(parameter.identity.sameObjectAs(
          new ExistingClassUntypedRewrite.ExactIdentity(single.parameter)
        ))
        assert(parameter.declaredType.identity.sameObjectAs(
          new ExistingClassUntypedRewrite.ExactIdentity(single.parameterType)
        ))
      }
    }
  }

  test("public Type slots keep exact by-name identity while semantic projection fails truthfully") {
    withContext {
      val byNameRoot = parseClass(fixtures.head._1)
      val byNameCapture = publicRight(ExistingClassUntypedRewrite.capture(byNameRoot))
      val byNameMethod = publicRight(byNameCapture.method(byNameCapture.members.head.ref))
      val byNameParameter = byNameMethod.parameterClauses.head.parameters.head
      assertPublicCode(byNameParameter.declaredType.semantic, "UNSUPPORTED_STRUCTURE")
      val parameterFailure = byNameParameter.declaredType.semantic.left.toOption.get
      assert(parameterFailure.detail.contains("U038"))
      assert(parameterFailure.detail.contains("UNSUPPORTED_TYPE_TOPOLOGY"))
      assert(parameterFailure.detail.contains("ByNameTypeTree"))
      assertEquals(byNameMethod.resultType.semantic,
        Right(TypeNormalForm.STypeIdent("Int")))

      val strictRoot = parseClass("class StrictProjection:\n  def eval(x: Int): Int = x\n")
      val strictCapture = publicRight(ExistingClassUntypedRewrite.capture(strictRoot))
      val strictMethod = publicRight(strictCapture.method(strictCapture.members.head.ref))
      assertEquals(
        strictMethod.parameterClauses.head.parameters.head.declaredType.semantic,
        Right(TypeNormalForm.STypeIdent("Int"))
      )
    }
  }

  test("RHS projection and the public term scope remain binder-safe for a by-name parameter") {
    withContext {
      val root = parseClass(fixtures.head._1)
      val captured = publicRight(ExistingClassUntypedRewrite.capture(root))
      val method = publicRight(captured.method(captured.members.head.ref))
      val parameter = method.parameterClauses.head.parameters.head
      val body = publicRight(method.body.semantic)
      val view = TermShapeBindingView.inspect(body).fold(problem => fail(problem.message), identity)
      assertEquals(view.category, TermBindingCategory.BoundReference)
      val projectedBinder = view.boundReference.getOrElse(fail("missing bound reference")).binder
      assertEquals(publicRight(method.parameterScope.binder(parameter.ref)), projectedBinder)
      assertEquals(publicRight(method.parameterScope.reference(parameter.ref)), body)

      val freeRoot = parseClass("class FreeByName:\n  def eval(x: => Int): Int = helper\n")
      val freeCapture = publicRight(ExistingClassUntypedRewrite.capture(freeRoot))
      val freeMethod = publicRight(freeCapture.method(freeCapture.members.head.ref))
      assertEquals(freeMethod.body.semantic, Right(TermShape.Identifier("helper", false)))

      val selectedRoot = parseClass(
        "class SelectedByName:\n  def eval(x: => Int): Int = obj.helper\n"
      )
      val selectedCapture = publicRight(ExistingClassUntypedRewrite.capture(selectedRoot))
      val selectedMethod = publicRight(selectedCapture.method(selectedCapture.members.head.ref))
      assertEquals(
        selectedMethod.body.semantic,
        Right(TermShape.Select(TermShape.Identifier("obj", false), "helper"))
      )

      val foreign = TermBindingInternals
        .persistentParameters(Vector(Vector("x")))
        .fold(problem => fail(problem.message), identity)
        .referenceAt(0, 0)
        .fold(problem => fail(problem.message), identity)
      assertPublicCode(
        captured.emptyPlan.replaceBody(method.ref, foreign),
        "SEMANTIC_FRAGMENT_INVALID"
      )
    }
  }

  test("the tiny two-parameter control is admitted with the same opaque by-name Type slot") {
    withContext {
      val root = parseClass(
        "class TwoParameterByName:\n  def combine(x: => Int, y: Int): Int = x + y\n"
      )
      val exact = right(ExistingUntpdClassMemberFilter.capture(root))
      val two = ExistingUntpdTwoParameterMethodView
        .capture(exact, 0).fold(problem => fail(problem.message), identity)
      assert(two.firstParameterType.isInstanceOf[untpd.ByNameTypeTree])
      assert(two.secondParameterType.isInstanceOf[untpd.Ident])
      assertEquals(two.firstParameter.mods.flags, Flags.Param)
      assertEquals(two.secondParameter.mods.flags, Flags.Param)
      val descriptor = ExistingUntpdOrdinaryMethodDescriptor
        .capture(exact, 0).fold(problem => fail(problem.message), identity)
      assertEquals(descriptor.parameterClauses.map(_.size), Vector(2))

      val captured = publicRight(ExistingClassUntypedRewrite.capture(root))
      val method = publicRight(captured.method(captured.members.head.ref))
      val parameters = method.parameterClauses.head.parameters
      assertEquals(parameters.size, 2)
      assertEquals(method.parameterClauses.head.kind,
        ExistingClassUntypedRewrite.ParameterClauseKind.Ordinary)
      assertPublicCode(parameters.head.declaredType.semantic, "UNSUPPORTED_STRUCTURE")
      assertEquals(parameters(1).declaredType.semantic,
        Right(TypeNormalForm.STypeIdent("Int")))
    }
  }

  private def firstMethod(root: untpd.TypeDef)(using Context): untpd.DefDef =
    root.rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U056ExistingByName.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def right[A](value: Either[ExistingUntpdClassMemberFilterError, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def publicRight[A](value: Either[Failure, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def assertPublicCode[A](value: Either[Failure, A], expected: String): Unit =
    value match
      case Left(problem) => assertEquals(problem.code, expected)
      case Right(success) => fail(s"expected $expected, found success $success")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
