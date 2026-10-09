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

class ExistingRepeatedParameterPublicBoundaryCharacterizationTest extends munit.FunSuite:
  private val fixtures = Vector(
    ("class Repeated:\n  def sum(xs: Int*): Int = xs.sum\n", "sum", "xs"),
    ("class RenamedRepeated:\n  def total(values: Int*): Int = values.sum\n", "total", "values")
  )

  test("RP1 raw parser topology retains exact PostfixOp repeated syntax") {
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

        val (elementType, repeatedMarker) = repeatedType(parameter)
        elementType match
          case untpd.Ident(name) => assertEquals(name.toString, "Int")
          case other => fail("expected repeated element Int Ident, found " + other)
        repeatedMarker match
          case untpd.Ident(name) => assertEquals(name.toString, "*")
          case other => fail("expected repeated marker Ident(*), found " + other)

        method.tpt match
          case untpd.Ident(name) => assertEquals(name.toString, "Int")
          case other => fail("expected result Int Ident, found " + other)
        method.rhs match
          case untpd.Select(untpd.Ident(name), selected) =>
            assertEquals(name.toString, parameterName)
            assertEquals(selected.toString, "sum")
          case other => fail("expected repeated-parameter sum selection, found " + other)

        Vector[untpd.Tree](
          method,
          parameter,
          parameter.tpt,
          elementType,
          repeatedMarker,
          method.tpt,
          method.rhs
        ).foreach { tree =>
          assert(tree.source.eq(method.source))
          assert(tree.span.exists)
        }
        val trees = ExistingUntpdClassMemberFilter.allTrees(root)
        assert(trees.forall(_.symbol == NoSymbol))
        assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))
      }

      val strict = firstMethod(parseClass(
        "class StrictSeq:\n  def sum(xs: Seq[Int]): Int = xs.sum\n"
      ))
      val strictParameter = strict.paramss.head.head.asInstanceOf[untpd.ValDef]
      assert(strictParameter.tpt.isInstanceOf[untpd.AppliedTypeTree])
      assert(!strictParameter.tpt.isInstanceOf[untpd.PostfixOp])
    }
  }

  test("single view descriptor and public method capture already admit RP1") {
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
        assert(single.parameterType.isInstanceOf[untpd.PostfixOp])
        assert(descriptor.method.eq(single.method))
        assert(descriptor.parameterClauses.head.head.tree.eq(single.parameter))
        assert(descriptor.parameterClauses.head.head.tpt.eq(single.parameterType))

        val publicCapture = publicRight(ExistingClassUntypedRewrite.capture(root))
        val method = publicRight(publicCapture.method(publicCapture.members.head.ref))
        assertEquals(method.diagnosticName, methodName)
        assertEquals(method.parameterClauses.size, 1)
        assertEquals(
          method.parameterClauses.head.kind,
          ExistingClassUntypedRewrite.ParameterClauseKind.Ordinary
        )
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

  test("public Type slots distinguish repeated PostfixOp from strict Seq and never normalize either") {
    withContext {
      val repeatedRoot = parseClass(fixtures.head._1)
      val repeatedCapture = publicRight(ExistingClassUntypedRewrite.capture(repeatedRoot))
      val repeatedMethod = publicRight(
        repeatedCapture.method(repeatedCapture.members.head.ref)
      )
      val repeatedParameter = repeatedMethod.parameterClauses.head.parameters.head
      assertPublicCode(repeatedParameter.declaredType.semantic, "UNSUPPORTED_STRUCTURE")
      val repeatedFailure = repeatedParameter.declaredType.semantic.left.toOption.get
      assert(repeatedFailure.detail.contains("U038"))
      assert(repeatedFailure.detail.contains("UNSUPPORTED_TYPE_TOPOLOGY"))
      assert(repeatedFailure.detail.contains("PostfixOp"))
      assertEquals(
        repeatedMethod.resultType.semantic,
        Right(TypeNormalForm.STypeIdent("Int"))
      )

      val strictRoot = parseClass(
        "class StrictSeqProjection:\n  def sum(xs: Seq[Int]): Int = xs.sum\n"
      )
      val strictCapture = publicRight(ExistingClassUntypedRewrite.capture(strictRoot))
      val strictMethod = publicRight(strictCapture.method(strictCapture.members.head.ref))
      val strictParameter = strictMethod.parameterClauses.head.parameters.head
      assertPublicCode(strictParameter.declaredType.semantic, "UNSUPPORTED_STRUCTURE")
      val strictFailure = strictParameter.declaredType.semantic.left.toOption.get
      assert(strictFailure.detail.contains("UNSUPPORTED_APPLIED_CONSTRUCTOR"))
      assert(!strictFailure.detail.contains("PostfixOp"))
      assertEquals(strictMethod.resultType.semantic, Right(TypeNormalForm.STypeIdent("Int")))
    }
  }

  test("RHS projection treats the repeated parameter as the existing normal bound reference") {
    withContext {
      val root = parseClass(fixtures.head._1)
      val captured = publicRight(ExistingClassUntypedRewrite.capture(root))
      val method = publicRight(captured.method(captured.members.head.ref))
      val parameter = method.parameterClauses.head.parameters.head
      val body = publicRight(method.body.semantic)
      body match
        case TermShape.Select(qualifier, "sum") =>
          val view = TermShapeBindingView.inspect(qualifier)
            .fold(problem => fail(problem.message), identity)
          assertEquals(view.category, TermBindingCategory.BoundReference)
          val projectedBinder =
            view.boundReference.getOrElse(fail("missing bound reference")).binder
          assertEquals(
            publicRight(method.parameterScope.binder(parameter.ref)),
            projectedBinder
          )
          assertEquals(
            publicRight(method.parameterScope.reference(parameter.ref)),
            qualifier
          )
        case other => fail("expected a bound-reference .sum selection, found " + other)

      val freeRoot = parseClass(
        "class FreeRepeated:\n  def sum(xs: Int*): Int = helper\n"
      )
      val freeCapture = publicRight(ExistingClassUntypedRewrite.capture(freeRoot))
      val freeMethod = publicRight(freeCapture.method(freeCapture.members.head.ref))
      assertEquals(freeMethod.body.semantic, Right(TermShape.Identifier("helper", false)))

      val foreign = TermBindingInternals
        .persistentParameters(Vector(Vector("xs")))
        .fold(problem => fail(problem.message), identity)
        .referenceAt(0, 0)
        .fold(problem => fail(problem.message), identity)
      assertPublicCode(
        captured.emptyPlan.replaceBody(method.ref, foreign),
        "SEMANTIC_FRAGMENT_INVALID"
      )
    }
  }

  test("the tiny two-parameter repeated-last control shares the same public boundary") {
    withContext {
      val root = parseClass(
        "class TwoParameterRepeated:\n  def combine(prefix: Int, xs: Int*): Int = prefix + xs.sum\n"
      )
      val exact = right(ExistingUntpdClassMemberFilter.capture(root))
      val two = ExistingUntpdTwoParameterMethodView
        .capture(exact, 0).fold(problem => fail(problem.message), identity)
      assert(two.firstParameterType.isInstanceOf[untpd.Ident])
      assert(two.secondParameterType.isInstanceOf[untpd.PostfixOp])
      assertEquals(two.firstParameter.mods.flags, Flags.Param)
      assertEquals(two.secondParameter.mods.flags, Flags.Param)
      val descriptor = ExistingUntpdOrdinaryMethodDescriptor
        .capture(exact, 0).fold(problem => fail(problem.message), identity)
      assertEquals(descriptor.parameterClauses.map(_.size), Vector(2))

      val captured = publicRight(ExistingClassUntypedRewrite.capture(root))
      val method = publicRight(captured.method(captured.members.head.ref))
      val parameters = method.parameterClauses.head.parameters
      assertEquals(parameters.size, 2)
      assertEquals(
        method.parameterClauses.head.kind,
        ExistingClassUntypedRewrite.ParameterClauseKind.Ordinary
      )
      assertEquals(parameters.head.declaredType.semantic,
        Right(TypeNormalForm.STypeIdent("Int")))
      assertPublicCode(parameters(1).declaredType.semantic, "UNSUPPORTED_STRUCTURE")
      val firstBinder = publicRight(method.parameterScope.binder(parameters.head.ref))
      val repeatedBinder = publicRight(method.parameterScope.binder(parameters(1).ref))
      assertNotEquals(firstBinder, repeatedBinder)
    }
  }

  private def repeatedType(parameter: untpd.ValDef): (untpd.Tree, untpd.Tree) =
    parameter.tpt match
      case untpd.PostfixOp(elementType, repeatedMarker) =>
        (elementType, repeatedMarker)
      case other => fail("expected PostfixOp repeated Type, found " + other)

  private def firstMethod(root: untpd.TypeDef)(using Context): untpd.DefDef =
    root.rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U057ExistingRepeated.scala", source)
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
      case Right(success) => fail("expected " + expected + ", found success " + success)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
