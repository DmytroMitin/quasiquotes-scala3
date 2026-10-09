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
import quasiquotes.terms.{TermBinder, TermBindingCategory, TermBindingInternals, TermShapeBindingView}
import quasiquotes.types.TypeNormalForm

class ExistingMixedCallingModeParameterTypeEditsCharacterizationTest
    extends munit.FunSuite:
  private val fixtures = Vector(
    (
      "class MixedModes:\n  def compute(lazyValue: => AnyVal, values: AnyVal*): Int = values.size\n  val untouched: Int = 9\n",
      "compute",
      "lazyValue",
      "values"
    ),
    (
      "class RenamedMixedModes:\n  def countLater(thunk: => AnyVal, items: AnyVal*): Int = items.size\n  val untouched: Int = 9\n",
      "countLater",
      "thunk",
      "items"
    )
  )

  test("mixed parser topology retains independent by-name and repeated-last wrappers") {
    withContext {
      fixtures.foreach { case (source, methodName, byNameName, repeatedName) =>
        val root = parseClass(source)
        val method = firstMethod(root)
        val parameters = method.paramss.head.map(_.asInstanceOf[untpd.ValDef])
        assertEquals(method.name.toString, methodName)
        assertEquals(method.mods.flags, Flags.Method)
        assertEquals(method.paramss.map(_.size), List(2))
        assertEquals(parameters.map(_.name.toString), List(byNameName, repeatedName))
        assert(parameters.forall(_.mods.flags == Flags.Param))
        assert(parameters.forall(_.rhs.isEmpty))

        val byName = byNameType(parameters.head)
        byName.result match
          case untpd.Ident(name) => assertEquals(name.toString, "AnyVal")
          case other => fail("expected by-name inner AnyVal Ident, found " + other)

        val repeated = repeatedType(parameters(1))
        val (element, marker) = repeatedParts(repeated)
        element match
          case untpd.Ident(name) => assertEquals(name.toString, "AnyVal")
          case other => fail("expected repeated element AnyVal Ident, found " + other)
        marker match
          case untpd.Ident(name) => assertEquals(name.toString, "*")
          case other => fail("expected repeated star Ident, found " + other)

        method.tpt match
          case untpd.Ident(name) => assertEquals(name.toString, "Int")
          case other => fail("expected result Int Ident, found " + other)
        method.rhs match
          case untpd.Select(untpd.Ident(name), selected) =>
            assertEquals(name.toString, repeatedName)
            assertEquals(selected.toString, "size")
          case other => fail("expected repeated-parameter size selection, found " + other)

        Vector[untpd.Tree](
          method,
          parameters.head,
          byName,
          byName.result,
          parameters(1),
          repeated,
          element,
          marker,
          method.tpt,
          method.rhs
        ).foreach { tree =>
          assert(tree.source.eq(method.source))
          assert(tree.span.exists)
        }
        assertNotEquals(byName.span, repeated.span)
        val trees = ExistingUntpdClassMemberFilter.allTrees(root)
        assert(trees.forall(_.symbol == NoSymbol))
        assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))
      }
    }
  }

  test("descriptor and public capture admit one ordinary mixed two-parameter clause") {
    withContext {
      fixtures.foreach { case (source, methodName, byNameName, repeatedName) =>
        val root = parseClass(source)
        val rawMethod = firstMethod(root)
        val rawParameters = rawMethod.paramss.head.map(_.asInstanceOf[untpd.ValDef])
        val exact = privateRight(ExistingUntpdClassMemberFilter.capture(root))
        val two = ExistingUntpdTwoParameterMethodView
          .capture(exact, 0).fold(problem => fail(problem.message), identity)
        val descriptor = ExistingUntpdOrdinaryMethodDescriptor
          .capture(exact, 0).fold(problem => fail(problem.message), identity)
        assert(two.firstParameterType.isInstanceOf[untpd.ByNameTypeTree])
        assert(two.secondParameterType.isInstanceOf[untpd.PostfixOp])
        assertEquals(descriptor.parameterClauses.map(_.size), Vector(2))

        val captured = publicRight(ExistingClassUntypedRewrite.capture(root))
        val method = publicRight(captured.method(captured.members.head.ref))
        assertEquals(method.diagnosticName, methodName)
        assertEquals(method.parameterClauses.size, 1)
        assertEquals(
          method.parameterClauses.head.kind,
          ExistingClassUntypedRewrite.ParameterClauseKind.Ordinary
        )
        val parameters = method.parameterClauses.head.parameters
        assertEquals(parameters.map(_.diagnosticName), Vector(byNameName, repeatedName))
        assertEquals(parameters.map(_.clauseIndex), Vector(0, 0))
        assertEquals(parameters.map(_.parameterIndex), Vector(0, 1))
        assert(parameters.head.identity.sameObjectAs(
          new ExistingClassUntypedRewrite.ExactIdentity(rawParameters.head)
        ))
        assert(parameters(1).identity.sameObjectAs(
          new ExistingClassUntypedRewrite.ExactIdentity(rawParameters(1))
        ))
        assert(parameters.head.declaredType.identity.sameObjectAs(
          new ExistingClassUntypedRewrite.ExactIdentity(rawParameters.head.tpt)
        ))
        assert(parameters(1).declaredType.identity.sameObjectAs(
          new ExistingClassUntypedRewrite.ExactIdentity(rawParameters(1).tpt)
        ))
        val firstBinder = publicRight(method.parameterScope.binder(parameters.head.ref))
        val secondBinder = publicRight(method.parameterScope.binder(parameters(1).ref))
        assertNotEquals(firstBinder, secondBinder)
      }
    }
  }

  test("public Type opacity and body binding remain truthful for both mixed parameters") {
    withContext {
      val root = parseClass(
        "class MixedBinding:\n  def compute(lazyValue: => AnyVal, values: AnyVal*): Int = lazyValue.hashCode + values.size\n"
      )
      val captured = publicRight(ExistingClassUntypedRewrite.capture(root))
      val method = publicRight(captured.method(captured.members.head.ref))
      val parameters = method.parameterClauses.head.parameters
      val firstFailure = left(method.parametersOrFailure(parameters.head))
      val secondFailure = left(method.parametersOrFailure(parameters(1)))
      assert(firstFailure.detail.contains("ByNameTypeTree"))
      assert(secondFailure.detail.contains("PostfixOp"))
      assertEquals(method.resultType.semantic, Right(TypeNormalForm.STypeIdent("Int")))

      val firstBinder = publicRight(method.parameterScope.binder(parameters.head.ref))
      val secondBinder = publicRight(method.parameterScope.binder(parameters(1).ref))
      val body = publicRight(method.body.semantic)
      val binders = collectBinders(body)
      assert(binders.contains(firstBinder))
      assert(binders.contains(secondBinder))
      assertNotEquals(firstBinder, secondBinder)

      val freeRoot = parseClass(
        "class MixedFree:\n  def compute(lazyValue: => AnyVal, values: AnyVal*): Int = helper\n"
      )
      val freeCapture = publicRight(ExistingClassUntypedRewrite.capture(freeRoot))
      val freeMethod = publicRight(freeCapture.method(freeCapture.members.head.ref))
      assertEquals(freeMethod.body.semantic, Right(TermShape.Identifier("helper", false)))

      val foreign = TermBindingInternals
        .persistentParameters(Vector(Vector("lazyValue", "values")))
        .fold(problem => fail(problem.message), identity)
        .referenceAt(0, 0)
        .fold(problem => fail(problem.message), identity)
      assertPublicCode(
        captured.emptyPlan.replaceBody(method.ref, foreign),
        "SEMANTIC_FRAGMENT_INVALID"
      )
    }
  }

  test("strict-neighbor controls remain structurally distinct from the mixed family") {
    withContext {
      val byNameStrict = firstMethod(parseClass(
        "class ByNameStrict:\n  def compute(lazyValue: => AnyVal, value: AnyVal): Int = 0\n"
      )).paramss.head.map(_.asInstanceOf[untpd.ValDef])
      assert(byNameStrict.head.tpt.isInstanceOf[untpd.ByNameTypeTree])
      assert(byNameStrict(1).tpt.isInstanceOf[untpd.Ident])

      val strictRepeated = firstMethod(parseClass(
        "class StrictRepeated:\n  def compute(value: AnyVal, values: AnyVal*): Int = values.size\n"
      )).paramss.head.map(_.asInstanceOf[untpd.ValDef])
      assert(strictRepeated.head.tpt.isInstanceOf[untpd.Ident])
      assert(strictRepeated(1).tpt.isInstanceOf[untpd.PostfixOp])
    }
  }

  extension (method: ExistingClassUntypedRewrite.MethodView)
    private def parametersOrFailure(
        parameter: ExistingClassUntypedRewrite.ParameterView
    ): Either[Failure, TypeNormalForm] =
      parameter.declaredType.semantic

  private def collectBinders(shape: TermShape): Vector[TermBinder] =
    val view = TermShapeBindingView.inspect(shape).fold(problem => fail(problem.message), identity)
    if view.category == TermBindingCategory.BoundReference then
      Vector(view.boundReference.getOrElse(fail("missing bound-reference view")).binder)
    else
      shape match
        case TermShape.Select(qualifier, _) => collectBinders(qualifier)
        case TermShape.Apply(function, arguments) =>
          collectBinders(function) ++ arguments.toVector.flatMap(collectBinders)
        case TermShape.Infix(left, _, right) => collectBinders(left) ++ collectBinders(right)
        case TermShape.Unary(_, operand) => collectBinders(operand)
        case TermShape.Tuple(elements) => elements.toVector.flatMap(collectBinders)
        case TermShape.If(condition, thenBranch, elseBranch) =>
          collectBinders(condition) ++ collectBinders(thenBranch) ++ collectBinders(elseBranch)
        case TermShape.Parenthesized(expression) => collectBinders(expression)
        case _ => Vector.empty

  private def byNameType(parameter: untpd.ValDef): untpd.ByNameTypeTree =
    parameter.tpt match
      case value: untpd.ByNameTypeTree => value
      case other => fail("expected ByNameTypeTree, found " + other)

  private def repeatedType(parameter: untpd.ValDef): untpd.PostfixOp =
    parameter.tpt match
      case value: untpd.PostfixOp => value
      case other => fail("expected PostfixOp repeated Type, found " + other)

  private def repeatedParts(repeated: untpd.PostfixOp): (untpd.Tree, untpd.Tree) =
    repeated match
      case untpd.PostfixOp(element, marker) => (element, marker)

  private def firstMethod(root: untpd.TypeDef)(using Context): untpd.DefDef =
    root.rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U060MixedCallingModes.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def privateRight[A](value: Either[ExistingUntpdClassMemberFilterError, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def publicRight[A](value: Either[Failure, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def left[A](value: Either[Failure, A]): Failure =
    value match
      case Left(problem) =>
        assertEquals(problem.code, "UNSUPPORTED_STRUCTURE")
        problem
      case Right(success) => fail("expected UNSUPPORTED_STRUCTURE, found success " + success)

  private def assertPublicCode[A](value: Either[Failure, A], expected: String): Unit =
    value match
      case Left(problem) => assertEquals(problem.code, expected)
      case Right(success) => fail("expected " + expected + ", found success " + success)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
