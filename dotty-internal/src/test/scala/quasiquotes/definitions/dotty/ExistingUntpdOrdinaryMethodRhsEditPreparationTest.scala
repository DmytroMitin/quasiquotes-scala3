package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.parser.TermShape
import quasiquotes.terms.{
  TermBinder,
  TermBindingCategory,
  TermParameterSpec,
  TermShapeBindings,
  TermShapeBindingView
}
import quasiquotes.types.TypeNormalForm

class ExistingUntpdOrdinaryMethodRhsEditPreparationTest extends munit.FunSuite:
  import ExistingUntpdOrdinaryMethodRhsEditPreparation.*

  test("prepares one bound parameter leaf without reconstructing a method or owner") {
    withContext {
      val descriptor = descriptorFor(
        "class BoundLeaf:\n  def change(value: Int): Int = value + 1\n"
      )

      val prepared = prepareBody(descriptor) { scope =>
        scope.reference(descriptor.parameterClauses.head.head)
      }.fold(problem => fail(problem.message), identity)

      assert(prepared.descriptor.eq(descriptor))
      assert(prepared.captured.eq(descriptor.captured))
      assert(prepared.method.eq(descriptor.method))
      assert(prepared.oldRhs.eq(descriptor.rhs))
      assertEquals(prepared.parameterBinders.map(_.size), Vector(1))
      val view = TermShapeBindingView.inspect(prepared.semanticReplacement)
        .fold(problem => fail(problem.message), identity)
      assertEquals(view.category, TermBindingCategory.BoundReference)
      assertEquals(
        view.boundReference.map(_.binder),
        Some(prepared.parameterBinders.head.head)
      )
      assertIdent(prepared.loweredReplacement, "value")
      assert(!prepared.loweredReplacement.source.exists)
      assert(!prepared.loweredReplacement.span.exists)
      assertEquals(prepared.loweredReplacement.symbol, NoSymbol)
      assertIdent(prepared.positionedReplacement, "value")
      assert(!prepared.positionedReplacement.eq(prepared.loweredReplacement))
      assertEquals(prepared.positionedReplacement.source, descriptor.rhs.source)
      assertEquals(prepared.positionedReplacement.span, descriptor.rhs.span)
      assertEquals(validatePreparedBody(prepared, descriptor), Right(()))
      assert(
        allTrees(prepared.positionedReplacement).forall(tree =>
          !tree.isInstanceOf[untpd.DefDef] &&
            !tree.isInstanceOf[untpd.Template] &&
            !tree.isInstanceOf[untpd.TypeDef]
        )
      )
    }
  }

  test("admits safe free identifiers and primitive literals but rejects lexical capture and placeholders") {
    withContext {
      val descriptor = descriptorFor("class Leaves:\n  def change(x: Int): Int = x\n")
      val safe = prepare(descriptor)(_ => Right(TermShape.Identifier("helper", false)))
      assertIdent(safe.loweredReplacement, "helper")

      Vector("1", "true", "false", "\"text\"").foreach { value =>
        val literal = prepare(descriptor)(_ => Right(TermShape.Literal(value)))
        assertEquals(literal.semanticReplacement, TermShape.Literal(value))
        assertUniformSite(literal)
      }

      assertCode(
        prepareBody(descriptor)(_ => Right(TermShape.Identifier("x", false))),
        "FREE_IDENTIFIER_PARAMETER_CAPTURE"
      )
      assertCode(
        prepareBody(descriptor)(_ => Right(TermShape.Identifier("x", true))),
        "PLACEHOLDER_IDENTIFIER_UNSUPPORTED"
      )
    }
  }

  test("prepares direct and selected Apply families with one to three admitted leaves") {
    withContext {
      val one = descriptorFor("class OneApply:\n  def change(x: Int): Int = x\n")
      val direct = prepare(one) { scope =>
        scope.reference(one.parameterClauses.head.head).map { x =>
          TermShape.Apply(TermShape.Identifier("helper", false), List(x))
        }
      }
      val selected = prepare(one) { scope =>
        scope.reference(one.parameterClauses.head.head).map { x =>
          TermShape.Apply(
            TermShape.Select(TermShape.Identifier("Math", false), "abs"),
            List(x)
          )
        }
      }
      val two = descriptorFor(
        "class TwoApply:\n  def change(x: Int, y: Int): Int = x + y\n"
      )
      val maximum = prepare(two) { scope =>
        for
          x <- scope.reference(two.parameterClauses.head(0))
          y <- scope.reference(two.parameterClauses.head(1))
        yield TermShape.Apply(
          TermShape.Select(TermShape.Identifier("Math", false), "max"),
          List(x, y)
        )
      }

      assertEquals(
        direct.replacementFamily,
        ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily.DirectIdentApply
      )
      assertEquals(
        selected.replacementFamily,
        ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily.DirectIdentQualifiedSelectedApply
      )
      assertEquals(
        maximum.replacementFamily,
        ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily.DirectIdentQualifiedSelectedApply
      )
      Vector(direct, selected, maximum).foreach(assertUniformSite)
      assertEquals(rawTopology(selected.loweredReplacement), "Apply(Select(Ident(Math),abs),[Ident(x)])")
      assertEquals(rawTopology(maximum.loweredReplacement), "Apply(Select(Ident(Math),max),[Ident(x),Ident(y)])")
    }
  }

  test("treats a selected member matching a parameter spelling as selection data") {
    withContext {
      val descriptor = descriptorFor("class Selection:\n  def change(x: Int): Int = x\n")
      val prepared = prepare(descriptor) { scope =>
        scope.reference(descriptor.parameterClauses.head.head).map { x =>
          TermShape.Apply(
            TermShape.Select(TermShape.Identifier("holder", false), "x"),
            List(x)
          )
        }
      }
      assertEquals(
        rawTopology(prepared.loweredReplacement),
        "Apply(Select(Ident(holder),x),[Ident(x)])"
      )
    }
  }

  test("rejects unsupported semantic families argument counts nested leaves and captured free functions") {
    withContext {
      val descriptor = descriptorFor("class Family:\n  def change(x: Int): Int = x\n")
      val unsupportedLambda = TermShapeBindings.lambda(
        Vector(TermParameterSpec("inner", TypeNormalForm.STypeIdent("Int")))
      ) { scope =>
        scope.reference(scope.parameterBinders.head.head)
      }.fold(problem => fail(problem.message), identity)

      assertCode(
        prepareBody(descriptor)(_ => Right(unsupportedLambda)),
        "SEMANTIC_REPLACEMENT_FAMILY_UNSUPPORTED"
      )
      assertCode(
        prepareBody(descriptor)(_ => Right(TermShape.Apply(TermShape.Identifier("helper", false), Nil))),
        "SEMANTIC_APPLY_ARGUMENT_COUNT_UNSUPPORTED"
      )
      assertCode(
        prepareBody(descriptor)(_ => Right(TermShape.Apply(
          TermShape.Identifier("helper", false),
          List.fill(4)(TermShape.Literal("1"))
        ))),
        "SEMANTIC_APPLY_ARGUMENT_COUNT_UNSUPPORTED"
      )
      assertCode(
        prepareBody(descriptor)(_ => Right(TermShape.Apply(
          TermShape.Identifier("helper", false),
          List(TermShape.Apply(TermShape.Identifier("nested", false), List(TermShape.Literal("1"))))
        ))),
        "SEMANTIC_APPLY_LEAF_REQUIRED"
      )
      assertCode(
        prepareBody(descriptor)(_ => Right(TermShape.Apply(
          TermShape.Identifier("x", false),
          List(TermShape.Literal("1"))
        ))),
        "FREE_IDENTIFIER_PARAMETER_CAPTURE"
      )
    }
  }

  test("returns graph-local public binders and fresh graphs and fragments on repeated preparation") {
    withContext {
      val descriptor = descriptorFor("class Repeat:\n  def change(x: Int, y: Int): Int = x\n")
      var leaked: ReplacementScope = null
      def run(): PreparedBody = prepare(descriptor) { scope =>
        leaked = scope
        scope.reference(descriptor.parameterClauses.head(0))
      }
      val first = run()
      val firstScope = leaked
      val second = run()

      assertNotEquals(first.parameterBinders.head(0), second.parameterBinders.head(0))
      assertNotEquals(first.parameterBinders.head(1), second.parameterBinders.head(1))
      assert(!first.semanticReplacement.asInstanceOf[AnyRef].eq(second.semanticReplacement.asInstanceOf[AnyRef]))
      allTrees(first.loweredReplacement).zip(allTrees(second.loweredReplacement))
        .foreach((left, right) => assert(!left.eq(right)))
      allTrees(first.positionedReplacement).zip(allTrees(second.positionedReplacement))
        .foreach((left, right) => assert(!left.eq(right)))
      assertCode(
        firstScope.reference(descriptor.parameterClauses.head.head),
        "REPLACEMENT_SCOPE_INACTIVE"
      )
    }
  }

  test("rejects foreign parameter wrappers descriptor wrappers binders scopes and semantic graphs") {
    withContext {
      val root = parseClass("class SameRaw:\n  def change(x: Int): Int = x\n")
      val capture = ExistingUntpdClassMemberFilter.capture(root).toOption.get
      val firstDescriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(capture, 0).toOption.get
      val secondDescriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(capture, 0).toOption.get
      val first = prepare(firstDescriptor)(_.reference(firstDescriptor.parameterClauses.head.head))
      val second = prepare(secondDescriptor)(_.reference(secondDescriptor.parameterClauses.head.head))
      val foreignDescriptor = descriptorFor("class SameRaw:\n  def change(x: Int): Int = x\n")
      val foreign = prepare(foreignDescriptor)(_.reference(foreignDescriptor.parameterClauses.head.head))

      assertCode(
        prepareBody(firstDescriptor)(_.reference(secondDescriptor.parameterClauses.head.head)),
        "PARAMETER_NOT_CAPTURED"
      )
      assertCode(
        prepareBody(firstDescriptor)(_.reference(foreignDescriptor.parameterClauses.head.head)),
        "PARAMETER_NOT_CAPTURED"
      )
      assertCode(
        prepareBody(firstDescriptor)(_ => Right(second.semanticReplacement)),
        "FOREIGN_BINDER_GRAPH"
      )
      assertCode(
        prepareBody(firstDescriptor)(_ => Right(foreign.semanticReplacement)),
        "FOREIGN_BINDER_GRAPH"
      )
      assertCode(
        validatePreparedBody(first, secondDescriptor),
        "FINAL_BODY_PREPARATION_INVARIANT_FAILED"
      )
      assertCode(
        validatePreparedBody(first.copy(parameterBinders = second.parameterBinders), firstDescriptor),
        "FINAL_BODY_PREPARATION_INVARIANT_FAILED"
      )
      assertCode(
        validatePreparedBody(first.copy(replacementScope = second.replacementScope), firstDescriptor),
        "FINAL_BODY_PREPARATION_INVARIANT_FAILED"
      )
      assertCode(
        validatePreparedBody(first.copy(replacementScope = foreign.replacementScope), firstDescriptor),
        "FINAL_BODY_PREPARATION_INVARIANT_FAILED"
      )
    }
  }

  test("fails closed for missing builders results shapes invalid descriptors and duplicate parameter names") {
    withContext {
      val descriptor = descriptorFor("class Boundaries:\n  def change(x: Int): Int = x\n")
      assertCode(prepareBody(descriptor)(null), "REPLACEMENT_BUILDER_REQUIRED")
      assertCode(
        prepareBody(descriptor)(_ => null),
        "REPLACEMENT_BUILDER_RESULT_REQUIRED"
      )
      assertCode(
        prepareBody(descriptor)(_ => Right(null)),
        "SEMANTIC_REPLACEMENT_REQUIRED"
      )
      assertCode(
        prepareBody(null)(_ => Right(TermShape.Literal("1"))),
        "INVALID_DESCRIPTOR"
      )
      assertCode(
        prepareBody(descriptor.copy(rhs = descriptor.resultType))(_ => Right(TermShape.Literal("1"))),
        "INVALID_DESCRIPTOR"
      )
      val duplicate = descriptorFor("class Duplicate:\n  def change(x: Int, x: Int): Int = x\n")
      assertCode(
        prepareBody(duplicate)(_ => Right(TermShape.Literal("1"))),
        "AMBIGUOUS_PARAMETER_NAME"
      )
    }
  }

  test("final validator rejects null carriers aliases contamination and foreign field swaps") {
    withContext {
      val firstDescriptor = descriptorFor("class FirstCarrier:\n  def change(x: Int): Int = x\n")
      val secondDescriptor = descriptorFor("class SecondCarrier:\n  def change(x: Int): Int = x\n")
      val first = prepare(firstDescriptor)(_.reference(firstDescriptor.parameterClauses.head.head))
      val second = prepare(secondDescriptor)(_.reference(secondDescriptor.parameterClauses.head.head))

      assertCode(validatePreparedBody(null, firstDescriptor), "FINAL_BODY_PREPARATION_INVARIANT_FAILED")
      assertCode(validatePreparedBody(first, null), "FINAL_BODY_PREPARATION_INVARIANT_FAILED")
      assertCode(
        validatePreparedBody(first.copy(loweredReplacement = first.positionedReplacement), firstDescriptor),
        "FINAL_BODY_PREPARATION_INVARIANT_FAILED"
      )
      assertCode(
        validatePreparedBody(first.copy(positionedReplacement = second.positionedReplacement), firstDescriptor),
        "FINAL_BODY_PREPARATION_INVARIANT_FAILED"
      )
      assertCode(
        validatePreparedBody(first.copy(oldRhs = second.oldRhs), firstDescriptor),
        "FINAL_BODY_PREPARATION_INVARIANT_FAILED"
      )
    }
  }

  test("matches accepted U029 and U034 family topology and replacement-only provenance") {
    withContext {
      val oneDescriptor = descriptorFor("class OneParity:\n  def change(x: Int): Int = x\n")
      val onePrepared = prepare(oneDescriptor) { scope =>
        scope.reference(oneDescriptor.parameterClauses.head.head).map { x =>
          TermShape.Apply(TermShape.Identifier("helper", false), List(x))
        }
      }
      val oneView = ExistingUntpdSingleParameterMethodView
        .capture(oneDescriptor.captured, oneDescriptor.memberIndex).toOption.get
      val oneOracle = ExistingUntpdSingleParameterMethodRhsRewriter
        .rewrite(oneView, freshSourceFree(onePrepared.loweredReplacement)).toOption.get
      assertParity(onePrepared, oneOracle.replacementFamily, oneOracle.positionedResult.positionedReplacement)

      val selectedPrepared = prepare(oneDescriptor) { scope =>
        scope.reference(oneDescriptor.parameterClauses.head.head).map { x =>
          TermShape.Apply(
            TermShape.Select(TermShape.Identifier("Math", false), "abs"),
            List(x)
          )
        }
      }
      val selectedOracle = ExistingUntpdSingleParameterMethodRhsRewriter
        .rewrite(oneView, freshSourceFree(selectedPrepared.loweredReplacement)).toOption.get
      assertParity(selectedPrepared, selectedOracle.replacementFamily, selectedOracle.positionedResult.positionedReplacement)

      val twoDescriptor = descriptorFor("class TwoParity:\n  def change(x: Int, y: Int): Int = x\n")
      val twoPrepared = prepare(twoDescriptor) { scope =>
        for
          x <- scope.reference(twoDescriptor.parameterClauses.head(0))
          y <- scope.reference(twoDescriptor.parameterClauses.head(1))
        yield TermShape.Apply(
          TermShape.Select(TermShape.Identifier("Math", false), "max"),
          List(x, y)
        )
      }
      val twoView = ExistingUntpdTwoParameterMethodView
        .capture(twoDescriptor.captured, twoDescriptor.memberIndex).toOption.get
      val twoOracle = ExistingUntpdTwoParameterMethodRhsRewriter
        .rewrite(twoView, freshSourceFree(twoPrepared.loweredReplacement)).toOption.get
      assertParity(twoPrepared, twoOracle.replacementFamily, twoOracle.positionedResult.positionedReplacement)
    }
  }

  test("success failure and repetition leave the exact complete original graph unchanged") {
    withContext {
      val descriptor = descriptorFor(
        "class Immutable:\n  val before: Int = 1\n  def change(x: Int, y: Int): Int = x + y\n  type After = String\n"
      , 1)
      val before = snapshot(descriptor.captured.originalRoot)
      prepare(descriptor)(_.reference(descriptor.parameterClauses.head(0)))
      prepare(descriptor)(_.reference(descriptor.parameterClauses.head(1)))
      assertCode(
        prepareBody(descriptor)(_ => Right(TermShape.Identifier("x", false))),
        "FREE_IDENTIFIER_PARAMETER_CAPTURE"
      )
      assertSnapshotUnchanged(descriptor.captured.originalRoot, before)
    }
  }

  test("prepared raw graphs contain only the replacement expression and its children") {
    withContext {
      val descriptor = descriptorFor("class NoRebuild:\n  def change(x: Int, y: Int): Int = x\n")
      val prepared = prepare(descriptor) { scope =>
        for
          x <- scope.reference(descriptor.parameterClauses.head(0))
          y <- scope.reference(descriptor.parameterClauses.head(1))
        yield TermShape.Apply(
          TermShape.Select(TermShape.Identifier("Math", false), "max"),
          List(x, y)
        )
      }
      Vector(prepared.loweredReplacement, prepared.positionedReplacement).foreach { root =>
        assert(allTrees(root).forall(tree =>
          !tree.isInstanceOf[untpd.DefDef] &&
            !tree.isInstanceOf[untpd.Template] &&
            !tree.isInstanceOf[untpd.TypeDef]
        ))
      }
    }
  }

  private def descriptorFor(
      source: String,
      memberIndex: Int = 0
  )(using Context): ExistingUntpdOrdinaryMethodDescriptor.Descriptor =
    val root = parseClass(source)
    val captured = ExistingUntpdClassMemberFilter.capture(root)
      .fold(problem => fail(problem.message), identity)
    ExistingUntpdOrdinaryMethodDescriptor.capture(captured, memberIndex)
      .fold(problem => fail(problem.message), identity)

  private def prepare(
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor
  )(
      build: ReplacementScope => Either[Error, TermShape]
  )(using Context): PreparedBody =
    prepareBody(descriptor)(build).fold(problem => fail(problem.message), identity)

  private def assertCode[A](result: Either[Error, A], expected: String): Unit =
    result match
      case Left(problem) =>
        assertEquals(problem.code, expected, clues(problem))
        assert(problem.detail.nonEmpty)
        assert(problem.message.nonEmpty)
      case Right(value) => fail(s"expected $expected, found success $value")

  private def assertUniformSite(prepared: PreparedBody)(using Context): Unit =
    val lowered = allTrees(prepared.loweredReplacement)
    val positioned = allTrees(prepared.positionedReplacement)
    assertEquals(lowered.size, positioned.size)
    lowered.foreach(tree =>
      assert(!tree.source.exists)
      assert(!tree.span.exists)
      assertEquals(tree.symbol, NoSymbol)
      assert(!tree.isInstanceOf[untpd.TypedSplice])
    )
    lowered.zip(positioned).foreach((left, right) => assert(!left.eq(right)))
    positioned.foreach(tree =>
      assertEquals(tree.source, prepared.oldRhs.source)
      assertEquals(tree.span, prepared.oldRhs.span)
      assertEquals(tree.symbol, NoSymbol)
      assert(!tree.isInstanceOf[untpd.TypedSplice])
    )

  private def assertParity(
      prepared: PreparedBody,
      oracleFamily: ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily,
      oraclePositioned: untpd.Tree
  )(using Context): Unit =
    assertEquals(prepared.replacementFamily, oracleFamily)
    assertEquals(rawTopology(prepared.positionedReplacement), rawTopology(oraclePositioned))
    allTrees(oraclePositioned).foreach(tree =>
      assertEquals(tree.source, prepared.oldRhs.source)
      assertEquals(tree.span, prepared.oldRhs.span)
      assertEquals(tree.symbol, NoSymbol)
      assert(!tree.isInstanceOf[untpd.TypedSplice])
    )
    assertUniformSite(prepared)

  private def freshSourceFree(tree: untpd.Tree)(using Context): untpd.Tree =
    tree.cloneIn(dotty.tools.dotc.util.NoSource).withSpan(dotty.tools.dotc.util.Spans.NoSpan)

  private def rawTopology(tree: untpd.Tree): String = tree match
    case ident: untpd.Ident => s"Ident(${ident.name})"
    case number: untpd.Number => s"Number(${number.digits})"
    case literal: untpd.Literal => s"Literal(${literal.const.value})"
    case select: untpd.Select => s"Select(${rawTopology(select.qualifier)},${select.name})"
    case apply: untpd.Apply =>
      s"Apply(${rawTopology(apply.fun)},[${apply.args.map(rawTopology).mkString(",")}])"
    case other => other.getClass.getSimpleName

  private final case class TreeSnapshot(
      tree: untpd.Tree,
      source: SourceFile,
      span: Span,
      symbol: Symbol,
      modifiers: Option[untpd.Modifiers]
  )

  private def snapshot(tree: untpd.Tree)(using Context): Vector[TreeSnapshot] =
    allTrees(tree).map { node =>
      val modifiers = node match
        case definition: untpd.DefTree => Option(definition.mods)
        case _ => None
      TreeSnapshot(node, node.source, node.span, node.symbol, modifiers)
    }

  private def assertSnapshotUnchanged(
      root: untpd.Tree,
      before: Vector[TreeSnapshot]
  )(using Context): Unit =
    val after = allTrees(root)
    assertEquals(after.size, before.size)
    after.zip(before).foreach { (actual, expected) =>
      assert(actual.eq(expected.tree))
      assert(actual.source.eq(expected.source))
      assertEquals(actual.span, expected.span)
      assertEquals(actual.symbol, expected.symbol)
      actual match
        case definition: untpd.DefTree =>
          assert(expected.modifiers.exists(definition.mods.eq))
        case _ => assertEquals(expected.modifiers, None)
    }

  private def assertIdent(tree: untpd.Tree, expected: String): Unit = tree match
    case ident: untpd.Ident => assertEquals(ident.name.toString, expected)
    case other => fail(s"expected Ident($expected), found ${other.getClass.getSimpleName}")

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ExistingUntpdClassMemberFilter.allTrees(tree)

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U041RhsEditPreparation.scala", source)
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
