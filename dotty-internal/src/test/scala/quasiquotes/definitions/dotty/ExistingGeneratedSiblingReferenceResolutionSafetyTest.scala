package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.{DefinitionKind, DefinitionModifiers, DefinitionName, DefinitionSemanticError, SemanticDefinition}
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.{Capture, Failure, MethodView, Result}
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class ExistingGeneratedSiblingReferenceResolutionSafetyTest extends munit.FunSuite:
  private val IntType = TypeNormalForm.STypeIdent("Int")

  test("A through D retain exact identities and generated free-name topology at each pre-Typer stage") {
    withContext {
      checkPositive("U063AAppendOnly", editEval = false, editSum = false, omit = false, editsFirst = true)
      checkPositive("U063BRepeated", editEval = false, editSum = true, omit = false, editsFirst = true)
      checkPositive("U063CBothModes", editEval = true, editSum = true, omit = false, editsFirst = true)
      checkPositive("U063DEditsFirst", editEval = true, editSum = true, omit = true, editsFirst = true)
      checkPositive("U063DOmitFirst", editEval = true, editSum = true, omit = true, editsFirst = false)
    }
  }

  test("omitted and duplicate sibling references pass the public pre-Typer transaction boundary") {
    withContext {
      val omittedFixture = captureFixture("U063OmittedPreTyper")
      val omittedPlan = publicRight(for
        p1 <- omittedFixture.captured.emptyPlan.omit(omittedFixture.captured.members(2).ref)
        p2 <- p1.append(
          generatedIdentifierMethod("bonus", "obsolete"),
          "generated/U063OmittedPreTyper.scala"
        )
      yield p2)
      val omittedResult = publicRight(ExistingClassUntypedRewrite(
        omittedFixture.captured,
        omittedPlan
      ))
      assertEquals(memberNames(directMembers(omittedResult.tree)), Vector("eval", "sum", "untouched", "bonus"))
      assertIdentifierBody(directMembers(omittedResult.tree).last, "obsolete")
      assertPreTyperClean(omittedResult.tree)

      val duplicateFixture = captureFixture("U063DuplicatePreTyper")
      val duplicatePlan = publicRight(duplicateFixture.captured.emptyPlan.append(
        generatedLiteralMethod("untouched", "5"),
        "generated/U063DuplicatePreTyper.scala"
      ))
      val duplicateResult = publicRight(ExistingClassUntypedRewrite(
        duplicateFixture.captured,
        duplicatePlan
      ))
      assertEquals(
        memberNames(directMembers(duplicateResult.tree)),
        Vector("eval", "sum", "obsolete", "untouched", "untouched")
      )
      assertPreTyperClean(duplicateResult.tree)
    }
  }

  test("foreign stale and cross-method references cannot spoof a generated sibling reference") {
    withContext {
      val fixture = captureFixture("U063Scope")
      val foreign = captureFixture("U063Foreign")
      val evalParameter = parameter(fixture.eval)
      val sumParameter = parameter(fixture.sum)

      assertPublicCode(
        fixture.captured.emptyPlan.replaceParameterType(parameter(foreign.eval).ref, IntType),
        "SELECTION_FAILED"
      )
      assertPublicCode(
        fixture.captured.emptyPlan.omit(foreign.captured.members(2).ref),
        "SELECTION_FAILED"
      )
      assertPublicCode(
        fixture.sum.parameterScope.reference(evalParameter.ref),
        "SELECTION_FAILED"
      )
      assertPublicCode(
        fixture.eval.parameterScope.reference(sumParameter.ref),
        "SELECTION_FAILED"
      )

      val firstResult = publicRight(ExistingClassUntypedRewrite(
        fixture.captured,
        publicRight(fixture.captured.emptyPlan.append(
          generatedLiteralMethod("firstBonus", "1"),
          "generated/U063Stale.scala"
        ))
      ))
      val recaptured = publicRight(ExistingClassUntypedRewrite.capture(firstResult.tree))
      assertPublicCode(
        recaptured.emptyPlan.replaceParameterType(evalParameter.ref, IntType),
        "SELECTION_FAILED"
      )
      assertPublicCode(
        recaptured.emptyPlan.omit(fixture.captured.members(2).ref),
        "SELECTION_FAILED"
      )

      val boundReference = publicRight(fixture.eval.parameterScope.reference(evalParameter.ref))
      val crossed = SemanticDefinition.concreteMethod(
        semantic(DefinitionName.fromSource("crossedBonus")),
        Vector.empty,
        IntType
      )(_ => Right(boundReference))
      assertSemanticCode(crossed, "DEFINITION_SEMANTIC_SCOPE_MISMATCH")
    }
  }

  test("inadmissible generated payload fails at lowering and valid earlier edits expose no partial owner") {
    withContext {
      val fixture = captureFixture("U063AtomicFailure")
      val invalidGenerated = new SemanticDefinition(
        DefinitionKind.Value,
        semantic(DefinitionName.fromSource("unsupportedValue")),
        DefinitionModifiers.empty,
        storage("ValueStorage", TypeNormalForm.STypeIdent("AnyVal"), TermShape.Literal("1"))
      )

      val directFailure = DefinitionGeneratedOriginLowering.lower(
        invalidGenerated,
        "generated/U063UnsupportedDirect.scala"
      ).left.toOption.getOrElse(fail("expected generated lowering failure"))
      assertEquals(directFailure.code, "EXACT_LOWERING_FAILED")

      val plan = publicRight(for
        p1 <- fixture.captured.emptyPlan.replaceParameterType(parameter(fixture.eval).ref, IntType)
        p2 <- p1.replaceParameterType(parameter(fixture.sum).ref, IntType)
        p3 <- p2.append(invalidGenerated, "generated/U063UnsupportedApply.scala")
      yield p3)
      assertPublicCode(ExistingClassUntypedRewrite(fixture.captured, plan), "LOWERING_FAILED")
      assert(directMembers(fixture.root).zip(fixture.original).forall((left, right) => left.eq(right)))
      assertPreTyperClean(fixture.root)
    }
  }

  test("empty plan remains the exact owner and all direct-member identities remain exact") {
    withContext {
      val fixture = captureFixture("U063Noop")
      val result = publicRight(ExistingClassUntypedRewrite(
        fixture.captured,
        fixture.captured.emptyPlan
      ))
      assert(!result.changed)
      assert(result.tree.eq(fixture.root))
      assert(result.directMemberIdentities.zip(fixture.captured.members).forall {
        case (actual, expected) => actual.sameObjectAs(expected.identity)
      })
    }
  }

  private final case class Fixture(
      root: untpd.TypeDef,
      original: Vector[untpd.Tree],
      captured: Capture,
      eval: MethodView,
      sum: MethodView
  )

  private def checkPositive(
      name: String,
      editEval: Boolean,
      editSum: Boolean,
      omit: Boolean,
      editsFirst: Boolean
  )(using Context): Unit =
    val fixture = captureFixture(name)
    val path = s"generated/$name.scala"
    val plan = publicRight(
      if editsFirst then
        for
          p1 <- if editEval then fixture.captured.emptyPlan.replaceParameterType(parameter(fixture.eval).ref, IntType)
            else Right(fixture.captured.emptyPlan)
          p2 <- if editSum then p1.replaceParameterType(parameter(fixture.sum).ref, IntType) else Right(p1)
          p3 <- if omit then p2.omit(fixture.captured.members(2).ref) else Right(p2)
          p4 <- p3.append(generatedApplyMethod("bonus", "sum"), path)
        yield p4
      else
        for
          p1 <- fixture.captured.emptyPlan.omit(fixture.captured.members(2).ref)
          p2 <- p1.append(generatedApplyMethod("bonus", "sum"), path)
          p3 <- p2.replaceParameterType(parameter(fixture.sum).ref, IntType)
          p4 <- p3.replaceParameterType(parameter(fixture.eval).ref, IntType)
        yield p4
    )
    val result = publicRight(ExistingClassUntypedRewrite(fixture.captured, plan))
    assertPositiveResult(fixture, result, editEval, editSum, omit, path)

  private def assertPositiveResult(
      fixture: Fixture,
      result: Result,
      editEval: Boolean,
      editSum: Boolean,
      omit: Boolean,
      path: String
  )(using Context): Unit =
    val members = directMembers(result.tree)
    val expected = Vector("eval", "sum") ++
      (if omit then Vector.empty else Vector("obsolete")) ++
      Vector("untouched", "bonus")
    assertEquals(memberNames(members), expected)
    assertOriginalOrFresh(members(0), fixture.original(0), editEval)
    assertOriginalOrFresh(members(1), fixture.original(1), editSum)
    if editEval then assert(parameter(members(0).asInstanceOf[untpd.DefDef]).tpt.isInstanceOf[untpd.ByNameTypeTree])
    if editSum then
      val oldRepeated = parameter(fixture.original(1).asInstanceOf[untpd.DefDef]).tpt.asInstanceOf[untpd.PostfixOp]
      val newRepeated = parameter(members(1).asInstanceOf[untpd.DefDef]).tpt.asInstanceOf[untpd.PostfixOp]
      val oldMarker = oldRepeated.asInstanceOf[untpd.PostfixOp].op
      val newMarker = newRepeated.asInstanceOf[untpd.PostfixOp].op
      assert(newMarker.eq(oldMarker))
    val untouchedIndex = if omit then 2 else 3
    assert(members(untouchedIndex).eq(fixture.original(3)))
    assert(result.directMemberIdentities(untouchedIndex).sameObjectAs(fixture.captured.members(3).identity))
    assertApplyBody(members.last, "sum", 3, path)
    assertPreTyperClean(result.tree)

  private def assertOriginalOrFresh(actual: untpd.Tree, original: untpd.Tree, fresh: Boolean): Unit =
    if fresh then assert(!actual.eq(original)) else assert(actual.eq(original))

  private def assertApplyBody(
      tree: untpd.Tree,
      sibling: String,
      argumentCount: Int,
      path: String
  )(using Context): Unit =
    val generated = tree.asInstanceOf[untpd.DefDef]
    assertEquals(generated.source.path, path)
    generated.rhs match
      case untpd.Apply(function: untpd.Ident, arguments) =>
        assertEquals(function.name.toString, sibling)
        assertEquals(arguments.size, argumentCount)
      case other => fail("expected generated free-name Apply, found " + other)

  private def assertIdentifierBody(tree: untpd.Tree, sibling: String)(using Context): Unit =
    tree.asInstanceOf[untpd.DefDef].rhs match
      case identifier: untpd.Ident => assertEquals(identifier.name.toString, sibling)
      case other => fail("expected generated free identifier, found " + other)

  private def captureFixture(name: String)(using Context): Fixture =
    val root = parseClass(Canonical.replace("GeneratedReferenceOwner", name))
    val captured = capture(root)
    Fixture(root, directMembers(root), captured, method(captured, 0), method(captured, 1))

  private def generatedApplyMethod(label: String, sibling: String): SemanticDefinition =
    semantic(SemanticDefinition.concreteMethod(
      semantic(DefinitionName.fromSource(label)),
      Vector.empty,
      IntType
    )(_ => Right(TermShape.Apply(
      TermShape.Identifier(sibling, false),
      List("10", "20", "30").map(TermShape.Literal(_))
    ))))

  private def generatedIdentifierMethod(label: String, sibling: String): SemanticDefinition =
    semantic(SemanticDefinition.concreteMethod(
      semantic(DefinitionName.fromSource(label)),
      Vector.empty,
      IntType
    )(_ => Right(TermShape.Identifier(sibling, false))))

  private def generatedLiteralMethod(label: String, literal: String): SemanticDefinition =
    semantic(SemanticDefinition.concreteMethod(
      semantic(DefinitionName.fromSource(label)),
      Vector.empty,
      IntType
    )(_ => Right(TermShape.Literal(literal))))

  private def memberNames(members: Vector[untpd.Tree]): Vector[String] =
    members.map(_.asInstanceOf[untpd.MemberDef].name.toString)

  private def directMembers(root: untpd.TypeDef)(using Context): Vector[untpd.Tree] =
    root.rhs.asInstanceOf[untpd.Template].body.toVector

  private def parameter(method: untpd.DefDef): untpd.ValDef =
    method.paramss.head.head.asInstanceOf[untpd.ValDef]

  private def parameter(method: MethodView): ExistingClassUntypedRewrite.ParameterView =
    method.parameterClauses.head.parameters.head

  private def capture(root: untpd.TypeDef)(using Context): Capture =
    publicRight(ExistingClassUntypedRewrite.capture(root))

  private def method(captured: Capture, index: Int)(using Context): MethodView =
    publicRight(captured.method(publicRight(captured.member(index)).ref))

  private def assertPreTyperClean(tree: untpd.Tree)(using Context): Unit =
    val trees = ExistingUntpdClassMemberFilter.allTrees(tree)
    assert(trees.forall(_.symbol == NoSymbol))
    assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U063Safety.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def semantic[A](value: Either[DefinitionSemanticError, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def storage(suffix: String, arguments: AnyRef*): AnyRef =
    val constructor = Class.forName(s"quasiquotes.definitions.SemanticDefinition$$$suffix")
      .getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor.newInstance(arguments*).asInstanceOf[AnyRef]

  private def publicRight[A](value: Either[Failure, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def assertPublicCode[A](value: Either[Failure, A], expected: String): Unit =
    value match
      case Left(problem) => assertEquals(problem.code, expected, clues(problem))
      case Right(success) => fail("expected " + expected + ", found success " + success)

  private def assertSemanticCode[A](
      value: Either[DefinitionSemanticError, A],
      expected: String
  ): Unit =
    value match
      case Left(problem) => assertEquals(problem.code, expected, clues(problem))
      case Right(success) => fail("expected " + expected + ", found success " + success)

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)

  private val Canonical =
    """class GeneratedReferenceOwner:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |""".stripMargin
