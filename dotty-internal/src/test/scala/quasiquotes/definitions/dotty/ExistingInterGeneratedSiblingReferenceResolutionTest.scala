package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.{DefinitionKind, DefinitionModifiers, DefinitionName, DefinitionSemanticError, SemanticDefinition}
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.{Capture, EditPlan, Failure, MethodView}
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class ExistingInterGeneratedSiblingReferenceResolutionTest extends munit.FunSuite:
  private val IntType = TypeNormalForm.STypeIdent("Int")

  test("two public semantic methods lower to independent free-name graphs and virtual sources") {
    withContext {
      val relay = generatedApplyMethod("relay", "sum")
      val bonus = generatedIdentifierMethod("bonus", "relay")

      relay.asMethod.flatMap(_.body).getOrElse(fail("missing relay body")) match
        case TermShape.Apply(TermShape.Identifier("sum", false), arguments) =>
          assertEquals(arguments, List("10", "20", "30").map(TermShape.Literal(_)))
        case other => fail("unexpected relay semantic body: " + other)
      bonus.asMethod.flatMap(_.body).getOrElse(fail("missing bonus body")) match
        case TermShape.Identifier("relay", false) => ()
        case other => fail("unexpected bonus semantic body: " + other)

      val loweredRelay = DefinitionGeneratedOriginLowering
        .lower(relay, "generated/U064Relay.scala")
        .fold(problem => fail(problem.message), identity)
      val loweredBonus = DefinitionGeneratedOriginLowering
        .lower(bonus, "generated/U064Bonus.scala")
        .fold(problem => fail(problem.message), identity)

      assertEquals(loweredRelay.generatedSource, "def relay: Int = sum(10, 20, 30)")
      assertEquals(loweredBonus.generatedSource, "def bonus: Int = relay")
      assertApplyBody(loweredRelay.tree, "sum", 3, "generated/U064Relay.scala")
      assertIdentifierBody(loweredBonus.tree, "relay", "generated/U064Bonus.scala")
      assert(!loweredRelay.sourceFile.eq(loweredBonus.sourceFile))
      assertPreTyperClean(loweredRelay.tree)
      assertPreTyperClean(loweredBonus.tree)
    }
  }

  test("dependency-first and forward append orders preserve exact intent order and distinct provenance") {
    withContext {
      checkPositive(
        "U064DependencyFirst",
        forward = false,
        editModes = false,
        omit = false,
        appendsFirst = false
      )
      checkPositive(
        "U064ForwardFirst",
        forward = true,
        editModes = false,
        omit = false,
        appendsFirst = false
      )
    }
  }

  test("calling-mode edits and omission compose with both append orders and both plan-building orders") {
    withContext {
      checkPositive("U064CDependency", forward = false, editModes = true, omit = false, appendsFirst = false)
      checkPositive("U064CForward", forward = true, editModes = true, omit = false, appendsFirst = false)
      checkPositive("U064D1", forward = false, editModes = true, omit = true, appendsFirst = false)
      checkPositive("U064D2", forward = false, editModes = true, omit = true, appendsFirst = true)
      checkPositive("U064D3", forward = true, editModes = true, omit = true, appendsFirst = false)
      checkPositive("U064D4", forward = true, editModes = true, omit = true, appendsFirst = true)
    }
  }

  test("retained-value two-hop control remains an ordinary pair of free identifiers") {
    withContext {
      val fixture = captureFixture("U064ValueControl")
      val relayPath = "generated/U064ValueRelay.scala"
      val bonusPath = "generated/U064ValueBonus.scala"
      val plan = publicRight(for
        p1 <- fixture.captured.emptyPlan.append(
          generatedIdentifierMethod("relay", "untouched"),
          relayPath
        )
        p2 <- p1.append(generatedIdentifierMethod("bonus", "relay"), bonusPath)
      yield p2)
      val result = publicRight(ExistingClassUntypedRewrite(fixture.captured, plan))
      val members = directMembers(result.tree)

      assertEquals(memberNames(members), Vector("eval", "sum", "obsolete", "untouched", "relay", "bonus"))
      assert(members(3).eq(fixture.original(3)))
      assertIdentifierBody(members(4), "untouched", relayPath)
      assertIdentifierBody(members(5), "relay", bonusPath)
      assertPreTyperClean(result.tree)
    }
  }

  test("missing duplicate and omitted targets remain structurally admitted before ordinary Typer") {
    withContext {
      val missing = captureFixture("U064MissingPreTyper")
      val missingResult = publicRight(ExistingClassUntypedRewrite(
        missing.captured,
        publicRight(missing.captured.emptyPlan.append(
          generatedIdentifierMethod("bonus", "relay"),
          "generated/U064MissingBonus.scala"
        ))
      ))
      assertEquals(memberNames(directMembers(missingResult.tree)), Vector("eval", "sum", "obsolete", "untouched", "bonus"))
      assertIdentifierBody(directMembers(missingResult.tree).last, "relay", "generated/U064MissingBonus.scala")

      val duplicate = captureFixture("U064DuplicatePreTyper")
      val duplicatePlan = publicRight(for
        p1 <- duplicate.captured.emptyPlan.append(
          generatedLiteralMethod("generatedDuplicate", "1"),
          "generated/U064DuplicateOne.scala"
        )
        p2 <- p1.append(
          generatedLiteralMethod("generatedDuplicate", "2"),
          "generated/U064DuplicateTwo.scala"
        )
      yield p2)
      val duplicateResult = publicRight(ExistingClassUntypedRewrite(duplicate.captured, duplicatePlan))
      assertEquals(
        memberNames(directMembers(duplicateResult.tree)).takeRight(2),
        Vector("generatedDuplicate", "generatedDuplicate")
      )

      val omitted = captureFixture("U064OmittedPreTyper")
      val omittedPlan = publicRight(for
        p1 <- omitted.captured.emptyPlan.omit(omitted.captured.members(2).ref)
        p2 <- p1.append(
          generatedIdentifierMethod("relay", "obsolete"),
          "generated/U064OmittedRelay.scala"
        )
        p3 <- p2.append(
          generatedIdentifierMethod("bonus", "relay"),
          "generated/U064OmittedBonus.scala"
        )
      yield p3)
      val omittedResult = publicRight(ExistingClassUntypedRewrite(omitted.captured, omittedPlan))
      assertEquals(memberNames(directMembers(omittedResult.tree)), Vector("eval", "sum", "untouched", "relay", "bonus"))
      assertPreTyperClean(missingResult.tree)
      assertPreTyperClean(duplicateResult.tree)
      assertPreTyperClean(omittedResult.tree)
    }
  }

  test("invalid sources payloads and capture-scope violations retain deterministic fail-closed stages") {
    withContext {
      val fixture = captureFixture("U064Failures")
      val foreign = captureFixture("U064Foreign")
      val evalParameter = parameter(fixture.eval)
      val sumParameter = parameter(fixture.sum)

      assertPublicCode(
        fixture.captured.emptyPlan.append(generatedIdentifierMethod("bonus", "relay"), " invalid/U064.scala"),
        "SEMANTIC_FRAGMENT_INVALID"
      )
      assertPublicCode(
        fixture.captured.emptyPlan.replaceParameterType(parameter(foreign.eval).ref, IntType),
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

      val boundReference = publicRight(fixture.eval.parameterScope.reference(evalParameter.ref))
      assertSemanticCode(
        SemanticDefinition.concreteMethod(
          semantic(DefinitionName.fromSource("crossedBonus")),
          Vector.empty,
          IntType
        )(_ => Right(boundReference)),
        "DEFINITION_SEMANTIC_SCOPE_MISMATCH"
      )

      val invalidGenerated = new SemanticDefinition(
        DefinitionKind.Value,
        semantic(DefinitionName.fromSource("unsupportedValue")),
        DefinitionModifiers.empty,
        storage("ValueStorage", TypeNormalForm.STypeIdent("AnyVal"), TermShape.Literal("1"))
      )
      val directFailure = DefinitionGeneratedOriginLowering
        .lower(invalidGenerated, "generated/U064UnsupportedDirect.scala")
        .left.toOption.getOrElse(fail("expected generated lowering failure"))
      assertEquals(directFailure.code, "EXACT_LOWERING_FAILED")

      val atomicPlan = publicRight(for
        p1 <- fixture.captured.emptyPlan.replaceParameterType(evalParameter.ref, IntType)
        p2 <- p1.replaceParameterType(sumParameter.ref, IntType)
        p3 <- p2.append(generatedApplyMethod("relay", "sum"), "generated/U064AtomicRelay.scala")
        p4 <- p3.append(invalidGenerated, "generated/U064UnsupportedApply.scala")
      yield p4)
      assertPublicCode(ExistingClassUntypedRewrite(fixture.captured, atomicPlan), "LOWERING_FAILED")
      assert(directMembers(fixture.root).zip(fixture.original).forall((left, right) => left.eq(right)))

      val noop = publicRight(ExistingClassUntypedRewrite(fixture.captured, fixture.captured.emptyPlan))
      assert(!noop.changed)
      assert(noop.tree.eq(fixture.root))
      assert(noop.directMemberIdentities.zip(fixture.captured.members).forall {
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
      forward: Boolean,
      editModes: Boolean,
      omit: Boolean,
      appendsFirst: Boolean
  )(using Context): Unit =
    val fixture = captureFixture(name)
    val relayPath = s"generated/${name}Relay.scala"
    val bonusPath = s"generated/${name}Bonus.scala"
    val evalParameter = parameter(fixture.eval).ref
    val sumParameter = parameter(fixture.sum).ref
    val plan = publicRight(
      if appendsFirst then
        for
          p1 <- appendPair(fixture.captured.emptyPlan, forward, relayPath, bonusPath)
          p2 <- if editModes then p1.replaceParameterType(evalParameter, IntType) else Right(p1)
          p3 <- if editModes then p2.replaceParameterType(sumParameter, IntType) else Right(p2)
          p4 <- if omit then p3.omit(fixture.captured.members(2).ref) else Right(p3)
        yield p4
      else
        for
          p1 <- if editModes then fixture.captured.emptyPlan.replaceParameterType(evalParameter, IntType)
            else Right(fixture.captured.emptyPlan)
          p2 <- if editModes then p1.replaceParameterType(sumParameter, IntType) else Right(p1)
          p3 <- if omit then p2.omit(fixture.captured.members(2).ref) else Right(p2)
          p4 <- appendPair(p3, forward, relayPath, bonusPath)
        yield p4
    )
    assertEquals(plan.editCount, 2 + (if editModes then 2 else 0) + (if omit then 1 else 0))
    val result = publicRight(ExistingClassUntypedRewrite(fixture.captured, plan))
    val members = directMembers(result.tree)
    val generatedNames = if forward then Vector("bonus", "relay") else Vector("relay", "bonus")
    val expected = Vector("eval", "sum") ++
      (if omit then Vector.empty else Vector("obsolete")) ++ Vector("untouched") ++ generatedNames

    assertEquals(memberNames(members), expected)
    assert(result.changed)
    assertEquals(result.directMemberIdentities.size, members.size)
    assertOriginalOrFresh(members(0), fixture.original(0), editModes)
    assertOriginalOrFresh(members(1), fixture.original(1), editModes)
    if editModes then
      assert(parameter(members(0).asInstanceOf[untpd.DefDef]).tpt.isInstanceOf[untpd.ByNameTypeTree])
      val oldRepeated = parameter(fixture.original(1).asInstanceOf[untpd.DefDef]).tpt.asInstanceOf[untpd.PostfixOp]
      val newRepeated = parameter(members(1).asInstanceOf[untpd.DefDef]).tpt.asInstanceOf[untpd.PostfixOp]
      assert(newRepeated.op.eq(oldRepeated.op))
    val untouchedIndex = if omit then 2 else 3
    assert(members(untouchedIndex).eq(fixture.original(3)))
    assert(result.directMemberIdentities(untouchedIndex).sameObjectAs(fixture.captured.members(3).identity))

    val generated = members.takeRight(2).map(_.asInstanceOf[untpd.DefDef])
    val byName = generated.map(method => method.name.toString -> method).toMap
    assertApplyBody(byName("relay"), "sum", 3, relayPath)
    assertIdentifierBody(byName("bonus"), "relay", bonusPath)
    assert(!byName("relay").source.eq(byName("bonus").source))
    assertEquals(generated.map(_.name.toString), generatedNames)
    assertPreTyperClean(result.tree)

  private def appendPair(
      plan: EditPlan,
      forward: Boolean,
      relayPath: String,
      bonusPath: String
  ): Either[Failure, EditPlan] =
    if forward then
      for
        p1 <- plan.append(generatedIdentifierMethod("bonus", "relay"), bonusPath)
        p2 <- p1.append(generatedApplyMethod("relay", "sum"), relayPath)
      yield p2
    else
      for
        p1 <- plan.append(generatedApplyMethod("relay", "sum"), relayPath)
        p2 <- p1.append(generatedIdentifierMethod("bonus", "relay"), bonusPath)
      yield p2

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

  private def assertApplyBody(
      tree: untpd.Tree,
      sibling: String,
      argumentCount: Int,
      path: String
  )(using Context): Unit =
    val generated = tree.asInstanceOf[untpd.DefDef]
    assertGeneratedOrigin(generated, path)
    generated.rhs match
      case untpd.Apply(function: untpd.Ident, arguments) =>
        assertEquals(function.name.toString, sibling)
        assertEquals(arguments.size, argumentCount)
        assert(function.source.eq(generated.source))
        assert(arguments.forall(_.source.eq(generated.source)))
      case other => fail("expected generated free-name Apply, found " + other)

  private def assertIdentifierBody(tree: untpd.Tree, sibling: String, path: String)(using Context): Unit =
    val generated = tree.asInstanceOf[untpd.DefDef]
    assertGeneratedOrigin(generated, path)
    generated.rhs match
      case identifier: untpd.Ident =>
        assertEquals(identifier.name.toString, sibling)
        assert(identifier.source.eq(generated.source))
      case other => fail("expected generated free identifier, found " + other)

  private def assertGeneratedOrigin(generated: untpd.DefDef, path: String): Unit =
    assertEquals(generated.source.path, path)
    assertEquals(generated.span.start, 0)
    assertEquals(generated.span.end, generated.source.content.length)

  private def assertOriginalOrFresh(actual: untpd.Tree, original: untpd.Tree, fresh: Boolean): Unit =
    if fresh then assert(!actual.eq(original)) else assert(actual.eq(original))

  private def captureFixture(name: String)(using Context): Fixture =
    val root = parseClass(Canonical.replace("GeneratedDependencyOwner", name))
    val captured = publicRight(ExistingClassUntypedRewrite.capture(root))
    Fixture(root, directMembers(root), captured, method(captured, 0), method(captured, 1))

  private def method(captured: Capture, index: Int)(using Context): MethodView =
    publicRight(captured.method(publicRight(captured.member(index)).ref))

  private def parameter(method: MethodView): ExistingClassUntypedRewrite.ParameterView =
    method.parameterClauses.head.parameters.head

  private def parameter(method: untpd.DefDef): untpd.ValDef =
    method.paramss.head.head.asInstanceOf[untpd.ValDef]

  private def memberNames(members: Vector[untpd.Tree]): Vector[String] =
    members.map(_.asInstanceOf[untpd.MemberDef].name.toString)

  private def directMembers(root: untpd.TypeDef)(using Context): Vector[untpd.Tree] =
    root.rhs.asInstanceOf[untpd.Template].body.toVector

  private def assertPreTyperClean(tree: untpd.Tree)(using Context): Unit =
    val trees = ExistingUntpdClassMemberFilter.allTrees(tree)
    assert(trees.forall(_.symbol == NoSymbol))
    assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U064InterGenerated.scala", source)
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

  private def assertSemanticCode[A](value: Either[DefinitionSemanticError, A], expected: String): Unit =
    value match
      case Left(problem) => assertEquals(problem.code, expected, clues(problem))
      case Right(success) => fail("expected " + expected + ", found success " + success)

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)

  private val Canonical =
    """class GeneratedDependencyOwner:
      |  def eval(lazyValue: => AnyVal): Int = 0
      |  def sum(values: AnyVal*): Int = values.size
      |  val obsolete: Int = 101
      |  val untouched: Int = 7
      |""".stripMargin
