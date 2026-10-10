package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.{DefinitionKind, DefinitionModifiers, DefinitionName, DefinitionSemanticError, SemanticDefinition}
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.{Capture, EditPlan, Failure, MethodView, Result}
import quasiquotes.definitions.dotty.ExistingUntpdOrdinaryMethodTypeEditPreparation.*
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class ExistingCrossMemberCallingModeOmitAppendSafetyTest extends munit.FunSuite:
  private val IntType = TypeNormalForm.STypeIdent("Int")

  test("A through E isolate edits omission and append while preserving ordered identities") {
    withContext {
      checkCase("U062AEdits", edits = true, omit = false, append = false)
      checkCase("U062BOmit", edits = false, omit = true, append = false)
      checkCase("U062CAppend", edits = false, omit = false, append = true)
      checkCase("U062DEditsOmit", edits = true, omit = true, append = false)
      checkCase("U062EEditsAppend", edits = true, omit = false, append = true)
    }
  }

  test("F edits omission and append compose in two legal immutable plan orders") {
    withContext {
      Vector(true, false).foreach { editsFirst =>
        val fixture = captureFixture(if editsFirst then "U062FEditsFirst" else "U062FOmitAppendFirst")
        val path = if editsFirst then "generated/U062FEditsFirst.scala" else "generated/U062FOmitAppendFirst.scala"
        val plan = publicRight(
          if editsFirst then
            for
              p1 <- fixture.captured.emptyPlan.replaceParameterType(parameter(fixture.eval).ref, IntType)
              p2 <- p1.replaceParameterType(parameter(fixture.sum).ref, IntType)
              p3 <- p2.omit(fixture.captured.members(2).ref)
              p4 <- p3.append(generatedValue("bonus", "5"), path)
            yield p4
          else
            for
              p1 <- fixture.captured.emptyPlan.omit(fixture.captured.members(2).ref)
              p2 <- p1.append(generatedValue("bonus", "5"), path)
              p3 <- p2.replaceParameterType(parameter(fixture.sum).ref, IntType)
              p4 <- p3.replaceParameterType(parameter(fixture.eval).ref, IntType)
            yield p4
        )
        assertEquals(plan.editCount, 4)
        assertFullResult(fixture, publicRight(ExistingClassUntypedRewrite(fixture.captured, plan)), path)
      }
    }
  }

  test("omission index shift keeps untouched exact and generated last") {
    withContext {
      val fixture = captureFixture("U062IndexShift")
      val path = "generated/U062IndexShift.scala"
      val plan = publicRight(for
        p1 <- fixture.captured.emptyPlan.omit(fixture.captured.members(2).ref)
        p2 <- p1.append(generatedValue("bonus", "5"), path)
      yield p2)
      val result = publicRight(ExistingClassUntypedRewrite(fixture.captured, plan))
      val members = directMembers(result.tree)

      assertEquals(memberNames(members), Vector("eval", "sum", "untouched", "bonus"))
      assert(members(0).eq(fixture.original(0)))
      assert(members(1).eq(fixture.original(1)))
      assert(members(2).eq(fixture.original(3)))
      assert(!members.exists(_.eq(fixture.original(2))))
      assert(result.directMemberIdentities(2).sameObjectAs(fixture.captured.members(3).identity))
      assertGenerated(members.last, path)
    }
  }

  test("conflicts foreign stale refs and invalid generated inputs fail without partial owner exposure") {
    withContext {
      val fixture = captureFixture("U062Failures")
      val evalParameter = parameter(fixture.eval)
      val sumParameter = parameter(fixture.sum)
      val evalEdited = publicRight(fixture.captured.emptyPlan.replaceParameterType(evalParameter.ref, IntType))
      val evalOmitted = publicRight(fixture.captured.emptyPlan.omit(fixture.captured.members(0).ref))
      assertPublicCode(evalEdited.replaceParameterType(evalParameter.ref, IntType), "EDIT_CONFLICT")
      assertPublicCode(evalEdited.omit(fixture.captured.members(0).ref), "EDIT_CONFLICT")
      assertPublicCode(evalOmitted.replaceParameterType(evalParameter.ref, IntType), "EDIT_CONFLICT")

      val omitted = publicRight(fixture.captured.emptyPlan.omit(fixture.captured.members(2).ref))
      assertPublicCode(omitted.omit(fixture.captured.members(2).ref), "EDIT_CONFLICT")

      val foreign = captureFixture("U062Foreign")
      assertPublicCode(
        fixture.captured.emptyPlan.omit(foreign.captured.members(2).ref),
        "SELECTION_FAILED"
      )
      assertPublicCode(
        fixture.captured.emptyPlan.replaceParameterType(parameter(foreign.eval).ref, IntType),
        "SELECTION_FAILED"
      )

      val rewritten = publicRight(ExistingClassUntypedRewrite(
        fixture.captured,
        publicRight(fixture.captured.emptyPlan.append(
          generatedValue("firstBonus", "1"),
          "generated/U062Stale.scala"
        ))
      ))
      val recaptured = publicRight(ExistingClassUntypedRewrite.capture(rewritten.tree))
      assertPublicCode(
        recaptured.emptyPlan.omit(fixture.captured.members(2).ref),
        "SELECTION_FAILED"
      )
      assertPublicCode(
        recaptured.emptyPlan.replaceParameterType(evalParameter.ref, IntType),
        "SELECTION_FAILED"
      )

      val evalReference = publicRight(fixture.eval.parameterScope.reference(evalParameter.ref))
      assertPublicCode(
        fixture.captured.emptyPlan.replaceBody(fixture.sum.ref, evalReference),
        "SEMANTIC_FRAGMENT_INVALID"
      )
      assertPublicCode(
        fixture.captured.emptyPlan.append(generatedValue("badPath", "1"), " invalid.scala"),
        "SEMANTIC_FRAGMENT_INVALID"
      )

      val invalidGenerated = new SemanticDefinition(
        DefinitionKind.Value,
        semantic(DefinitionName.fromSource("unsupportedValue")),
        DefinitionModifiers.empty,
        storage("ValueStorage", TypeNormalForm.STypeIdent("AnyVal"), TermShape.Literal("1"))
      )
      val invalidPlan = publicRight(for
        p1 <- fixture.captured.emptyPlan.replaceParameterType(evalParameter.ref, IntType)
        p2 <- p1.replaceParameterType(sumParameter.ref, IntType)
        p3 <- p2.append(invalidGenerated, "generated/U062Unsupported.scala")
      yield p3)
      assertPublicCode(ExistingClassUntypedRewrite(fixture.captured, invalidPlan), "LOWERING_FAILED")
      assert(directMembers(fixture.root).zip(fixture.original).forall((left, right) => left.eq(right)))
      assertPreTyperClean(fixture.root)
    }
  }

  test("copied and cross-substituted calling-mode preparation evidence remains rejected") {
    withContext {
      val exact = ExistingUntpdClassMemberFilter.capture(owner("U062Evidence"))
        .fold(problem => fail(problem.message), identity)
      val evalDescriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(exact, 0)
        .fold(problem => fail(problem.message), identity)
      val sumDescriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(exact, 1)
        .fold(problem => fail(problem.message), identity)
      val byName = prepareParameterType(
        evalDescriptor,
        evalDescriptor.parameterClauses.head.head,
        IntType
      ).fold(problem => fail(problem.message), identity)
      val repeated = prepareParameterType(
        sumDescriptor,
        sumDescriptor.parameterClauses.head.head,
        IntType
      ).fold(problem => fail(problem.message), identity)

      assertPreparationCode(
        validatePreparedParameterType(byName.copy(evidence = repeated.evidence), evalDescriptor),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
      assertPreparationCode(
        validatePreparedParameterType(repeated.copy(evidence = byName.evidence), sumDescriptor),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
      assertPreparationCode(
        validatePreparedParameterType(byName, sumDescriptor),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
      assertPreparationCode(
        validatePreparedParameterType(repeated, evalDescriptor),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
    }
  }

  private final case class Fixture(
      root: untpd.TypeDef,
      original: Vector[untpd.Tree],
      captured: Capture,
      eval: MethodView,
      sum: MethodView
  )

  private def checkCase(
      name: String,
      edits: Boolean,
      omit: Boolean,
      append: Boolean
  )(using Context): Unit =
    val fixture = captureFixture(name)
    val path = s"generated/$name.scala"
    val withEdits =
      if edits then
        for
          p1 <- fixture.captured.emptyPlan.replaceParameterType(parameter(fixture.eval).ref, IntType)
          p2 <- p1.replaceParameterType(parameter(fixture.sum).ref, IntType)
        yield p2
      else Right(fixture.captured.emptyPlan)
    val withOmit = withEdits.flatMap(plan =>
      if omit then plan.omit(fixture.captured.members(2).ref) else Right(plan)
    )
    val complete = withOmit.flatMap(plan =>
      if append then plan.append(generatedValue("bonus", "5"), path) else Right(plan)
    )
    val result = publicRight(ExistingClassUntypedRewrite(fixture.captured, publicRight(complete)))
    assertMatrixResult(fixture, result, edits, omit, append, path)

  private def assertFullResult(fixture: Fixture, result: Result, path: String)(using Context): Unit =
    assertMatrixResult(fixture, result, edits = true, omit = true, append = true, path)

  private def assertMatrixResult(
      fixture: Fixture,
      result: Result,
      edits: Boolean,
      omit: Boolean,
      append: Boolean,
      path: String
  )(using Context): Unit =
    val members = directMembers(result.tree)
    val expectedNames = Vector("eval", "sum") ++
      (if omit then Vector.empty else Vector("obsolete")) ++
      Vector("untouched") ++
      (if append then Vector("bonus") else Vector.empty)
    assert(result.changed)
    assert(!result.tree.eq(fixture.root))
    assert(!result.tree.rhs.eq(fixture.root.rhs))
    assertEquals(memberNames(members), expectedNames)

    if edits then
      assertFreshMethodEnvelope(fixture.original(0), members(0))
      assertFreshMethodEnvelope(fixture.original(1), members(1))
      assertFreshByName(parameterTree(fixture.original(0)), parameterTree(members(0)))
      assertFreshRepeated(parameterTree(fixture.original(1)), parameterTree(members(1)))
      assert(!result.directMemberIdentities(0).sameObjectAs(fixture.captured.members(0).identity))
      assert(!result.directMemberIdentities(1).sameObjectAs(fixture.captured.members(1).identity))
    else
      assert(members(0).eq(fixture.original(0)))
      assert(members(1).eq(fixture.original(1)))
      assert(result.directMemberIdentities(0).sameObjectAs(fixture.captured.members(0).identity))
      assert(result.directMemberIdentities(1).sameObjectAs(fixture.captured.members(1).identity))

    val untouchedIndex = if omit then 2 else 3
    assert(members(untouchedIndex).eq(fixture.original(3)))
    assert(result.directMemberIdentities(untouchedIndex).sameObjectAs(fixture.captured.members(3).identity))
    if omit then assert(!members.exists(_.eq(fixture.original(2))))
    else
      assert(members(2).eq(fixture.original(2)))
      assert(result.directMemberIdentities(2).sameObjectAs(fixture.captured.members(2).identity))
    if append then
      assertGenerated(members.last, path)
      assert(result.directMemberIdentities.last.sameObjectAs(
        new ExistingClassUntypedRewrite.ExactIdentity(members.last)
      ))
    assertEquals(result.directMemberIdentities.size, members.size)
    assertPreTyperClean(result.tree)

  private def assertFreshByName(original: untpd.ValDef, rewritten: untpd.ValDef): Unit =
    val oldWrapper = original.tpt.asInstanceOf[untpd.ByNameTypeTree]
    val newWrapper = rewritten.tpt.asInstanceOf[untpd.ByNameTypeTree]
    assert(!rewritten.eq(original))
    assert(rewritten.mods.eq(original.mods))
    assertEquals(rewritten.mods.flags, Flags.Param)
    assert(!newWrapper.eq(oldWrapper))
    assertEquals((newWrapper.source, newWrapper.span), (oldWrapper.source, oldWrapper.span))
    assertFreshIdent(newWrapper.result, oldWrapper.result)

  private def assertFreshMethodEnvelope(
      original: untpd.Tree,
      rewritten: untpd.Tree
  )(using Context): Unit =
    val oldMethod = original.asInstanceOf[untpd.DefDef]
    val newMethod = rewritten.asInstanceOf[untpd.DefDef]
    assert(!newMethod.eq(oldMethod))
    assert(newMethod.mods.eq(oldMethod.mods))
    assertEquals(newMethod.mods.flags, Flags.Method)
    assertEquals((newMethod.source, newMethod.span), (oldMethod.source, oldMethod.span))
    assert(newMethod.tpt.eq(oldMethod.tpt))
    assert(newMethod.rhs.eq(oldMethod.rhs))

  private def assertFreshRepeated(original: untpd.ValDef, rewritten: untpd.ValDef): Unit =
    val oldWrapper = original.tpt.asInstanceOf[untpd.PostfixOp]
    val newWrapper = rewritten.tpt.asInstanceOf[untpd.PostfixOp]
    val (oldElement, oldMarker) = oldWrapper match
      case untpd.PostfixOp(element, marker) => (element, marker)
    val (newElement, newMarker) = newWrapper match
      case untpd.PostfixOp(element, marker) => (element, marker)
    assert(!rewritten.eq(original))
    assert(rewritten.mods.eq(original.mods))
    assertEquals(rewritten.mods.flags, Flags.Param)
    assert(!newWrapper.eq(oldWrapper))
    assertEquals((newWrapper.source, newWrapper.span), (oldWrapper.source, oldWrapper.span))
    assertFreshIdent(newElement, oldElement)
    assert(newMarker.eq(oldMarker))

  private def assertFreshIdent(actual: untpd.Tree, original: untpd.Tree): Unit =
    actual match
      case ident: untpd.Ident =>
        assertEquals(ident.name.toString, "Int")
        assert(!ident.eq(original))
        assertEquals((ident.source, ident.span), (original.source, original.span))
      case other => fail("expected fresh Int Ident, found " + other)

  private def assertGenerated(tree: untpd.Tree, path: String): Unit =
    val generated = tree.asInstanceOf[untpd.ValDef]
    assertEquals(generated.name.toString, "bonus")
    assertEquals(generated.source.path, path)
    assertEquals(generated.span.start, 0)
    assertEquals(generated.span.end, generated.source.content.length)
    assert(generated.source.content.mkString.contains("5"))

  private def captureFixture(name: String)(using Context): Fixture =
    val root = owner(name)
    val captured = capture(root)
    Fixture(root, directMembers(root), captured, method(captured, 0), method(captured, 1))

  private def owner(name: String)(using Context): untpd.TypeDef =
    parseClass(
      s"class $name:\n" +
        "  def eval(lazyValue: => AnyVal): Int = 0\n" +
        "  def sum(values: AnyVal*): Int = values.size\n" +
        "  val obsolete: Int = 101\n" +
        "  val untouched: Int = 7\n"
    )

  private def generatedValue(label: String, literal: String): SemanticDefinition =
    semantic(SemanticDefinition.immutableValue(
      semantic(DefinitionName.fromSource(label)),
      IntType,
      TermShape.Literal(literal)
    ))

  private def memberNames(members: Vector[untpd.Tree]): Vector[String] =
    members.map(_.asInstanceOf[untpd.MemberDef].name.toString)

  private def directMembers(root: untpd.TypeDef)(using Context): Vector[untpd.Tree] =
    root.rhs.asInstanceOf[untpd.Template].body.toVector

  private def parameterTree(method: untpd.Tree): untpd.ValDef =
    parameter(method.asInstanceOf[untpd.DefDef])

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
    val unit = CompilationUnit("U062Safety.scala", source)
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

  private def assertPreparationCode[A](
      value: Either[ExistingUntpdOrdinaryMethodTypeEditPreparation.Error, A],
      expected: String
  ): Unit =
    value match
      case Left(problem) => assertEquals(problem.code, expected)
      case Right(success) => fail("expected " + expected + ", found success " + success)

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)
