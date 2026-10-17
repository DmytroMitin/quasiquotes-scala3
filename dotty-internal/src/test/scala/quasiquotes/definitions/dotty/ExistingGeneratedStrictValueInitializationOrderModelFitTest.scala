package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.{DefinitionKind, DefinitionModifiers, DefinitionName, DefinitionSemanticError, SemanticDefinition}
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.{Capture, Failure, MethodView}
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class ExistingGeneratedStrictValueInitializationOrderModelFitTest extends munit.FunSuite:
  test("public value and method models lower exact raw references with independent origins") {
    withContext {
      val seed = generatedLiteralValue("seed", "7")
      val bonusValue = generatedIdentifierValue("bonus", "seed")
      val relayValue = generatedIdentifierMethod("relay", "seed")
      val relayCall = generatedApplyMethod("relay", "sum")

      assertEquals(seed.kind, DefinitionKind.Value)
      assertEquals(seed.asValue.flatMap(_.body), Some(TermShape.Literal("7")))
      assertEquals(bonusValue.asValue.flatMap(_.body), Some(TermShape.Identifier("seed", false)))
      assertEquals(relayValue.asMethod.flatMap(_.body), Some(TermShape.Identifier("seed", false)))
      relayCall.asMethod.flatMap(_.body).getOrElse(fail("missing method body")) match
        case TermShape.Apply(TermShape.Identifier("sum", false), arguments) =>
          assertEquals(arguments, List("10", "20", "30").map(TermShape.Literal(_)))
        case other => fail("unexpected relay body: " + other)

      val loweredSeed = lower(seed, "generated/U070SemanticSeed.scala")
      val loweredBonus = lower(bonusValue, "generated/U070SemanticBonus.scala")
      val loweredRelay = lower(relayCall, "generated/U070SemanticRelay.scala")

      assertEquals(loweredSeed.generatedSource, "val seed: Int = 7")
      assertEquals(loweredBonus.generatedSource, "val bonus: Int = seed")
      assertEquals(loweredRelay.generatedSource, "def relay: Int = sum(10, 20, 30)")
      assertLiteralValue(loweredSeed.tree, "seed", "7", loweredSeed.virtualSourceName)
      assertIdentifierMember(loweredBonus.tree, "bonus", "seed", loweredBonus.virtualSourceName)
      assertApplyMethod(loweredRelay.tree, "relay", "sum", loweredRelay.virtualSourceName)
      assert(!loweredSeed.sourceFile.eq(loweredBonus.sourceFile))
      assert(!loweredBonus.sourceFile.eq(loweredRelay.sourceFile))
      Vector(loweredSeed.tree, loweredBonus.tree, loweredRelay.tree).foreach(assertPreTyperClean)
    }
  }

  test("dependency-first and forward strict-value plans retain caller order and untouched identity") {
    withContext {
      checkValueOrder("U070StructuralDependency", forward = false)
      checkValueOrder("U070StructuralForward", forward = true)
    }
  }

  test("invalid path unsupported RHS stale refs scope leakage atomic failure and no-op stay fail-closed") {
    withContext {
      val fixture = captureFixture("U070StructuralFailures")
      val foreign = captureFixture("U070StructuralForeign")
      val sumParameter = fixture.sum.parameterClauses.head.parameters.head
      val foreignParameter = foreign.sum.parameterClauses.head.parameters.head

      assertPublicCode(
        fixture.captured.emptyPlan.append(
          generatedIdentifierValue("bonus", "untouched"),
          " invalid/U070.scala"
        ),
        "SEMANTIC_FRAGMENT_INVALID"
      )
      assertPublicCode(
        fixture.captured.emptyPlan.replaceParameterType(foreignParameter.ref, IntType),
        "SELECTION_FAILED"
      )
      assertPublicCode(
        fixture.sum.parameterScope.reference(foreignParameter.ref),
        "SELECTION_FAILED"
      )

      val boundReference = publicRight(fixture.sum.parameterScope.reference(sumParameter.ref))
      val leaked = semantic(SemanticDefinition.immutableValue(
        semantic(DefinitionName.fromSource("leaked")),
        IntType,
        boundReference
      ))
      val leakedFailure = DefinitionGeneratedOriginLowering
        .lower(leaked, "generated/U070Leaked.scala")
        .left.toOption.getOrElse(fail("expected leaked bound-reference lowering failure"))
      assertEquals(
        leakedFailure.code,
        "UNSUPPORTED_SEMANTIC_VALUE"
      )

      val unsupported = SemanticDefinition.immutableValue(
        semantic(DefinitionName.fromSource("unsupported")),
        IntType,
        TermShape.Unsupported("u070", "unsupported genuine RHS")
      )
      unsupported match
        case Left(problem) => assertEquals(problem.code, "DEFINITION_SEMANTIC_UNSUPPORTED")
        case Right(value) =>
          val failure = DefinitionGeneratedOriginLowering
            .lower(value, "generated/U070Unsupported.scala")
            .left.toOption.getOrElse(fail("expected unsupported lowering failure"))
          assertEquals(failure.code, "SEMANTIC_ADAPTER_FAILED")

      val invalidGenerated = new SemanticDefinition(
        DefinitionKind.Value,
        semantic(DefinitionName.fromSource("invalidValue")),
        DefinitionModifiers.empty,
        storage("ValueStorage", TypeNormalForm.STypeIdent("AnyVal"), TermShape.Literal("1"))
      )
      val atomicPlan = publicRight(for
        p1 <- fixture.captured.emptyPlan.replaceParameterType(sumParameter.ref, IntType)
        p2 <- p1.append(
          generatedIdentifierMethod("relay", "sum"),
          "generated/U070AtomicRelay.scala"
        )
        p3 <- p2.append(invalidGenerated, "generated/U070AtomicInvalid.scala")
      yield p3)
      assertPublicCode(ExistingClassUntypedRewrite(fixture.captured, atomicPlan), "LOWERING_FAILED")
      assert(directMembers(fixture.root).zip(fixture.original).forall((left, right) => left.eq(right)))

      val noop = publicRight(ExistingClassUntypedRewrite(
        fixture.captured,
        fixture.captured.emptyPlan
      ))
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
      sum: MethodView
  )

  private def checkValueOrder(name: String, forward: Boolean)(using Context): Unit =
    val fixture = captureFixture(name)
    val seed = generatedLiteralValue("seed", "7")
    val bonus = generatedIdentifierValue("bonus", "seed")
    val seedPath = s"generated/${name}Seed.scala"
    val bonusPath = s"generated/${name}Bonus.scala"
    val plan = publicRight(
      if forward then
        for
          p1 <- fixture.captured.emptyPlan.append(bonus, bonusPath)
          p2 <- p1.append(seed, seedPath)
        yield p2
      else
        for
          p1 <- fixture.captured.emptyPlan.append(seed, seedPath)
          p2 <- p1.append(bonus, bonusPath)
        yield p2
    )
    val result = publicRight(ExistingClassUntypedRewrite(fixture.captured, plan))
    val members = directMembers(result.tree)
    val generated = members.takeRight(2).map(_.asInstanceOf[untpd.ValDef])
    val expectedNames = if forward then Vector("bonus", "seed") else Vector("seed", "bonus")
    val expectedPaths = if forward then Vector(bonusPath, seedPath) else Vector(seedPath, bonusPath)

    assertEquals(generated.map(_.name.toString), expectedNames)
    assertEquals(generated.map(_.source.path), expectedPaths)
    assert(!generated.head.source.eq(generated.last.source))
    assert(generated.forall(member => !member.source.eq(fixture.root.source)))
    assert(generated.zip(expectedPaths).forall { (member, path) =>
      member.source.path == path && member.span.start == 0 &&
        member.span.end == member.source.content.length
    })
    val byName = generated.map(member => member.name.toString -> member).toMap
    assertLiteralValue(byName("seed"), "seed", "7", seedPath)
    assertIdentifierMember(byName("bonus"), "bonus", "seed", bonusPath)
    assert(members(0).eq(fixture.original(0)))
    assert(members(1).eq(fixture.original(1)))
    assert(result.directMemberIdentities(0).sameObjectAs(fixture.captured.members(0).identity))
    assert(result.directMemberIdentities(1).sameObjectAs(fixture.captured.members(1).identity))
    assertPreTyperClean(result.tree)

  private def assertLiteralValue(
      tree: untpd.Tree,
      name: String,
      literal: String,
      path: String
  )(using Context): Unit =
    tree match
      case value: untpd.ValDef =>
        assertEquals(value.name.toString, name)
        assertGeneratedOrigin(value, path)
        value.rhs match
          case untpd.Number(digits, untpd.NumberKind.Whole(10)) => assertEquals(digits, literal)
          case other => fail("expected generated number, found " + other)
      case other => fail("expected generated ValDef, found " + other)

  private def assertIdentifierMember(
      tree: untpd.Tree,
      name: String,
      sibling: String,
      path: String
  )(using Context): Unit =
    val member = tree.asInstanceOf[untpd.MemberDef]
    assertEquals(member.name.toString, name)
    assertGeneratedOrigin(member, path)
    val rhs = member match
      case value: untpd.ValDef => value.rhs
      case method: untpd.DefDef => method.rhs
    rhs match
      case identifier: untpd.Ident =>
        assertEquals(identifier.name.toString, sibling)
        assert(identifier.source.eq(member.source))
      case other => fail("expected generated identifier, found " + other)

  private def assertApplyMethod(
      tree: untpd.Tree,
      name: String,
      sibling: String,
      path: String
  )(using Context): Unit =
    tree match
      case method: untpd.DefDef =>
        assertEquals(method.name.toString, name)
        assertGeneratedOrigin(method, path)
        method.rhs match
          case untpd.Apply(function: untpd.Ident, arguments) =>
            assertEquals(function.name.toString, sibling)
            assertEquals(arguments.size, 3)
            assert(function.source.eq(method.source))
            assert(arguments.forall(_.source.eq(method.source)))
          case other => fail("expected generated Apply, found " + other)
      case other => fail("expected generated DefDef, found " + other)

  private def assertGeneratedOrigin(member: untpd.MemberDef, path: String): Unit =
    assertEquals(member.source.path, path)
    assertEquals(member.span.start, 0)
    assertEquals(member.span.end, member.source.content.length)

  private def assertPreTyperClean(tree: untpd.Tree)(using Context): Unit =
    val trees = ExistingUntpdClassMemberFilter.allTrees(tree)
    assert(trees.forall(_.symbol == NoSymbol))
    assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))

  private def lower(
      definition: SemanticDefinition,
      path: String
  )(using Context): DefinitionGeneratedOriginLowering.Lowered =
    DefinitionGeneratedOriginLowering
      .lower(definition, path)
      .fold(problem => fail(problem.message), identity)

  private def generatedLiteralValue(label: String, literal: String): SemanticDefinition =
    semantic(SemanticDefinition.immutableValue(
      semantic(DefinitionName.fromSource(label)),
      IntType,
      TermShape.Literal(literal)
    ))

  private def generatedIdentifierValue(label: String, sibling: String): SemanticDefinition =
    semantic(SemanticDefinition.immutableValue(
      semantic(DefinitionName.fromSource(label)),
      IntType,
      TermShape.Identifier(sibling, false)
    ))

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

  private def captureFixture(name: String)(using Context): Fixture =
    val root = parseClass(Canonical.replace("GeneratedValueOwner", name))
    val captured = publicRight(ExistingClassUntypedRewrite.capture(root))
    val sum = publicRight(captured.method(publicRight(captured.member(0)).ref))
    Fixture(root, directMembers(root), captured, sum)

  private def directMembers(root: untpd.TypeDef)(using Context): Vector[untpd.Tree] =
    root.rhs.asInstanceOf[untpd.Template].body.toVector

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U070GeneratedStrictValue.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def storage(suffix: String, arguments: AnyRef*): AnyRef =
    val constructor = Class.forName(s"quasiquotes.definitions.SemanticDefinition$$$suffix")
      .getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor.newInstance(arguments*).asInstanceOf[AnyRef]

  private def semantic[A](value: Either[DefinitionSemanticError, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def publicRight[A](value: Either[Failure, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def assertPublicCode[A](value: Either[Failure, A], expected: String): Unit =
    value match
      case Left(problem) => assertEquals(problem.code, expected, clues(problem))
      case Right(success) => fail("expected " + expected + ", found success " + success)

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)

  private val IntType = TypeNormalForm.STypeIdent("Int")

  private val Canonical =
    """class GeneratedValueOwner:
      |  def sum(values: Int*): Int = values.size
      |  val untouched: Int = 7
      |""".stripMargin
