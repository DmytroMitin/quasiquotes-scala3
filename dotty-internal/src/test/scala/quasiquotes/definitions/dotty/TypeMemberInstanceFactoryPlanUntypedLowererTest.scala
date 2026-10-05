package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

import quasiquotes.definitions.ScopedType.*
import quasiquotes.definitions.ScopedTypeAlias
import quasiquotes.definitions.TypeMemberInstanceFactoryPlan
import quasiquotes.definitions.TypeMemberInstanceFactoryPlan.*

class TypeMemberInstanceFactoryPlanUntypedLowererTest extends munit.FunSuite:
  private final case class Names(
      factory: String,
      firstTypeParameter: String,
      secondTypeParameter: String,
      target: String,
      member: String
  ):
    val source =
      s"def $factory[$firstTypeParameter, $secondTypeParameter]: $target[$firstTypeParameter] { type $member = $secondTypeParameter } = new $target[$firstTypeParameter] { type $member = $secondTypeParameter }"

  private val canonical = Names("instance", "A", "Out0", "HasOut", "Out")
  private val renamed = Names("make", "Element", "Result0", "Container", "Result")

  test("lowers canonical and renamed plans to fresh parser-equivalent source-free trees") {
    withContext {
      Vector(canonical, renamed).foreach { names =>
        val plan = validPlan(names)
        val snapshot = plan.roleSnapshot
        val firstBinder = plan.firstTypeParameter.binderId
        val secondBinder = plan.secondTypeParameter.binderId
        val first = lower(plan)
        val second = lower(plan)

        assertEquals(structure(first), structure(parseOne(names.source)))
        assertEquals(structure(second), structure(first))
        assert(!(first eq second))
        assert(
          allTrees(first).zip(allTrees(second)).forall { case (left, right) => !(left eq right) }
        )
        assertEquals(plan.roleSnapshot, snapshot)
        assertEquals(plan.firstTypeParameter.binderId, firstBinder)
        assertEquals(plan.secondTypeParameter.binderId, secondBinder)
        assertSourceFree(first)
        assertEquals(
          TypeMemberInstanceFactoryPlanUntypedLowerer.validateCandidate(plan, first),
          Right(())
        )
      }
    }
  }

  test("validates the supplied semantic graph without recreating binder identities") {
    withContext {
      assertCode(TypeMemberInstanceFactoryPlanUntypedLowerer.lower(null), "PLAN_REQUIRED")

      val valid = validPlan(canonical)
      val firstReference = TypeParameterReference(
        valid.firstTypeParameter.binderId,
        valid.firstTypeParameter.displayName
      )
      val secondReference = TypeParameterReference(
        valid.secondTypeParameter.binderId,
        valid.secondTypeParameter.displayName
      )
      val secondTarget = Applied(SourceName(canonical.target), Vector(secondReference))
      val otherTarget = Applied(SourceName("Other"), Vector(firstReference))
      val wrongResultAlias = valid.resultAlias.copy(rhs = firstReference)
      val wrongAnonymousAlias = valid.anonymousAlias.copy(rhs = firstReference)
      val corruptions = Vector(
        copyPlan(
          valid,
          secondTypeParameter = valid.secondTypeParameter.copy(
            binderId = valid.firstTypeParameter.binderId
          )
        ),
        copyPlan(
          valid,
          secondTypeParameter = valid.secondTypeParameter.copy(
            displayName = valid.firstTypeParameter.displayName
          )
        ),
        copyPlan(valid, resultTarget = secondTarget),
        copyPlan(
          valid,
          resultType = Refinement(otherTarget, Vector(valid.resultAlias))
        ),
        copyPlan(
          valid,
          resultAlias = wrongResultAlias,
          resultType = Refinement(valid.resultTarget, Vector(wrongResultAlias))
        ),
        copyPlan(valid, anonymousParentTarget = otherTarget),
        copyPlan(valid, anonymousAlias = valid.anonymousAlias.copy(memberName = "Other")),
        copyPlan(valid, anonymousAlias = wrongAnonymousAlias),
        copyPlan(valid, anonymousAlias = valid.resultAlias),
        copyPlan(valid, roleSnapshot = valid.roleSnapshot.copy(memberSourceName = "Other"))
      )
      corruptions.foreach { corrupt =>
        assertCode(TypeMemberInstanceFactoryPlanUntypedLowerer.lower(corrupt), "PLAN_INVALID")
      }
    }
  }

  test("decodes Core-valid backticked source roles without inventing new identities") {
    withContext {
      val plan = TypeMemberInstanceFactoryPlan
        .create("`def`", "`type`", "`match`", "`class`", "`object`")
        .fold(problem => fail(problem.message), identity)
      val raw = lower(plan)

      assertEquals(raw.name.toString, "def")
      assertEquals(raw.leadingTypeParams.map(_.name.toString), List("type", "match"))
      val resultAlias = raw.tpt.asInstanceOf[untpd.RefinedTypeTree]
        .refinements.head.asInstanceOf[untpd.TypeDef]
      assertEquals(resultAlias.name.toString, "object")
      assertSourceFree(raw)
    }
  }

  test("candidate validation rejects role swaps alias forgeries and pre-Typer contamination") {
    withContext {
      val plan = validPlan(canonical)
      val raw = lower(plan)
      val parameters = raw.paramss.head.map(_.asInstanceOf[untpd.TypeDef])
      val swappedParameters = untpd.cpy.DefDef(raw)(
        paramss = List(List(parameters(1), parameters(0)))
      )

      val refined = raw.tpt.asInstanceOf[untpd.RefinedTypeTree]
      val resultAlias = refined.refinements.head.asInstanceOf[untpd.TypeDef]
      val wrongResultAlias = untpd.cpy.TypeDef(resultAlias)(
        rhs = untpd.Ident(typeName(canonical.firstTypeParameter))
      )
      val wrongResult = untpd.cpy.DefDef(raw)(
        tpt = untpd.cpy.RefinedTypeTree(refined)(
          tpt = refined.tpt,
          refinements = wrongResultAlias :: Nil
        )
      )
      val missingResult = untpd.cpy.DefDef(raw)(
        tpt = untpd.cpy.RefinedTypeTree(refined)(tpt = refined.tpt, refinements = Nil)
      )
      val duplicateResult = untpd.cpy.DefDef(raw)(
        tpt = untpd.cpy.RefinedTypeTree(refined)(
          tpt = refined.tpt,
          refinements = resultAlias :: untpd.cpy.TypeDef(resultAlias)() :: Nil
        )
      )

      val fresh = raw.rhs.asInstanceOf[untpd.New]
      val template = fresh.tpt.asInstanceOf[untpd.Template]
      val bodyAlias = template.body.head.asInstanceOf[untpd.TypeDef]
      val wrongBodyAlias = untpd.cpy.TypeDef(bodyAlias)(name = typeName("Other"))
      val wrongBody = untpd.cpy.DefDef(raw)(
        rhs = untpd.New(untpd.cpy.Template(template)(body = wrongBodyAlias :: Nil))
      )
      val missingBody = untpd.cpy.DefDef(raw)(
        rhs = untpd.New(untpd.cpy.Template(template)(body = Nil))
      )
      val duplicateBody = untpd.cpy.DefDef(raw)(
        rhs = untpd.New(
          untpd.cpy.Template(template)(body = bodyAlias :: untpd.cpy.TypeDef(bodyAlias)() :: Nil)
        )
      )

      val symbol = newSymbol(NoSymbol, typeName("U047Symbol"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(typeName(canonical.secondTypeParameter)).withType(symbol.typeRef)
      val typedSpliceAlias = untpd.cpy.TypeDef(resultAlias)(rhs = untpd.TypedSplice(symbolBearing))
      val typedSplice = untpd.cpy.DefDef(raw)(
        tpt = untpd.cpy.RefinedTypeTree(refined)(
          tpt = refined.tpt,
          refinements = typedSpliceAlias :: Nil
        )
      )

      Vector(
        swappedParameters,
        wrongResult,
        missingResult,
        duplicateResult,
        wrongBody,
        missingBody,
        duplicateBody,
        typedSplice,
        parseOne(canonical.source)
      ).foreach { forged =>
        assertCode(
          TypeMemberInstanceFactoryPlanUntypedLowerer.validateCandidate(plan, forged),
          "EXACT_RAW_INVARIANT_FAILED"
        )
      }
    }
  }

  private def validPlan(names: Names): Plan =
    TypeMemberInstanceFactoryPlan
      .create(
        names.factory,
        names.firstTypeParameter,
        names.secondTypeParameter,
        names.target,
        names.member
      )
      .fold(problem => fail(problem.message), identity)

  private def copyPlan(
      plan: Plan,
      firstTypeParameter: TypeParameter = null,
      secondTypeParameter: TypeParameter = null,
      resultType: Refinement = null,
      resultTarget: Applied = null,
      resultAlias: ScopedTypeAlias = null,
      anonymousParentTarget: Applied = null,
      anonymousAlias: ScopedTypeAlias = null,
      roleSnapshot: RoleSnapshot = null
  ): Plan =
    new Plan(
      plan.factoryDisplayName,
      Option(firstTypeParameter).getOrElse(plan.firstTypeParameter),
      Option(secondTypeParameter).getOrElse(plan.secondTypeParameter),
      Option(resultType).getOrElse(plan.resultType),
      Option(resultTarget).getOrElse(plan.resultTarget),
      Option(resultAlias).getOrElse(plan.resultAlias),
      Option(anonymousParentTarget).getOrElse(plan.anonymousParentTarget),
      Option(anonymousAlias).getOrElse(plan.anonymousAlias),
      Option(roleSnapshot).getOrElse(plan.roleSnapshot)
    )

  private def lower(plan: Plan)(using Context): untpd.DefDef =
    TypeMemberInstanceFactoryPlanUntypedLowerer
      .lower(plan)
      .fold(problem => fail(problem.message), identity)

  private def assertSourceFree(root: untpd.DefDef)(using Context): Unit =
    val trees = allTrees(root)
    assertEquals(trees.size, 19)
    trees.foreach { tree =>
      assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
      assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
      assertEquals(tree.symbol, NoSymbol)
      assert(!tree.isInstanceOf[untpd.TypedSplice])
    }

  private def assertCode[A](
      result: Either[TypeMemberInstanceFactoryPlanUntypedLoweringError, A],
      expected: String
  ): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, expected, clues(problem.detail))
      case Right(_) => fail(s"expected $expected")

  private def structure(tree: untpd.Tree)(using Context): String =
    tree match
      case value: untpd.DefDef =>
        s"DefDef(${value.name},${value.mods.flags},${value.paramss.map(_.map(structure))},${structure(value.tpt)},${structure(value.rhs)})"
      case value: untpd.TypeDef =>
        s"TypeDef(${value.name},${value.mods.flags},${structure(value.rhs)})"
      case value: untpd.TypeBoundsTree =>
        s"TypeBounds(${structure(value.lo)},${structure(value.hi)},${structure(value.alias)})"
      case value: untpd.RefinedTypeTree =>
        s"Refined(${structure(value.tpt)},${value.refinements.map(structure)})"
      case value: untpd.AppliedTypeTree =>
        s"Applied(${structure(value.tpt)},${value.args.map(structure)})"
      case value: untpd.New => s"New(${structure(value.tpt)})"
      case value: untpd.Template =>
        s"Template(${structure(value.constr)},${value.parentsOrDerived.map(structure)},${structure(value.self)},${value.body.map(structure)})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeDef => Vector(value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.Template =>
        (Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body).filterNot(_.isEmpty)
      case _ => Vector.empty

  private def parseOne(source: String)(using outerContext: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed = new Parsers.Parser(SourceFile.virtual("U047Expected.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.DefDef]

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
