package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

import quasiquotes.definitions.CurriedMethodInstanceFactoryPlan
import quasiquotes.definitions.CurriedMethodInstanceFactoryPlan.*
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

class CurriedMethodInstanceFactoryPlanUntypedLowererTest extends munit.FunSuite:
  private final case class Names(
      factory: String,
      typeParameter: String,
      carrier: String,
      target: String,
      member: String,
      firstParameter: String,
      secondParameter: String
  ):
    val source =
      s"def $factory[$typeParameter]($carrier: $typeParameter => $typeParameter => $typeParameter): $target[$typeParameter] = new $target[$typeParameter] { override def $member($firstParameter: $typeParameter)($secondParameter: $typeParameter): $typeParameter = $carrier($firstParameter)($secondParameter) }"

  private val canonical = Names("instance", "A", "combineFunction", "Curried", "combine", "a", "b")
  private val renamed = Names("make", "Element", "mergeFunction", "Aggregator", "merge", "left", "right")

  test("lowers canonical and renamed plans to fresh parser-equivalent source-free graphs") {
    withContext {
      Vector(canonical, renamed).foreach { names =>
        val plan = validPlan(names)
        val snapshot = plan.roleSnapshot
        val binders = declarationBinderIds(plan)
        val first = lower(plan)
        val second = lower(plan)
        val parsed = parseOne(names.source)

        assertEquals(structure(first), structure(parsed))
        assertEquals(structure(second), structure(parsed))
        assertSourceFree(first)
        assertSourceFree(second)
        assert(!(first eq second))
        assert(allTrees(first).zip(allTrees(second)).forall { case (left, right) => !(left eq right) })
        assertEquals(plan.roleSnapshot, snapshot)
        assertEquals(declarationBinderIds(plan), binders)
      }
    }
  }

  test("decodes Core-valid backticked roles without inventing a second lexical grammar") {
    withContext {
      val plan = CurriedMethodInstanceFactoryPlan
        .create("`def`", "`type`", "`val`", "`class`", "`object`", "`match`", "`given`")
        .fold(problem => fail(problem.message), identity)
      val raw = lower(plan)
      assertEquals(raw.name.toString, "def")
      assertSourceFree(raw)
    }
  }

  test("rejects missing and corrupted semantic plans before construction") {
    withContext {
      assertCode(CurriedMethodInstanceFactoryPlanUntypedLowerer.lower(null), "PLAN_REQUIRED")
      val valid = validPlan(canonical)
      val wrongType = TypeParameterReference(valid.strictCarrier.binderId, valid.typeParameter.displayName)
      val wrongTerm = TermReference(valid.typeParameter.binderId)
      val corruptions = Vector(
        copyPlan(valid, strictCarrier = valid.strictCarrier.copy(mode = null)),
        copyPlan(valid, strictCarrier = valid.strictCarrier.copy(parameterType = valid.strictCarrier.parameterType.copy(firstArgument = wrongType))),
        copyPlan(valid, resultTarget = Applied(SourceName("Other"), valid.resultTarget.arguments)),
        copyPlan(valid, firstParameter = valid.firstParameter.copy(parameterType = wrongType)),
        copyPlan(valid, secondParameter = valid.secondParameter.copy(parameterType = wrongType)),
        copyPlan(valid, methodOverride = valid.methodOverride.copy(resultType = wrongType)),
        copyPlan(valid, methodOverride = valid.methodOverride.copy(body = valid.methodOverride.body.copy(callee = wrongTerm))),
        copyPlan(valid, methodOverride = valid.methodOverride.copy(body = valid.methodOverride.body.copy(firstArgument = TermReference(valid.secondParameter.binderId)))),
        copyPlan(valid, roleSnapshot = valid.roleSnapshot.copy(memberSourceName = "other")),
        copyPlan(valid, firstParameter = valid.firstParameter.copy(binderId = valid.strictCarrier.binderId))
      )
      corruptions.foreach(plan => assertCode(CurriedMethodInstanceFactoryPlanUntypedLowerer.lower(plan), "PLAN_INVALID"))
    }
  }

  test("candidate validator rejects flattened swapped malformed and contaminated raw graphs") {
    withContext {
      val plan = validPlan(canonical)
      val raw = lower(plan)
      val member = raw.rhs.asInstanceOf[untpd.New].tpt.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]
      val carrier = raw.paramss(1).head.asInstanceOf[untpd.ValDef]
      val outerFunction = carrier.tpt.asInstanceOf[untpd.Function]
      val first = member.paramss.head.head.asInstanceOf[untpd.ValDef]
      val second = member.paramss(1).head.asInstanceOf[untpd.ValDef]
      val flattenedCarrier = untpd.cpy.ValDef(carrier)(
        tpt = untpd.Function(
          List(untpd.Ident(typeName(canonical.typeParameter)), untpd.Ident(typeName(canonical.typeParameter))),
          untpd.Ident(typeName(canonical.typeParameter))
        )
      )
      val wrongCarrier = untpd.cpy.DefDef(raw)(paramss = List(raw.paramss.head, flattenedCarrier :: Nil))
      val flattenedBody = untpd.cpy.DefDef(member)(
        rhs = untpd.Apply(
          untpd.Ident(termName(canonical.carrier)),
          List(untpd.Ident(termName(canonical.firstParameter)), untpd.Ident(termName(canonical.secondParameter)))
        )
      )
      val swappedBody = untpd.cpy.DefDef(member)(
        rhs = untpd.Apply(
          untpd.Apply(untpd.Ident(termName(canonical.carrier)), untpd.Ident(termName(canonical.secondParameter)) :: Nil),
          untpd.Ident(termName(canonical.firstParameter)) :: Nil
        )
      )
      val oneClause = untpd.cpy.DefDef(member)(paramss = List(List(first, second)))
      val wrongFlags = untpd
        .DefDef(member.name, member.paramss, member.tpt, member.rhs)
        .withMods(untpd.Modifiers(Flags.Method))
      val wrongFirstType = untpd.cpy.ValDef(first)(tpt = untpd.Ident(typeName("Wrong")))
      val symbol = NoSymbol
      val typedSplice = untpd.TypedSplice(untpd.Ident(typeName(canonical.typeParameter)).withType(symbol.typeRef))
      val typedSpliceMember = untpd.cpy.DefDef(member)(tpt = typedSplice)

      Vector(
        wrongCarrier,
        replaceMember(raw, flattenedBody),
        replaceMember(raw, swappedBody),
        replaceMember(raw, oneClause),
        replaceMember(raw, wrongFlags),
        replaceMember(raw, untpd.cpy.DefDef(member)(paramss = List(wrongFirstType :: Nil, second :: Nil))),
        replaceMember(raw, typedSpliceMember),
        parseOne(canonical.source)
      ).foreach { candidate =>
        assertCode(
          CurriedMethodInstanceFactoryPlanUntypedLowerer.validateCandidate(plan, candidate),
          "EXACT_RAW_INVARIANT_FAILED"
        )
      }
      assertEquals(outerFunction.args.size, 1)
    }
  }

  private def replaceMember(root: untpd.DefDef, member: untpd.DefDef)(using Context): untpd.DefDef =
    val fresh = root.rhs.asInstanceOf[untpd.New]
    val template = fresh.tpt.asInstanceOf[untpd.Template]
    val replaced = untpd.cpy.Template(template)(body = member :: Nil)
    untpd.cpy.DefDef(root)(rhs = untpd.cpy.New(fresh)(tpt = replaced))

  private def validPlan(names: Names): Plan =
    CurriedMethodInstanceFactoryPlan
      .create(
        names.factory,
        names.typeParameter,
        names.carrier,
        names.target,
        names.member,
        names.firstParameter,
        names.secondParameter
      )
      .fold(problem => fail(problem.message), identity)

  private def copyPlan(
      plan: Plan,
      strictCarrier: StrictCarrier = null,
      resultTarget: Applied = null,
      firstParameter: NestedParameter = null,
      secondParameter: NestedParameter = null,
      methodOverride: MethodOverride = null,
      roleSnapshot: RoleSnapshot = null
  ): Plan =
    new Plan(
      plan.factoryDisplayName,
      plan.typeParameter,
      Option(strictCarrier).getOrElse(plan.strictCarrier),
      Option(resultTarget).getOrElse(plan.resultTarget),
      plan.anonymousParentTarget,
      Option(firstParameter).getOrElse(plan.firstParameter),
      Option(secondParameter).getOrElse(plan.secondParameter),
      Option(methodOverride).getOrElse(plan.methodOverride),
      Option(roleSnapshot).getOrElse(plan.roleSnapshot)
    )

  private def declarationBinderIds(plan: Plan): Vector[BinderId] =
    Vector(
      plan.typeParameter.binderId,
      plan.strictCarrier.binderId,
      plan.firstParameter.binderId,
      plan.secondParameter.binderId
    )

  private def lower(plan: Plan)(using Context): untpd.DefDef =
    CurriedMethodInstanceFactoryPlanUntypedLowerer
      .lower(plan)
      .fold(problem => fail(problem.message), identity)

  private def assertSourceFree(root: untpd.DefDef)(using Context): Unit =
    val trees = allTrees(root)
    assertEquals(trees.size, 29)
    trees.foreach { tree =>
      assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
      assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
      assertEquals(tree.symbol, NoSymbol)
      assert(!tree.isInstanceOf[untpd.TypedSplice])
    }

  private def assertCode[A](
      result: Either[CurriedMethodInstanceFactoryPlanUntypedLoweringError, A],
      expected: String
  ): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, expected, clues(problem.detail))
      case Right(_) => fail(s"expected $expected")

  private def structure(tree: untpd.Tree)(using Context): String =
    tree match
      case value: untpd.DefDef =>
        s"DefDef(${value.name},${value.mods.flags},${value.paramss.map(_.map(structure))},${structure(value.tpt)},${structure(value.rhs)})"
      case value: untpd.TypeDef => s"TypeDef(${value.name},${value.mods.flags},${structure(value.rhs)})"
      case value: untpd.ValDef => s"ValDef(${value.name},${value.mods.flags},${structure(value.tpt)},${structure(value.rhs)})"
      case value: untpd.TypeBoundsTree => s"TypeBounds(${structure(value.lo)},${structure(value.hi)},${structure(value.alias)})"
      case value: untpd.Function => s"Function(${value.args.map(structure)},${structure(value.body)})"
      case value: untpd.AppliedTypeTree => s"Applied(${structure(value.tpt)},${value.args.map(structure)})"
      case value: untpd.New => s"New(${structure(value.tpt)})"
      case value: untpd.Template => s"Template(${structure(value.constr)},${value.parentsOrDerived.map(structure)},${structure(value.self)},${value.body.map(structure)})"
      case value: untpd.Apply => s"Apply(${structure(value.fun)},${value.args.map(structure)})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef => value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeDef => Vector(value.rhs).filterNot(_.isEmpty)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree => Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.Function => value.args.toVector :+ value.body
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.Template => (Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++ Vector(value.self) ++ value.body).filterNot(_.isEmpty)
      case value: untpd.Apply => value.fun +: value.args.toVector
      case _ => Vector.empty

  private def parseOne(source: String)(using outerContext: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed = new Parsers.Parser(SourceFile.virtual("U051Expected.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.DefDef]

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
