package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

import quasiquotes.definitions.DelegatedForwardingMethodPlan.{
  ContextualParameter,
  ContextualReference,
  MethodIdentity,
  TypeParameter
}
import quasiquotes.definitions.ParameterlessDelegatedForwardingPlan
import quasiquotes.definitions.ParameterlessDelegatedForwardingPlan.{
  Plan,
  RoleSnapshot,
  ValidatedBody
}
import quasiquotes.definitions.ScopedType.*

class ParameterlessDelegatedForwardingMethodUntypedLowererTest extends munit.FunSuite:
  private final case class Names(
      method: String,
      typeParameter: String,
      contextualParameter: String,
      evidenceConstructor: String
  ):
    val source =
      s"def $method[$typeParameter](using $contextualParameter: $evidenceConstructor[$typeParameter]): $typeParameter = $contextualParameter.$method"

  private val canonical = Names("empty", "A", "inst", "Empty")
  private val renamed = Names("obtain", "Element", "evidence", "Provider")

  test("lowers canonical and renamed plans to fresh parser-equivalent source-free trees") {
    withContext {
      Vector(canonical, renamed).foreach { names =>
        val plan = validPlan(names)
        val snapshot = plan.roleSnapshot
        val typeBinder = plan.typeParameter.binderId
        val contextualBinder = plan.contextualParameter.binderId
        val first = lower(plan)
        val second = lower(plan)

        assertEquals(structure(first), structure(parseOne(names.source)))
        assertEquals(structure(second), structure(first))
        assert(!(first eq second))
        assert(allTrees(first).zip(allTrees(second)).forall { case (left, right) => !(left eq right) })
        assertEquals(plan.roleSnapshot, snapshot)
        assertEquals(plan.typeParameter.binderId, typeBinder)
        assertEquals(plan.contextualParameter.binderId, contextualBinder)
        assertSourceFree(first)
        assertEquals(
          ParameterlessDelegatedForwardingMethodUntypedLowerer.validateCandidate(plan, first),
          Right(())
        )
      }
    }
  }

  test("validates the supplied graph directly and rejects semantic identity corruption") {
    withContext {
      assertCode(ParameterlessDelegatedForwardingMethodUntypedLowerer.lower(null), "PLAN_REQUIRED")

      val valid = validPlan(canonical)
      val foreignTypeReference = TypeParameterReference(
        valid.contextualParameter.binderId,
        valid.typeParameter.displayName
      )
      val corruptions = Vector(
        copyPlan(
          valid,
          contextualParameter = valid.contextualParameter.copy(
            binderId = valid.typeParameter.binderId
          )
        ),
        copyPlan(valid, evidenceTypeArgument = foreignTypeReference),
        copyPlan(valid, resultType = foreignTypeReference),
        copyPlan(
          valid,
          body = valid.body.copy(
            receiver = ContextualReference(valid.typeParameter.binderId)
          )
        ),
        copyPlan(
          valid,
          body = valid.body.copy(
            selectedMethodIdentity = new MethodIdentity(valid.methodIdentity.sourceName)
          )
        ),
        copyPlan(
          valid,
          contextualParameter = valid.contextualParameter.copy(
            parameterType = Applied(SourceName("Other"), Vector(valid.evidenceTypeArgument))
          )
        ),
        copyPlan(valid, roleSnapshot = valid.roleSnapshot.copy(methodSourceName = "other"))
      )

      corruptions.foreach { corrupt =>
        assertCode(
          ParameterlessDelegatedForwardingMethodUntypedLowerer.lower(corrupt),
          "PLAN_INVALID"
        )
      }
    }
  }

  test("decodes Core-valid backticked roles without replacing graph identities") {
    withContext {
      val plan = ParameterlessDelegatedForwardingPlan
        .create("`def`", "`type`", "`given`", "`class`")
        .fold(problem => fail(problem.message), identity)
      val raw = lower(plan)

      assertEquals(raw.name.toString, "def")
      assertEquals(raw.leadingTypeParams.map(_.name.toString), List("type"))
      assertEquals(raw.trailingParamss.head.head.name.toString, "given")
      assertSourceFree(raw)
    }
  }

  test("candidate validation rejects clause body role and pre-Typer contamination") {
    withContext {
      val plan = validPlan(renamed)
      val raw = lower(plan)
      val contextual = raw.trailingParamss.head.head.asInstanceOf[untpd.ValDef]
      val ordinary = untpd
        .ValDef(termName("value"), untpd.Ident(typeName(renamed.typeParameter)), untpd.EmptyTree)
        .withMods(untpd.Modifiers(dotty.tools.dotc.core.Flags.Param))
      val ordinaryClause = untpd.cpy.DefDef(raw)(
        paramss = raw.leadingTypeParams :: List(ordinary) :: List(contextual) :: Nil
      )
      val application = untpd.cpy.DefDef(raw)(rhs = untpd.Apply(raw.rhs, Nil))
      val wrongReceiver = untpd.cpy.DefDef(raw)(
        rhs = untpd.Select(untpd.Ident(termName("other")), termName(renamed.method))
      )
      val wrongMember = untpd.cpy.DefDef(raw)(
        rhs = untpd.Select(
          untpd.Ident(termName(renamed.contextualParameter)),
          termName("other")
        )
      )
      val symbol = newSymbol(NoSymbol, typeName("U048Symbol"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(typeName(renamed.typeParameter)).withType(symbol.typeRef)
      val typedSplice = untpd.cpy.DefDef(raw)(tpt = untpd.TypedSplice(symbolBearing))

      Vector(
        ordinaryClause,
        application,
        wrongReceiver,
        wrongMember,
        typedSplice,
        parseOne(renamed.source)
      ).foreach { forged =>
        assertCode(
          ParameterlessDelegatedForwardingMethodUntypedLowerer.validateCandidate(plan, forged),
          "EXACT_RAW_INVARIANT_FAILED"
        )
      }
    }
  }

  private def validPlan(names: Names): Plan =
    ParameterlessDelegatedForwardingPlan
      .create(
        names.method,
        names.typeParameter,
        names.contextualParameter,
        names.evidenceConstructor
      )
      .fold(problem => fail(problem.message), identity)

  private def copyPlan(
      plan: Plan,
      typeParameter: TypeParameter = null,
      contextualParameter: ContextualParameter = null,
      evidenceTypeArgument: TypeParameterReference = null,
      resultType: TypeParameterReference = null,
      body: ValidatedBody = null,
      roleSnapshot: RoleSnapshot = null
  ): Plan =
    new Plan(
      plan.methodIdentity,
      Option(typeParameter).getOrElse(plan.typeParameter),
      Option(contextualParameter).getOrElse(plan.contextualParameter),
      Option(evidenceTypeArgument).getOrElse(plan.evidenceTypeArgument),
      Option(resultType).getOrElse(plan.resultType),
      Option(body).getOrElse(plan.body),
      Option(roleSnapshot).getOrElse(plan.roleSnapshot)
    )

  private def lower(plan: Plan)(using Context): untpd.DefDef =
    ParameterlessDelegatedForwardingMethodUntypedLowerer
      .lower(plan)
      .fold(problem => fail(problem.message), identity)

  private def assertSourceFree(root: untpd.DefDef)(using Context): Unit =
    val trees = allTrees(root)
    assertEquals(trees.size, 10)
    trees.foreach { tree =>
      assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
      assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
      assertEquals(tree.symbol, NoSymbol)
      assert(!tree.isInstanceOf[untpd.TypedSplice])
    }

  private def assertCode[A](
      result: Either[ParameterlessDelegatedForwardingMethodUntypedLoweringError, A],
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
      case value: untpd.ValDef =>
        s"ValDef(${value.name},${value.mods.flags},${structure(value.tpt)},${structure(value.rhs)})"
      case value: untpd.TypeBoundsTree =>
        s"TypeBounds(${structure(value.lo)},${structure(value.hi)},${structure(value.alias)})"
      case value: untpd.AppliedTypeTree =>
        s"Applied(${structure(value.tpt)},${value.args.map(structure)})"
      case value: untpd.Select => s"Select(${structure(value.qualifier)},${value.name})"
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
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def parseOne(source: String)(using outerContext: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed = new Parsers.Parser(SourceFile.virtual("U048Expected.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.DefDef]

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
