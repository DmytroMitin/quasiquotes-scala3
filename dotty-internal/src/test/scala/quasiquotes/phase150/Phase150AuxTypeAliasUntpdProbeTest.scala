package quasiquotes.phase150

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.Param
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

import quasiquotes.parser.BinderId
import quasiquotes.phase150.AuxTypeAliasSemanticPlanProbe.*
import quasiquotes.phase150.AuxTypeAliasSemanticPlanProbe.TypeNode.*

class Phase150AuxTypeAliasUntpdProbeTest extends munit.FunSuite:
  private val Canonical =
    "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }"

  test("source-free direct lowering exactly matches the frozen parser topology") {
    withContext {
      val parsed = parseOne(Canonical)
      val raw = Phase150AuxTypeAliasUntpdProbe.lower(validPlan()).fold(error => fail(error.message), identity)

      assertEquals(structure(raw), structure(parsed))
      assertExactShape(raw, "Aux", Vector("N", "M", "Out0"), "Nat", "Add", "Out")
      allTrees(raw).foreach { tree =>
        assert(!tree.source.exists)
        assert(!tree.span.exists)
        assertEquals(tree.symbol, NoSymbol)
        assert(!tree.isInstanceOf[untpd.TypedSplice])
      }
    }
  }

  test("generated origin positions all 18 nodes without parsing or typed splices") {
    withContext {
      val result = Phase150AuxTypeAliasUntpdProbe
        .position(validPlan(), "generated/Phase150AuxTypeAlias.scala")
        .fold(error => fail(error.message), identity)

      assertEquals(result.generatedSource, Canonical)
      assertEquals(result.virtualSourceName, "generated/Phase150AuxTypeAlias.scala")
      assertExactShape(result.tree, "Aux", Vector("N", "M", "Out0"), "Nat", "Add", "Out")
      val trees = allTrees(result.tree)
      assertEquals(trees.size, 18)
      trees.foreach { tree =>
        assert(tree.source.exists)
        assertEquals(tree.source.path, result.virtualSourceName)
        assertEquals(tree.source.content.mkString, Canonical)
        assert(tree.span.exists)
        assertEquals(tree.symbol, NoSymbol)
        assert(!tree.isInstanceOf[untpd.TypedSplice])
      }
    }
  }

  test("dynamic names preserve exact structure and deterministic source") {
    withContext {
      val plan = validPlan("Evidence", "Left", "Right", "Result0", "Domain", "Combine", "Result")
      val first = Phase150AuxTypeAliasUntpdProbe.position(plan, "generated/DynamicAux.scala").fold(error => fail(error.message), identity)
      val second = Phase150AuxTypeAliasUntpdProbe.position(plan, "generated/DynamicAux.scala").fold(error => fail(error.message), identity)

      assertEquals(first.generatedSource, "type Evidence[Left <: Domain, Right <: Domain, Result0 <: Domain] = Combine[Left, Right] { type Result = Result0 }")
      assertEquals(first.generatedSource, second.generatedSource)
      assertEquals(structure(first.tree), structure(second.tree))
    }
  }

  test("raw and generated-origin failures have separate deterministic categories") {
    withContext {
      assertEquals(Phase150AuxTypeAliasUntpdProbe.lower(null).left.toOption.map(_.code), Some("RAW_LOWERING_FAILED"))
      assertEquals(
        Phase150AuxTypeAliasUntpdProbe.position(validPlan(), "bad\nsource.scala").left.toOption.map(_.code),
        Some("GENERATED_ORIGIN_FAILED")
      )
    }
  }

  private def validPlan(
      aliasName: String = "Aux",
      firstName: String = "N",
      secondName: String = "M",
      outputName: String = "Out0",
      upperName: String = "Nat",
      constructorName: String = "Add",
      memberName: String = "Out"
  ): Plan =
    val parameters = Vector(
      TypeParameter(BinderId(1), firstName, SourceName(upperName)),
      TypeParameter(BinderId(2), secondName, SourceName(upperName)),
      TypeParameter(BinderId(3), outputName, SourceName(upperName))
    )
    val applied = Applied(
      SourceName(constructorName),
      Vector(BinderReference(BinderId(1), firstName), BinderReference(BinderId(2), secondName))
    )
    AuxTypeAliasSemanticPlanProbe
      .create(aliasName, parameters, Refinement(applied, Vector(TypeAlias(memberName, BinderReference(BinderId(3), outputName)))))
      .fold(error => fail(error.message), identity)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

  private def parseOne(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed = new Parser(SourceFile.virtual("Phase150AliasOracle.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def assertExactShape(
      definition: untpd.TypeDef,
      aliasName: String,
      parameterNames: Vector[String],
      upperName: String,
      constructorName: String,
      memberName: String
  ): Unit =
    assertEquals(definition.name.toString, aliasName)
    definition.rhs match
      case lambda: untpd.LambdaTypeTree =>
        assertEquals(lambda.tparams.map(_.name.toString), parameterNames.toList)
        lambda.tparams.foreach { parameter =>
          assertEquals(parameter.mods.flags, Param)
          parameter.rhs match
            case untpd.TypeBoundsTree(lo, untpd.Ident(upper), alias) =>
              assert(lo.isEmpty)
              assert(alias.isEmpty)
              assertEquals(upper.toString, upperName)
            case other => fail(s"unexpected parameter bounds: $other")
        }
        lambda.body match
          case untpd.RefinedTypeTree(
                untpd.AppliedTypeTree(untpd.Ident(constructor), List(untpd.Ident(first), untpd.Ident(second))),
                List(member: untpd.TypeDef)
              ) =>
            assertEquals(constructor.toString, constructorName)
            assertEquals(List(first.toString, second.toString), parameterNames.take(2).toList)
            assertEquals(member.name.toString, memberName)
            member.rhs match
              case untpd.Ident(output) => assertEquals(output.toString, parameterNames(2))
              case other => fail(s"unexpected alias RHS: $other")
          case other => fail(s"unexpected alias body: $other")
      case other => fail(s"expected LambdaTypeTree, found $other")

  private def structure(tree: untpd.Tree)(using Context): String =
    tree match
      case value: untpd.TypeDef => s"TypeDef(${value.name},${value.mods.flags},${structure(value.rhs)})"
      case value: untpd.LambdaTypeTree => s"TypeLambda(${value.tparams.map(structure)},${structure(value.body)})"
      case value: untpd.TypeBoundsTree => s"TypeBounds(${structure(value.lo)},${structure(value.hi)},${structure(value.alias)})"
      case value: untpd.RefinedTypeTree => s"Refined(${structure(value.tpt)},${value.refinements.map(structure)})"
      case value: untpd.AppliedTypeTree => s"Applied(${structure(value.tpt)},${value.args.map(structure)})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.LambdaTypeTree => value.tparams.toVector :+ value.body
      case value: untpd.TypeBoundsTree => Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case _ => Vector.empty
