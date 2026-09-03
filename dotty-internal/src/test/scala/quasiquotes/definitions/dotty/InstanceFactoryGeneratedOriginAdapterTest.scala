package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.InstanceFactoryPlan
import quasiquotes.definitions.InstanceFactoryPlan.*
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

class InstanceFactoryGeneratedOriginAdapterTest extends munit.FunSuite:
  private final case class Names(
      factory: String,
      typeParameter: String,
      emptyCarrier: String,
      functionCarrier: String,
      target: String,
      emptyMember: String,
      combineMember: String,
      firstNested: String,
      secondNested: String
  ):
    val source =
      s"def $factory[$typeParameter]($emptyCarrier: => $typeParameter, $functionCarrier: ($typeParameter, $typeParameter) => $typeParameter): $target[$typeParameter] = new $target[$typeParameter] { override def $emptyMember: $typeParameter = $emptyCarrier; override def $combineMember($firstNested: $typeParameter, $secondNested: $typeParameter): $typeParameter = $functionCarrier($firstNested, $secondNested) }"

  private val canonical =
    Names("instance", "A", "emptyValue", "combineFunction", "Monoid", "empty", "combine", "a", "a1")
  private val renamed =
    Names("make", "Element", "fallbackValue", "selection", "Choice", "fallback", "select", "left", "right")

  test("renders deterministic complete generated origins without mutating source-free lowering") {
    withContext {
      Vector(canonical, renamed).zipWithIndex.foreach { case (names, index) =>
        val plan = validPlan(names)
        val sourceFree = InstanceFactoryPlanUntypedLowerer
          .lower(plan)
          .fold(problem => fail(problem.message), identity)
        val before = structure(sourceFree)
        val virtualName = s"<quasiquotes-generated:u017-instance-factory-$index>"
        val first = InstanceFactoryGeneratedOriginAdapter
          .lower(plan, virtualName)
          .fold(problem => fail(problem.message), identity)
        val second = InstanceFactoryGeneratedOriginAdapter
          .lower(plan, virtualName)
          .fold(problem => fail(problem.message), identity)

        assertEquals(first.generatedSource, names.source)
        assertEquals(second.generatedSource, names.source)
        assertEquals(first.virtualSourceName, virtualName)
        assertEquals(structure(first.tree), before)
        assertEquals(structure(second.tree), before)
        assertEquals(structure(sourceFree), before)
        assert(InstanceFactoryPlanUntypedLowerer.allTrees(sourceFree).forall(tree =>
          !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol
        ))

        val positioned = InstanceFactoryPlanUntypedLowerer.allTrees(first.tree)
        assertEquals(positioned.size, 33)
        positioned.foreach { tree =>
          assert(tree.source.exists, clues(tree.getClass.getSimpleName))
          assertEquals(tree.source.path, virtualName)
          assertEquals(tree.source.content.mkString, names.source)
          assert(tree.span.exists, clues(tree.getClass.getSimpleName))
          assert(tree.span.start >= 0)
          assert(tree.span.start <= tree.span.point)
          assert(tree.span.point <= tree.span.end)
          assert(tree.span.end <= names.source.length)
          assertEquals(tree.symbol, NoSymbol)
          assert(!tree.isInstanceOf[untpd.TypedSplice])
          InstanceFactoryPlanUntypedLowerer.directChildren(tree).filterNot(_.isEmpty).foreach { child =>
            assert(child.span.start >= tree.span.start)
            assert(child.span.end <= tree.span.end)
          }
        }
        assertEquals(first.tree.span.start, 0)
        assertEquals(first.tree.span.end, names.source.length)
      }
    }
  }

  test("rejects missing inputs and invalid virtual source names deterministically") {
    withContext {
      assertEquals(
        InstanceFactoryGeneratedOriginAdapter
          .lower(null, "<quasiquotes-generated:u017-null>")
          .left
          .toOption
          .map(_.code),
        Some("PLAN_REQUIRED")
      )
      val plan = validPlan(canonical)
      Vector(null, "", "bad\nname.scala", "bad\rname.scala", "bad\u0000name.scala")
        .foreach { virtualName =>
          assertEquals(
            InstanceFactoryGeneratedOriginAdapter
              .lower(plan, virtualName)
              .left
              .toOption
              .map(_.code),
            Some("GENERATED_ORIGIN_INVALID")
          )
        }
    }
  }

  private def validPlan(names: Names): Plan =
    val typeBinder = BinderId(0)
    val emptyBinder = BinderId(1)
    val functionBinder = BinderId(2)
    val firstBinder = BinderId(3)
    val secondBinder = BinderId(4)
    val typeReference = TypeParameterReference(typeBinder, names.typeParameter)
    InstanceFactoryPlan
      .create(
        names.factory,
        TypeParameter(typeBinder, names.typeParameter),
        ByNameCarrier(
          emptyBinder,
          names.emptyCarrier,
          ParameterMode.ByName,
          ValueType(typeReference)
        ),
        BinaryFunctionCarrier(
          functionBinder,
          names.functionCarrier,
          ParameterMode.ByValue,
          BinaryFunctionType(typeReference, typeReference, typeReference)
        ),
        Applied(SourceName(names.target), Vector(typeReference)),
        EmptyOverride(names.emptyMember, TermReference(emptyBinder)),
        CombineOverride(
          names.combineMember,
          NestedParameter(firstBinder, names.firstNested, typeReference),
          NestedParameter(secondBinder, names.secondNested, typeReference),
          typeReference,
          CombineBody(
            TermReference(functionBinder),
            Vector(TermReference(firstBinder), TermReference(secondBinder))
          )
        )
      )
      .fold(problem => fail(problem.message), identity)

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
      case value: untpd.ByNameTypeTree => s"ByName(${structure(value.result)})"
      case value: untpd.Function =>
        s"Function(${value.args.map(structure)},${structure(value.body)})"
      case value: untpd.AppliedTypeTree =>
        s"Applied(${structure(value.tpt)},${value.args.map(structure)})"
      case value: untpd.Apply =>
        s"Apply(${structure(value.fun)},${value.args.map(structure)})"
      case value: untpd.New => s"New(${structure(value.tpt)})"
      case value: untpd.Template =>
        s"Template(${structure(value.constr)},${value.parentsOrDerived.map(structure)},${structure(value.self)},${value.body.map(structure)})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
