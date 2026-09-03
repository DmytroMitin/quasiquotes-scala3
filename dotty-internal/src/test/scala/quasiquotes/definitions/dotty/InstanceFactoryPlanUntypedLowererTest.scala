package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.definitions.InstanceFactoryPlan
import quasiquotes.definitions.InstanceFactoryPlan.*
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

class InstanceFactoryPlanUntypedLowererTest extends munit.FunSuite:
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

  test("lowers canonical and fully renamed plans to parser-equivalent source-free raw trees") {
    withContext {
      Vector(canonical, renamed).foreach { names =>
        val plan = validPlan(names)
        val raw = InstanceFactoryPlanUntypedLowerer
          .lower(plan)
          .fold(problem => fail(problem.message), identity)
        assertEquals(structure(raw), structure(parseOne(names.source)))
        val trees = allTrees(raw)
        assertEquals(trees.size, 33)
        trees.foreach { tree =>
          assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
          assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
          assertEquals(tree.symbol, NoSymbol)
          assert(!tree.isInstanceOf[untpd.TypedSplice])
        }
      }
    }
  }

  test("uses BinderId roles and rejects impossible or corrupt plan carriers") {
    withContext {
      assertEquals(
        InstanceFactoryPlanUntypedLowerer.lower(null),
        Left(InstanceFactoryPlanUntypedLoweringError("PLAN_REQUIRED", "the accepted InstanceFactoryPlan must be present."))
      )

      val collisionNames = canonical.copy(
        functionCarrier = "same",
        firstNested = "same"
      )
      val collision = validPlan(collisionNames)
      assertEquals(
        InstanceFactoryPlanUntypedLowerer.lower(collision).left.toOption.map(_.code),
        Some("TERM_SCOPE_COLLISION")
      )

      val valid = validPlan(canonical)
      val corrupt = new Plan(
        valid.factoryDisplayName,
        valid.typeParameter,
        valid.emptyValue,
        valid.combineFunction,
        valid.targetType,
        valid.emptyOverride,
        valid.combineOverride.copy(
          body = valid.combineOverride.body.copy(
            callee = TermReference(BinderId(99))
          )
        )
      )
      assertEquals(
        InstanceFactoryPlanUntypedLowerer.lower(corrupt).left.toOption.map(_.code),
        Some("PLAN_INVALID")
      )
    }
  }

  test("characterizes direct anonymous-template shell constructors") {
    withContext {
      given SourceFile = NoSource
      val constructor = untpd.emptyConstructor
      assertEquals(constructor.name.toString, "<init>")
      val self = untpd.EmptyValDef
      assertEquals(self.name.toString, "_")
      val parsedSelf = parseOne(canonical.source).rhs
        .asInstanceOf[untpd.New]
        .tpt
        .asInstanceOf[untpd.Template]
        .self
      assert(self.isEmpty)
      assert(parsedSelf.isEmpty)
      assertEquals(self.getClass, parsedSelf.getClass)
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
      case value: untpd.ByNameTypeTree => Vector(value.result)
      case value: untpd.Function => value.args.toVector :+ value.body
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.Template =>
        Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body
      case value: untpd.Apply => value.fun +: value.args.toVector
      case _ => Vector.empty

  private def parseOne(source: String)(using outerContext: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed = new Parsers.Parser(SourceFile.virtual("U017Expected.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.DefDef]

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
