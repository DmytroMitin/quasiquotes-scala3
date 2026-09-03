package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.definitions.DefinitionName
import quasiquotes.definitions.InstanceFactoryPlan
import quasiquotes.definitions.InstanceFactoryPlan.*
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

import scala.util.control.NonFatal

/** Source-free exact raw lowering for the accepted N017 instance-factory plan. */
private[quasiquotes] object InstanceFactoryPlanUntypedLowerer:
  def lower(
      plan: Plan
  )(using Context): Either[InstanceFactoryPlanUntypedLoweringError, untpd.DefDef] =
    for
      present <- Option(plan).toRight(
        error("PLAN_REQUIRED", "the accepted InstanceFactoryPlan must be present.")
      )
      validated <- revalidate(present)
      _ <- validateTermScopes(validated)
      raw <- construct(validated)
      _ <- validateRaw(raw)
    yield raw

  private def revalidate(
      plan: Plan
  ): Either[InstanceFactoryPlanUntypedLoweringError, Plan] =
    InstanceFactoryPlan
      .create(
        plan.factoryDisplayName,
        plan.typeParameter,
        plan.emptyValue,
        plan.combineFunction,
        plan.targetType,
        plan.emptyOverride,
        plan.combineOverride
      )
      .left
      .map(problem => error("PLAN_INVALID", problem.message))

  private def validateTermScopes(
      plan: Plan
  ): Either[InstanceFactoryPlanUntypedLoweringError, Unit] =
    val outer = Vector(plan.emptyValue.displayName, plan.combineFunction.displayName)
    val nested = Vector(
      plan.combineOverride.firstParameter.displayName,
      plan.combineOverride.secondParameter.displayName
    )
    Either.cond(
      outer.distinct.size == outer.size &&
        nested.distinct.size == nested.size &&
        !nested.contains(plan.combineFunction.displayName),
      (),
      error(
        "TERM_SCOPE_COLLISION",
        "the accepted plan must retain distinct outer and nested declarations and must not shadow the combine-function carrier."
      )
    )

  private def construct(
      plan: Plan
  )(using Context): Either[InstanceFactoryPlanUntypedLoweringError, untpd.DefDef] =
    try
      given SourceFile = NoSource
      val typeNames = Map(plan.typeParameter.binderId -> plan.typeParameter.displayName)
      val termNames = Map(
        plan.emptyValue.binderId -> plan.emptyValue.displayName,
        plan.combineFunction.binderId -> plan.combineFunction.displayName,
        plan.combineOverride.firstParameter.binderId ->
          plan.combineOverride.firstParameter.displayName,
        plan.combineOverride.secondParameter.binderId ->
          plan.combineOverride.secondParameter.displayName
      )

      for
        factoryName <- decodedTermName(plan.factoryDisplayName, "FACTORY_NAME_INVALID")
        typeParameterName <- typeReferenceName(
          plan.typeParameter.binderId,
          typeNames,
          "TYPE_BINDER_ROLE_UNKNOWN"
        )
        emptyCarrierName <- termReferenceName(
          plan.emptyValue.binderId,
          termNames,
          "TERM_BINDER_ROLE_UNKNOWN"
        )
        functionCarrierName <- termReferenceName(
          plan.combineFunction.binderId,
          termNames,
          "TERM_BINDER_ROLE_UNKNOWN"
        )
        targetName <- plan.targetType.constructor match
          case SourceName(value) => decodedTypeName(value, "TARGET_TYPE_INVALID")
          case _ => Left(error("TARGET_TYPE_INVALID", "the target constructor must be one source name."))
        emptyMemberName <- decodedTermName(
          plan.emptyOverride.memberDisplayName,
          "EMPTY_OVERRIDE_NAME_INVALID"
        )
        combineMemberName <- decodedTermName(
          plan.combineOverride.memberDisplayName,
          "COMBINE_OVERRIDE_NAME_INVALID"
        )
        firstNestedName <- termReferenceName(
          plan.combineOverride.firstParameter.binderId,
          termNames,
          "TERM_BINDER_ROLE_UNKNOWN"
        )
        secondNestedName <- termReferenceName(
          plan.combineOverride.secondParameter.binderId,
          termNames,
          "TERM_BINDER_ROLE_UNKNOWN"
        )
        emptyBodyName <- termReferenceName(
          plan.emptyOverride.body.binderId,
          termNames,
          "TERM_REFERENCE_ROLE_UNKNOWN"
        )
        combineCalleeName <- termReferenceName(
          plan.combineOverride.body.callee.binderId,
          termNames,
          "TERM_REFERENCE_ROLE_UNKNOWN"
        )
        combineArgumentNames <- sequence(
          plan.combineOverride.body.arguments.map(reference =>
            termReferenceName(
              reference.binderId,
              termNames,
              "TERM_REFERENCE_ROLE_UNKNOWN"
            )
          )
        )
      yield
        val rawTypeParameter = untpd
          .TypeDef(
            typeName(typeParameterName),
            untpd.TypeBoundsTree(untpd.EmptyTree, untpd.EmptyTree)
          )
          .withMods(untpd.Modifiers(Flags.Param))
        val rawEmptyCarrier = untpd
          .ValDef(
            termName(emptyCarrierName),
            untpd.ByNameTypeTree(untpd.Ident(typeName(typeParameterName))),
            untpd.EmptyTree
          )
          .withMods(untpd.Modifiers(Flags.Param))
        val rawFunctionCarrier = untpd
          .ValDef(
            termName(functionCarrierName),
            untpd.Function(
              List(
                untpd.Ident(typeName(typeParameterName)),
                untpd.Ident(typeName(typeParameterName))
              ),
              untpd.Ident(typeName(typeParameterName))
            ),
            untpd.EmptyTree
          )
          .withMods(untpd.Modifiers(Flags.Param))
        val rawTarget = appliedTarget(targetName, typeParameterName)
        val rawParent = appliedTarget(targetName, typeParameterName)
        val rawEmptyOverride = untpd
          .DefDef(
            termName(emptyMemberName),
            Nil,
            untpd.Ident(typeName(typeParameterName)),
            untpd.Ident(termName(emptyBodyName))
          )
          .withMods(untpd.Modifiers(Flags.Method | Flags.Override))
        val rawFirstNested = untpd
          .ValDef(
            termName(firstNestedName),
            untpd.Ident(typeName(typeParameterName)),
            untpd.EmptyTree
          )
          .withMods(untpd.Modifiers(Flags.Param))
        val rawSecondNested = untpd
          .ValDef(
            termName(secondNestedName),
            untpd.Ident(typeName(typeParameterName)),
            untpd.EmptyTree
          )
          .withMods(untpd.Modifiers(Flags.Param))
        val rawCombineOverride = untpd
          .DefDef(
            termName(combineMemberName),
            List(List(rawFirstNested, rawSecondNested)),
            untpd.Ident(typeName(typeParameterName)),
            untpd.Apply(
              untpd.Ident(termName(combineCalleeName)),
              combineArgumentNames.map(name => untpd.Ident(termName(name))).toList
            )
          )
          .withMods(untpd.Modifiers(Flags.Method | Flags.Override))
        val rawConstructor = untpd.emptyConstructor.cloneIn(NoSource)
        val rawTemplate = untpd.Template(
          rawConstructor,
          rawParent :: Nil,
          Nil,
          untpd.EmptyValDef,
          List(rawEmptyOverride, rawCombineOverride)
        )
        untpd
          .DefDef(
            termName(factoryName),
            List(
              List(rawTypeParameter),
              List(rawEmptyCarrier, rawFunctionCarrier)
            ),
            rawTarget,
            untpd.New(rawTemplate)
          )
          .withMods(untpd.Modifiers(Flags.Method))
    catch
      case NonFatal(exception) =>
        Left(
          error(
            "EXACT_RAW_LOWERING_FAILED",
            Option(exception.getMessage)
              .filter(_.nonEmpty)
              .getOrElse(exception.getClass.getSimpleName)
          )
        )

  private def appliedTarget(
      constructor: String,
      argument: String
  )(using SourceFile): untpd.AppliedTypeTree =
    untpd.AppliedTypeTree(
      untpd.Ident(typeName(constructor)),
      untpd.Ident(typeName(argument)) :: Nil
    )

  private def typeReferenceName(
      binderId: BinderId,
      environment: Map[BinderId, String],
      code: String
  ): Either[InstanceFactoryPlanUntypedLoweringError, String] =
    Option(binderId)
      .flatMap(environment.get)
      .toRight(error(code, "the Type BinderId did not resolve to one admitted declaration role."))

  private def termReferenceName(
      binderId: BinderId,
      environment: Map[BinderId, String],
      code: String
  ): Either[InstanceFactoryPlanUntypedLoweringError, String] =
    Option(binderId)
      .flatMap(environment.get)
      .toRight(error(code, "the Term BinderId did not resolve to one admitted declaration role."))

  private def decodedTermName(
      value: String,
      code: String
  ): Either[InstanceFactoryPlanUntypedLoweringError, String] =
    decodedName(value, code)

  private def decodedTypeName(
      value: String,
      code: String
  ): Either[InstanceFactoryPlanUntypedLoweringError, String] =
    decodedName(value, code)

  private def decodedName(
      value: String,
      code: String
  ): Either[InstanceFactoryPlanUntypedLoweringError, String] =
    Option(value)
      .toRight(error(code, "the source name must be present."))
      .flatMap(name =>
        DefinitionName
          .fromSource(name)
          .left
          .map(problem => error(code, problem.message))
          .map(_.decoded)
      )

  private def sequence[A](
      values: Vector[Either[InstanceFactoryPlanUntypedLoweringError, A]]
  ): Either[InstanceFactoryPlanUntypedLoweringError, Vector[A]] =
    values.foldLeft(
      Right(Vector.empty): Either[InstanceFactoryPlanUntypedLoweringError, Vector[A]]
    ) { (result, value) =>
      for
        accumulated <- result
        next <- value
      yield accumulated :+ next
    }

  private def validateRaw(
      raw: untpd.DefDef
  )(using Context): Either[InstanceFactoryPlanUntypedLoweringError, Unit] =
    val trees = allTrees(raw)
    val invalid = trees.filter(tree =>
      tree.source.exists || tree.span.exists || tree.symbol != NoSymbol ||
        tree.isInstanceOf[untpd.TypedSplice]
    )
    Either.cond(
      trees.size == 33 && invalid.isEmpty,
      (),
      error(
        "EXACT_RAW_INVARIANT_FAILED",
        s"expected 33 source/span/symbol-free nodes; found ${trees.size} nodes and ${invalid.size} invalid nodes."
      )
    )

  private[dotty] def allTrees(
      tree: untpd.Tree
  )(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private[dotty] def directChildren(
      tree: untpd.Tree
  )(using Context): Vector[untpd.Tree] =
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

  private def error(
      code: String,
      detail: String
  ): InstanceFactoryPlanUntypedLoweringError =
    InstanceFactoryPlanUntypedLoweringError(code, detail)
