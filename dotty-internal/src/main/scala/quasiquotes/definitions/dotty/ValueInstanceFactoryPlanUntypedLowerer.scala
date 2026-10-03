package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.definitions.DefinitionName
import quasiquotes.definitions.ScopedType.*
import quasiquotes.definitions.ValueInstanceFactoryPlan
import quasiquotes.definitions.ValueInstanceFactoryPlan.Plan

import scala.util.control.NonFatal

/** Source-free exact raw lowering for the accepted N052 one-value factory plan. */
private[quasiquotes] object ValueInstanceFactoryPlanUntypedLowerer:
  private final case class Names(
      factory: String,
      typeParameter: String,
      carrier: String,
      target: String,
      member: String
  )

  def lower(
      plan: Plan
  )(using Context): Either[ValueInstanceFactoryPlanUntypedLoweringError, untpd.DefDef] =
    for
      names <- validatedNames(plan)
      raw <- construct(names)
      _ <- validateCandidateWithNames(raw, names)
    yield raw

  private[dotty] def validateCandidate(
      plan: Plan,
      raw: untpd.DefDef
  )(using Context): Either[ValueInstanceFactoryPlanUntypedLoweringError, Unit] =
    for
      names <- validatedNames(plan)
      candidate <- Option(raw).toRight(error(
        "EXACT_RAW_INVARIANT_FAILED",
        "the one-value factory raw candidate must be present."
      ))
      _ <- validateCandidateWithNames(candidate, names)
    yield ()

  private def validatedNames(
      plan: Plan
  ): Either[ValueInstanceFactoryPlanUntypedLoweringError, Names] =
    for
      present <- Option(plan).toRight(error(
        "PLAN_REQUIRED",
        "the accepted ValueInstanceFactoryPlan must be present."
      ))
      snapshot <- ValueInstanceFactoryPlan
        .validate(present)
        .left
        .map(problem => error("PLAN_INVALID", problem.message))
      factory <- decoded(snapshot.factorySourceName, "FACTORY_NAME_INVALID")
      typeParameter <- decoded(snapshot.typeParameterSourceName, "TYPE_PARAMETER_NAME_INVALID")
      carrier <- decoded(snapshot.carrierSourceName, "CARRIER_NAME_INVALID")
      target <- decoded(snapshot.targetConstructorSourceName, "TARGET_TYPE_INVALID")
      member <- decoded(snapshot.memberSourceName, "MEMBER_NAME_INVALID")
      _ <- requireRoleIdentities(present)
    yield Names(factory, typeParameter, carrier, target, member)

  private def requireRoleIdentities(
      plan: Plan
  ): Either[ValueInstanceFactoryPlanUntypedLoweringError, Unit] =
    val typeEnvironment = Map(plan.typeParameter.binderId -> plan.typeParameter.displayName)
    val termEnvironment = Map(plan.strictCarrier.binderId -> plan.strictCarrier.displayName)
    val targetReference = plan.resultTarget match
      case Applied(_, Vector(reference: TypeParameterReference)) => Option(reference.binderId)
      case _ => None
    val parentReference = plan.anonymousParentTarget match
      case Applied(_, Vector(reference: TypeParameterReference)) => Option(reference.binderId)
      case _ => None
    val resolved = Vector(
      Option(plan.strictCarrier.parameterType).flatMap(reference => typeEnvironment.get(reference.binderId)),
      targetReference.flatMap(typeEnvironment.get),
      parentReference.flatMap(typeEnvironment.get),
      Option(plan.valueOverride.valueType).flatMap(reference => typeEnvironment.get(reference.binderId)),
      Option(plan.valueOverride.body).flatMap(reference => termEnvironment.get(reference.binderId))
    )
    Either.cond(
      resolved.forall(_.nonEmpty),
      (),
      error(
        "PLAN_ROLE_UNRESOLVED",
        "every Type and term reference must resolve through the plan's existing declaration identities."
      )
    )

  private def construct(
      names: Names
  )(using Context): Either[ValueInstanceFactoryPlanUntypedLoweringError, untpd.DefDef] =
    try
      given SourceFile = NoSource
      val typeParameter = untpd
        .TypeDef(
          typeName(names.typeParameter),
          untpd.TypeBoundsTree(untpd.EmptyTree, untpd.EmptyTree)
        )
        .withMods(untpd.Modifiers(Flags.Param))
      val carrier = untpd
        .ValDef(
          termName(names.carrier),
          untpd.Ident(typeName(names.typeParameter)),
          untpd.EmptyTree
        )
        .withMods(untpd.Modifiers(Flags.Param))
      val resultTarget = appliedTarget(names.target, names.typeParameter)
      val parentTarget = appliedTarget(names.target, names.typeParameter)
      val member = untpd
        .ValDef(
          termName(names.member),
          untpd.Ident(typeName(names.typeParameter)),
          untpd.Ident(termName(names.carrier))
        )
        .withMods(untpd.Modifiers(Flags.Override))
      val template = untpd.Template(
        untpd.emptyConstructor.cloneIn(NoSource),
        parentTarget :: Nil,
        Nil,
        untpd.EmptyValDef,
        member :: Nil
      )
      Right(
        untpd
          .DefDef(
            termName(names.factory),
            List(typeParameter :: Nil, carrier :: Nil),
            resultTarget,
            untpd.New(template)
          )
          .withMods(untpd.Modifiers(Flags.Method))
      )
    catch
      case NonFatal(exception) =>
        Left(error(
          "EXACT_RAW_LOWERING_FAILED",
          Option(exception.getMessage).filter(_.nonEmpty)
            .getOrElse(exception.getClass.getSimpleName)
        ))

  private def validateCandidateWithNames(
      raw: untpd.DefDef,
      names: Names
  )(using Context): Either[ValueInstanceFactoryPlanUntypedLoweringError, Unit] =
    try
      val topologyValid =
        raw.name.toString == names.factory &&
          raw.mods.flags == Flags.Method &&
          (raw.paramss match
            case List(List(typeParameter: untpd.TypeDef), List(carrier: untpd.ValDef)) =>
              typeParameter.name.toString == names.typeParameter &&
                typeParameter.mods.flags == Flags.Param &&
                (typeParameter.rhs match
                  case bounds: untpd.TypeBoundsTree =>
                    bounds.lo.isEmpty && bounds.hi.isEmpty && bounds.alias.isEmpty
                  case _ => false) &&
                carrier.name.toString == names.carrier &&
                carrier.mods.flags == Flags.Param &&
                typeIdent(carrier.tpt, names.typeParameter) && carrier.rhs.isEmpty
            case _ => false) &&
          appliedTargetMatches(raw.tpt, names.target, names.typeParameter) &&
          (raw.rhs match
            case untpd.New(template: untpd.Template) =>
              template.constr.name.toString == "<init>" &&
                template.constr.mods.flags == Flags.EmptyFlags &&
                template.constr.paramss.isEmpty &&
                template.constr.tpt.isEmpty && template.constr.rhs.isEmpty &&
                template.parentsOrDerived.size == 1 &&
                appliedTargetMatches(
                  template.parentsOrDerived.head,
                  names.target,
                  names.typeParameter
                ) &&
                template.derived.isEmpty && template.self.isEmpty &&
                (template.body match
                  case List(member: untpd.ValDef) =>
                    member.name.toString == names.member &&
                      member.mods.flags == Flags.Override &&
                      typeIdent(member.tpt, names.typeParameter) &&
                      termIdent(member.rhs, names.carrier)
                  case _ => false)
            case _ => false)
      val trees = allTrees(raw)
      val rawValid = trees.size == 17 && trees.forall(tree =>
        !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol &&
          !tree.isInstanceOf[untpd.TypedSplice]
      )
      Either.cond(
        topologyValid && rawValid,
        (),
        error(
          "EXACT_RAW_INVARIANT_FAILED",
          s"expected the exact 17-node source/span/symbol-free one-value factory; observed ${trees.size} material nodes."
        )
      )
    catch case NonFatal(_) =>
      Left(error(
        "EXACT_RAW_INVARIANT_FAILED",
        "the one-value factory raw candidate is malformed."
      ))

  private def appliedTarget(
      constructor: String,
      argument: String
  )(using SourceFile): untpd.AppliedTypeTree =
    untpd.AppliedTypeTree(
      untpd.Ident(typeName(constructor)),
      untpd.Ident(typeName(argument)) :: Nil
    )

  private def appliedTargetMatches(
      tree: untpd.Tree,
      constructor: String,
      argument: String
  ): Boolean =
    tree match
      case untpd.AppliedTypeTree(
            untpd.Ident(observedConstructor),
            List(untpd.Ident(observedArgument))
          ) =>
        observedConstructor.toString == constructor && observedArgument.toString == argument
      case _ => false

  private def typeIdent(tree: untpd.Tree, expected: String): Boolean =
    tree match
      case untpd.Ident(name) => name.toString == expected
      case _ => false

  private def termIdent(tree: untpd.Tree, expected: String): Boolean =
    tree match
      case untpd.Ident(name) => name.toString == expected
      case _ => false

  private def decoded(
      value: String,
      code: String
  ): Either[ValueInstanceFactoryPlanUntypedLoweringError, String] =
    Option(value)
      .toRight(error(code, "the source name must be present."))
      .flatMap(name => DefinitionName.fromSource(name).left.map(problem => error(code, problem.message)))
      .map(_.decoded)

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
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.Template =>
        (Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body).filterNot(_.isEmpty)
      case _ => Vector.empty

  private def error(
      code: String,
      detail: String
  ): ValueInstanceFactoryPlanUntypedLoweringError =
    ValueInstanceFactoryPlanUntypedLoweringError(code, detail)
