package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.definitions.DefinitionName
import quasiquotes.definitions.ScopedType.*
import quasiquotes.definitions.ScopedTypeAlias
import quasiquotes.definitions.TypeMemberInstanceFactoryPlan
import quasiquotes.definitions.TypeMemberInstanceFactoryPlan.Plan

import scala.util.control.NonFatal

/** Source-free exact raw lowering for the accepted N053 Type-member factory plan. */
private[quasiquotes] object TypeMemberInstanceFactoryPlanUntypedLowerer:
  private final case class Names(
      factory: String,
      firstTypeParameter: String,
      secondTypeParameter: String,
      target: String,
      member: String
  )

  def lower(
      plan: Plan
  )(using Context): Either[
    TypeMemberInstanceFactoryPlanUntypedLoweringError,
    untpd.DefDef
  ] =
    for
      names <- validatedNames(plan)
      raw <- construct(names)
      _ <- validateCandidateWithNames(raw, names)
    yield raw

  private[dotty] def validateCandidate(
      plan: Plan,
      raw: untpd.DefDef
  )(using Context): Either[TypeMemberInstanceFactoryPlanUntypedLoweringError, Unit] =
    for
      names <- validatedNames(plan)
      candidate <- Option(raw).toRight(error(
        "EXACT_RAW_INVARIANT_FAILED",
        "the Type-member factory raw candidate must be present."
      ))
      _ <- validateCandidateWithNames(candidate, names)
    yield ()

  private def validatedNames(
      plan: Plan
  ): Either[TypeMemberInstanceFactoryPlanUntypedLoweringError, Names] =
    for
      present <- Option(plan).toRight(error(
        "PLAN_REQUIRED",
        "the accepted TypeMemberInstanceFactoryPlan must be present."
      ))
      snapshot <- TypeMemberInstanceFactoryPlan
        .validate(present)
        .left
        .map(problem => error("PLAN_INVALID", problem.message))
      factory <- decoded(snapshot.factorySourceName, "FACTORY_NAME_INVALID")
      first <- decoded(
        snapshot.firstTypeParameterSourceName,
        "FIRST_TYPE_PARAMETER_NAME_INVALID"
      )
      second <- decoded(
        snapshot.secondTypeParameterSourceName,
        "SECOND_TYPE_PARAMETER_NAME_INVALID"
      )
      target <- decoded(snapshot.targetConstructorSourceName, "TARGET_TYPE_INVALID")
      member <- decoded(snapshot.memberSourceName, "MEMBER_NAME_INVALID")
      _ <- requireRoleIdentities(present)
    yield Names(factory, first, second, target, member)

  private def requireRoleIdentities(
      plan: Plan
  ): Either[TypeMemberInstanceFactoryPlanUntypedLoweringError, Unit] =
    val environment = Map(
      plan.firstTypeParameter.binderId -> plan.firstTypeParameter.displayName,
      plan.secondTypeParameter.binderId -> plan.secondTypeParameter.displayName
    )
    val first = plan.firstTypeParameter.displayName
    val second = plan.secondTypeParameter.displayName
    val resolved = Vector(
      targetReference(plan.resultTarget).flatMap(environment.get).contains(first),
      aliasReference(plan.resultAlias).flatMap(environment.get).contains(second),
      targetReference(plan.anonymousParentTarget).flatMap(environment.get).contains(first),
      aliasReference(plan.anonymousAlias).flatMap(environment.get).contains(second)
    )
    Either.cond(
      resolved.forall(identity) && !plan.resultAlias.eq(plan.anonymousAlias),
      (),
      error(
        "PLAN_ROLE_UNRESOLVED",
        "every target and concrete alias reference must resolve through the plan's existing Type declaration identities."
      )
    )

  private def targetReference(target: Applied): Option[quasiquotes.parser.BinderId] =
    target match
      case Applied(_, Vector(reference: TypeParameterReference)) =>
        Option(reference.binderId)
      case _ => None

  private def aliasReference(alias: ScopedTypeAlias): Option[quasiquotes.parser.BinderId] =
    Option(alias).flatMap { present =>
      present.rhs match
        case reference: TypeParameterReference => Option(reference.binderId)
        case _ => None
    }

  private def construct(
      names: Names
  )(using Context): Either[TypeMemberInstanceFactoryPlanUntypedLoweringError, untpd.DefDef] =
    try
      given SourceFile = NoSource
      val first = typeParameter(names.firstTypeParameter)
      val second = typeParameter(names.secondTypeParameter)
      val resultAlias = concreteAlias(names.member, names.secondTypeParameter)
      val resultType = untpd.RefinedTypeTree(
        appliedTarget(names.target, names.firstTypeParameter),
        resultAlias :: Nil
      )
      val bodyAlias = concreteAlias(names.member, names.secondTypeParameter)
      val template = untpd.Template(
        untpd.emptyConstructor.cloneIn(NoSource),
        appliedTarget(names.target, names.firstTypeParameter) :: Nil,
        Nil,
        untpd.EmptyValDef,
        bodyAlias :: Nil
      )
      Right(
        untpd
          .DefDef(
            termName(names.factory),
            List(first :: second :: Nil),
            resultType,
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

  private def typeParameter(name: String)(using SourceFile): untpd.TypeDef =
    untpd
      .TypeDef(
        typeName(name),
        untpd.TypeBoundsTree(untpd.EmptyTree, untpd.EmptyTree)
      )
      .withMods(untpd.Modifiers(Flags.Param))

  private def concreteAlias(member: String, rhs: String)(using SourceFile): untpd.TypeDef =
    untpd.TypeDef(typeName(member), untpd.Ident(typeName(rhs)))

  private def appliedTarget(
      constructor: String,
      argument: String
  )(using SourceFile): untpd.AppliedTypeTree =
    untpd.AppliedTypeTree(
      untpd.Ident(typeName(constructor)),
      untpd.Ident(typeName(argument)) :: Nil
    )

  private def validateCandidateWithNames(
      raw: untpd.DefDef,
      names: Names
  )(using Context): Either[TypeMemberInstanceFactoryPlanUntypedLoweringError, Unit] =
    try
      val topologyValid =
        raw.name.toString == names.factory &&
          raw.mods.flags == Flags.Method &&
          (raw.paramss match
            case List(List(first: untpd.TypeDef, second: untpd.TypeDef)) =>
              typeParameterMatches(first, names.firstTypeParameter) &&
                typeParameterMatches(second, names.secondTypeParameter) &&
                !(first eq second)
            case _ => false) &&
          (raw.tpt match
            case untpd.RefinedTypeTree(base, List(alias: untpd.TypeDef)) =>
              appliedTargetMatches(base, names.target, names.firstTypeParameter) &&
                concreteAliasMatches(alias, names.member, names.secondTypeParameter)
            case _ => false) &&
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
                  names.firstTypeParameter
                ) &&
                template.derived.isEmpty && template.self.isEmpty &&
                (template.body match
                  case List(alias: untpd.TypeDef) =>
                    concreteAliasMatches(alias, names.member, names.secondTypeParameter) &&
                      (raw.tpt match
                        case refinement: untpd.RefinedTypeTree =>
                          !(refinement.refinements.head eq alias)
                        case _ => false)
                  case _ => false)
            case _ => false)
      val trees = allTrees(raw)
      val rawValid = trees.size == 19 && trees.forall(tree =>
        !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol &&
          !tree.isInstanceOf[untpd.TypedSplice]
      )
      Either.cond(
        topologyValid && rawValid,
        (),
        error(
          "EXACT_RAW_INVARIANT_FAILED",
          s"expected the exact 19-node source/span/symbol-free Type-member factory; observed ${trees.size} material nodes."
        )
      )
    catch case NonFatal(_) =>
      Left(error(
        "EXACT_RAW_INVARIANT_FAILED",
        "the Type-member factory raw candidate is malformed."
      ))

  private def typeParameterMatches(parameter: untpd.TypeDef, expected: String): Boolean =
    parameter.name.toString == expected && parameter.mods.flags == Flags.Param &&
      (parameter.rhs match
        case bounds: untpd.TypeBoundsTree =>
          bounds.lo.isEmpty && bounds.hi.isEmpty && bounds.alias.isEmpty
        case _ => false)

  private def concreteAliasMatches(
      alias: untpd.TypeDef,
      member: String,
      rhs: String
  ): Boolean =
    alias.name.toString == member && alias.mods.flags == Flags.EmptyFlags &&
      typeIdent(alias.rhs, rhs)

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

  private def decoded(
      value: String,
      code: String
  ): Either[TypeMemberInstanceFactoryPlanUntypedLoweringError, String] =
    Option(value)
      .toRight(error(code, "the source name must be present."))
      .flatMap(name =>
        DefinitionName.fromSource(name).left.map(problem => error(code, problem.message))
      )
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
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.Template =>
        (Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body).filterNot(_.isEmpty)
      case _ => Vector.empty

  private def error(
      code: String,
      detail: String
  ): TypeMemberInstanceFactoryPlanUntypedLoweringError =
    TypeMemberInstanceFactoryPlanUntypedLoweringError(code, detail)
