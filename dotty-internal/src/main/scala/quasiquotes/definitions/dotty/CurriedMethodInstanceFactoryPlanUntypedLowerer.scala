package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.definitions.CurriedMethodInstanceFactoryPlan
import quasiquotes.definitions.CurriedMethodInstanceFactoryPlan.Plan
import quasiquotes.definitions.DefinitionName
import quasiquotes.definitions.ScopedType.*
import scala.util.control.NonFatal

/** Source-free exact raw lowering for the accepted N056 curried-method factory plan. */
private[quasiquotes] object CurriedMethodInstanceFactoryPlanUntypedLowerer:
  private final case class Names(
      factory: String,
      typeParameter: String,
      carrier: String,
      target: String,
      member: String,
      firstParameter: String,
      secondParameter: String
  )

  def lower(
      plan: Plan
  )(using Context): Either[CurriedMethodInstanceFactoryPlanUntypedLoweringError, untpd.DefDef] =
    for
      names <- validatedNames(plan)
      raw <- construct(names)
      _ <- validateCandidateWithNames(raw, names)
    yield raw

  private[dotty] def validateCandidate(
      plan: Plan,
      raw: untpd.DefDef
  )(using Context): Either[CurriedMethodInstanceFactoryPlanUntypedLoweringError, Unit] =
    for
      names <- validatedNames(plan)
      candidate <- Option(raw).toRight(error(
        "EXACT_RAW_INVARIANT_FAILED",
        "the curried-method factory raw candidate must be present."
      ))
      _ <- validateCandidateWithNames(candidate, names)
    yield ()

  private def validatedNames(
      plan: Plan
  ): Either[CurriedMethodInstanceFactoryPlanUntypedLoweringError, Names] =
    for
      present <- Option(plan).toRight(error(
        "PLAN_REQUIRED",
        "the accepted CurriedMethodInstanceFactoryPlan must be present."
      ))
      snapshot <- CurriedMethodInstanceFactoryPlan
        .validate(present)
        .left
        .map(problem => error("PLAN_INVALID", problem.message))
      factory <- decoded(snapshot.factorySourceName, "FACTORY_NAME_INVALID")
      typeParameter <- decoded(snapshot.typeParameterSourceName, "TYPE_PARAMETER_NAME_INVALID")
      carrier <- decoded(snapshot.carrierSourceName, "CARRIER_NAME_INVALID")
      target <- decoded(snapshot.targetConstructorSourceName, "TARGET_TYPE_INVALID")
      member <- decoded(snapshot.memberSourceName, "MEMBER_NAME_INVALID")
      firstParameter <- decoded(snapshot.firstParameterSourceName, "FIRST_PARAMETER_NAME_INVALID")
      secondParameter <- decoded(snapshot.secondParameterSourceName, "SECOND_PARAMETER_NAME_INVALID")
      names = Names(factory, typeParameter, carrier, target, member, firstParameter, secondParameter)
      _ <- requireRoleIdentities(present, names)
    yield names

  private def requireRoleIdentities(
      plan: Plan,
      names: Names
  ): Either[CurriedMethodInstanceFactoryPlanUntypedLoweringError, Unit] =
    val typeEnvironment = Map(plan.typeParameter.binderId -> names.typeParameter)
    val termEnvironment = Map(
      plan.strictCarrier.binderId -> names.carrier,
      plan.firstParameter.binderId -> names.firstParameter,
      plan.secondParameter.binderId -> names.secondParameter
    )
    def typeRole(reference: TypeParameterReference): Option[String] =
      Option(reference).flatMap(value => Option(value.binderId).flatMap(typeEnvironment.get))
    def termRole(reference: CurriedMethodInstanceFactoryPlan.TermReference): Option[String] =
      Option(reference).flatMap(value => Option(value.binderId).flatMap(termEnvironment.get))
    def targetRole(target: Applied): Option[String] =
      target match
        case Applied(SourceName(constructor), Vector(reference: TypeParameterReference))
            if typeRole(reference).contains(names.typeParameter) => Some(constructor)
        case _ => None

    val carrierType = Option(plan.strictCarrier.parameterType)
    val body = Option(plan.methodOverride.body)
    val resolved = Vector(
      carrierType.flatMap(value => typeRole(value.firstArgument)),
      carrierType.flatMap(value => typeRole(value.secondArgument)),
      carrierType.flatMap(value => typeRole(value.result)),
      targetRole(plan.resultTarget),
      targetRole(plan.anonymousParentTarget),
      typeRole(plan.firstParameter.parameterType),
      typeRole(plan.secondParameter.parameterType),
      typeRole(plan.methodOverride.resultType),
      body.flatMap(value => termRole(value.callee)),
      body.flatMap(value => termRole(value.firstArgument)),
      body.flatMap(value => termRole(value.secondArgument))
    )
    Either.cond(
      resolved.forall(_.nonEmpty),
      (),
      error(
        "PLAN_ROLE_UNRESOLVED",
        "every Type and term edge must resolve through the plan's retained declaration identities."
      )
    )

  private def construct(
      names: Names
  )(using Context): Either[CurriedMethodInstanceFactoryPlanUntypedLoweringError, untpd.DefDef] =
    try
      given SourceFile = NoSource
      val typeParameter = untpd
        .TypeDef(
          typeName(names.typeParameter),
          untpd.TypeBoundsTree(untpd.EmptyTree, untpd.EmptyTree)
        )
        .withMods(untpd.Modifiers(Flags.Param))
      val carrierType = untpd.Function(
        untpd.Ident(typeName(names.typeParameter)) :: Nil,
        untpd.Function(
          untpd.Ident(typeName(names.typeParameter)) :: Nil,
          untpd.Ident(typeName(names.typeParameter))
        )
      )
      val carrier = untpd
        .ValDef(termName(names.carrier), carrierType, untpd.EmptyTree)
        .withMods(untpd.Modifiers(Flags.Param))
      val firstParameter = untpd
        .ValDef(
          termName(names.firstParameter),
          untpd.Ident(typeName(names.typeParameter)),
          untpd.EmptyTree
        )
        .withMods(untpd.Modifiers(Flags.Param))
      val secondParameter = untpd
        .ValDef(
          termName(names.secondParameter),
          untpd.Ident(typeName(names.typeParameter)),
          untpd.EmptyTree
        )
        .withMods(untpd.Modifiers(Flags.Param))
      val body = untpd.Apply(
        untpd.Apply(
          untpd.Ident(termName(names.carrier)),
          untpd.Ident(termName(names.firstParameter)) :: Nil
        ),
        untpd.Ident(termName(names.secondParameter)) :: Nil
      )
      val member = untpd
        .DefDef(
          termName(names.member),
          List(firstParameter :: Nil, secondParameter :: Nil),
          untpd.Ident(typeName(names.typeParameter)),
          body
        )
        .withMods(untpd.Modifiers(Flags.Method | Flags.Override))
      val template = untpd.Template(
        untpd.emptyConstructor.cloneIn(NoSource),
        appliedTarget(names.target, names.typeParameter) :: Nil,
        Nil,
        untpd.EmptyValDef,
        member :: Nil
      )
      Right(
        untpd
          .DefDef(
            termName(names.factory),
            List(typeParameter :: Nil, carrier :: Nil),
            appliedTarget(names.target, names.typeParameter),
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
  )(using Context): Either[CurriedMethodInstanceFactoryPlanUntypedLoweringError, Unit] =
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
                nestedUnaryType(carrier.tpt, names.typeParameter) && carrier.rhs.isEmpty
            case _ => false) &&
          appliedTargetMatches(raw.tpt, names.target, names.typeParameter) &&
          (raw.rhs match
            case untpd.New(template: untpd.Template) =>
              template.constr.name.toString == "<init>" &&
                template.constr.mods.flags == Flags.EmptyFlags &&
                template.constr.paramss.isEmpty && template.constr.tpt.isEmpty &&
                template.constr.rhs.isEmpty &&
                template.parentsOrDerived.size == 1 &&
                appliedTargetMatches(template.parentsOrDerived.head, names.target, names.typeParameter) &&
                template.derived.isEmpty && template.self.isEmpty &&
                (template.body match
                  case List(member: untpd.DefDef) => memberMatches(member, names)
                  case _ => false)
            case _ => false)
      val trees = allTrees(raw)
      val rawValid = trees.size == 29 && trees.forall(tree =>
        !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol &&
          !tree.isInstanceOf[untpd.TypedSplice]
      )
      Either.cond(
        topologyValid && rawValid,
        (),
        error(
          "EXACT_RAW_INVARIANT_FAILED",
          s"expected the exact 29-node source/span/symbol-free curried-method factory; observed ${trees.size} material nodes."
        )
      )
    catch case NonFatal(_) =>
      Left(error(
        "EXACT_RAW_INVARIANT_FAILED",
        "the curried-method factory raw candidate is malformed."
      ))

  private def memberMatches(member: untpd.DefDef, names: Names)(using Context): Boolean =
    member.name.toString == names.member &&
      member.mods.flags == (Flags.Method | Flags.Override) &&
      (member.paramss match
        case List(List(first: untpd.ValDef), List(second: untpd.ValDef)) =>
          parameterMatches(first, names.firstParameter, names.typeParameter) &&
            parameterMatches(second, names.secondParameter, names.typeParameter)
        case _ => false) &&
      typeIdent(member.tpt, names.typeParameter) &&
      (member.rhs match
        case untpd.Apply(
              untpd.Apply(untpd.Ident(callee), List(untpd.Ident(firstArgument))),
              List(untpd.Ident(secondArgument))
            ) =>
          callee.toString == names.carrier &&
            firstArgument.toString == names.firstParameter &&
            secondArgument.toString == names.secondParameter
        case _ => false)

  private def parameterMatches(
      parameter: untpd.ValDef,
      expectedName: String,
      expectedType: String
  )(using Context): Boolean =
    parameter.name.toString == expectedName && parameter.mods.flags == Flags.Param &&
      typeIdent(parameter.tpt, expectedType) && parameter.rhs.isEmpty

  private def nestedUnaryType(tree: untpd.Tree, expected: String): Boolean =
    tree match
      case untpd.Function(
            List(untpd.Ident(first)),
            untpd.Function(List(untpd.Ident(second)), untpd.Ident(result))
          ) =>
        first.toString == expected && second.toString == expected && result.toString == expected
      case _ => false

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

  private def decoded(
      value: String,
      code: String
  ): Either[CurriedMethodInstanceFactoryPlanUntypedLoweringError, String] =
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
      case value: untpd.Function => value.args.toVector :+ value.body
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.Template =>
        (Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body).filterNot(_.isEmpty)
      case value: untpd.Apply => value.fun +: value.args.toVector
      case _ => Vector.empty

  private def error(
      code: String,
      detail: String
  ): CurriedMethodInstanceFactoryPlanUntypedLoweringError =
    CurriedMethodInstanceFactoryPlanUntypedLoweringError(code, detail)
