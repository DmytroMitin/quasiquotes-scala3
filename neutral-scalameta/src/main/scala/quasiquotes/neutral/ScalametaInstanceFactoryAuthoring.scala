package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionName
import _root_.quasiquotes.definitions.InstanceFactoryPlan
import _root_.quasiquotes.definitions.InstanceFactoryPlan.*
import _root_.quasiquotes.definitions.ScopedType.*
import _root_.quasiquotes.parser.BinderId

import scala.annotation.nowarn
import scala.meta.*
import scala.util.control.NonFatal

/** Direct structural authoring for the exact bounded N017 instance-factory plan. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaInstanceFactoryAuthoring:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  private final case class RoleSnapshot(
      factoryName: String,
      typeParameterName: String,
      emptyCarrierName: String,
      emptyCarrierMode: ParameterMode,
      combineCarrierName: String,
      combineCarrierMode: ParameterMode,
      targetConstructorName: String,
      emptyMemberName: String,
      combineMemberName: String,
      firstNestedName: String,
      secondNestedName: String,
      typeReferenceRoles: Vector[Int],
      emptyBodyRole: Int,
      combineCalleeRole: Int,
      combineArgumentRoles: Vector[Int]
  ) derives CanEqual

  def author(plan: Plan): Either[Error, Defn.Def] =
    Option(plan)
      .toRight(missing)
      .flatMap(authorPresent)

  private def authorPresent(plan: Plan): Either[Error, Defn.Def] =
    for
      validated <- InstanceFactoryPlan
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
        .map(_ => planUnsupported)
      expected <- roleSnapshot(validated)
      _ <- requireRepresentableNames(validated)
      authored <- construct(validated)
      _ <- requireFresh(authored)
      _ <- requireExactRoundTrip(authored, expected)
    yield authored

  private def requireRepresentableNames(plan: Plan): Either[Error, Unit] =
    val termNames = Vector(
      plan.factoryDisplayName,
      plan.emptyValue.displayName,
      plan.combineFunction.displayName,
      plan.emptyOverride.memberDisplayName,
      plan.combineOverride.memberDisplayName,
      plan.combineOverride.firstParameter.displayName,
      plan.combineOverride.secondParameter.displayName
    )
    val typeNames = Vector(
      plan.typeParameter.displayName,
      plan.targetType.constructor match
        case SourceName(value) => value
        case _ => ""
    )
    val targetName = typeNames(1)
    val outerCarrierNames = Set(
      plan.emptyValue.displayName,
      plan.combineFunction.displayName
    )
    val memberNames = Vector(
      plan.emptyOverride.memberDisplayName,
      plan.combineOverride.memberDisplayName
    )
    val fiveRoleNamesRemainResolvable =
      plan.typeParameter.displayName != targetName &&
        plan.emptyValue.displayName != plan.combineFunction.displayName &&
        plan.combineOverride.firstParameter.displayName !=
          plan.combineOverride.secondParameter.displayName &&
        plan.combineOverride.firstParameter.displayName !=
          plan.combineFunction.displayName &&
        plan.combineOverride.secondParameter.displayName !=
          plan.combineFunction.displayName &&
        memberNames.forall(name => !outerCarrierNames.contains(name))

    for
      _ <- Either.cond(fiveRoleNamesRemainResolvable, (), nameUnsupported)
      _ <- traverseUnit(termNames)(requireFreshTermName)
      _ <- traverseUnit(typeNames)(requireFreshTypeName)
    yield ()

  private def requireFreshTermName(source: String): Either[Error, Unit] =
    for
      expected <- exactPlainName(source)
      authored <- ScalametaTermDefinitionNameAuthoring
        .author(expected)
        .toRight(nameUnsupported)
      _ <- Either.cond(authored.value == source, (), nameUnsupported)
    yield ()

  private def requireFreshTypeName(source: String): Either[Error, Unit] =
    for
      expected <- exactPlainName(source)
      authored <- try Right(Type.Name(expected.decoded))
        catch case NonFatal(_) => Left(nameUnsupported)
      projected <- ScalametaDefinitionNameProjection
        .project(authored)
        .left
        .map(_ => nameUnsupported)
      _ <- Either.cond(projected == expected && authored.value == source, (), nameUnsupported)
    yield ()

  private def exactPlainName(source: String): Either[Error, DefinitionName] =
    Option(source)
      .toRight(nameUnsupported)
      .flatMap(value => DefinitionName.fromSource(value).left.map(_ => nameUnsupported))
      .flatMap { name =>
        Either.cond(name.source == name.decoded, name, nameUnsupported)
      }

  private def construct(plan: Plan): Either[Error, Defn.Def] =
    try
      val typeName = plan.typeParameter.displayName
      val targetName = plan.targetType.constructor match
        case SourceName(value) => value
        case _ => throw new IllegalArgumentException("revalidated target constructor")
      val typeParameter = Type.Param(
        Nil,
        Type.Name(typeName),
        Type.ParamClause(Nil),
        Type.Bounds.empty
      )
      val emptyParameter = Term.Param(
        Nil,
        Term.Name(plan.emptyValue.displayName),
        Some(Type.ByName(Type.Name(typeName))),
        None
      )
      val combineParameter = Term.Param(
        Nil,
        Term.Name(plan.combineFunction.displayName),
        Some(
          Type.Function(
            List(Type.Name(typeName), Type.Name(typeName)),
            Type.Name(typeName)
          )
        ),
        None
      )
      val emptyMember = Defn.Def(
        List(Mod.Override()),
        Term.Name(plan.emptyOverride.memberDisplayName),
        Nil,
        Some(Type.Name(typeName)),
        Term.Name(plan.emptyValue.displayName)
      )
      val firstNested = plan.combineOverride.firstParameter.displayName
      val secondNested = plan.combineOverride.secondParameter.displayName
      val combineMember = Defn.Def(
        List(Mod.Override()),
        Term.Name(plan.combineOverride.memberDisplayName),
        List(
          Member.ParamClauseGroup(
            Type.ParamClause(Nil),
            List(
              Term.ParamClause(
                List(
                  Term.Param(Nil, Term.Name(firstNested), Some(Type.Name(typeName)), None),
                  Term.Param(Nil, Term.Name(secondNested), Some(Type.Name(typeName)), None)
                )
              )
            )
          )
        ),
        Some(Type.Name(typeName)),
        Term.Apply(
          Term.Name(plan.combineFunction.displayName),
          Term.ArgClause(List(Term.Name(firstNested), Term.Name(secondNested)))
        )
      )
      val parentType = Type.Apply(
        Type.Name(targetName),
        Type.ArgClause(List(Type.Name(typeName)))
      )
      val template = Template(
        Nil,
        List(
          Init(
            parentType,
            Name.Anonymous(),
            List.empty[Term.ArgClause]
          )
        ),
        Self(Name.Anonymous(), None),
        List(emptyMember, combineMember),
        Nil
      )
      Right(
        Defn.Def(
          Nil,
          Term.Name(plan.factoryDisplayName),
          List(
            Member.ParamClauseGroup(
              Type.ParamClause(List(typeParameter)),
              List(Term.ParamClause(List(emptyParameter, combineParameter)))
            )
          ),
          Some(
            Type.Apply(
              Type.Name(targetName),
              Type.ArgClause(List(Type.Name(typeName)))
            )
          ),
          Term.NewAnonymous(template)
        )
      )
    catch case NonFatal(_) => Left(constructionUnsupported)

  private def requireFresh(authored: Defn.Def): Either[Error, Unit] =
    Either.cond(
      allTrees(authored).forall(_.pos == Position.None),
      (),
      constructionUnsupported
    )

  private def requireExactRoundTrip(
      authored: Defn.Def,
      expected: RoleSnapshot
  ): Either[Error, Unit] =
    ScalametaInstanceFactoryProjection.project(authored) match
      case Right(ProjectedInstanceFactory(projected, None)) =>
        roleSnapshot(projected).flatMap { actual =>
          Either.cond(actual == expected, (), roundTripFailed)
        }
      case _ => Left(roundTripFailed)

  private def roleSnapshot(plan: Plan): Either[Error, RoleSnapshot] =
    val declarationBinders = Vector(
      plan.typeParameter.binderId,
      plan.emptyValue.binderId,
      plan.combineFunction.binderId,
      plan.combineOverride.firstParameter.binderId,
      plan.combineOverride.secondParameter.binderId
    )
    def roleOf(binder: BinderId): Int = declarationBinders.indexOf(binder)

    plan.targetType match
      case Applied(SourceName(targetName), Vector(targetReference: TypeParameterReference)) =>
        Right(
          RoleSnapshot(
            plan.factoryDisplayName,
            plan.typeParameter.displayName,
            plan.emptyValue.displayName,
            plan.emptyValue.mode,
            plan.combineFunction.displayName,
            plan.combineFunction.mode,
            targetName,
            plan.emptyOverride.memberDisplayName,
            plan.combineOverride.memberDisplayName,
            plan.combineOverride.firstParameter.displayName,
            plan.combineOverride.secondParameter.displayName,
            Vector(
              plan.emptyValue.valueType.reference,
              plan.combineFunction.functionType.firstArgument,
              plan.combineFunction.functionType.secondArgument,
              plan.combineFunction.functionType.result,
              targetReference,
              plan.combineOverride.firstParameter.parameterType,
              plan.combineOverride.secondParameter.parameterType,
              plan.combineOverride.resultType
            ).map(reference => roleOf(reference.binderId)),
            roleOf(plan.emptyOverride.body.binderId),
            roleOf(plan.combineOverride.body.callee.binderId),
            plan.combineOverride.body.arguments.map(reference => roleOf(reference.binderId))
          )
        )
      case _ => Left(planUnsupported)

  private def traverseUnit[A](
      values: Vector[A]
  )(validate: A => Either[Error, Unit]): Either[Error, Unit] =
    values.foldLeft(Right(()): Either[Error, Unit]) { (validated, value) =>
      validated.flatMap(_ => validate(value))
    }

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)

  private def missing: Error =
    error(
      "NEUTRAL_INSTANCE_FACTORY_AUTHORING_MISSING",
      "the InstanceFactoryPlan.Plan must be present."
    )

  private def planUnsupported: Error =
    error(
      "NEUTRAL_INSTANCE_FACTORY_AUTHORING_PLAN_UNSUPPORTED",
      "the input is outside the existing InstanceFactoryPlan.create contract."
    )

  private def nameUnsupported: Error =
    error(
      "NEUTRAL_INSTANCE_FACTORY_AUTHORING_NAME_UNSUPPORTED",
      "a declaration or target name is outside the exact fresh N017 spelling and lexical-role intersection."
    )

  private def constructionUnsupported: Error =
    error(
      "NEUTRAL_INSTANCE_FACTORY_AUTHORING_CONSTRUCTION_UNSUPPORTED",
      "the exact direct fresh Scalameta instance-factory tree could not be constructed."
    )

  private def roundTripFailed: Error =
    error(
      "NEUTRAL_INSTANCE_FACTORY_AUTHORING_ROUNDTRIP_FAILED",
      "the fresh definition did not round-trip through N017 with the same five-role semantics and no source span."
    )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
