package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionName
import _root_.quasiquotes.definitions.DelegatedForwardingMethodPlan
import _root_.quasiquotes.definitions.DelegatedForwardingMethodPlan.*
import _root_.quasiquotes.definitions.ScopedType.*
import _root_.quasiquotes.parser.BinderId

import scala.annotation.nowarn
import scala.meta.*
import scala.util.control.NonFatal

/** Direct structural authoring for the exact bounded AUXify-043 forwarding plan. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaDelegatedForwardingMethodAuthoring:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  private enum DeclarationRole derives CanEqual:
    case TypeParameter, OrdinaryParameter, ContextualParameter

  private enum SelectedMemberRole derives CanEqual:
    case GeneratedMethod

  private final case class RoleSnapshot(
      methodSourceName: String,
      typeParameterSourceName: String,
      ordinaryParameterSourceName: String,
      contextualParameterSourceName: String,
      contextualConstructorSourceName: String,
      resultTypeSourceName: String,
      ordinaryTypeReferenceRole: DeclarationRole,
      contextualTypeArgumentRole: DeclarationRole,
      bodyReceiverRole: DeclarationRole,
      selectedMemberSourceName: String,
      selectedMemberRole: SelectedMemberRole,
      bodyArgumentRole: DeclarationRole
  ) derives CanEqual

  def author(plan: Plan): Either[Error, Defn.Def] =
    Option(plan)
      .toRight(missing)
      .flatMap(authorPresent)

  private def authorPresent(plan: Plan): Either[Error, Defn.Def] =
    for
      validated <- revalidate(plan)
      expected <- roleSnapshot(validated)
      _ <- requireRepresentableNames(validated)
      _ <- requireLexicalRoles(validated)
      authored <- construct(validated)
      _ <- requireFresh(authored)
      _ <- requireExactRoundTrip(authored, expected)
    yield authored

  private def revalidate(plan: Plan): Either[Error, Plan] =
    try
      DelegatedForwardingMethodPlan
        .create(
          plan.methodIdentity.sourceName,
          plan.typeParameter,
          plan.ordinaryParameter,
          plan.contextualParameter,
          plan.resultType,
          ForwardingBody(
            plan.body.receiver,
            plan.body.selectedMethodIdentity.sourceName,
            plan.body.argument
          )
        )
        .left
        .map(_ => planUnsupported)
    catch case NonFatal(_) => Left(planUnsupported)

  private def requireRepresentableNames(plan: Plan): Either[Error, Unit] =
    try
      for
        _ <- traverseUnit(
          Vector(
            plan.methodIdentity.sourceName,
            plan.ordinaryParameter.displayName,
            plan.contextualParameter.displayName
          )
        )(requireFreshTermName)
        _ <- traverseUnit(
          Vector(
            plan.typeParameter.displayName,
            contextualConstructorName(plan),
            plan.resultType.value
          )
        )(requireFreshTypeName)
      yield ()
    catch case NonFatal(_) => Left(nameUnsupported)

  private def requireFreshTermName(source: String): Either[Error, Unit] =
    for
      expected <- exactName(source)
      authored <- ScalametaTermDefinitionNameAuthoring
        .author(expected)
        .toRight(nameUnsupported)
      _ <- Either.cond(authored.value == source, (), nameUnsupported)
    yield ()

  private def requireFreshTypeName(source: String): Either[Error, Unit] =
    for
      expected <- exactName(source)
      authored <- try Right(Type.Name(expected.decoded))
        catch case NonFatal(_) => Left(nameUnsupported)
      projected <- ScalametaDefinitionNameProjection
        .project(authored)
        .left
        .map(_ => nameUnsupported)
      _ <- Either.cond(
        projected == expected && authored.value == source,
        (),
        nameUnsupported
      )
    yield ()

  private def exactName(source: String): Either[Error, DefinitionName] =
    Option(source)
      .toRight(nameUnsupported)
      .flatMap(value => DefinitionName.fromSource(value).left.map(_ => nameUnsupported))

  private def requireLexicalRoles(plan: Plan): Either[Error, Unit] =
    Either.cond(
      plan.ordinaryParameter.displayName != plan.contextualParameter.displayName &&
        contextualConstructorName(plan) != plan.typeParameter.displayName &&
        plan.resultType.value != plan.typeParameter.displayName,
      (),
      lexicalRoleUnsupported
    )

  private def construct(plan: Plan): Either[Error, Defn.Def] =
    try
      val typeName = plan.typeParameter.displayName
      val methodName = plan.methodIdentity.sourceName
      val ordinaryName = plan.ordinaryParameter.displayName
      val contextualName = plan.contextualParameter.displayName
      val typeParameter = Type.Param(
        Nil,
        Type.Name(typeName),
        Type.ParamClause(Nil),
        Type.Bounds.empty
      )
      val ordinaryParameter = Term.Param(
        Nil,
        Term.Name(ordinaryName),
        Some(Type.Name(typeName)),
        None
      )
      val contextualParameter = Term.Param(
        Nil,
        Term.Name(contextualName),
        Some(
          Type.Apply(
            Type.Name(contextualConstructorName(plan)),
            Type.ArgClause(List(Type.Name(typeName)))
          )
        ),
        None
      )
      Right(
        Defn.Def(
          Nil,
          Term.Name(methodName),
          List(
            Member.ParamClauseGroup(
              Type.ParamClause(List(typeParameter)),
              List(
                Term.ParamClause(List(ordinaryParameter)),
                Term.ParamClause(List(contextualParameter), Some(Mod.Using()))
              )
            )
          ),
          Some(Type.Name(plan.resultType.value)),
          Term.Apply(
            Term.Select(Term.Name(contextualName), Term.Name(methodName)),
            Term.ArgClause(List(Term.Name(ordinaryName)))
          )
        )
      )
    catch case NonFatal(_) => Left(constructionFailed)

  private def requireFresh(authored: Defn.Def): Either[Error, Unit] =
    try
      Either.cond(
        allTrees(authored).forall(_.pos == Position.None),
        (),
        constructionFailed
      )
    catch case NonFatal(_) => Left(constructionFailed)

  private def requireExactRoundTrip(
      authored: Defn.Def,
      expected: RoleSnapshot
  ): Either[Error, Unit] =
    try
      ScalametaDelegatedForwardingMethodProjection.project(authored) match
        case Right(ProjectedDelegatedForwardingMethod(projected, None)) =>
          roleSnapshot(projected) match
            case Right(actual) => Either.cond(actual == expected, (), roundTripFailed)
            case Left(_) => Left(roundTripFailed)
        case _ => Left(roundTripFailed)
    catch case NonFatal(_) => Left(roundTripFailed)

  private def roleSnapshot(plan: Plan): Either[Error, RoleSnapshot] =
    try
      val declarations = Vector(
        plan.typeParameter.binderId -> DeclarationRole.TypeParameter,
        plan.ordinaryParameter.binderId -> DeclarationRole.OrdinaryParameter,
        plan.contextualParameter.binderId -> DeclarationRole.ContextualParameter
      )
      def roleOf(binder: BinderId): Either[Error, DeclarationRole] =
        declarations
          .collectFirst { case (`binder`, role) => role }
          .toRight(planUnsupported)

      plan.contextualParameter.parameterType match
        case Applied(
              SourceName(contextualConstructor),
              Vector(contextualReference: TypeParameterReference)
            ) =>
          for
            ordinaryTypeRole <- roleOf(plan.ordinaryParameter.parameterType.binderId)
            contextualTypeRole <- roleOf(contextualReference.binderId)
            receiverRole <- roleOf(plan.body.receiver.binderId)
            selectedRole <- Either.cond(
              plan.methodIdentity eq plan.body.selectedMethodIdentity,
              SelectedMemberRole.GeneratedMethod,
              planUnsupported
            )
            argumentRole <- roleOf(plan.body.argument.binderId)
          yield RoleSnapshot(
            plan.methodIdentity.sourceName,
            plan.typeParameter.displayName,
            plan.ordinaryParameter.displayName,
            plan.contextualParameter.displayName,
            contextualConstructor,
            plan.resultType.value,
            ordinaryTypeRole,
            contextualTypeRole,
            receiverRole,
            plan.body.selectedMethodIdentity.sourceName,
            selectedRole,
            argumentRole
          )
        case _ => Left(planUnsupported)
    catch case NonFatal(_) => Left(planUnsupported)

  private def contextualConstructorName(plan: Plan): String =
    plan.contextualParameter.parameterType match
      case Applied(SourceName(value), Vector(_: TypeParameterReference)) => value
      case _ => throw new IllegalArgumentException("revalidated contextual Type")

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
      "NEUTRAL_DELEGATED_FORWARDING_AUTHORING_MISSING",
      "the DelegatedForwardingMethodPlan.Plan must be present."
    )

  private def planUnsupported: Error =
    error(
      "NEUTRAL_DELEGATED_FORWARDING_AUTHORING_PLAN_UNSUPPORTED",
      "the input is outside the existing DelegatedForwardingMethodPlan.create contract."
    )

  private def nameUnsupported: Error =
    error(
      "NEUTRAL_DELEGATED_FORWARDING_AUTHORING_NAME_UNSUPPORTED",
      "a declaration, Type, or selected-member name is outside the exact fresh AUXify-043 spelling intersection."
    )

  private def lexicalRoleUnsupported: Error =
    error(
      "NEUTRAL_DELEGATED_FORWARDING_AUTHORING_LEXICAL_ROLE_UNSUPPORTED",
      "source spelling would collapse distinct delegated-forwarding declaration or Type roles."
    )

  private def constructionFailed: Error =
    error(
      "NEUTRAL_DELEGATED_FORWARDING_AUTHORING_CONSTRUCTION_FAILED",
      "the exact direct fresh Scalameta delegated-forwarding method could not be constructed."
    )

  private def roundTripFailed: Error =
    error(
      "NEUTRAL_DELEGATED_FORWARDING_AUTHORING_ROUNDTRIP_FAILED",
      "the authored method did not reproject with the same three-role semantics and no provenance."
    )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
