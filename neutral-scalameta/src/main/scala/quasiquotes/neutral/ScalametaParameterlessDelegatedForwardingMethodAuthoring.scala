package quasiquotes.neutral

import quasiquotes.definitions.DefinitionName
import quasiquotes.definitions.ParameterlessDelegatedForwardingPlan
import quasiquotes.definitions.ParameterlessDelegatedForwardingPlan.Plan
import quasiquotes.definitions.ScopedType.*

import scala.annotation.nowarn
import scala.meta.*
import scala.util.control.NonFatal

/** Direct structural authoring for the exact AUXify-083 parameterless forwarder. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaParameterlessDelegatedForwardingMethodAuthoring:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def author(plan: Plan): Either[Error, Defn.Def] =
    Option(plan)
      .toRight(missing)
      .flatMap(authorPresent)

  private def authorPresent(plan: Plan): Either[Error, Defn.Def] =
    for
      expected <- ParameterlessDelegatedForwardingPlan.validate(plan).left.map(_ => planUnsupported)
      _ <- requireRepresentableNames(plan)
      _ <- requireLexicalRoles(plan)
      authored <- construct(plan)
      _ <- requireFresh(authored)
      _ <- requireExactRoundTrip(authored, expected)
    yield authored

  private def requireRepresentableNames(plan: Plan): Either[Error, Unit] =
    try
      for
        _ <- requireFreshTermName(plan.methodIdentity.sourceName)
        _ <- requireFreshTypeName(plan.typeParameter.displayName)
        _ <- requireFreshTermName(plan.contextualParameter.displayName)
        _ <- requireFreshTypeName(contextualConstructorName(plan))
      yield ()
    catch case NonFatal(_) => Left(nameUnsupported)

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
      .flatMap(name => Either.cond(name.source == name.decoded, name, nameUnsupported))

  private def requireLexicalRoles(plan: Plan): Either[Error, Unit] =
    try
      Either.cond(
        contextualConstructorName(plan) != plan.typeParameter.displayName,
        (),
        lexicalRoleUnsupported
      )
    catch case NonFatal(_) => Left(lexicalRoleUnsupported)

  private def construct(plan: Plan): Either[Error, Defn.Def] =
    try
      val methodName = plan.methodIdentity.sourceName
      val typeParameterName = plan.typeParameter.displayName
      val contextualParameterName = plan.contextualParameter.displayName
      val contextualConstructor = contextualConstructorName(plan)
      val typeParameter = Type.Param(
        Nil,
        Type.Name(typeParameterName),
        Type.ParamClause(Nil),
        Type.Bounds.empty
      )
      val contextualParameter = Term.Param(
        List(Mod.Using()),
        Term.Name(contextualParameterName),
        Some(
          Type.Apply(
            Type.Name(contextualConstructor),
            Type.ArgClause(List(Type.Name(typeParameterName)))
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
                Term.ParamClause(
                  List(contextualParameter),
                  Some(Mod.Using())
                )
              )
            )
          ),
          Some(Type.Name(typeParameterName)),
          Term.Select(Term.Name(contextualParameterName), Term.Name(methodName))
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
      expected: ParameterlessDelegatedForwardingPlan.RoleSnapshot
  ): Either[Error, Unit] =
    try
      ScalametaParameterlessDelegatedForwardingMethodProjection.project(authored) match
        case Right(ProjectedParameterlessDelegatedForwardingMethod(projected, None))
            if projected.body.selectedMethodIdentity.eq(projected.methodIdentity) =>
          Either.cond(projected.roleSnapshot == expected, (), roundTripFailed)
        case _ => Left(roundTripFailed)
    catch case NonFatal(_) => Left(roundTripFailed)

  private def contextualConstructorName(plan: Plan): String =
    plan.contextualParameter.parameterType match
      case Applied(SourceName(value), Vector(reference: TypeParameterReference))
          if reference.eq(plan.evidenceTypeArgument) =>
        value
      case _ => throw new IllegalArgumentException("revalidated unary contextual Type")

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)

  private def missing: Error =
    error(
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_AUTHORING_MISSING",
      "the ParameterlessDelegatedForwardingPlan.Plan must be present."
    )

  private def planUnsupported: Error =
    error(
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_AUTHORING_PLAN_UNSUPPORTED",
      "the input is outside the complete ParameterlessDelegatedForwardingPlan contract."
    )

  private def nameUnsupported: Error =
    error(
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_AUTHORING_NAME_UNSUPPORTED",
      "a source role is outside the exact fresh parameterless-forwarder spelling intersection."
    )

  private def lexicalRoleUnsupported: Error =
    error(
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_AUTHORING_LEXICAL_ROLE_UNSUPPORTED",
      "source spelling would collapse the evidence constructor and non-higher-kinded Type parameter."
    )

  private def constructionFailed: Error =
    error(
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_AUTHORING_CONSTRUCTION_FAILED",
      "the exact direct fresh Scalameta parameterless forwarder could not be constructed."
    )

  private def roundTripFailed: Error =
    error(
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_AUTHORING_ROUNDTRIP_FAILED",
      "the authored method did not reproject with the same roles, MethodIdentity coherence, and no provenance."
    )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
