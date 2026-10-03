package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionName
import _root_.quasiquotes.definitions.ScopedType.*
import _root_.quasiquotes.definitions.TypeMemberInstanceFactoryPlan
import _root_.quasiquotes.definitions.TypeMemberInstanceFactoryPlan.Plan

import scala.annotation.nowarn
import scala.meta.*
import scala.util.control.NonFatal

/** Direct structural authoring for the exact AUXify-082 abstract-Type-member factory. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaTypeMemberInstanceFactoryAuthoring:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def author(plan: Plan): Either[Error, Defn.Def] =
    Option(plan)
      .toRight(missing)
      .flatMap(authorPresent)

  private def authorPresent(plan: Plan): Either[Error, Defn.Def] =
    for
      expected <- TypeMemberInstanceFactoryPlan.validate(plan).left.map(_ => planUnsupported)
      _ <- requireRepresentableNames(plan)
      _ <- requireLexicalRoles(plan)
      authored <- construct(plan)
      _ <- requireFresh(authored)
      _ <- requireExactRoundTrip(authored, expected)
    yield authored

  private def requireRepresentableNames(plan: Plan): Either[Error, Unit] =
    try
      for
        _ <- requireFreshTermName(plan.factoryDisplayName)
        _ <- traverseUnit(
          Vector(
            plan.firstTypeParameter.displayName,
            plan.secondTypeParameter.displayName,
            targetConstructorName(plan),
            plan.resultAlias.memberName
          )
        )(requireFreshTypeName)
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
        plan.firstTypeParameter.displayName != plan.secondTypeParameter.displayName &&
          targetConstructorName(plan) != plan.firstTypeParameter.displayName &&
          targetConstructorName(plan) != plan.secondTypeParameter.displayName &&
          plan.resultAlias.memberName != plan.secondTypeParameter.displayName,
        (),
        lexicalRoleUnsupported
      )
    catch case NonFatal(_) => Left(lexicalRoleUnsupported)

  private def construct(plan: Plan): Either[Error, Defn.Def] =
    try
      val firstName = plan.firstTypeParameter.displayName
      val secondName = plan.secondTypeParameter.displayName
      val targetName = targetConstructorName(plan)
      val memberName = plan.resultAlias.memberName
      val first = typeParameter(firstName)
      val second = typeParameter(secondName)
      val target = Type.Apply(
        Type.Name(targetName),
        Type.ArgClause(List(Type.Name(firstName)))
      )
      val resultAlias = concreteAlias(memberName, secondName)
      val anonymousAlias = concreteAlias(memberName, secondName)
      val result = Type.Refine(
        Some(target),
        Stat.Block(List(resultAlias))
      )
      val template = Template(
        Nil,
        List(
          Init(
            target,
            Name.Anonymous(),
            List.empty[Term.ArgClause]
          )
        ),
        Self(Name.Anonymous(), None),
        List(anonymousAlias),
        Nil
      )
      Right(
        Defn.Def(
          Nil,
          Term.Name(plan.factoryDisplayName),
          List(
            Member.ParamClauseGroup(
              Type.ParamClause(List(first, second)),
              Nil
            )
          ),
          Some(result),
          Term.NewAnonymous(template)
        )
      )
    catch case NonFatal(_) => Left(constructionFailed)

  private def typeParameter(name: String): Type.Param =
    Type.Param(
      Nil,
      Type.Name(name),
      Type.ParamClause(Nil),
      Type.Bounds.empty
    )

  private def concreteAlias(memberName: String, rhsName: String): Defn.Type =
    Defn.Type(
      Nil,
      Type.Name(memberName),
      Type.ParamClause(Nil),
      Type.Name(rhsName),
      Type.Bounds.empty
    )

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
      expected: TypeMemberInstanceFactoryPlan.RoleSnapshot
  ): Either[Error, Unit] =
    try
      ScalametaTypeMemberInstanceFactoryProjection.project(authored) match
        case Right(ProjectedTypeMemberInstanceFactory(projected, None)) =>
          Either.cond(projected.roleSnapshot == expected, (), roundTripFailed)
        case _ => Left(roundTripFailed)
    catch case NonFatal(_) => Left(roundTripFailed)

  private def targetConstructorName(plan: Plan): String =
    plan.resultTarget match
      case Applied(SourceName(value), Vector(_: TypeParameterReference)) => value
      case _ => throw new IllegalArgumentException("revalidated unary target Type")

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
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_AUTHORING_MISSING",
      "the TypeMemberInstanceFactoryPlan.Plan must be present."
    )

  private def planUnsupported: Error =
    error(
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_AUTHORING_PLAN_UNSUPPORTED",
      "the input is outside the TypeMemberInstanceFactoryPlan.create contract."
    )

  private def nameUnsupported: Error =
    error(
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_AUTHORING_NAME_UNSUPPORTED",
      "a declaration or target name is outside the exact fresh abstract-Type-member factory spelling intersection."
    )

  private def lexicalRoleUnsupported: Error =
    error(
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_AUTHORING_LEXICAL_ROLE_UNSUPPORTED",
      "source spelling would collapse a target, Type-parameter, or concrete-alias role."
    )

  private def constructionFailed: Error =
    error(
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_AUTHORING_CONSTRUCTION_FAILED",
      "the exact direct fresh Scalameta abstract-Type-member factory could not be constructed."
    )

  private def roundTripFailed: Error =
    error(
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_AUTHORING_ROUNDTRIP_FAILED",
      "the authored factory did not reproject with the same roles and no provenance."
    )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
