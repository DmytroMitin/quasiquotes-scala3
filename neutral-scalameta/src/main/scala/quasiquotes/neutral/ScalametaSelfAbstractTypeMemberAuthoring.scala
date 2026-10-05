package quasiquotes.neutral

import _root_.quasiquotes.definitions.*

import scala.annotation.nowarn
import scala.meta.*
import scala.util.control.NonFatal

/** Direct structural authoring for the exact bounded AUXify-046/086 family. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaSelfAbstractTypeMemberAuthoring:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def author(plan: SelfAbstractTypeMemberPlan): Either[Error, Decl.Type] =
    Option(plan)
      .toRight(missing)
      .flatMap(authorPresent)

  private def authorPresent(
      plan: SelfAbstractTypeMemberPlan
  ): Either[Error, Decl.Type] =
    for
      validated <- revalidate(plan)
      expected <- snapshot(validated)
      _ <- requireRepresentableNames(validated)
      _ <- requireLexicalRoles(validated)
      authored <- construct(validated)
      _ <- requireFresh(authored)
      _ <- requireExactRoundTrip(authored, validated, expected)
    yield authored

  private def revalidate(
      plan: SelfAbstractTypeMemberPlan
  ): Either[Error, SelfAbstractTypeMemberPlan] =
    try
      SelfAbstractTypeMemberPlan
        .validate(plan)
        .left
        .map(_ => planUnsupported)
        .map(_ => plan)
    catch case NonFatal(_) => Left(planUnsupported)

  private def requireRepresentableNames(
      plan: SelfAbstractTypeMemberPlan
  ): Either[Error, Unit] =
    try
      for
        _ <- requireFreshTypeName(plan.memberName)
        _ <- requireFreshExternalAlias(plan.selfAlias.source)
        _ <- requireFreshTypeName(plan.upperBaseName)
      yield ()
    catch case NonFatal(_) => Left(nameUnsupported)

  private def requireFreshTypeName(source: String): Either[Error, Unit] =
    for
      expected <- Option(source)
        .toRight(nameUnsupported)
        .flatMap(value => DefinitionName.fromSource(value).left.map(_ => nameUnsupported))
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

  private def requireFreshExternalAlias(source: String): Either[Error, Unit] =
    try
      val authored = Term.Name(source)
      Either.cond(authored.value == source, (), nameUnsupported)
    catch case NonFatal(_) => Left(nameUnsupported)

  private def requireLexicalRoles(
      plan: SelfAbstractTypeMemberPlan
  ): Either[Error, Unit] =
    try
      Either.cond(
        plan.upperBaseName != plan.memberName,
        (),
        lexicalRoleUnsupported
      )
    catch case NonFatal(_) => Left(lexicalRoleUnsupported)

  private def construct(
      plan: SelfAbstractTypeMemberPlan
  ): Either[Error, Decl.Type] =
    try
      val memberName = plan.memberName
      val selfAlias = plan.selfAlias.source
      val lower = plan.lowerBoundOption.map(_ =>
        Type.Singleton(Term.Name(selfAlias))
      )
      val upper: Type = plan.fBoundRefinementOption match
        case Some(_) =>
          val refinementAlias = Defn.Type(
            Nil,
            Type.Name(memberName),
            Type.ParamClause(Nil),
            Type.Select(Term.Name(selfAlias), Type.Name(memberName)),
            Type.Bounds.empty
          )
          Type.Refine(
            Some(Type.Name(plan.upperBaseName)),
            Stat.Block(List(refinementAlias))
          )
        case None => Type.Name(plan.upperBaseName)

      Right(
        Decl.Type(
          Nil,
          Type.Name(memberName),
          Type.ParamClause(Nil),
          Type.Bounds(
            lower,
            Some(upper),
            Nil,
            Nil
          )
        )
      )
    catch case NonFatal(_) => Left(constructionFailed)

  private def requireFresh(authored: Decl.Type): Either[Error, Unit] =
    try
      Either.cond(
        allTrees(authored).forall(_.pos == Position.None),
        (),
        constructionFailed
      )
    catch case NonFatal(_) => Left(constructionFailed)

  private def requireExactRoundTrip(
      authored: Decl.Type,
      plan: SelfAbstractTypeMemberPlan,
      expected: SelfAbstractTypeMemberRoleSnapshot
  ): Either[Error, Unit] =
    try
      ScalametaSelfAbstractTypeMemberProjection.projectMatrix(
        authored,
        plan.memberName,
        plan.selfAlias.source,
        plan.upperBaseName
      ) match
        case Right(ProjectedSelfAbstractTypeMember(projected, None)) =>
          snapshot(projected) match
            case Right(actual) =>
              Either.cond(actual == expected, (), roundTripFailed)
            case Left(_) => Left(roundTripFailed)
        case _ => Left(roundTripFailed)
    catch case NonFatal(_) => Left(roundTripFailed)

  private def snapshot(
      plan: SelfAbstractTypeMemberPlan
  ): Either[Error, SelfAbstractTypeMemberRoleSnapshot] =
    try Right(plan.roleSnapshot)
    catch case NonFatal(_) => Left(planUnsupported)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)

  private def missing: Error =
    error(
      "NEUTRAL_SELF_MEMBER_AUTHORING_MISSING",
      "the SelfAbstractTypeMemberPlan must be present."
    )

  private def planUnsupported: Error =
    error(
      "NEUTRAL_SELF_MEMBER_AUTHORING_PLAN_UNSUPPORTED",
      "the input is outside the closed SelfAbstractTypeMemberPlan matrix contract."
    )

  private def nameUnsupported: Error =
    error(
      "NEUTRAL_SELF_MEMBER_AUTHORING_NAME_UNSUPPORTED",
      "a member, upper-base, or external-alias spelling is outside the exact fresh Scalameta intersection."
    )

  private def lexicalRoleUnsupported: Error =
    error(
      "NEUTRAL_SELF_MEMBER_AUTHORING_LEXICAL_ROLE_UNSUPPORTED",
      "the upper-base spelling would resolve to the abstract member being declared instead of an external base."
    )

  private def constructionFailed: Error =
    error(
      "NEUTRAL_SELF_MEMBER_AUTHORING_CONSTRUCTION_FAILED",
      "the exact direct fresh Scalameta self abstract-Type member could not be constructed."
    )

  private def roundTripFailed: Error =
    error(
      "NEUTRAL_SELF_MEMBER_AUTHORING_ROUNDTRIP_FAILED",
      "the authored declaration did not reproject with the same complete matrix semantics and no provenance."
    )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
