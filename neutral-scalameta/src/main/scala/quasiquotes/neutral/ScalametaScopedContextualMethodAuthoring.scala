package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionName
import _root_.quasiquotes.definitions.ScopedContextualMethodPlan
import _root_.quasiquotes.definitions.ScopedType
import _root_.quasiquotes.definitions.ScopedType.*
import _root_.quasiquotes.parser.BinderId

import scala.annotation.nowarn
import scala.meta.*
import scala.util.control.NonFatal

/** Direct structural authoring for the exact bounded AUXify-037 scoped plan. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaScopedContextualMethodAuthoring:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  private enum DeclarationRole derives CanEqual:
    case FirstTypeParameter, SecondTypeParameter, ContextualTermParameter

  private final case class RoleSnapshot(
      methodSourceName: String,
      firstTypeParameterSourceName: String,
      secondTypeParameterSourceName: String,
      firstUpperBoundSourceName: String,
      secondUpperBoundSourceName: String,
      contextualTermSourceName: String,
      contextualConstructorSourceName: String,
      contextualArgumentRoles: Vector[DeclarationRole],
      refinementConstructorSourceName: String,
      refinementArgumentRoles: Vector[DeclarationRole],
      refinementMemberSourceName: String,
      selectedPrefixRole: DeclarationRole,
      selectedMemberSourceName: String,
      bodyRole: DeclarationRole
  ) derives CanEqual

  def author(plan: ScopedContextualMethodPlan): Either[Error, Defn.Def] =
    Option(plan)
      .toRight(missing)
      .flatMap(authorPresent)

  private def authorPresent(
      plan: ScopedContextualMethodPlan
  ): Either[Error, Defn.Def] =
    for
      validated <- revalidate(plan)
      _ <- requireProjectorStructure(validated)
      expected <- roleSnapshot(validated)
      _ <- requireRepresentableNames(validated)
      _ <- requireLexicalRoles(validated)
      authored <- construct(validated)
      _ <- requireFresh(authored)
      _ <- requireExactRoundTrip(authored, expected)
    yield authored

  private def revalidate(
      plan: ScopedContextualMethodPlan
  ): Either[Error, ScopedContextualMethodPlan] =
    try
      ScopedContextualMethodPlan
        .create(
          plan.methodDisplayName,
          plan.typeParameters,
          plan.contextualTermBinderId,
          plan.contextualDisplayName,
          plan.contextualType,
          plan.resultType,
          plan.bodyTermBinderId
        )
        .left
        .map(_ => planUnsupported)
    catch case NonFatal(_) => Left(planUnsupported)

  private def requireProjectorStructure(
      plan: ScopedContextualMethodPlan
  ): Either[Error, Unit] =
    try
      Either.cond(
        plan.typeParameters(0).upperBound == plan.typeParameters(1).upperBound,
        (),
        structureUnsupported
      )
    catch case NonFatal(_) => Left(structureUnsupported)

  private def requireRepresentableNames(
      plan: ScopedContextualMethodPlan
  ): Either[Error, Unit] =
    try
      val parameters = plan.typeParameters
      for
        _ <- traverseUnit(
          Vector(plan.methodDisplayName, plan.contextualDisplayName)
        )(requireFreshTermName)
        _ <- traverseUnit(
          Vector(
            parameters(0).displayName,
            parameters(1).displayName,
            upperBoundName(parameters(0)),
            upperBoundName(parameters(1)),
            contextualConstructorName(plan),
            plan.refinementMember.memberName
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

  private def requireLexicalRoles(
      plan: ScopedContextualMethodPlan
  ): Either[Error, Unit] =
    try
      val firstName = plan.typeParameters(0).displayName
      val secondName = plan.typeParameters(1).displayName
      val upperBound = upperBoundName(plan.typeParameters(0))
      val constructor = contextualConstructorName(plan)
      Either.cond(
        upperBound != firstName &&
          upperBound != secondName &&
          constructor != firstName &&
          constructor != secondName,
        (),
        lexicalRoleUnsupported
      )
    catch case NonFatal(_) => Left(lexicalRoleUnsupported)

  private def construct(
      plan: ScopedContextualMethodPlan
  ): Either[Error, Defn.Def] =
    try
      val parameters = plan.typeParameters
      val firstName = parameters(0).displayName
      val secondName = parameters(1).displayName
      val upperBound = upperBoundName(parameters(0))
      val contextualName = plan.contextualDisplayName
      val memberName = plan.refinementMember.memberName
      val authoredParameters = List(firstName, secondName).map { name =>
        Type.Param(
          Nil,
          Type.Name(name),
          Type.ParamClause(Nil),
          Type.Bounds(None, Some(Type.Name(upperBound)), Nil, Nil)
        )
      }
      val applied = Type.Apply(
        Type.Name(contextualConstructorName(plan)),
        Type.ArgClause(List(Type.Name(firstName), Type.Name(secondName)))
      )
      val refinementMember = Defn.Type(
        Nil,
        Type.Name(memberName),
        Type.ParamClause(Nil),
        Type.Select(Term.Name(contextualName), Type.Name(memberName)),
        Type.Bounds.empty
      )
      val resultType = Type.Refine(
        Some(applied),
        Stat.Block(List(refinementMember))
      )
      val contextualParameter = Term.Param(
        Nil,
        Term.Name(contextualName),
        Some(applied),
        None
      )
      Right(
        Defn.Def(
          Nil,
          Term.Name(plan.methodDisplayName),
          List(
            Member.ParamClauseGroup(
              Type.ParamClause(authoredParameters),
              List(
                Term.ParamClause(
                  List(contextualParameter),
                  Some(Mod.Using())
                )
              )
            )
          ),
          Some(resultType),
          Term.Name(contextualName)
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
      ScalametaScopedContextualMethodProjection.project(authored) match
        case Right(ProjectedScopedContextualMethod(projected, None)) =>
          roleSnapshot(projected) match
            case Right(actual) => Either.cond(actual == expected, (), roundTripFailed)
            case Left(_) => Left(roundTripFailed)
        case _ => Left(roundTripFailed)
    catch case NonFatal(_) => Left(roundTripFailed)

  private def roleSnapshot(
      plan: ScopedContextualMethodPlan
  ): Either[Error, RoleSnapshot] =
    try
      val parameters = plan.typeParameters
      val declarations = Vector(
        parameters(0).binderId -> DeclarationRole.FirstTypeParameter,
        parameters(1).binderId -> DeclarationRole.SecondTypeParameter,
        plan.contextualTermBinderId -> DeclarationRole.ContextualTermParameter
      )
      def roleOf(binder: BinderId): Either[Error, DeclarationRole] =
        declarations
          .collectFirst { case (`binder`, role) => role }
          .toRight(planUnsupported)

      def referenceRoles(
          values: Vector[ScopedType]
      ): Either[Error, Vector[DeclarationRole]] =
        values.foldLeft(
          Right(Vector.empty): Either[Error, Vector[DeclarationRole]]
        ) { (accumulated, value) =>
          for
            roles <- accumulated
            reference <- value match
              case current: TypeParameterReference => Right(current)
              case _ => Left(planUnsupported)
            role <- roleOf(reference.binderId)
          yield roles :+ role
        }

      (plan.contextualType, plan.resultType.base) match
        case (
              Applied(SourceName(contextualConstructor), contextualArguments),
              Applied(SourceName(refinementConstructor), refinementArguments)
            ) =>
          for
            contextualRoles <- referenceRoles(contextualArguments)
            refinementRoles <- referenceRoles(refinementArguments)
            selectedPrefixRole <- roleOf(plan.selectedResult.prefixTermBinderId)
            bodyRole <- roleOf(plan.bodyTermBinderId)
          yield RoleSnapshot(
            plan.methodDisplayName,
            parameters(0).displayName,
            parameters(1).displayName,
            upperBoundName(parameters(0)),
            upperBoundName(parameters(1)),
            plan.contextualDisplayName,
            contextualConstructor,
            contextualRoles,
            refinementConstructor,
            refinementRoles,
            plan.refinementMember.memberName,
            selectedPrefixRole,
            plan.selectedResult.memberExpectation,
            bodyRole
          )
        case _ => Left(planUnsupported)
    catch case NonFatal(_) => Left(planUnsupported)

  private def upperBoundName(
      parameter: _root_.quasiquotes.definitions.ScopedTypeParameter
  ): String =
    parameter.upperBound match
      case SourceName(value) => value
      case _ => throw new IllegalArgumentException("revalidated source-named upper bound")

  private def contextualConstructorName(
      plan: ScopedContextualMethodPlan
  ): String =
    plan.contextualType.constructor match
      case SourceName(value) => value
      case _ => throw new IllegalArgumentException("revalidated source-named constructor")

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
      "NEUTRAL_SCOPED037_AUTHORING_MISSING",
      "the ScopedContextualMethodPlan must be present."
    )

  private def planUnsupported: Error =
    error(
      "NEUTRAL_SCOPED037_AUTHORING_PLAN_UNSUPPORTED",
      "the input is outside the existing ScopedContextualMethodPlan.create contract."
    )

  private def structureUnsupported: Error =
    error(
      "NEUTRAL_SCOPED037_AUTHORING_STRUCTURE_UNSUPPORTED",
      "the two Type parameters must share one source-named upper bound for exact scoped projection."
    )

  private def nameUnsupported: Error =
    error(
      "NEUTRAL_SCOPED037_AUTHORING_NAME_UNSUPPORTED",
      "a declaration or Type name is outside the exact fresh scoped-037 spelling intersection."
    )

  private def lexicalRoleUnsupported: Error =
    error(
      "NEUTRAL_SCOPED037_AUTHORING_LEXICAL_ROLE_UNSUPPORTED",
      "source spelling would collapse an external Type name into a local Type-parameter role."
    )

  private def constructionFailed: Error =
    error(
      "NEUTRAL_SCOPED037_AUTHORING_CONSTRUCTION_FAILED",
      "the exact direct fresh Scalameta scoped contextual method could not be constructed."
    )

  private def roundTripFailed: Error =
    error(
      "NEUTRAL_SCOPED037_AUTHORING_ROUNDTRIP_FAILED",
      "the authored method did not reproject with the same three-role semantics and no provenance."
    )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
