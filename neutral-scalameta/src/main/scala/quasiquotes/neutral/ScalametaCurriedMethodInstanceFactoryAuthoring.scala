package quasiquotes.neutral

import _root_.quasiquotes.definitions.CurriedMethodInstanceFactoryPlan
import _root_.quasiquotes.definitions.CurriedMethodInstanceFactoryPlan.Plan
import _root_.quasiquotes.definitions.DefinitionName
import _root_.quasiquotes.definitions.ScopedType.*

import scala.annotation.nowarn
import scala.meta.*
import scala.util.control.NonFatal

/** Direct structural authoring for the exact AUXify-087 curried-method factory. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaCurriedMethodInstanceFactoryAuthoring:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def author(plan: Plan): Either[Error, Defn.Def] =
    Option(plan).toRight(missing).flatMap(authorPresent)

  private def authorPresent(plan: Plan): Either[Error, Defn.Def] =
    for
      expected <- CurriedMethodInstanceFactoryPlan.validate(plan).left.map(_ => planUnsupported)
      _ <- requireRepresentableNames(plan)
      _ <- requireLexicalRoles(plan)
      authored <- construct(plan)
      _ <- requireFresh(authored)
      _ <- requireExactRoundTrip(authored, expected)
    yield authored

  private def requireRepresentableNames(plan: Plan): Either[Error, Unit] =
    try
      for
        _ <- traverseUnit(
          Vector(
            plan.factoryDisplayName,
            plan.strictCarrier.displayName,
            plan.methodOverride.memberDisplayName,
            plan.firstParameter.displayName,
            plan.secondParameter.displayName
          )
        )(requireFreshTermName)
        _ <- traverseUnit(Vector(plan.typeParameter.displayName, targetConstructorName(plan)))(requireFreshTypeName)
      yield ()
    catch case NonFatal(_) => Left(nameUnsupported)

  private def requireFreshTermName(source: String): Either[Error, Unit] =
    for
      expected <- exactPlainName(source)
      authored <- ScalametaTermDefinitionNameAuthoring.author(expected).toRight(nameUnsupported)
      _ <- Either.cond(authored.value == source, (), nameUnsupported)
    yield ()

  private def requireFreshTypeName(source: String): Either[Error, Unit] =
    for
      expected <- exactPlainName(source)
      authored <- try Right(Type.Name(expected.decoded))
        catch case NonFatal(_) => Left(nameUnsupported)
      projected <- ScalametaDefinitionNameProjection.project(authored).left.map(_ => nameUnsupported)
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
        targetConstructorName(plan) != plan.typeParameter.displayName &&
          plan.strictCarrier.displayName != plan.methodOverride.memberDisplayName &&
          plan.strictCarrier.displayName != plan.firstParameter.displayName &&
          plan.strictCarrier.displayName != plan.secondParameter.displayName &&
          plan.firstParameter.displayName != plan.secondParameter.displayName,
        (),
        lexicalRoleUnsupported
      )
    catch case NonFatal(_) => Left(lexicalRoleUnsupported)

  private def construct(plan: Plan): Either[Error, Defn.Def] =
    try
      val typeName = plan.typeParameter.displayName
      val carrierName = plan.strictCarrier.displayName
      val targetName = targetConstructorName(plan)
      val typeParameter = Type.Param(Nil, Type.Name(typeName), Type.ParamClause(Nil), Type.Bounds.empty)
      val carrierType = Type.Function(
        List(Type.Name(typeName)),
        Type.Function(List(Type.Name(typeName)), Type.Name(typeName))
      )
      val carrier = Term.Param(Nil, Term.Name(carrierName), Some(carrierType), None)
      val target = Type.Apply(Type.Name(targetName), Type.ArgClause(List(Type.Name(typeName))))
      val first = Term.Param(Nil, Term.Name(plan.firstParameter.displayName), Some(Type.Name(typeName)), None)
      val second = Term.Param(Nil, Term.Name(plan.secondParameter.displayName), Some(Type.Name(typeName)), None)
      val body = Term.Apply(
        Term.Apply(Term.Name(carrierName), Term.ArgClause(List(Term.Name(plan.firstParameter.displayName)))),
        Term.ArgClause(List(Term.Name(plan.secondParameter.displayName)))
      )
      val member = Defn.Def(
        List(Mod.Override()),
        Term.Name(plan.methodOverride.memberDisplayName),
        List(
          Member.ParamClauseGroup(
            Type.ParamClause(Nil),
            List(Term.ParamClause(List(first)), Term.ParamClause(List(second)))
          )
        ),
        Some(Type.Name(typeName)),
        body
      )
      val template = Template(
        Nil,
        List(Init(target, Name.Anonymous(), List.empty[Term.ArgClause])),
        Self(Name.Anonymous(), None),
        List(member),
        Nil
      )
      Right(
        Defn.Def(
          Nil,
          Term.Name(plan.factoryDisplayName),
          List(
            Member.ParamClauseGroup(
              Type.ParamClause(List(typeParameter)),
              List(Term.ParamClause(List(carrier)))
            )
          ),
          Some(target),
          Term.NewAnonymous(template)
        )
      )
    catch case NonFatal(_) => Left(constructionFailed)

  private def requireFresh(authored: Defn.Def): Either[Error, Unit] =
    try Either.cond(allTrees(authored).forall(_.pos == Position.None), (), constructionFailed)
    catch case NonFatal(_) => Left(constructionFailed)

  private def requireExactRoundTrip(
      authored: Defn.Def,
      expected: CurriedMethodInstanceFactoryPlan.RoleSnapshot
  ): Either[Error, Unit] =
    try
      ScalametaCurriedMethodInstanceFactoryProjection.project(authored) match
        case Right(ProjectedCurriedMethodInstanceFactory(projected, None)) =>
          Either.cond(projected.roleSnapshot == expected, (), roundTripFailed)
        case _ => Left(roundTripFailed)
    catch case NonFatal(_) => Left(roundTripFailed)

  private def targetConstructorName(plan: Plan): String = plan.resultTarget match
    case Applied(SourceName(value), Vector(_: TypeParameterReference)) => value
    case _ => throw new IllegalArgumentException("revalidated unary target Type")

  private def traverseUnit[A](values: Vector[A])(validate: A => Either[Error, Unit]): Either[Error, Unit] =
    values.foldLeft(Right(()): Either[Error, Unit])((validated, value) => validated.flatMap(_ => validate(value)))

  private def allTrees(root: Tree): List[Tree] = root :: root.children.toList.flatMap(allTrees)
  private def missing = error("NEUTRAL_CURRIED_METHOD_FACTORY_AUTHORING_MISSING", "the CurriedMethodInstanceFactoryPlan.Plan must be present.")
  private def planUnsupported = error("NEUTRAL_CURRIED_METHOD_FACTORY_AUTHORING_PLAN_UNSUPPORTED", "the input is outside the CurriedMethodInstanceFactoryPlan.create contract.")
  private def nameUnsupported = error("NEUTRAL_CURRIED_METHOD_FACTORY_AUTHORING_NAME_UNSUPPORTED", "a role name is outside the exact fresh curried-method factory spelling intersection.")
  private def lexicalRoleUnsupported = error("NEUTRAL_CURRIED_METHOD_FACTORY_AUTHORING_LEXICAL_ROLE_UNSUPPORTED", "source spelling would collapse required curried-method roles.")
  private def constructionFailed = error("NEUTRAL_CURRIED_METHOD_FACTORY_AUTHORING_CONSTRUCTION_FAILED", "the exact direct fresh Scalameta factory could not be constructed.")
  private def roundTripFailed = error("NEUTRAL_CURRIED_METHOD_FACTORY_AUTHORING_ROUNDTRIP_FAILED", "the authored factory did not reproject with the same roles and no provenance.")
  private def error(code: String, detail: String): Error = Error(code, detail)
