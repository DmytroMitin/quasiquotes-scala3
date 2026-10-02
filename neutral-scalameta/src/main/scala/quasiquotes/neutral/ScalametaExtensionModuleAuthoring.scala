package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionName
import _root_.quasiquotes.definitions.ExtensionModulePlan
import _root_.quasiquotes.definitions.ExtensionModulePlan.Plan
import _root_.quasiquotes.definitions.ScopedType.*

import scala.annotation.nowarn
import scala.meta.*
import scala.util.control.NonFatal

/** Direct structural authoring for the exact AUXify-045 extension module. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaExtensionModuleAuthoring:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def author(plan: Plan): Either[Error, Defn.Object] =
    Option(plan)
      .toRight(missing)
      .flatMap(authorPresent)

  private def authorPresent(plan: Plan): Either[Error, Defn.Object] =
    for
      validated <- revalidate(plan)
      expected = validated.roleSnapshot
      _ <- requireRepresentableNames(validated)
      _ <- requireLexicalRoles(validated)
      authored <- construct(validated)
      _ <- requireFresh(authored)
      _ <- requireExactRoundTrip(authored, expected)
    yield authored

  private def revalidate(plan: Plan): Either[Error, Plan] =
    try
      ExtensionModulePlan
        .create(
          plan.moduleDisplayName,
          plan.typeParameter.displayName,
          plan.receiverParameter.displayName,
          plan.methodIdentity.sourceName,
          plan.ordinaryArgument.displayName,
          plan.contextualParameter.displayName,
          evidenceConstructorName(plan)
        )
        .left
        .map(_ => planUnsupported)
    catch case NonFatal(_) => Left(planUnsupported)

  private def requireRepresentableNames(plan: Plan): Either[Error, Unit] =
    try
      for
        _ <- traverseUnit(
          Vector(
            plan.moduleDisplayName,
            plan.receiverParameter.displayName,
            plan.methodIdentity.sourceName,
            plan.ordinaryArgument.displayName,
            plan.contextualParameter.displayName
          )
        )(requireFreshTermName)
        _ <- traverseUnit(
          Vector(
            plan.typeParameter.displayName,
            evidenceConstructorName(plan)
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
      _ <- Either.cond(
        expected.source == expected.decoded && authored.value == source,
        (),
        nameUnsupported
      )
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
        expected.source == expected.decoded &&
          projected == expected &&
          authored.value == source,
        (),
        nameUnsupported
      )
    yield ()

  private def exactName(source: String): Either[Error, DefinitionName] =
    Option(source)
      .toRight(nameUnsupported)
      .flatMap(value => DefinitionName.fromSource(value).left.map(_ => nameUnsupported))

  private def requireLexicalRoles(plan: Plan): Either[Error, Unit] =
    try
      Either.cond(
        Vector(
          plan.receiverParameter.displayName,
          plan.ordinaryArgument.displayName,
          plan.contextualParameter.displayName
        ).distinct.size == 3 &&
          evidenceConstructorName(plan) != plan.typeParameter.displayName,
        (),
        lexicalRoleUnsupported
      )
    catch case NonFatal(_) => Left(lexicalRoleUnsupported)

  private def construct(plan: Plan): Either[Error, Defn.Object] =
    try
      val typeName = plan.typeParameter.displayName
      val receiverName = plan.receiverParameter.displayName
      val methodName = plan.methodIdentity.sourceName
      val ordinaryName = plan.ordinaryArgument.displayName
      val contextualName = plan.contextualParameter.displayName
      val typeParameter = Type.Param(
        Nil,
        Type.Name(typeName),
        Type.ParamClause(Nil),
        Type.Bounds.empty
      )
      val receiver = Term.Param(
        Nil,
        Term.Name(receiverName),
        Some(Type.Name(typeName)),
        None
      )
      val ordinary = Term.Param(
        Nil,
        Term.Name(ordinaryName),
        Some(Type.Name(typeName)),
        None
      )
      val contextual = Term.Param(
        List(Mod.Using()),
        Term.Name(contextualName),
        Some(
          Type.Apply(
            Type.Name(evidenceConstructorName(plan)),
            Type.ArgClause(List(Type.Name(typeName)))
          )
        ),
        None
      )
      val method = Defn.Def(
        Nil,
        Term.Name(methodName),
        List(
          Member.ParamClauseGroup(
            Type.ParamClause(Nil),
            List(
              Term.ParamClause(List(ordinary)),
              Term.ParamClause(List(contextual), Some(Mod.Using()))
            )
          )
        ),
        Some(Type.Name(typeName)),
        Term.Apply(
          Term.Select(Term.Name(contextualName), Term.Name(methodName)),
          Term.ArgClause(List(Term.Name(receiverName), Term.Name(ordinaryName)))
        )
      )
      val extension = Defn.ExtensionGroup(
        Some(
          Member.ParamClauseGroup(
            Type.ParamClause(List(typeParameter)),
            List(Term.ParamClause(List(receiver)))
          )
        ),
        method
      )
      Right(
        Defn.Object(
          Nil,
          Term.Name(plan.moduleDisplayName),
          Template(
            Nil,
            Nil,
            Self(Name.Anonymous(), None),
            List(extension),
            Nil
          )
        )
      )
    catch case NonFatal(_) => Left(constructionFailed)

  private def requireFresh(authored: Defn.Object): Either[Error, Unit] =
    try
      Either.cond(
        allTrees(authored).forall(_.pos == Position.None),
        (),
        constructionFailed
      )
    catch case NonFatal(_) => Left(constructionFailed)

  private def requireExactRoundTrip(
      authored: Defn.Object,
      expected: ExtensionModulePlan.RoleSnapshot
  ): Either[Error, Unit] =
    try
      ScalametaExtensionModuleProjection.project(authored) match
        case Right(ProjectedExtensionModule(projected, None)) =>
          Either.cond(projected.roleSnapshot == expected, (), roundTripFailed)
        case _ => Left(roundTripFailed)
    catch case NonFatal(_) => Left(roundTripFailed)

  private def evidenceConstructorName(plan: Plan): String =
    plan.contextualParameter.parameterType match
      case Applied(SourceName(value), Vector(_: TypeParameterReference)) => value
      case _ => throw new IllegalArgumentException("revalidated unary evidence Type")

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
      "NEUTRAL_EXTENSION_MODULE_AUTHORING_MISSING",
      "the ExtensionModulePlan.Plan must be present."
    )

  private def planUnsupported: Error =
    error(
      "NEUTRAL_EXTENSION_MODULE_AUTHORING_PLAN_UNSUPPORTED",
      "the input is outside the existing ExtensionModulePlan.create contract."
    )

  private def nameUnsupported: Error =
    error(
      "NEUTRAL_EXTENSION_MODULE_AUTHORING_NAME_UNSUPPORTED",
      "a declaration or Type name is outside the exact fresh extension-module spelling intersection."
    )

  private def lexicalRoleUnsupported: Error =
    error(
      "NEUTRAL_EXTENSION_MODULE_AUTHORING_LEXICAL_ROLE_UNSUPPORTED",
      "source spelling would collapse distinct extension-module Term or Type roles."
    )

  private def constructionFailed: Error =
    error(
      "NEUTRAL_EXTENSION_MODULE_AUTHORING_CONSTRUCTION_FAILED",
      "the exact direct fresh Scalameta extension module could not be constructed."
    )

  private def roundTripFailed: Error =
    error(
      "NEUTRAL_EXTENSION_MODULE_AUTHORING_ROUNDTRIP_FAILED",
      "the authored module did not reproject with the same seven-role semantics and no provenance."
    )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
