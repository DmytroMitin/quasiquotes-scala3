package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

import quasiquotes.neutral.ScalametaContextualMethodProjection

import scala.meta.*

/** Exact backend for the single admitted Scalameta contextual-method slice. */
private[quasiquotes] object ScalametaContextualMethodBackend:
  def lower(
      definition: Defn.Def,
      virtualSourceName: String
  )(using Context): Either[
    ScalametaContextualMethodBackendError,
    GeneratedOriginDefinitionResult
  ] =
    for
      projected <- ScalametaContextualMethodProjection
        .project(definition)
        .left
        .map(error =>
          failure(
            "NEUTRAL_PROJECTION_FAILED",
            s"${error.code}: ${error.detail}"
          )
        )
      lowered <- PublicContextualMethodGeneratedOriginAdapter
        .lower(projected.result, virtualSourceName)
        .left
        .map(error => failure("EXACT_LOWERING_FAILED", error.message))
    yield lowered

  def project(
      definition: untpd.DefDef
  )(using Context): Either[ScalametaContextualMethodBackendError, Defn.Def] =
    Option(definition)
      .toRight(failure("EXACT_DEFINITION_MISSING", "the exact definition must be present."))
      .flatMap(projectPresent)

  private def projectPresent(
      definition: untpd.DefDef
  )(using Context): Either[ScalametaContextualMethodBackendError, Defn.Def] =
    for
      _ <- require(
        definition.mods.flags == Flags.Method &&
          !definition.mods.hasAnnotations &&
          !definition.mods.hasPrivateWithin,
        "EXACT_METHOD_SHAPE_UNSUPPORTED",
        "expected an unannotated public method definition."
      )
      typeParameter <- definition.leadingTypeParams match
        case List(value: untpd.TypeDef) => Right(value)
        case _ =>
          Left(
            failure(
              "EXACT_METHOD_SHAPE_UNSUPPORTED",
              "expected exactly one leading type parameter."
            )
          )
      _ <- require(
        typeParameter.mods.flags == Flags.Param &&
          !typeParameter.mods.hasAnnotations &&
          !typeParameter.mods.hasPrivateWithin &&
          isWildcardBounds(typeParameter.rhs),
        "EXACT_TYPE_PARAMETER_UNSUPPORTED",
        "expected one unannotated parameter with wildcard bounds."
      )
      contextualParameter <- definition.trailingParamss match
        case List(List(value: untpd.ValDef)) => Right(value)
        case _ =>
          Left(
            failure(
              "EXACT_METHOD_SHAPE_UNSUPPORTED",
              "expected exactly one trailing clause containing one value parameter."
            )
          )
      _ <- require(
        contextualParameter.mods.flags == (Flags.Param | Flags.Given) &&
          !contextualParameter.mods.hasAnnotations &&
          !contextualParameter.mods.hasPrivateWithin &&
          contextualParameter.rhs.isEmpty,
        "EXACT_CONTEXTUAL_PARAMETER_UNSUPPORTED",
        "expected one source-free contextual parameter without a default."
      )
      contextualType <- projectType(contextualParameter.tpt)
      resultType <- projectType(definition.tpt)
      body <- definition.rhs match
        case untpd.Ident(name) => Right(Term.Name(name.toString))
        case _ =>
          Left(
            failure(
              "EXACT_BODY_UNSUPPORTED",
              "the admitted exact body must be one identifier."
            )
          )
      neutral = constructNeutral(
        definition.name.toString,
        typeParameter.name.toString,
        contextualParameter.name.toString,
        contextualType,
        resultType,
        body
      )
      _ <- ScalametaContextualMethodProjection
        .project(neutral)
        .left
        .map(error =>
          failure(
            "EXACT_TO_NEUTRAL_VALIDATION_FAILED",
            s"${error.code}: ${error.detail}"
          )
        )
      _ <- require(
        neutral.pos == Position.None,
        "EXACT_TO_NEUTRAL_PROVENANCE_INVARIANT",
        "structural reverse projection must not invent a source position."
      )
    yield neutral

  private def constructNeutral(
      methodName: String,
      typeParameterName: String,
      contextualParameterName: String,
      contextualType: Type,
      resultType: Type,
      body: Term.Name
  ): Defn.Def =
    val typeParameter = Type.Param(
      Nil,
      Type.Name(typeParameterName),
      Type.ParamClause(Nil),
      Type.Bounds(None, None, Nil, Nil)
    )
    val contextualParameter = Term.Param(
      Nil,
      Term.Name(contextualParameterName),
      Some(contextualType),
      None
    )
    val parameterGroup = Member.ParamClauseGroup(
      Type.ParamClause(List(typeParameter)),
      List(
        Term.ParamClause(
          List(contextualParameter),
          Some(Mod.Using())
        )
      )
    )
    Defn.Def(
      Nil,
      Term.Name(methodName),
      List(parameterGroup),
      Some(resultType),
      body
    )

  private def projectType(
      tree: untpd.Tree
  )(using Context): Either[ScalametaContextualMethodBackendError, Type] =
    tree match
      case untpd.Ident(name) => Right(Type.Name(name.toString))
      case untpd.AppliedTypeTree(constructor, arguments) if arguments.nonEmpty =>
        for
          projectedConstructor <- projectType(constructor)
          projectedArguments <- traverse(arguments)(projectType)
        yield Type.Apply(
          projectedConstructor,
          Type.ArgClause(projectedArguments)
        )
      case _ =>
        Left(
          failure(
            "EXACT_TYPE_UNSUPPORTED",
            "only exact identifiers and nonempty applied type trees are admitted."
          )
        )

  private def traverse[A, B](
      values: List[A]
  )(
      projectValue: A => Either[ScalametaContextualMethodBackendError, B]
  ): Either[ScalametaContextualMethodBackendError, List[B]] =
    values.foldLeft[
      Either[ScalametaContextualMethodBackendError, List[B]]
    ](Right(Nil)) { (result, value) =>
      for
        completed <- result
        next <- projectValue(value)
      yield completed :+ next
    }

  private def isWildcardBounds(tree: untpd.Tree): Boolean =
    tree match
      case untpd.WildcardTypeBoundsTree() => true
      case _ => false

  private def require(
      condition: Boolean,
      code: String,
      detail: String
  ): Either[ScalametaContextualMethodBackendError, Unit] =
    Either.cond(condition, (), failure(code, detail))

  private def failure(
      code: String,
      detail: String
  ): ScalametaContextualMethodBackendError =
    ScalametaContextualMethodBackendError(code, detail)
