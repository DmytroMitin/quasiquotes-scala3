package quasiquotes.neutral

import quasiquotes.definitions.DefinitionName
import quasiquotes.definitions.ExtensionModulePlan
import quasiquotes.definitions.ExtensionModulePlan.Plan

import scala.annotation.nowarn
import scala.meta.*

private[quasiquotes] final case class ProjectedExtensionModule(
    plan: Plan,
    sourceSpan: Option[NeutralSourceSpan]
)

/** Exact Scalameta 4.17.3 projector for the AUXify-045 extension module. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaExtensionModuleProjection:
  def project(
      definition: Defn.Object
  ): Either[NeutralProjectionError, ProjectedExtensionModule] =
    Option(definition)
      .toRight(
        error(
          "NEUTRAL_EXTENSION_MODULE_MISSING",
          "the Scalameta Defn.Object must be present."
        )
      )
      .flatMap(projectPresent)

  private def projectPresent(
      definition: Defn.Object
  ): Either[NeutralProjectionError, ProjectedExtensionModule] =
    for
      _ <- require(
        definition.mods.isEmpty &&
          definition.templ.early.isEmpty &&
          definition.templ.inits.isEmpty &&
          definition.templ.self.isEmpty &&
          definition.templ.derives.isEmpty,
        "NEUTRAL_EXTENSION_MODULE_OBJECT_TOPOLOGY_UNSUPPORTED",
        "the exact extension module has no modifiers, parents, early definitions, self declaration, or derives entries."
      )
      moduleName <- plainTermName(definition.name)
      statement <- exactlyOne(
        definition.templ.stats,
        "NEUTRAL_EXTENSION_MODULE_OBJECT_BODY_UNSUPPORTED",
        "the exact module body must contain one extension group."
      )
      extension <- statement match
        case value: Defn.ExtensionGroup => Right(value)
        case _ =>
          Left(
            error(
              "NEUTRAL_EXTENSION_MODULE_EXTENSION_REQUIRED",
              "the sole object body statement must be one Defn.ExtensionGroup."
            )
          )
      extensionGroup <- extension.paramClauseGroup.toRight(
        error(
          "NEUTRAL_EXTENSION_MODULE_EXTENSION_PARAMETERS_UNSUPPORTED",
          "the extension group must declare one Type parameter and one receiver clause."
        )
      )
      typeParameter <- projectTypeParameter(extensionGroup.tparamClause.values)
      typeParameterName <- typeParameter.name match
        case name: Type.Name => plainTypeName(name)
        case _ =>
          Left(
            error(
              "NEUTRAL_EXTENSION_MODULE_TYPE_PARAMETER_UNSUPPORTED",
              "the extension Type parameter must have one direct source name."
            )
          )
      receiver <- projectReceiver(extensionGroup.paramClauses)
      receiverName <- plainTermParameterName(receiver.name)
      _ <- requireDirectTypeReference(
        receiver.decltpe,
        typeParameterName,
        "NEUTRAL_EXTENSION_MODULE_RECEIVER_TYPE_MISMATCH",
        "the extension receiver Type"
      )
      method <- extension.body match
        case value: Defn.Def => Right(value)
        case _ =>
          Left(
            error(
              "NEUTRAL_EXTENSION_MODULE_METHOD_REQUIRED",
              "the exact extension group body must be one nested Defn.Def."
            )
          )
      _ <- require(
        method.mods.isEmpty,
        "NEUTRAL_EXTENSION_MODULE_METHOD_UNSUPPORTED",
        "the nested method must have no modifiers."
      )
      methodName <- plainTermName(method.name)
      methodGroup <- exactlyOne(
        method.paramClauseGroups,
        "NEUTRAL_EXTENSION_MODULE_METHOD_CLAUSES_UNSUPPORTED",
        "the nested method must have one parameter-clause group."
      )
      _ <- require(
        methodGroup.tparamClause.values.isEmpty,
        "NEUTRAL_EXTENSION_MODULE_METHOD_TYPE_PARAMETERS_UNSUPPORTED",
        "the nested method must have no Type parameters."
      )
      clauses <- methodGroup.paramClauses match
        case ordinary :: contextual :: Nil => Right(ordinary -> contextual)
        case _ =>
          Left(
            error(
              "NEUTRAL_EXTENSION_MODULE_METHOD_CLAUSES_UNSUPPORTED",
              "the nested method requires one ordinary clause followed by one final using clause."
            )
          )
      ordinary <- projectOrdinary(clauses._1)
      ordinaryName <- plainTermParameterName(ordinary.name)
      _ <- requireDirectTypeReference(
        ordinary.decltpe,
        typeParameterName,
        "NEUTRAL_EXTENSION_MODULE_ORDINARY_TYPE_MISMATCH",
        "the ordinary argument Type"
      )
      contextual <- projectContextual(clauses._2)
      contextualName <- plainTermParameterName(contextual.name)
      evidenceConstructor <- projectEvidenceType(contextual, typeParameterName)
      _ <- requireDirectTypeReference(
        method.decltpe,
        typeParameterName,
        "NEUTRAL_EXTENSION_MODULE_RESULT_TYPE_MISMATCH",
        "the nested method result Type"
      )
      application <- method.body match
        case value: Term.Apply => Right(value)
        case _ =>
          Left(
            error(
              "NEUTRAL_EXTENSION_MODULE_BODY_APPLICATION_UNSUPPORTED",
              "the nested method body must be one direct application."
            )
          )
      selection <- application.fun match
        case value: Term.Select => Right(value)
        case _ =>
          Left(
            error(
              "NEUTRAL_EXTENSION_MODULE_BODY_SELECTION_UNSUPPORTED",
              "the applied function must be one direct Term selection."
            )
          )
      bodyReceiver <- selection.qual match
        case name: Term.Name => plainTermName(name)
        case _ =>
          Left(
            error(
              "NEUTRAL_EXTENSION_MODULE_BODY_SELECTION_UNSUPPORTED",
              "the delegated body receiver must be one direct Term name."
            )
          )
      _ <- require(
        bodyReceiver == contextualName,
        "NEUTRAL_EXTENSION_MODULE_BODY_RECEIVER_MISMATCH",
        "the delegated body receiver must be the exact contextual evidence parameter."
      )
      selectedName <- plainTermName(selection.name)
      _ <- require(
        selectedName == methodName,
        "NEUTRAL_EXTENSION_MODULE_BODY_SELECTED_METHOD_MISMATCH",
        "the selected member must equal the exact nested method declaration."
      )
      arguments <- application.args match
        case List(first: Term.Name, second: Term.Name) =>
          for
            firstName <- plainTermName(first)
            secondName <- plainTermName(second)
          yield firstName -> secondName
        case _ =>
          Left(
            error(
              "NEUTRAL_EXTENSION_MODULE_BODY_ARGUMENTS_UNSUPPORTED",
              "the delegated body must apply exactly two direct Term-name arguments."
            )
          )
      _ <- require(
        arguments == (receiverName -> ordinaryName),
        "NEUTRAL_EXTENSION_MODULE_BODY_ARGUMENT_ROLE_MISMATCH",
        "the delegated body arguments must be the receiver then the ordinary argument."
      )
      plan <- ExtensionModulePlan
        .create(
          moduleName,
          typeParameterName,
          receiverName,
          methodName,
          ordinaryName,
          contextualName,
          evidenceConstructor
        )
        .left
        .map(mapPlanError)
    yield ProjectedExtensionModule(plan, truthfulSpan(definition))

  private def projectTypeParameter(
      parameters: List[Type.Param]
  ): Either[NeutralProjectionError, Type.Param] =
    parameters match
      case List(parameter)
          if parameter.mods.isEmpty &&
            parameter.tparamClause.values.isEmpty &&
            parameter.bounds.lo.isEmpty &&
            parameter.bounds.hi.isEmpty &&
            parameter.bounds.context.isEmpty &&
            parameter.bounds.view.isEmpty =>
        Right(parameter)
      case _ =>
        Left(
          error(
            "NEUTRAL_EXTENSION_MODULE_TYPE_PARAMETER_UNSUPPORTED",
            "the extension group requires one unmodified, unbounded, unnested Type parameter."
          )
        )

  private def projectReceiver(
      clauses: List[Term.ParamClause]
  ): Either[NeutralProjectionError, Term.Param] =
    clauses match
      case List(clause) if clause.mod.isEmpty =>
        clause.values match
          case List(parameter)
              if parameter.mods.isEmpty &&
                parameter.default.isEmpty &&
                !hasUnsupportedParameterType(parameter) =>
            Right(parameter)
          case _ => Left(receiverUnsupported)
      case _ => Left(receiverUnsupported)

  private def projectOrdinary(
      clause: Term.ParamClause
  ): Either[NeutralProjectionError, Term.Param] =
    if clause.mod.nonEmpty then
      Left(
        error(
          "NEUTRAL_EXTENSION_MODULE_ORDINARY_CLAUSE_UNSUPPORTED",
          "the first nested method clause must be ordinary."
        )
      )
    else
      clause.values match
        case List(parameter)
            if parameter.mods.isEmpty &&
              parameter.default.isEmpty &&
              !hasUnsupportedParameterType(parameter) =>
          Right(parameter)
        case _ =>
          Left(
            error(
              "NEUTRAL_EXTENSION_MODULE_ORDINARY_PARAMETER_UNSUPPORTED",
              "the ordinary clause must contain one unmodified, non-defaulted parameter."
            )
          )

  private def projectContextual(
      clause: Term.ParamClause
  ): Either[NeutralProjectionError, Term.Param] =
    if !clause.mod.exists(_.isInstanceOf[Mod.Using]) then
      Left(
        error(
          "NEUTRAL_EXTENSION_MODULE_CONTEXTUAL_CLAUSE_UNSUPPORTED",
          "the second and final nested method clause must be a using clause."
        )
      )
    else
      clause.values match
        case List(parameter)
            if parameter.mods.nonEmpty &&
              parameter.mods.forall(_.isInstanceOf[Mod.Using]) &&
              parameter.default.isEmpty &&
              !hasUnsupportedParameterType(parameter) =>
          Right(parameter)
        case _ =>
          Left(
            error(
              "NEUTRAL_EXTENSION_MODULE_CONTEXTUAL_PARAMETER_UNSUPPORTED",
              "the final using clause must contain one using-marked, non-defaulted parameter."
            )
          )

  private def projectEvidenceType(
      parameter: Term.Param,
      typeParameterName: String
  ): Either[NeutralProjectionError, String] =
    parameter.decltpe match
      case Some(Type.Apply(constructor: Type.Name, List(argument: Type.Name))) =>
        for
          constructorName <- plainTypeName(constructor)
          argumentName <- plainTypeName(argument)
          _ <- require(
            argumentName == typeParameterName,
            "NEUTRAL_EXTENSION_MODULE_EVIDENCE_TYPE_ARGUMENT_MISMATCH",
            "the evidence Type argument must reference the exact extension Type parameter."
          )
        yield constructorName
      case _ =>
        Left(
          error(
            "NEUTRAL_EXTENSION_MODULE_EVIDENCE_TYPE_UNSUPPORTED",
            "the contextual evidence Type must be one direct source-named unary application."
          )
        )

  private def requireDirectTypeReference(
      value: Option[Type],
      expected: String,
      code: String,
      role: String
  ): Either[NeutralProjectionError, Unit] =
    value match
      case Some(name: Type.Name) =>
        plainTypeName(name).flatMap { actual =>
          require(actual == expected, code, s"$role must reference the exact extension Type parameter.")
        }
      case _ =>
        Left(error(code, s"$role must be one direct Type-parameter name."))

  private def plainTermParameterName(name: Name): Either[NeutralProjectionError, String] =
    name match
      case term: Term.Name => plainTermName(term)
      case _ => Left(nameUnsupported)

  private def plainTermName(name: Term.Name): Either[NeutralProjectionError, String] =
    ScalametaDefinitionNameProjection
      .project(name)
      .left
      .map(_ => nameUnsupported)
      .flatMap(requirePlainName)

  private def plainTypeName(name: Type.Name): Either[NeutralProjectionError, String] =
    ScalametaDefinitionNameProjection
      .project(name)
      .left
      .map(_ => nameUnsupported)
      .flatMap(requirePlainName)

  private def requirePlainName(name: DefinitionName): Either[NeutralProjectionError, String] =
    Either.cond(name.source == name.decoded, name.source, nameUnsupported)

  private def hasUnsupportedParameterType(parameter: Term.Param): Boolean =
    parameter.decltpe.exists {
      case _: Type.ByName | _: Type.Repeated => true
      case _ => false
    }

  private def mapPlanError(problem: ExtensionModulePlan.ModelError): NeutralProjectionError =
    problem.code match
      case "TERM_ROLE_NAMES_MUST_BE_DISTINCT" =>
        error("NEUTRAL_EXTENSION_MODULE_TERM_ROLE_COLLISION", problem.detail)
      case "EVIDENCE_TYPE_CONSTRUCTOR_ROLE_COLLAPSE" =>
        error("NEUTRAL_EXTENSION_MODULE_LEXICAL_ROLE_UNSUPPORTED", problem.detail)
      case _ =>
        error(
          "NEUTRAL_EXTENSION_MODULE_PLAN_UNSUPPORTED",
          s"${problem.code}: ${problem.detail}"
        )

  private def truthfulSpan(tree: Tree): Option[NeutralSourceSpan] =
    tree.pos match
      case Position.None => None
      case position => Some(NeutralSourceSpan(position.start, position.end))

  private def exactlyOne[A](
      values: List[A],
      code: String,
      detail: String
  ): Either[NeutralProjectionError, A] =
    values match
      case value :: Nil => Right(value)
      case _ => Left(error(code, detail))

  private def require(
      condition: Boolean,
      code: String,
      detail: String
  ): Either[NeutralProjectionError, Unit] =
    Either.cond(condition, (), error(code, detail))

  private def receiverUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_EXTENSION_MODULE_RECEIVER_UNSUPPORTED",
      "the extension group requires one ordinary, unmodified, explicitly typed, non-defaulted receiver."
    )

  private def nameUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_EXTENSION_MODULE_NAME_UNSUPPORTED",
      "every extension-module role must use one exact plain fresh-representable source name."
    )

  private def error(code: String, detail: String): NeutralProjectionError =
    NeutralProjectionError(code, detail)
