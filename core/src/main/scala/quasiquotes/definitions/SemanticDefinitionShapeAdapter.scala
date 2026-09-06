package quasiquotes.definitions

import quasiquotes.parser.{BinderId, TermShape, TypeShape}
import quasiquotes.terms.{TermBindingFailure, TermShapeBindingView}
import quasiquotes.types.{ResolvedTypeNameId, TypeNormalForm}

private[quasiquotes] object SemanticDefinitionShapeAdapter:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def adapt(definition: SemanticDefinition): Either[Error, DefinitionShape] =
    Option(definition).toRight(missing("the semantic Definition must be present.")).flatMap {
      present =>
        for
          header <- readHeader(present)
          kind = header.kind
          name = header.name
          modifiers = header.modifiers
          valueView = header.valueView
          methodView = header.methodView
          typeView = header.typeView
          _ <- validateName(name)
          _ <- Either.cond(
            modifiers == DefinitionModifiers.empty,
            (),
            unsupported("only empty V1 Definition modifiers are currently supported.")
          )
          _ <- validateViewRelation(kind, valueView, methodView, typeView)
          shape <-
            if kind == DefinitionKind.Value then adaptValue(name, valueView.get)
            else if kind == DefinitionKind.Method then adaptMethod(name, methodView.get)
            else if kind == DefinitionKind.TypeMember then adaptTypeMember(name, typeView.get)
            else Left(unsupported(s"Definition kind `${kind.code}` is outside the current five-family adapter."))
        yield shape
    }

  private final case class Header(
      kind: DefinitionKind,
      name: DefinitionName,
      modifiers: DefinitionModifiers,
      valueView: Option[ValueDefinitionView],
      methodView: Option[MethodDefinitionView],
      typeView: Option[TypeDefinitionView]
  )

  private def readHeader(definition: SemanticDefinition): Either[Error, Header] =
    try
      for
        kind <- Option(definition.kind).toRight(malformed("the Definition kind must be present."))
        name <- Option(definition.name).toRight(malformed("the Definition name must be present."))
        modifiers <- Option(definition.modifiers)
          .toRight(malformed("the Definition modifiers must be present."))
        valueView <- Option(definition.asValue)
          .toRight(malformed("the value view container must be present."))
        methodView <- Option(definition.asMethod)
          .toRight(malformed("the method view container must be present."))
        typeView <- Option(definition.asType)
          .toRight(malformed("the type view container must be present."))
      yield Header(kind, name, modifiers, valueView, methodView, typeView)
    catch
      case _: ClassCastException | _: NullPointerException =>
        Left(malformed("the Definition storage/view relation is corrupt."))

  private def validateName(name: DefinitionName): Either[Error, Unit] =
    try
      DefinitionName.fromSource(name.source)
        .left.map(error => malformed(error.detail))
        .flatMap(validated =>
          Either.cond(
            validated == name,
            (),
            malformed("the Definition name does not match its validated source spelling.")
          )
        )
    catch
      case _: NullPointerException => Left(malformed("the Definition name is corrupt."))

  private def validateViewRelation(
      kind: DefinitionKind,
      valueView: Option[ValueDefinitionView],
      methodView: Option[MethodDefinitionView],
      typeView: Option[TypeDefinitionView]
  ): Either[Error, Unit] =
    val views = Vector(valueView.isDefined, methodView.isDefined, typeView.isDefined)
    if views.count(identity) != 1 then
      Left(malformed("exactly one typed Definition view must be present."))
    else if kind == DefinitionKind.Value && valueView.isDefined then Right(())
    else if kind == DefinitionKind.Method && methodView.isDefined then Right(())
    else if kind == DefinitionKind.TypeMember && typeView.isDefined then Right(())
    else if
      kind != DefinitionKind.Value &&
        kind != DefinitionKind.Method &&
        kind != DefinitionKind.TypeMember
    then Left(unsupported(s"Definition kind `${kind.code}` is not supported."))
    else Left(malformed("the Definition kind and typed view contradict each other."))

  private def adaptValue(
      name: DefinitionName,
      view: ValueDefinitionView
  ): Either[Error, DefinitionShape] =
    for
      declaredType <- typeShape(view.declaredType, "value declared type")
      body <- requiredBody(view.body, "value body")
      _ <- validateStandaloneBody(body)
      shape <- mapFactory(DefinitionShape.immutableVal(name, declaredType, body))
    yield shape

  private def adaptMethod(
      name: DefinitionName,
      view: MethodDefinitionView
  ): Either[Error, DefinitionShape] =
    for
      clauses <- Option(view.parameterClauses)
        .toRight(malformed("method parameter clauses must be present."))
      scope <- Option(view.parameterScope)
        .toRight(malformed("the method parameter scope must be present."))
      resultType <- typeShape(view.resultType, "method result type")
      body <- requiredBody(view.body, "method body")
      shape <- clauses match
        case Vector() =>
          for
            validatedBody <- validateMethodBody(scope, Vector.empty, body)
            shape <- mapFactory(DefinitionShape.parameterlessDef(name, resultType, validatedBody))
          yield shape
        case Vector(null) => Left(malformed("the method parameter clause must be present."))
        case Vector(clause) =>
          Option(clause.kind)
            .toRight(malformed("the method parameter clause kind must be present."))
            .flatMap { kind =>
              if kind != DefinitionParameterClauseKind.Ordinary then
                Left(unsupported("only an ordinary V1 method clause is supported."))
              else
                Option(clause.parameters)
                  .toRight(malformed("ordinary method parameters must be present."))
                  .flatMap {
                    case Vector(parameter) =>
                      adaptSingleParameter(name, scope, parameter, resultType, body)
                    case Vector(first, second) =>
                      adaptTwoParameters(name, scope, first, second, resultType, body)
                    case parameters =>
                      Left(
                        unsupported(
                          s"one ordinary clause must contain one or two parameters; found ${parameters.size}."
                        )
                      )
                  }
            }
        case other =>
          Left(
            unsupported(
              s"methods currently admit zero clauses or one ordinary clause; found ${other.size}."
            )
          )
    yield shape

  private def adaptSingleParameter(
      name: DefinitionName,
      scope: DefinitionParameterScope,
      parameter: DefinitionParameter,
      resultType: TypeShape,
      body: TermShape
  ): Either[Error, DefinitionShape] =
    for
      present <- presentParameter(parameter, 0)
      parameterType <- typeShape(present.declaredType, "method parameter 0 type")
      binder <- recoverBinder(scope, 0)
      validatedBody <- validateMethodBody(scope, Vector(1), body)
      shape <- mapFactory(
        DefinitionShape.singleParameterDef(
          name,
          binder,
          present.name,
          parameterType,
          resultType,
          validatedBody
        )
      )
    yield shape

  private def adaptTwoParameters(
      name: DefinitionName,
      scope: DefinitionParameterScope,
      firstParameter: DefinitionParameter,
      secondParameter: DefinitionParameter,
      resultType: TypeShape,
      body: TermShape
  ): Either[Error, DefinitionShape] =
    for
      first <- presentParameter(firstParameter, 0)
      second <- presentParameter(secondParameter, 1)
      _ <- Either.cond(
        first.name != second.name,
        (),
        malformed("ordinary method parameter names must be distinct.")
      )
      firstType <- typeShape(first.declaredType, "method parameter 0 type")
      secondType <- typeShape(second.declaredType, "method parameter 1 type")
      firstBinder <- recoverBinder(scope, 0)
      secondBinder <- recoverBinder(scope, 1)
      validatedBody <- validateMethodBody(scope, Vector(2), body)
      shape <- mapFactory(
        DefinitionShape.twoParameterDef(
          name,
          firstBinder,
          first.name,
          firstType,
          secondBinder,
          second.name,
          secondType,
          resultType,
          validatedBody
        )
      )
    yield shape

  private def adaptTypeMember(
      name: DefinitionName,
      view: TypeDefinitionView
  ): Either[Error, DefinitionShape] =
    for
      aliasedType <- requiredType(view.aliasedType, "type alias target")
      rhs <- typeShape(aliasedType, "type alias target")
      shape <- mapFactory(DefinitionShape.simpleTypeAlias(name, rhs))
    yield shape

  private def presentParameter(
      parameter: DefinitionParameter,
      index: Int
  ): Either[Error, DefinitionParameter] =
    Option(parameter)
      .toRight(malformed(s"method parameter $index must be present."))
      .flatMap { present =>
        Option(present.name)
          .toRight(malformed(s"method parameter $index must have a name."))
          .flatMap(validateName)
          .map(_ => present)
      }

  private def recoverBinder(
      scope: DefinitionParameterScope,
      parameterIndex: Int
  ): Either[Error, BinderId] =
    try
      scope.reference(0, parameterIndex)
        .left.map(error => adapterFailed(error.message))
        .flatMap {
          case TermShape.BoundReference(binderId, _) if binderId != null => Right(binderId)
          case _ =>
            Left(
              adapterFailed(
                s"method parameter $parameterIndex did not produce its persistent bound reference."
              )
            )
        }
    catch
      case _: ClassCastException | _: NullPointerException =>
        Left(adapterFailed("the persistent method parameter scope is corrupt."))

  private def typeShape(
      normalForm: TypeNormalForm,
      component: String
  ): Either[Error, TypeShape] =
    Option(normalForm).toRight(malformed(s"the $component must be present.")).flatMap {
      case TypeNormalForm.STypeResolved(id) =>
        validateResolvedId(id, component).flatMap(_ =>
          Left(unsupported(s"the $component uses a resolved Type identity."))
        )
      case TypeNormalForm.STypeIdent(name) =>
        Option(name)
          .filter(_.nonEmpty)
          .toRight(malformed(s"the $component identifier must be present."))
          .flatMap(value => roundTrip(normalForm, TypeShape.Identifier(value), component))
      case TypeNormalForm.STypeApply(constructor, arguments) =>
        for
          presentArguments <- Option(arguments)
            .toRight(malformed(s"the $component arguments must be present."))
          convertedConstructor <- typeConstructorShape(constructor, s"$component constructor")
          convertedArguments <- collectTypes(
            presentArguments,
            index => s"$component argument $index"
          )
          shape <- roundTrip(
            normalForm,
            TypeShape.Apply(convertedConstructor, convertedArguments),
            component
          )
        yield shape
      case TypeNormalForm.STypeTuple(elements) =>
        for
          presentElements <- Option(elements)
            .toRight(malformed(s"the $component tuple elements must be present."))
          converted <- collectTypes(presentElements, index => s"$component tuple element $index")
          shape <- roundTrip(normalForm, TypeShape.Tuple(converted), component)
        yield shape
      case TypeNormalForm.STypeFunction(arguments, result) =>
        for
          presentArguments <- Option(arguments)
            .toRight(malformed(s"the $component function arguments must be present."))
          convertedArguments <- collectTypes(
            presentArguments,
            index => s"$component function argument $index"
          )
          convertedResult <- typeShape(result, s"$component function result")
          shape <- roundTrip(
            normalForm,
            TypeShape.Function(convertedArguments, convertedResult),
            component
          )
        yield shape
    }

  private def typeConstructorShape(
      normalForm: TypeNormalForm,
      component: String
  ): Either[Error, TypeShape] =
    Option(normalForm).toRight(malformed(s"the $component must be present.")).flatMap {
      case TypeNormalForm.STypeIdent(name) =>
        Option(name)
          .filter(_.nonEmpty)
          .toRight(malformed(s"the $component identifier must be present."))
          .map(TypeShape.Identifier(_))
      case TypeNormalForm.STypeResolved(id) =>
        validateResolvedId(id, component).flatMap(_ =>
          Left(unsupported(s"the $component uses a resolved Type identity."))
        )
      case _ =>
        Left(unsupported(s"the $component is not a lossless unresolved constructor identifier."))
    }

  private def collectTypes(
      values: List[TypeNormalForm],
      component: Int => String
  ): Either[Error, List[TypeShape]] =
    values.zipWithIndex.foldRight[Either[Error, List[TypeShape]]](Right(Nil)) {
      case ((value, index), result) =>
        for
          head <- typeShape(value, component(index))
          tail <- result
        yield head :: tail
    }

  private def roundTrip(
      original: TypeNormalForm,
      shape: TypeShape,
      component: String
  ): Either[Error, TypeShape] =
    TypeNormalForm.fromShape(shape) match
      case Right(recovered) if recovered == original => Right(shape)
      case Right(_) =>
        Left(adapterFailed(s"the $component structural inverse did not round-trip exactly."))
      case Left(error) => Left(unsupported(s"the $component is not losslessly representable: ${error.message}"))

  private def validateResolvedId(
      id: ResolvedTypeNameId,
      component: String
  ): Either[Error, Unit] =
    Option(id).toRight(malformed(s"the $component resolved identity must be present.")).flatMap {
      present =>
        Option(present.owners)
          .filter(_.nonEmpty)
          .toRight(malformed(s"the $component resolved owners must be present."))
          .flatMap { owners =>
            owners.zipWithIndex.foldLeft[Either[Error, Unit]](Right(())) {
              case (result, (owner, index)) =>
                result.flatMap { _ =>
                  Option(owner)
                    .toRight(malformed(s"the $component resolved owner $index must be present."))
                    .flatMap { current =>
                      Either.cond(
                        current.kind != null && current.name != null && current.name.nonEmpty,
                        (),
                        malformed(s"the $component resolved owner $index is malformed.")
                      )
                    }
                }
            }
          }
          .flatMap { _ =>
            Either.cond(
              present.terminalName != null && present.terminalName.nonEmpty,
              (),
              malformed(s"the $component resolved terminal name is malformed.")
            )
          }
    }

  private def requiredBody(
      body: Option[TermShape],
      component: String
  ): Either[Error, TermShape] =
    Option(body)
      .toRight(malformed(s"the $component container must be present."))
      .flatMap(_.toRight(malformed(s"the $component must be present.")))
      .flatMap(value => Option(value).toRight(malformed(s"the $component must be present.")))

  private def requiredType(
      value: Option[TypeNormalForm],
      component: String
  ): Either[Error, TypeNormalForm] =
    Option(value)
      .toRight(malformed(s"the $component container must be present."))
      .flatMap(_.toRight(malformed(s"the $component must be present.")))
      .flatMap(present => Option(present).toRight(malformed(s"the $component must be present.")))

  private def validateStandaloneBody(body: TermShape): Either[Error, Unit] =
    TermShapeBindingView.inspect(body)
      .left.map(mapTermValidation)
      .map(_ => ())

  private def validateMethodBody(
      scope: DefinitionParameterScope,
      expectedParameterCounts: Vector[Int],
      body: TermShape
  ): Either[Error, TermShape] =
    try
      scope.validateDefinitionBody(expectedParameterCounts, body)
        .left.map(mapTermValidation)
    catch
      case _: ClassCastException | _: NullPointerException =>
        Left(adapterFailed("the persistent method parameter scope is corrupt."))

  private def mapTermValidation(error: TermBindingFailure): Error =
    error.code match
      case "TERM_BINDING_UNSUPPORTED" => unsupported(error.message)
      case "TERM_BINDER_SCOPE_MISMATCH" | "TERM_BINDER_UNBOUND" |
          "TERM_BINDER_COLLISION" =>
        adapterFailed(error.message)
      case _ => malformed(error.message)

  private def mapFactory[A <: DefinitionShape](
      result: Either[DefinitionError, A]
  ): Either[Error, DefinitionShape] =
    result.left.map {
      case error: DefinitionError.UnsupportedDefinitionBody => unsupported(error.message)
      case error => adapterFailed(error.message)
    }

  private def failure(code: String, detail: String): Error = Error(code, detail)
  private def missing(detail: String): Error = failure("MISSING_INPUT", detail)
  private def malformed(detail: String): Error = failure("MALFORMED_SEMANTIC_VALUE", detail)
  private def unsupported(detail: String): Error = failure("UNSUPPORTED_SEMANTIC_VALUE", detail)
  private def adapterFailed(detail: String): Error = failure("SEMANTIC_ADAPTER_FAILED", detail)
