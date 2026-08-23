package quasiquotes.types

import quasiquotes.parser.TypeShape

sealed trait TypeNormalForm derives CanEqual:
  final def render: String = TypeNormalForm.render(this)

object TypeNormalForm:
  final case class STypeIdent(name: String) extends TypeNormalForm
  final case class STypeResolved(id: ResolvedTypeNameId) extends TypeNormalForm
  final case class STypeApply(constructor: TypeNormalForm, arguments: List[TypeNormalForm]) extends TypeNormalForm
  final case class STypeTuple(elements: List[TypeNormalForm]) extends TypeNormalForm
  final case class STypeFunction(arguments: List[TypeNormalForm], result: TypeNormalForm) extends TypeNormalForm

  def fromShape(shape: TypeShape): Either[TypeQuasiquoteError, TypeNormalForm] =
    shape match
      case TypeShape.Identifier(name) => normalizeIdentifier(name)
      case TypeShape.Parenthesized(typeShape) => fromShape(typeShape)
      case TypeShape.Apply(TypeShape.Identifier(name), arguments)
          if AppliedTypeConstructorPolicy
            .forNormalFormSource(name, arguments.size)
            .isDefined =>
        collect(arguments.map(fromShape))
          .map(argumentForms => STypeApply(STypeIdent(name), argumentForms))
      case TypeShape.Apply(TypeShape.Identifier(name), arguments) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedAppliedConstructor(name, arguments.size)))
      case TypeShape.Apply(TypeShape.Select(qualifier, name), _) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.selectedConstructor(qualifier, name)))
      case TypeShape.Apply(_, _) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedTypeSyntax("structural normal-form conversion")))
      case TypeShape.Tuple(elements) if elements.size == 2 || elements.size == 3 =>
        collect(elements.map(fromShape)).map(STypeTuple(_))
      case TypeShape.Tuple(elements) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedTupleArity("structural normal-form conversion", elements.size)))
      case TypeShape.Function(arguments, result) if arguments.size == 1 || arguments.size == 2 =>
        for
          argumentForms <- collect(arguments.map(fromShape))
          resultForm <- fromShape(result)
        yield STypeFunction(argumentForms, resultForm)
      case TypeShape.Function(arguments, result) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedFunctionArity("structural normal-form conversion", arguments.size)))
      case TypeShape.Select(qualifier, name) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.selectedType(qualifier, name)))
      case _ =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedTypeSyntax("structural normal-form conversion")))

  private[quasiquotes] def fromShapeResolved(
      shape: TypeShape,
      environment: ResolvedTypeEnvironment
  ): Either[TypeQuasiquoteError, TypeNormalForm] =
    shape match
      case selected @ TypeShape.Select(_, _) =>
        environment.resolveSelected(selected).map(STypeResolved(_))
      case TypeShape.Apply(selected @ TypeShape.Select(_, _), arguments) =>
        for
          id <- environment.resolveSelected(selected)
          _ <- AppliedTypeConstructorPolicy
            .forResolved(id, arguments.size)
            .toRight(
              TypeQuasiquoteError(
                TypeNameResolutionDiagnostics.constructorPolicyMismatch(id, arguments.size)
              )
            )
          argumentForms <- collect(arguments.map(fromShapeResolved(_, environment)))
        yield STypeApply(STypeResolved(id), argumentForms)
      case TypeShape.Parenthesized(inner) =>
        fromShapeResolved(inner, environment)
      case TypeShape.Apply(TypeShape.Identifier(name), arguments)
          if AppliedTypeConstructorPolicy.forNormalFormSource(name, arguments.size).isDefined =>
        collect(arguments.map(fromShapeResolved(_, environment)))
          .map(forms => STypeApply(STypeIdent(name), forms))
      case TypeShape.Tuple(elements) if elements.size == 2 || elements.size == 3 =>
        collect(elements.map(fromShapeResolved(_, environment))).map(STypeTuple(_))
      case TypeShape.Function(arguments, result) if arguments.size == 1 || arguments.size == 2 =>
        for
          argumentForms <- collect(arguments.map(fromShapeResolved(_, environment)))
          resultForm <- fromShapeResolved(result, environment)
        yield STypeFunction(argumentForms, resultForm)
      case other =>
        fromShape(other)

  def render(normalForm: TypeNormalForm): String =
    normalForm match
      case STypeIdent(name) => s"STypeIdent($name)"
      case STypeResolved(id) => s"STypeResolved(${id.render})"
      case STypeApply(constructor, arguments) =>
        s"STypeApply(${render(constructor)}, [${arguments.map(render).mkString(", ")}])"
      case STypeTuple(elements) =>
        s"STypeTuple([${elements.map(render).mkString(", ")}])"
      case STypeFunction(arguments, result) =>
        s"STypeFunction([${arguments.map(render).mkString(", ")}], ${render(result)})"

  private def normalizeIdentifier(name: String): Either[TypeQuasiquoteError, TypeNormalForm] =
    name match
      case "Int" | "String" | "Boolean" | "AnyVal" => Right(STypeIdent(name))
      case other => Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedNormalFormIdentifier(other)))

  private def collect[A](values: List[Either[TypeQuasiquoteError, A]]): Either[TypeQuasiquoteError, List[A]] =
    values.foldRight[Either[TypeQuasiquoteError, List[A]]](Right(Nil)) { (value, accumulated) =>
      for
        head <- value
        tail <- accumulated
      yield head :: tail
    }
