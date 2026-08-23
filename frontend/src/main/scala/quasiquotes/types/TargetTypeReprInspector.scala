package quasiquotes.types

import scala.quoted.*

object TargetTypeReprInspector:
  private[quasiquotes] final case class Inspection[T](
      normalForm: TypeNormalForm,
      originalsByPath: Map[Vector[Int], T]
  )

  def inspect(using q: Quotes)(target: q.reflect.TypeRepr): Either[TypeQuasiquoteError, TypeNormalForm] =
    inspectWithOrigins(target).map(_.normalForm)

  def inspectResolved(using q: Quotes)(
      target: q.reflect.TypeRepr,
      environment: GlobalSelectedTypeEnvironment[q.reflect.TypeRepr]
  ): Either[TypeQuasiquoteError, TypeNormalForm] =
    inspectResolvedWithOrigins(target, environment).map(_.normalForm)

  private[quasiquotes] def inspectWithOrigins(using q: Quotes)(
      target: q.reflect.TypeRepr
  ): Either[TypeQuasiquoteError, Inspection[q.reflect.TypeRepr]] =
    inspectAt(target, Vector.empty)

  private[quasiquotes] def inspectResolvedWithOrigins(using q: Quotes)(
      target: q.reflect.TypeRepr,
      environment: GlobalSelectedTypeEnvironment[q.reflect.TypeRepr]
  ): Either[TypeQuasiquoteError, Inspection[q.reflect.TypeRepr]] =
    inspectResolvedAt(target, Vector.empty, environment)

  private def inspectAt(using q: Quotes)(
      target: q.reflect.TypeRepr,
      path: Vector[Int]
  ): Either[TypeQuasiquoteError, Inspection[q.reflect.TypeRepr]] =
    import q.reflect.*

    target match
      case TypeRef(_, "Int") => scalar(target, path, TypeNormalForm.STypeIdent("Int"))
      case TypeRef(_, "String") => scalar(target, path, TypeNormalForm.STypeIdent("String"))
      case TypeRef(_, "Boolean") => scalar(target, path, TypeNormalForm.STypeIdent("Boolean"))
      case AppliedType(TypeRef(_, name), arguments)
          if AppliedTypeConstructorPolicy
            .forNormalFormSource(name, arguments.size)
            .isDefined =>
        collect(arguments.zipWithIndex.map((argument, index) => inspectAt(argument, path :+ index)))
          .map(children => composite(target, path, TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent(name), children.map(_.normalForm)), children))
      case AppliedType(TypeRef(_, "Tuple2"), first :: second :: Nil) =>
        collect(List(first, second).zipWithIndex.map((argument, index) => inspectAt(argument, path :+ index)))
          .map(children => composite(target, path, TypeNormalForm.STypeTuple(children.map(_.normalForm)), children))
      case AppliedType(TypeRef(_, "Tuple3"), first :: second :: third :: Nil) =>
        collect(List(first, second, third).zipWithIndex.map((argument, index) => inspectAt(argument, path :+ index)))
          .map(children => composite(target, path, TypeNormalForm.STypeTuple(children.map(_.normalForm)), children))
      case AppliedType(TypeRef(_, "Function1"), argument :: result :: Nil) =>
        collect(List(argument, result).zipWithIndex.map((child, index) => inspectAt(child, path :+ index)))
          .map(children => composite(target, path, TypeNormalForm.STypeFunction(List(children.head.normalForm), children(1).normalForm), children))
      case AppliedType(TypeRef(_, "Function2"), first :: second :: result :: Nil) =>
        collect(List(first, second, result).zipWithIndex.map((child, index) => inspectAt(child, path :+ index)))
          .map(children => composite(target, path, TypeNormalForm.STypeFunction(children.take(2).map(_.normalForm), children(2).normalForm), children))
      case _ =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedTargetType))

  private def inspectResolvedAt(using q: Quotes)(
      target: q.reflect.TypeRepr,
      path: Vector[Int],
      environment: GlobalSelectedTypeEnvironment[q.reflect.TypeRepr]
  ): Either[TypeQuasiquoteError, Inspection[q.reflect.TypeRepr]] =
    import q.reflect.*

    target match
      case AppliedType(TypeRef(_, "Tuple2"), first :: second :: Nil) =>
        collect(
          List(first, second).zipWithIndex.map((argument, index) =>
            inspectResolvedAt(argument, path :+ index, environment)
          )
        ).map(children =>
          composite(target, path, TypeNormalForm.STypeTuple(children.map(_.normalForm)), children)
        )
      case AppliedType(TypeRef(_, "Tuple3"), first :: second :: third :: Nil) =>
        collect(
          List(first, second, third).zipWithIndex.map((argument, index) =>
            inspectResolvedAt(argument, path :+ index, environment)
          )
        ).map(children =>
          composite(target, path, TypeNormalForm.STypeTuple(children.map(_.normalForm)), children)
        )
      case AppliedType(TypeRef(_, "Function1"), argument :: result :: Nil) =>
        collect(
          List(argument, result).zipWithIndex.map((child, index) =>
            inspectResolvedAt(child, path :+ index, environment)
          )
        ).map(children =>
          composite(
            target,
            path,
            TypeNormalForm.STypeFunction(List(children.head.normalForm), children(1).normalForm),
            children
          )
        )
      case AppliedType(TypeRef(_, "Function2"), first :: second :: result :: Nil) =>
        collect(
          List(first, second, result).zipWithIndex.map((child, index) =>
            inspectResolvedAt(child, path :+ index, environment)
          )
        ).map(children =>
          composite(
            target,
            path,
            TypeNormalForm.STypeFunction(children.take(2).map(_.normalForm), children(2).normalForm),
            children
          )
        )
      case applied @ AppliedType(constructor: TypeRef, arguments) =>
        ResolvedTypeReflection.deriveTypeRef(constructor).flatMap { id =>
          environment.binding(id) match
            case Some(GlobalSelectedTypeEnvironment.Binding(_, _, GlobalSelectedTypeEnvironment.WitnessRole.Constructor(arity)))
                if arity == arguments.size && AppliedTypeConstructorPolicy.forResolved(id, arity).isDefined =>
              collect(
                arguments.zipWithIndex.map((argument, index) =>
                  inspectResolvedAt(argument, path :+ index, environment)
                )
              ).map(children =>
                composite(
                  target,
                  path,
                  TypeNormalForm.STypeApply(
                    TypeNormalForm.STypeResolved(id),
                    children.map(_.normalForm)
                  ),
                  children
                )
              )
            case Some(_) =>
              Left(
                TypeQuasiquoteError(
                  TypeNameResolutionDiagnostics.constructorPolicyMismatch(id, arguments.size)
                )
              )
            case None =>
              inspectFixedAppliedResolvedFallback(applied, constructor.name, arguments, path, environment)
        }
      case typeRef: TypeRef =>
        ResolvedTypeReflection.deriveTypeRef(typeRef).flatMap { id =>
          environment.binding(id) match
            case Some(GlobalSelectedTypeEnvironment.Binding(_, _, GlobalSelectedTypeEnvironment.WitnessRole.Terminal)) =>
              scalar(target, path, TypeNormalForm.STypeResolved(id))
            case Some(_) =>
              Left(
                TypeQuasiquoteError(
                  TypeNameResolutionDiagnostics.resolvedFamilyUnsupported(id.canonicalSource)
                )
              )
            case None =>
              fixedTerminal(typeRef.name)
                .map(normalForm => scalar(target, path, normalForm))
                .getOrElse(
                  Left(
                    TypeQuasiquoteError(
                      TypeNameResolutionDiagnostics.unresolved(id.canonicalSource)
                    )
                  )
                )
        }
      case _ =>
        inspectAt(target, path)

  private def inspectFixedAppliedResolvedFallback(using q: Quotes)(
      target: q.reflect.TypeRepr,
      name: String,
      arguments: List[q.reflect.TypeRepr],
      path: Vector[Int],
      environment: GlobalSelectedTypeEnvironment[q.reflect.TypeRepr]
  ): Either[TypeQuasiquoteError, Inspection[q.reflect.TypeRepr]] =
    if AppliedTypeConstructorPolicy.forNormalFormSource(name, arguments.size).isDefined then
      collect(
        arguments.zipWithIndex.map((argument, index) =>
          inspectResolvedAt(argument, path :+ index, environment)
        )
      ).map(children =>
        composite(
          target,
          path,
          TypeNormalForm.STypeApply(
            TypeNormalForm.STypeIdent(name),
            children.map(_.normalForm)
          ),
          children
        )
      )
    else
      Left(TypeQuasiquoteError(TypeNameResolutionDiagnostics.resolvedFamilyUnsupported(name)))

  private def fixedTerminal(name: String): Option[TypeNormalForm] =
    name match
      case "Int" | "String" | "Boolean" => Some(TypeNormalForm.STypeIdent(name))
      case _ => None

  private def scalar[T](target: T, path: Vector[Int], normalForm: TypeNormalForm): Either[TypeQuasiquoteError, Inspection[T]] =
    Right(Inspection(normalForm, Map(path -> target)))

  private def composite[T](
      target: T,
      path: Vector[Int],
      normalForm: TypeNormalForm,
      children: List[Inspection[T]]
  ): Inspection[T] =
    Inspection(
      normalForm,
      children.foldLeft(Map(path -> target))((origins, child) => origins ++ child.originalsByPath)
    )

  private def collect[A](
      values: List[Either[TypeQuasiquoteError, A]]
  ): Either[TypeQuasiquoteError, List[A]] =
    values.foldRight[Either[TypeQuasiquoteError, List[A]]](Right(Nil)) {
      (value, accumulated) =>
        for
          head <- value
          tail <- accumulated
        yield head :: tail
    }
