package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.types.{AppliedTypeConstructorPolicy, TypeNormalForm}

/** Read-only semantic projection of exact Type slots retained by U037. */
private[quasiquotes] object ExistingUntpdOrdinaryMethodTypeSlotProjection:
  import ExistingUntpdOrdinaryMethodDescriptor.{Descriptor, Parameter}

  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def projectParameter(
      descriptor: Descriptor,
      parameter: Parameter
  )(using Context): Either[Error, TypeNormalForm] =
    validateDescriptor(descriptor).flatMap { valid =>
      Option(parameter)
        .toRight(error("PARAMETER_REQUIRED", "the capture-local parameter handle was null."))
        .flatMap { selected =>
          val exactMatches = valid.parameterClauses.iterator
            .flatMap(clause => Option(clause).iterator.flatMap(_.iterator))
            .filter(candidate => candidate != null && candidate.eq(selected))
            .toVector
          if exactMatches.size != 1 then
            Left(
              error(
                "PARAMETER_NOT_CAPTURED",
                s"the supplied parameter handle occurred ${exactMatches.size} times in the exact descriptor; exactly one capture-local handle is required."
              )
            )
          else projectSlot(exactMatches.head.tpt, "parameter type")
        }
    }

  def projectResult(
      descriptor: Descriptor
  )(using Context): Either[Error, TypeNormalForm] =
    validateDescriptor(descriptor)
      .flatMap(valid => projectSlot(valid.resultType, "result type"))

  private def validateDescriptor(
      descriptor: Descriptor
  )(using Context): Either[Error, Descriptor] =
    ExistingUntpdOrdinaryMethodDescriptor
      .validate(descriptor)
      .left
      .map(problem =>
        error(
          "INVALID_DESCRIPTOR",
          s"U037 descriptor validation failed before Type-slot decoding: ${problem.message}"
        )
      )
      .map(_ => descriptor)

  private def projectSlot(
      tree: untpd.Tree,
      path: String
  )(using Context): Either[Error, TypeNormalForm] =
    decode(tree, path).flatMap { semantic =>
      validateSemantic(semantic, path).map(_ => semantic)
    }

  private def decode(
      tree: untpd.Tree,
      path: String
  )(using Context): Either[Error, TypeNormalForm] =
    Option(tree)
      .filterNot(_.isEmpty)
      .toRight(error("MALFORMED_TYPE_SLOT", s"$path was null or EmptyTree."))
      .flatMap { present =>
        if present.isInstanceOf[untpd.TypedSplice] then
          Left(error("TYPED_SPLICE_TYPE_SLOT", s"$path contains TypedSplice."))
        else if present.symbol != NoSymbol then
          Left(error("SYMBOL_BEARING_TYPE_SLOT", s"$path contains a symbol-bearing ${nodeKind(present)}."))
        else
          present match
            case untpd.Ident(name) if name.isTypeName =>
              name.toString match
                case admitted @ ("Int" | "String" | "Boolean" | "AnyVal") =>
                  Right(TypeNormalForm.STypeIdent(admitted))
                case unsupported =>
                  Left(error("UNSUPPORTED_IDENTIFIER", s"$path identifier $unsupported is outside the structural TypeNormalForm source family."))
            case _: untpd.Ident =>
              Left(error("UNSUPPORTED_IDENTIFIER", s"$path used a term-name identifier where a Type name was required."))
            case applied: untpd.AppliedTypeTree =>
              decodeApplied(applied, path)
            case tuple: untpd.Tuple =>
              decodeTuple(tuple, path)
            case function: untpd.Function =>
              decodeFunction(function, path)
            case parens: untpd.Parens =>
              decode(parens.t, s"$path parenthesized")
            case _: untpd.Select =>
              Left(error("UNSUPPORTED_SELECTED_TYPE", s"$path is selected or qualified raw Type syntax and carries no accepted resolution authority."))
            case unsupported =>
              Left(error("UNSUPPORTED_TYPE_TOPOLOGY", s"$path raw topology ${nodeKind(unsupported)} is outside the structural TypeNormalForm source family."))
      }

  private def decodeApplied(
      applied: untpd.AppliedTypeTree,
      path: String
  )(using Context): Either[Error, TypeNormalForm] =
    for
      constructor <- Option(applied.tpt)
        .filterNot(_.isEmpty)
        .toRight(error("MALFORMED_TYPE_SLOT", s"$path application constructor was null or EmptyTree."))
      arguments <- Option(applied.args)
        .toRight(error("MALFORMED_TYPE_SLOT", s"$path application argument sequence was null."))
      constructorName <- constructor match
        case untpd.Ident(name) if name.isTypeName => Right(name.toString)
        case _: untpd.Select =>
          Left(error("UNSUPPORTED_SELECTED_TYPE", s"$path application constructor is selected or qualified and carries no accepted resolution authority."))
        case other =>
          Left(error("UNSUPPORTED_TYPE_TOPOLOGY", s"$path application constructor ${nodeKind(other)} is not a direct Type identifier."))
      _ <- AppliedTypeConstructorPolicy
        .forNormalFormSource(constructorName, arguments.size)
        .toRight(
          error(
            "UNSUPPORTED_APPLIED_CONSTRUCTOR",
            s"$path constructor $constructorName with arity ${arguments.size} is outside AppliedTypeConstructorPolicy."
          )
        )
      projected <- decodeAll(arguments, s"$path argument")
    yield TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent(constructorName), projected)

  private def decodeTuple(
      tuple: untpd.Tuple,
      path: String
  )(using Context): Either[Error, TypeNormalForm] =
    Option(tuple.trees)
      .toRight(error("MALFORMED_TYPE_SLOT", s"$path tuple element sequence was null."))
      .flatMap { elements =>
        if elements.size != 2 && elements.size != 3 then
          Left(error("UNSUPPORTED_TUPLE_ARITY", s"$path tuple arity ${elements.size} is outside the admitted arities 2 and 3."))
        else decodeAll(elements, s"$path tuple element").map(TypeNormalForm.STypeTuple(_))
      }

  private def decodeFunction(
      function: untpd.Function,
      path: String
  )(using Context): Either[Error, TypeNormalForm] =
    for
      arguments <- Option(function.args)
        .toRight(error("MALFORMED_TYPE_SLOT", s"$path function argument sequence was null."))
      _ <- Either.cond(
        arguments.size == 1 || arguments.size == 2,
        (),
        error("UNSUPPORTED_FUNCTION_ARITY", s"$path function arity ${arguments.size} is outside the admitted arities 1 and 2.")
      )
      result <- Option(function.body)
        .filterNot(_.isEmpty)
        .toRight(error("MALFORMED_TYPE_SLOT", s"$path function result was null or EmptyTree."))
      projectedArguments <- decodeAll(arguments, s"$path function argument")
      projectedResult <- decode(result, s"$path function result")
    yield TypeNormalForm.STypeFunction(projectedArguments, projectedResult)

  private def decodeAll(
      trees: List[untpd.Tree],
      path: String
  )(using Context): Either[Error, List[TypeNormalForm]] =
    trees.zipWithIndex.foldRight[Either[Error, List[TypeNormalForm]]](Right(Nil)) {
      case ((tree, index), accumulated) =>
        for
          projected <- Option(tree)
            .toRight(error("MALFORMED_TYPE_SLOT", s"$path $index was null."))
            .flatMap(decode(_, s"$path $index"))
          rest <- accumulated
        yield projected :: rest
    }

  private def validateSemantic(
      semantic: TypeNormalForm,
      path: String
  ): Either[Error, Unit] =
    semantic match
      case TypeNormalForm.STypeIdent("Int" | "String" | "Boolean" | "AnyVal") => Right(())
      case TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent(name), arguments)
          if arguments != null && AppliedTypeConstructorPolicy
            .forNormalFormSource(name, arguments.size)
            .isDefined =>
        validateSemanticAll(arguments, s"$path application argument")
      case TypeNormalForm.STypeTuple(elements)
          if elements != null && (elements.size == 2 || elements.size == 3) =>
        validateSemanticAll(elements, s"$path tuple element")
      case TypeNormalForm.STypeFunction(arguments, result)
          if arguments != null && result != null && (arguments.size == 1 || arguments.size == 2) =>
        validateSemanticAll(arguments, s"$path function argument")
          .flatMap(_ => validateSemantic(result, s"$path function result"))
      case other =>
        Left(error("INTERNAL_SEMANTIC_INVARIANT_FAILED", s"$path decoded to an invalid semantic value: ${Option(other).fold("null")(_.render)}."))

  private def validateSemanticAll(
      values: List[TypeNormalForm],
      path: String
  ): Either[Error, Unit] =
    values.zipWithIndex.foldLeft[Either[Error, Unit]](Right(())) {
      case (validated, (value, index)) =>
        validated.flatMap(_ =>
          Option(value)
            .toRight(error("INTERNAL_SEMANTIC_INVARIANT_FAILED", s"$path $index was null."))
            .flatMap(validateSemantic(_, s"$path $index"))
        )
    }

  private def nodeKind(tree: untpd.Tree): String =
    Option(tree).fold("null")(_.getClass.getSimpleName)

  private def error(code: String, detail: String): Error =
    Error(code, detail)
