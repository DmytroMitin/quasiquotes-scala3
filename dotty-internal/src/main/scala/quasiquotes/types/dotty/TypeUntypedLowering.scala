package quasiquotes.types.dotty

import dotty.tools.dotc.ast.untpd

import quasiquotes.terms.dotty.CompletedTypeUntypedLowerer
import quasiquotes.types.{AppliedTypeConstructorPolicy, TypeNormalForm}

/** Exact-version source-free lowering for public semantic Type values.
  *
  * The facade is deliberately context-free. It verifies source, span, raw
  * topology, and `TypedSplice` absence recursively. Inspecting `Tree.symbol`
  * requires a Dotty `Context` on the supported compiler lines, so this facade
  * relies on its sole construction authority's fresh untyped constructors for
  * the corresponding `NoSymbol` property rather than changing the public
  * signature.
  */
object TypeUntypedLowering:
  /** Stable public diagnostic boundary; callers branch on `code`. */
  final case class Failure(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  /** Lowers one admitted `TypeNormalForm` to a fresh source-free raw Type tree. */
  def lower(
      tpe: TypeNormalForm
  ): Either[Failure, untpd.Tree] =
    Option(tpe)
      .toRight(
        Failure(
          "MISSING_INPUT",
          "the semantic TypeNormalForm must be present."
        )
      )
      .flatMap { present =>
        for
          _ <- validateStructure(present, "type")
          _ <- validateAdmission(present)
          raw <- CompletedTypeUntypedLowerer
            .lower(present)
            .left
            .map(problem => Failure("EXACT_LOWERING_FAILED", problem.message))
          _ <- validateRaw(present, raw, "type")
        yield raw
      }

  private def validateStructure(
      tpe: TypeNormalForm,
      path: String
  ): Either[Failure, Unit] =
    tpe match
      case TypeNormalForm.STypeIdent(name) =>
        requirePresent(name, path, "identifier name")
      case TypeNormalForm.STypeResolved(id) =>
        Option(id)
          .toRight(malformed(path, "resolved identity must be present."))
          .flatMap { present =>
            Option(present.owners)
              .toRight(malformed(path, "resolved owners must be present."))
              .flatMap(owners =>
                validateElements(owners, s"$path resolved owner") { (owner, ownerPath) =>
                  for
                    _ <- Option(owner.kind)
                      .toRight(malformed(ownerPath, "kind must be present."))
                    _ <- requirePresent(owner.name, ownerPath, "name")
                  yield ()
                }
              )
              .flatMap(_ => requirePresent(present.terminalName, path, "resolved terminal name"))
          }
      case TypeNormalForm.STypeApply(constructor, arguments) =>
        for
          presentConstructor <- Option(constructor)
            .toRight(malformed(path, "application constructor must be present."))
          presentArguments <- Option(arguments)
            .toRight(malformed(path, "application arguments must be present."))
          _ <- validateStructure(presentConstructor, s"$path constructor")
          _ <- validateElements(presentArguments, s"$path argument") {
            (argument, argumentPath) => validateStructure(argument, argumentPath)
          }
        yield ()
      case TypeNormalForm.STypeTuple(elements) =>
        Option(elements)
          .toRight(malformed(path, "tuple elements must be present."))
          .flatMap(present =>
            validateElements(present, s"$path tuple element") {
              (element, elementPath) => validateStructure(element, elementPath)
            }
          )
      case TypeNormalForm.STypeFunction(arguments, result) =>
        for
          presentArguments <- Option(arguments)
            .toRight(malformed(path, "function arguments must be present."))
          presentResult <- Option(result)
            .toRight(malformed(path, "function result must be present."))
          _ <- validateElements(presentArguments, s"$path function argument") {
            (argument, argumentPath) => validateStructure(argument, argumentPath)
          }
          _ <- validateStructure(presentResult, s"$path function result")
        yield ()

  private def validateAdmission(
      tpe: TypeNormalForm
  ): Either[Failure, Unit] =
    tpe match
      case TypeNormalForm.STypeIdent("Int" | "String" | "Boolean") =>
        Right(())
      case TypeNormalForm.STypeApply(
            TypeNormalForm.STypeIdent(name),
            arguments
          ) if AppliedTypeConstructorPolicy
            .forConstruction(name, arguments.size)
            .isDefined =>
        validateAll(arguments)(validateAdmission)
      case TypeNormalForm.STypeTuple(elements)
          if elements.size == 2 || elements.size == 3 =>
        validateAll(elements)(validateAdmission)
      case TypeNormalForm.STypeFunction(arguments, result)
          if arguments.size == 1 || arguments.size == 2 =>
        validateAll(arguments)(validateAdmission)
          .flatMap(_ => validateAdmission(result))
      case unsupported =>
        Left(
          Failure(
            "UNSUPPORTED_SEMANTIC_VALUE",
            s"Unsupported completed type at the exact-version untyped backend boundary: ${unsupported.render}."
          )
        )

  private def validateRaw(
      semantic: TypeNormalForm,
      raw: untpd.Tree,
      path: String
  ): Either[Failure, Unit] =
    Option(raw)
      .toRight(invariant(path, "the raw node was null."))
      .flatMap { present =>
        if present.source.exists then
          Left(invariant(path, s"${present.getClass.getSimpleName} retained a source."))
        else if present.span.exists then
          Left(invariant(path, s"${present.getClass.getSimpleName} retained a span."))
        else if present.isInstanceOf[untpd.TypedSplice] then
          Left(invariant(path, "the raw tree contained a TypedSplice."))
        else
          (semantic, present) match
            case (TypeNormalForm.STypeIdent(expected), untpd.Ident(actual))
                if actual.isTypeName && actual.toString == expected =>
              Right(())
            case (
                  TypeNormalForm.STypeApply(expectedConstructor, expectedArguments),
                  untpd.AppliedTypeTree(actualConstructor, actualArguments)
                ) if actualArguments != null &&
                  actualArguments.size == expectedArguments.size =>
              for
                _ <- validateRaw(expectedConstructor, actualConstructor, s"$path constructor")
                _ <- validateRawPairs(
                  expectedArguments,
                  actualArguments,
                  s"$path argument"
                )
              yield ()
            case (
                  TypeNormalForm.STypeTuple(expectedElements),
                  untpd.Tuple(actualElements)
                ) if actualElements != null &&
                  actualElements.size == expectedElements.size =>
              validateRawPairs(expectedElements, actualElements, s"$path tuple element")
            case (
                  TypeNormalForm.STypeFunction(expectedArguments, expectedResult),
                  untpd.Function(actualArguments, actualResult)
                ) if actualArguments != null &&
                  actualArguments.size == expectedArguments.size =>
              for
                _ <- validateRawPairs(
                  expectedArguments,
                  actualArguments,
                  s"$path function argument"
                )
                _ <- validateRaw(expectedResult, actualResult, s"$path function result")
              yield ()
            case _ =>
              Left(
                invariant(
                  path,
                  s"raw topology ${present.getClass.getSimpleName} did not match ${semantic.render}."
                )
              )
      }

  private def validateElements[A](
      values: Iterable[A],
      path: String
  )(
      validate: (A, String) => Either[Failure, Unit]
  ): Either[Failure, Unit] =
    values.zipWithIndex.foldLeft[Either[Failure, Unit]](Right(())) {
      case (result, (value, index)) =>
        result.flatMap(_ =>
          Option(value)
            .toRight(malformed(s"$path $index", "value must be present."))
            .flatMap(present => validate(present, s"$path $index"))
        )
    }

  private def validateAll(
      values: List[TypeNormalForm]
  )(
      validate: TypeNormalForm => Either[Failure, Unit]
  ): Either[Failure, Unit] =
    values.foldLeft[Either[Failure, Unit]](Right(())) { (result, value) =>
      result.flatMap(_ => validate(value))
    }

  private def validateRawPairs(
      semantic: List[TypeNormalForm],
      raw: List[untpd.Tree],
      path: String
  ): Either[Failure, Unit] =
    semantic.zip(raw).zipWithIndex.foldLeft[Either[Failure, Unit]](Right(())) {
      case (result, ((expected, actual), index)) =>
        result.flatMap(_ => validateRaw(expected, actual, s"$path $index"))
    }

  private def requirePresent(
      value: String,
      path: String,
      label: String
  ): Either[Failure, Unit] =
    Option(value)
      .filter(_.nonEmpty)
      .toRight(malformed(path, s"$label must be present and nonempty."))
      .map(_ => ())

  private def malformed(path: String, detail: String): Failure =
    Failure("MALFORMED_SEMANTIC_VALUE", s"$path: $detail")

  private def invariant(path: String, detail: String): Failure =
    Failure("INTERNAL_INVARIANT_FAILED", s"$path: $detail")
