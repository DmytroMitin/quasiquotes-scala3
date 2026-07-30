package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.types.TypeNormalForm

private[quasiquotes] object CompletedTypeUntypedLowerer:
  import CompletedTypeUntypedLoweringError.*

  def lower(
      normalForm: TypeNormalForm
  ): Either[CompletedTypeUntypedLoweringError, untpd.Tree] =
    given SourceFile = NoSource
    lowerType(normalForm)

  private def lowerType(
      normalForm: TypeNormalForm
  )(using SourceFile): Either[CompletedTypeUntypedLoweringError, untpd.Tree] =
    normalForm match
      case TypeNormalForm.STypeIdent(name @ ("Int" | "String" | "Boolean")) =>
        Right(untpd.Ident(typeName(name)))
      case TypeNormalForm.STypeApply(
            TypeNormalForm.STypeIdent(name @ ("List" | "Option")),
            argument :: Nil
          ) =>
        lowerType(argument).map { rawArgument =>
          untpd.AppliedTypeTree(
            untpd.Ident(typeName(name)),
            rawArgument :: Nil
          )
        }
      case TypeNormalForm.STypeTuple(elements)
          if elements.size == 2 || elements.size == 3 =>
        lowerTypes(elements).map(untpd.Tuple(_))
      case TypeNormalForm.STypeFunction(arguments, result)
          if arguments.size == 1 || arguments.size == 2 =>
        for
          rawArguments <- lowerTypes(arguments)
          rawResult <- lowerType(result)
        yield untpd.Function(rawArguments, rawResult)
      case unsupported =>
        Left(UnsupportedCompletedType(unsupported.render))

  private def lowerTypes(
      normalForms: List[TypeNormalForm]
  )(using SourceFile): Either[
    CompletedTypeUntypedLoweringError,
    List[untpd.Tree]
  ] =
    normalForms.foldRight[
      Either[CompletedTypeUntypedLoweringError, List[untpd.Tree]]
    ](Right(Nil)) { (normalForm, result) =>
      for
        raw <- lowerType(normalForm)
        rest <- result
      yield raw :: rest
    }

private[quasiquotes] sealed trait CompletedTypeUntypedLoweringError
    derives CanEqual:
  def message: String

private[quasiquotes] object CompletedTypeUntypedLoweringError:
  final case class UnsupportedCompletedType(normalForm: String)
      extends CompletedTypeUntypedLoweringError:
    def message: String =
      s"Unsupported completed type at the exact-version untyped backend boundary: $normalForm."
