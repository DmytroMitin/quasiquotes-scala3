package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.{typeName as dottyTypeName}
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.parser.TermShape

/** Source-free exact lowering for only canonical primitive Typed terms. */
private[quasiquotes] object PrimitiveTypedTermShapeUntypedLowerer:
  import PrimitiveTypedTermShapeUntypedLowererError.*

  private val AdmittedPrimitiveTypes = Set("Int", "String", "Boolean")

  def lower(
      shape: TermShape
  )(using Context): Either[
    PrimitiveTypedTermShapeUntypedLowererError,
    untpd.Typed
  ] =
    given SourceFile = NoSource

    Option(shape) match
      case None => Left(MissingTermShape)
      case Some(typed: TermShape.Typed) => lowerTyped(typed)
      case Some(other) => Left(WrongTermShapeFamily(other.getClass.getSimpleName))

  private def lowerTyped(
      typed: TermShape.Typed
  )(using Context, SourceFile): Either[
    PrimitiveTypedTermShapeUntypedLowererError,
    untpd.Typed
  ] =
    for
      expression <- Option(typed.expression).toRight(MissingTypedExpression)
      primitive <- Option(typed.typeName).toRight(MissingPrimitiveTypeName)
      _ <- Either.cond(
        AdmittedPrimitiveTypes(primitive),
        (),
        UnsupportedPrimitiveTypeName(primitive)
      )
      rawExpression <- CoreTermShapeUntypedLowerer
        .lower(expression)
        .left
        .map(ExpressionLoweringFailure.apply)
      raw = untpd.Typed(
        rawExpression,
        untpd.Ident(dottyTypeName(primitive))
      )
      _ <- validateRaw(raw, primitive)
    yield raw

  private def validateRaw(
      tree: untpd.Tree,
      expectedTypeName: String
  )(using Context): Either[PrimitiveTypedTermShapeUntypedLowererError, Unit] =
    for
      present <- Option(tree).toRight(
        RawTopologyMismatch("expected Typed, found null.")
      )
      _ <- present match
        case typed: untpd.Typed =>
          for
            _ <- Option(typed.expr).toRight(
              RawTopologyMismatch("expected Typed expression, found null.")
            )
            _ <- Option(typed.tpt).toRight(
              RawTopologyMismatch(
                s"expected primitive type Ident($expectedTypeName), found null."
              )
            )
            .flatMap {
              case untpd.Ident(name)
                  if name.isTypeName && name.toString == expectedTypeName =>
                Right(())
              case other =>
                Left(
                  RawTopologyMismatch(
                    s"expected primitive type Ident($expectedTypeName), found ${other.getClass.getSimpleName}($other)."
                  )
                )
            }
          yield ()
        case other =>
          Left(
            RawTopologyMismatch(
              s"expected Typed, found ${other.getClass.getSimpleName}."
            )
          )
      _ <- CoreTermShapeUntypedLowerer
        .verifySourceFree(present)
        .left
        .map(SourceFreeInvariantFailure.apply)
    yield ()

  private[dotty] def validateRawForTest(
      tree: untpd.Tree,
      expectedTypeName: String
  )(using Context): Either[PrimitiveTypedTermShapeUntypedLowererError, Unit] =
    validateRaw(tree, expectedTypeName)
