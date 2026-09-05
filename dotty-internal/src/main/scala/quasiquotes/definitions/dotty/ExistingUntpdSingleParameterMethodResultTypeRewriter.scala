package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.terms.dotty.CompletedTypeUntypedLowerer
import quasiquotes.types.TypeNormalForm

/** Bounded primitive result-type rewrite of one exact U028 method view. */
private[quasiquotes] object ExistingUntpdSingleParameterMethodResultTypeRewriter:
  final case class Result(
      view: ExistingUntpdSingleParameterMethodView.View,
      normalForm: TypeNormalForm,
      loweredResultType: untpd.Tree,
      positionedResultType: untpd.Tree,
      positionedMethod: untpd.DefDef,
      positionedTemplate: untpd.Template,
      positionedRoot: untpd.TypeDef
  )

  def rewrite(
      view: ExistingUntpdSingleParameterMethodView.View,
      normalForm: TypeNormalForm
  )(using Context): Either[ExistingUntpdSingleParameterMethodResultTypeRewriteError, Result] =
    for
      presentView <- Option(view).toRight(
        error("VIEW_REQUIRED", "the U028 method view was null.")
      )
      oldResultType <- Option(presentView.resultType)
        .filterNot(_.isEmpty)
        .toRight(
          error(
            "OLD_RESULT_TYPE_REQUIRED",
            "the old result-type transformation site was null or EmptyTree."
          )
        )
      _ <- Either.cond(
        Option(oldResultType.source).exists(_.exists) && oldResultType.span.exists,
        (),
        error(
          "OLD_RESULT_TYPE_PROVENANCE_REQUIRED",
          "the old result-type transformation site must provide both source and span."
        )
      )
      _ <- ExistingUntpdSingleParameterMethodView
        .validate(presentView)
        .left
        .map(problem => error("VIEW_INVALID", problem.detail))
      _ <- Either.cond(
        Option(presentView.method).exists(method =>
          Option(method.source).exists(_.exists) && method.span.exists
        ),
        (),
        error(
          "OLD_METHOD_PROVENANCE_REQUIRED",
          "the original method shell must provide both source and span for reconstruction."
        )
      )
      presentNormalForm <- Option(normalForm).toRight(
        error("RESULT_TYPE_REQUIRED", "the semantic replacement result type was null.")
      )
      admitted <- presentNormalForm match
        case value @ TypeNormalForm.STypeIdent("Int" | "String" | "Boolean") => Right(value)
        case value: TypeNormalForm.STypeIdent =>
          Left(
            error(
              "RESULT_TYPE_IDENTIFIER_UNSUPPORTED",
              s"U030 does not admit primitive identifier ${Option(value.name).getOrElse("null")}."
            )
          )
        case other =>
          Left(
            error(
              "RESULT_TYPE_FAMILY_UNSUPPORTED",
              s"U030 admits no non-primitive TypeNormalForm family; found ${other.getClass.getSimpleName}."
            )
          )
      lowered <- CompletedTypeUntypedLowerer
        .lower(admitted)
        .left
        .map(problem => error("RESULT_TYPE_LOWERING_FAILED", problem.message))
      result <- reconstruct(presentView, admitted, lowered)
      _ <- validateResult(result)
    yield result

  private[dotty] def validateResult(
      result: Result
  )(using Context): Either[ExistingUntpdSingleParameterMethodResultTypeRewriteError, Unit] =
    val expectedName = Option(result)
      .flatMap(value => Option(value.normalForm))
      .collect { case TypeNormalForm.STypeIdent(name) if name != null => name }
    val lowered = Option(result).flatMap(value => Option(value.loweredResultType))
    val loweredIdent = lowered.collect { case value: untpd.Ident => value }
    val loweredTopologyValid =
      loweredIdent.filter(value => value.source != null).exists(value =>
        Option(value.name).exists(name => expectedName.contains(name.toString)) &&
          ExistingUntpdClassMemberFilter.allTrees(value).size == 1
      )
    if lowered.exists(_.source == null) then
      Left(
        error(
          "LOWERED_RESULT_TYPE_PROVENANCE",
          "the authoritative lowered primitive type had a null source field."
        )
      )
    else if !loweredTopologyValid then
      Left(
        error(
          "LOWERED_RESULT_TYPE_TOPOLOGY",
          "the authoritative lowerer did not return the exact expected primitive Ident leaf."
        )
      )
    else if lowered.exists(tree => tree.source.exists || tree.span.exists) then
      Left(
        error(
          "LOWERED_RESULT_TYPE_PROVENANCE",
          "the authoritative lowered primitive type was not source/span-free."
        )
      )
    else if lowered.exists(tree =>
        tree.symbol != NoSymbol || tree.isInstanceOf[untpd.TypedSplice]
      )
    then
      Left(
        error(
          "LOWERED_RESULT_TYPE_PRE_TYPER_REQUIRED",
          "the authoritative lowered primitive type carried a symbol or TypedSplice."
        )
      )
    else
      validatePositionedResult(result, expectedName.get)

  private def validatePositionedResult(
      result: Result,
      expectedName: String
  )(using Context): Either[ExistingUntpdSingleParameterMethodResultTypeRewriteError, Unit] =
    val positionedType = Option(result.positionedResultType)
    val positionedTypeValid = positionedType.exists {
      case value: untpd.Ident =>
        Option(value.name).exists(_.toString == expectedName) &&
          ExistingUntpdClassMemberFilter.allTrees(value).size == 1
      case _ => false
    }
    if !positionedTypeValid then
      Left(
        error(
          "POSITIONED_RESULT_TYPE_TOPOLOGY",
          "the positioned replacement was not the exact expected primitive Ident leaf."
        )
      )
    else
      val view = Option(result.view).filter(value =>
        ExistingUntpdSingleParameterMethodView.validate(value).isRight
      )
      val method = Option(result.positionedMethod)
      val template = Option(result.positionedTemplate)
      val root = Option(result.positionedRoot)
      val parameter = method
        .flatMap(value => Option(value.paramss))
        .filter(_.size == 1)
        .flatMap(_.headOption)
        .filter(_ != null)
        .filter(_.size == 1)
        .flatMap(_.headOption)
        .collect { case value: untpd.ValDef => value }
      val bodyIdentityValid = view.exists { validView =>
        Option(validView.captured)
          .flatMap(value => Option(value.originalTemplate))
          .flatMap(value => Option(value.body))
          .exists { originalBody =>
            template.exists { rebuilt =>
              Option(rebuilt.body).exists { body =>
                body.size == originalBody.size &&
                  originalBody.indices.forall { index =>
                    if index == validView.memberIndex then
                      method.exists(body(index).eq)
                    else body(index).eq(originalBody(index))
                  }
              }
            }
          }
      }
      val valid = view.exists(value =>
        result.normalForm == TypeNormalForm.STypeIdent(expectedName) &&
          !result.loweredResultType.eq(result.positionedResultType) &&
          !result.positionedResultType.eq(value.resultType) &&
          result.positionedResultType.source == value.resultType.source &&
          result.positionedResultType.span == value.resultType.span &&
          result.positionedResultType.symbol == NoSymbol &&
          method.exists(rebuilt =>
            !rebuilt.eq(value.method) &&
              rebuilt.paramss.eq(value.method.paramss) &&
              rebuilt.tpt.eq(result.positionedResultType) &&
              rebuilt.rhs.eq(value.rhs) &&
              rebuilt.mods.eq(value.method.mods) &&
              rebuilt.source == value.method.source &&
              rebuilt.span == value.method.span &&
              rebuilt.symbol == NoSymbol
          ) &&
          parameter.exists(param =>
            param.eq(value.parameter) && param.tpt.eq(value.parameterType)
          ) &&
          template.exists(rebuilt =>
            !rebuilt.eq(value.captured.originalTemplate) &&
              rebuilt.constr.eq(value.captured.originalTemplate.constr) &&
              rebuilt.parentsOrDerived.eq(value.captured.originalTemplate.parentsOrDerived) &&
              rebuilt.derived.eq(value.captured.originalTemplate.derived) &&
              rebuilt.self.eq(value.captured.originalTemplate.self) &&
              rebuilt.source == value.captured.originalTemplate.source &&
              rebuilt.span == value.captured.originalTemplate.span
          ) &&
          root.exists(rebuilt =>
            !rebuilt.eq(value.captured.originalRoot) &&
              rebuilt.rhs.eq(result.positionedTemplate) &&
              rebuilt.mods.eq(value.captured.originalRoot.mods) &&
              rebuilt.source == value.captured.originalRoot.source &&
              rebuilt.span == value.captured.originalRoot.span
          )
      ) && bodyIdentityValid
      Either.cond(
        valid,
        (),
        error(
          "FINAL_REWRITE_INVARIANT_FAILED",
          "the final U030 result violated exact view, primitive type, parameter/RHS, member-order, shell, or transformation-site invariants."
        )
      )

  private def reconstruct(
      view: ExistingUntpdSingleParameterMethodView.View,
      normalForm: TypeNormalForm,
      lowered: untpd.Tree
  )(using Context): Either[ExistingUntpdSingleParameterMethodResultTypeRewriteError, Result] =
    given SourceFile = NoSource
    val positionedResultType = lowered
      .cloneIn(view.resultType.source)
      .withSpan(view.resultType.span)
    val sourceFreeMethod = untpd
      .DefDef(
        view.method.name,
        view.method.paramss,
        lowered,
        view.rhs
      )
      .withMods(view.method.mods)
    val positionedMethod = untpd.cpy
      .DefDef(sourceFreeMethod)(
        sourceFreeMethod.name,
        sourceFreeMethod.paramss,
        positionedResultType,
        sourceFreeMethod.rhs
      )
      .cloneIn(view.method.source)
      .withSpan(view.method.span)
    val body = view.captured.originalTemplate.body.toVector.updated(
      view.memberIndex,
      positionedMethod
    )
    ExistingUntpdClassMemberFilter
      .reconstruct(view.captured, body)
      .left
      .map(problem => error("RECONSTRUCTION_FAILED", problem.message))
      .map(reconstructed =>
        Result(
          view,
          normalForm,
          lowered,
          positionedResultType,
          positionedMethod,
          reconstructed.template,
          reconstructed.root
        )
      )

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdSingleParameterMethodResultTypeRewriteError =
    ExistingUntpdSingleParameterMethodResultTypeRewriteError(code, detail)
