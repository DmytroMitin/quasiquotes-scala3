package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.types.TypeNormalForm

/** Atomic parameter-type, result-type, and RHS rewrite of one exact U028 view. */
private[quasiquotes] object ExistingUntpdSingleParameterMethodAtomicRewriter:
  final case class Result(
      view: ExistingUntpdSingleParameterMethodView.View,
      parameterRewrite: ExistingUntpdSingleParameterMethodParameterTypeRewriter.Result,
      resultRewrite: ExistingUntpdSingleParameterMethodResultTypeRewriter.Result,
      rhsRewrite: ExistingUntpdSingleParameterMethodRhsRewriter.Result,
      positionedMethod: untpd.DefDef,
      positionedTemplate: untpd.Template,
      positionedRoot: untpd.TypeDef
  )

  def rewrite(
      view: ExistingUntpdSingleParameterMethodView.View,
      parameterType: TypeNormalForm,
      resultType: TypeNormalForm,
      replacementRhs: untpd.Tree
  )(using Context): Either[ExistingUntpdSingleParameterMethodAtomicRewriteError, Result] =
    for
      presentView <- Option(view).toRight(
        error("VIEW_REQUIRED", "the U028 method view was null.")
      )
      oldParameter <- requiredTree(
        presentView.parameter,
        "OLD_PARAMETER_REQUIRED",
        "the old parameter shell"
      )
      oldParameterType <- requiredTree(
        presentView.parameterType,
        "OLD_PARAMETER_TYPE_REQUIRED",
        "the old parameter-type transformation site"
      )
      oldResultType <- requiredTree(
        presentView.resultType,
        "OLD_RESULT_TYPE_REQUIRED",
        "the old result-type transformation site"
      )
      oldRhs <- requiredTree(
        presentView.rhs,
        "OLD_RHS_REQUIRED",
        "the old RHS transformation site"
      )
      oldMethod <- requiredTree(
        presentView.method,
        "OLD_METHOD_REQUIRED",
        "the old method shell"
      )
      _ <- requireSite(
        oldParameterType,
        "OLD_PARAMETER_TYPE_PROVENANCE_REQUIRED",
        "the old parameter-type transformation site"
      )
      _ <- requireSite(
        oldParameter,
        "OLD_PARAMETER_PROVENANCE_REQUIRED",
        "the old parameter shell"
      )
      _ <- requireSite(
        oldResultType,
        "OLD_RESULT_TYPE_PROVENANCE_REQUIRED",
        "the old result-type transformation site"
      )
      _ <- requireSite(
        oldRhs,
        "OLD_RHS_PROVENANCE_REQUIRED",
        "the old RHS transformation site"
      )
      _ <- requireSite(
        oldMethod,
        "OLD_METHOD_PROVENANCE_REQUIRED",
        "the old method shell"
      )
      _ <- ExistingUntpdSingleParameterMethodView
        .validate(presentView)
        .left.map(problem => error("VIEW_INVALID", problem.detail))
      presentParameterType <- Option(parameterType).toRight(
        error("PARAMETER_TYPE_REQUIRED", "the semantic replacement parameter type was null.")
      )
      presentResultType <- Option(resultType).toRight(
        error("RESULT_TYPE_REQUIRED", "the semantic replacement result type was null.")
      )
      presentReplacementRhs <- Option(replacementRhs)
        .filterNot(_.isEmpty)
        .toRight(
          error("RHS_REPLACEMENT_REQUIRED", "the replacement RHS was null or EmptyTree.")
        )
      parameterRewrite <- ExistingUntpdSingleParameterMethodParameterTypeRewriter
        .rewrite(presentView, presentParameterType)
        .left.map(problem => error("PARAMETER_REWRITE_FAILED", problem.message))
      resultRewrite <- ExistingUntpdSingleParameterMethodResultTypeRewriter
        .rewrite(presentView, presentResultType)
        .left.map(problem => error("RESULT_REWRITE_FAILED", problem.message))
      rhsRewrite <- ExistingUntpdSingleParameterMethodRhsRewriter
        .rewrite(presentView, presentReplacementRhs)
        .left.map(problem => error("RHS_REWRITE_FAILED", problem.message))
      result <- compose(presentView, parameterRewrite, resultRewrite, rhsRewrite)
    yield result

  private[dotty] def compose(
      view: ExistingUntpdSingleParameterMethodView.View,
      parameterRewrite: ExistingUntpdSingleParameterMethodParameterTypeRewriter.Result,
      resultRewrite: ExistingUntpdSingleParameterMethodResultTypeRewriter.Result,
      rhsRewrite: ExistingUntpdSingleParameterMethodRhsRewriter.Result
  )(using Context): Either[ExistingUntpdSingleParameterMethodAtomicRewriteError, Result] =
    for
      presentView <- Option(view).toRight(
        error("VIEW_REQUIRED", "the U028 method view was null.")
      )
      parameter <- Option(parameterRewrite).toRight(
        error("PARAMETER_COMPONENT_INVALID", "the U031 component result was null.")
      )
      resultType <- Option(resultRewrite).toRight(
        error("RESULT_COMPONENT_INVALID", "the U030 component result was null.")
      )
      rhs <- Option(rhsRewrite).toRight(
        error("RHS_COMPONENT_INVALID", "the U029 component result was null.")
      )
      _ <- ExistingUntpdSingleParameterMethodParameterTypeRewriter
        .validateResult(parameter)
        .left.map(problem => error("PARAMETER_COMPONENT_INVALID", problem.message))
      _ <- Either.cond(
        parameter.view.eq(presentView),
        (),
        error("PARAMETER_COMPONENT_INVALID", "the U031 component did not retain the original U028 view.")
      )
      _ <- ExistingUntpdSingleParameterMethodResultTypeRewriter
        .validateResult(resultType)
        .left.map(problem => error("RESULT_COMPONENT_INVALID", problem.message))
      _ <- Either.cond(
        resultType.view.eq(presentView),
        (),
        error("RESULT_COMPONENT_INVALID", "the U030 component did not retain the original U028 view.")
      )
      _ <- ExistingUntpdSingleParameterMethodRhsRewriter
        .validateResult(
          rhs.view,
          rhs.replacementFamily,
          rhs.structuralResult,
          rhs.positionedResult
        )
        .left.map(problem => error("RHS_COMPONENT_INVALID", problem.message))
      _ <- Either.cond(
        rhs.view.eq(presentView),
        (),
        error("RHS_COMPONENT_INVALID", "the U029 component did not retain the original U028 view.")
      )
      _ <- Either.cond(
        !parameter.loweredParameterType.eq(resultType.loweredResultType) &&
          !parameter.positionedParameterType.eq(resultType.positionedResultType),
        (),
        error(
          "TYPE_FRAGMENT_ALIAS",
          "the independently lowered parameter and result type fragments aliased."
        )
      )
      result <- reconstruct(presentView, parameter, resultType, rhs)
      _ <- validateResult(result)
    yield result

  private[dotty] def validateResult(
      result: Result
  )(using Context): Either[ExistingUntpdSingleParameterMethodAtomicRewriteError, Unit] =
    Option(result).toRight(
      error("FINAL_REWRITE_INVARIANT_FAILED", "the U032 result was null.")
    ).flatMap { value =>
      val view = Option(value.view).filter(candidate =>
        ExistingUntpdSingleParameterMethodView.validate(candidate).isRight
      )
      val parameterComponent = Option(value.parameterRewrite)
      val resultComponent = Option(value.resultRewrite)
      val rhsComponent = Option(value.rhsRewrite)
      val method = Option(value.positionedMethod)
      val template = Option(value.positionedTemplate)
      val root = Option(value.positionedRoot)
      val exactParameter = method
        .flatMap(candidate => Option(candidate.paramss))
        .filter(_.size == 1)
        .flatMap(_.headOption)
        .filter(_ != null)
        .filter(_.size == 1)
        .flatMap(_.headOption)
        .collect { case candidate: untpd.ValDef => candidate }
      val bodyIdentityValid = view.exists { originalView =>
        Option(originalView.captured)
          .flatMap(capture => Option(capture.originalTemplate))
          .flatMap(original => Option(original.body))
          .exists { originalBody =>
            template.flatMap(rebuilt => Option(rebuilt.body)).exists { rebuiltBody =>
              rebuiltBody.size == originalBody.size &&
              originalBody.indices.forall { index =>
                if index == originalView.memberIndex then
                  method.exists(rebuiltBody(index).eq)
                else rebuiltBody(index).eq(originalBody(index))
              }
            }
          }
      }
      val componentsValid =
        (for
          parameter <- parameterComponent
          resultType <- resultComponent
          rhs <- rhsComponent
        yield
          ExistingUntpdSingleParameterMethodParameterTypeRewriter
            .validateResult(parameter)
            .isRight &&
            ExistingUntpdSingleParameterMethodResultTypeRewriter
              .validateResult(resultType)
              .isRight &&
            ExistingUntpdSingleParameterMethodRhsRewriter
              .validateResult(
                rhs.view,
                rhs.replacementFamily,
                rhs.structuralResult,
                rhs.positionedResult
              )
              .isRight
        ).contains(true)
      val valid = view.exists { originalView =>
        parameterComponent.exists { parameter =>
          resultComponent.exists { resultType =>
            rhsComponent.exists { rhs =>
              val freshParameter = parameter.positionedParameter
              val positionedResultType = resultType.positionedResultType
              val positionedRhs = rhs.positionedResult.positionedReplacement
              parameter.view.eq(originalView) &&
              resultType.view.eq(originalView) &&
              rhs.view.eq(originalView) &&
              !parameter.loweredParameterType.eq(resultType.loweredResultType) &&
              !parameter.positionedParameterType.eq(resultType.positionedResultType) &&
              !freshParameter.eq(originalView.parameter) &&
              !freshParameter.tpt.eq(originalView.parameterType) &&
              freshParameter.tpt.eq(parameter.positionedParameterType) &&
              freshParameter.name == originalView.parameter.name &&
              freshParameter.mods.eq(originalView.parameter.mods) &&
              freshParameter.rhs.isEmpty &&
              !positionedResultType.eq(originalView.resultType) &&
              !positionedRhs.eq(originalView.rhs) &&
              exactParameter.exists(_.eq(freshParameter)) &&
              method.exists(rebuilt =>
                !rebuilt.eq(originalView.method) &&
                  !rebuilt.eq(parameter.positionedMethod) &&
                  !rebuilt.eq(resultType.positionedMethod) &&
                  !rebuilt.eq(rhs.positionedResult.positionedTarget) &&
                  rebuilt.tpt.eq(positionedResultType) &&
                  rebuilt.rhs.eq(positionedRhs) &&
                  rebuilt.name == originalView.method.name &&
                  rebuilt.mods.eq(originalView.method.mods) &&
                  rebuilt.source == originalView.method.source &&
                  rebuilt.span == originalView.method.span &&
                  rebuilt.symbol == NoSymbol
              ) &&
              template.exists(rebuilt =>
                !rebuilt.eq(originalView.captured.originalTemplate) &&
                  rebuilt.constr.eq(originalView.captured.originalTemplate.constr) &&
                  rebuilt.parentsOrDerived.eq(originalView.captured.originalTemplate.parentsOrDerived) &&
                  rebuilt.derived.eq(originalView.captured.originalTemplate.derived) &&
                  rebuilt.self.eq(originalView.captured.originalTemplate.self) &&
                  rebuilt.source == originalView.captured.originalTemplate.source &&
                  rebuilt.span == originalView.captured.originalTemplate.span
              ) &&
              root.exists(rebuilt =>
                !rebuilt.eq(originalView.captured.originalRoot) &&
                  rebuilt.rhs.eq(value.positionedTemplate) &&
                  rebuilt.mods.eq(originalView.captured.originalRoot.mods) &&
                  rebuilt.source == originalView.captured.originalRoot.source &&
                  rebuilt.span == originalView.captured.originalRoot.span
              )
            }
          }
        }
      }
      val graphValid = root.exists(candidate =>
        ExistingUntpdClassMemberFilter.allTrees(candidate).forall(tree =>
          tree != null && tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
        )
      )
      Either.cond(
        componentsValid && valid && bodyIdentityValid && graphValid,
        (),
        error(
          "FINAL_REWRITE_INVARIANT_FAILED",
          "the final U032 result violated component linkage, independent type freshness, atomic field replacement, member identity/order, shell provenance, or pre-Typer invariants."
        )
      )
    }

  private def reconstruct(
      view: ExistingUntpdSingleParameterMethodView.View,
      parameterRewrite: ExistingUntpdSingleParameterMethodParameterTypeRewriter.Result,
      resultRewrite: ExistingUntpdSingleParameterMethodResultTypeRewriter.Result,
      rhsRewrite: ExistingUntpdSingleParameterMethodRhsRewriter.Result
  )(using Context): Either[ExistingUntpdSingleParameterMethodAtomicRewriteError, Result] =
    val parameter = parameterRewrite.positionedParameter
    val resultType = resultRewrite.positionedResultType
    val rhs = rhsRewrite.positionedResult.positionedReplacement
    given SourceFile = NoSource
    val sourceFreeMethod = untpd
      .DefDef(view.method.name, List(List(parameter)), resultType, rhs)
      .withMods(view.method.mods)
    val positionedMethod = untpd.cpy
      .DefDef(sourceFreeMethod)(
        sourceFreeMethod.name,
        sourceFreeMethod.paramss,
        sourceFreeMethod.tpt,
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
      .left.map(problem => error("RECONSTRUCTION_FAILED", problem.message))
      .map(reconstructed =>
        Result(
          view,
          parameterRewrite,
          resultRewrite,
          rhsRewrite,
          positionedMethod,
          reconstructed.template,
          reconstructed.root
        )
      )

  private def requiredTree(
      tree: untpd.Tree,
      code: String,
      label: String
  ): Either[ExistingUntpdSingleParameterMethodAtomicRewriteError, untpd.Tree] =
    Option(tree).filterNot(_.isEmpty).toRight(
      error(code, s"$label was null or EmptyTree.")
    )

  private def requireSite(
      tree: untpd.Tree,
      code: String,
      label: String
  ): Either[ExistingUntpdSingleParameterMethodAtomicRewriteError, Unit] =
    Either.cond(
      Option(tree.source).exists(_.exists) && tree.span.exists,
      (),
      error(code, s"$label must provide both source and span.")
    )

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdSingleParameterMethodAtomicRewriteError =
    ExistingUntpdSingleParameterMethodAtomicRewriteError(code, detail)
