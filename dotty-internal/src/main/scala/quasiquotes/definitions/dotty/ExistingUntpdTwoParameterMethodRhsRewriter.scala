package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

/** Bounded RHS-only rewrite of one exact U033 two-parameter method view. */
private[quasiquotes] object ExistingUntpdTwoParameterMethodRhsRewriter:
  type ReplacementFamily = ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily

  final case class Result(
      view: ExistingUntpdTwoParameterMethodView.View,
      replacementFamily: ReplacementFamily,
      structuralResult: ExistingUntpdMethodBodyRewriter.Result,
      positionedResult: ExistingUntpdMethodBodyRewriteOriginAdapter.Result
  )

  def rewrite(
      view: ExistingUntpdTwoParameterMethodView.View,
      replacementBody: untpd.Tree
  )(using Context): Either[ExistingUntpdTwoParameterMethodRhsRewriteError, Result] =
    for
      structural <- ExistingUntpdMethodBodyRewriter
        .rewriteTwoParameter(view, replacementBody)
        .left
        .map(problem => error(problem.code, problem.detail))
      family <- ExistingUntpdSingleParameterMethodRhsRewriter
        .classify(structural.replacementBody, "U034")
        .left
        .map(problem => error(problem.code, problem.detail))
      positioned <- family match
        case ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily.SingleNode =>
          ExistingUntpdMethodBodyRewriteOriginAdapter.adapt(structural)
            .left.map(problem => error(problem.code, problem.detail))
        case ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily.DirectIdentApply =>
          ExistingUntpdMethodBodyRewriteOriginAdapter.adaptApply(structural)
            .left.map(problem => error(problem.code, problem.detail))
        case ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily.DirectIdentQualifiedSelectedApply =>
          ExistingUntpdMethodBodyRewriteOriginAdapter.adaptSelectedApply(structural)
            .left.map(problem => error(problem.code, problem.detail))
      _ <- validateResult(view, family, structural, positioned)
    yield Result(view, family, structural, positioned)

  private[dotty] def validateResult(
      view: ExistingUntpdTwoParameterMethodView.View,
      replacementFamily: ReplacementFamily,
      structural: ExistingUntpdMethodBodyRewriter.Result,
      positioned: ExistingUntpdMethodBodyRewriteOriginAdapter.Result
  )(using Context): Either[ExistingUntpdTwoParameterMethodRhsRewriteError, Unit] =
    val structuralParameters = exactParameters(Option(structural).map(_.rebuiltTarget))
    val positionedParameters = exactParameters(Option(positioned).map(_.positionedTarget))
    val bodyIdentityValid =
      Option(view)
        .flatMap(value => Option(value.captured))
        .flatMap(value => Option(value.originalTemplate))
        .flatMap(value => Option(value.body))
        .exists { originalBody =>
          Option(positioned)
            .flatMap(value => Option(value.positionedTemplate))
            .flatMap(value => Option(value.body))
            .exists { positionedBody =>
              originalBody.size == positionedBody.size &&
              originalBody.indices.forall { index =>
                if index == view.memberIndex then
                  positionedBody(index).eq(positioned.positionedTarget)
                else positionedBody(index).eq(originalBody(index))
              }
            }
        }
    val familyValid =
      Option(structural)
        .flatMap(value => Option(value.replacementBody))
        .exists(replacement =>
          ExistingUntpdSingleParameterMethodRhsRewriter
            .classify(replacement, "U034")
            .contains(replacementFamily)
        )
    val valid =
      Option(view).exists(value => ExistingUntpdTwoParameterMethodView.validate(value).isRight) &&
        Option(structural).exists(value =>
          value.originalRoot.eq(view.captured.originalRoot) &&
            value.originalTemplate.eq(view.captured.originalTemplate) &&
            value.originalTarget.eq(view.method) &&
            value.rebuiltTarget.tpt.eq(view.resultType) &&
            value.rebuiltTarget.rhs.eq(value.replacementBody)
        ) &&
        structuralParameters.exists { case (first, second) =>
          exactParameterIdentity(view, first, second)
        } &&
        Option(positioned).exists(value =>
          value.structuralResult.eq(structural) &&
            value.positionedTarget.tpt.eq(view.resultType) &&
            value.positionedTarget.rhs.eq(value.positionedReplacement)
        ) &&
        positionedParameters.exists { case (first, second) =>
          exactParameterIdentity(view, first, second)
        } &&
        bodyIdentityValid && familyValid
    Either.cond(
      valid,
      (),
      error(
        "FINAL_REWRITE_INVARIANT_FAILED",
        "the final U034 result did not preserve its validated view, replacement family, both exact parameter/type identities, result type, direct-member order, or positioned reconstruction linkage."
      )
    )

  private def exactParameterIdentity(
      view: ExistingUntpdTwoParameterMethodView.View,
      first: untpd.ValDef,
      second: untpd.ValDef
  ): Boolean =
    first.eq(view.firstParameter) &&
      first.tpt.eq(view.firstParameterType) &&
      second.eq(view.secondParameter) &&
      second.tpt.eq(view.secondParameterType)

  private def exactParameters(
      method: Option[untpd.DefDef]
  ): Option[(untpd.ValDef, untpd.ValDef)] =
    method
      .flatMap(value => Option(value.paramss))
      .filter(_.size == 1)
      .flatMap(_.headOption)
      .filter(_ != null)
      .filter(_.size == 2)
      .flatMap { clause =>
        (clause(0), clause(1)) match
          case (first: untpd.ValDef, second: untpd.ValDef) => Some((first, second))
          case _ => None
      }

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdTwoParameterMethodRhsRewriteError =
    ExistingUntpdTwoParameterMethodRhsRewriteError(code, detail)
