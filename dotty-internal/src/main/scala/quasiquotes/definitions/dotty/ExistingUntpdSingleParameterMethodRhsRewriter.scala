package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

/** Bounded RHS-only rewrite of one exact U028 single-parameter method view. */
private[quasiquotes] object ExistingUntpdSingleParameterMethodRhsRewriter:
  enum ReplacementFamily:
    case SingleNode
    case DirectIdentApply
    case DirectIdentQualifiedSelectedApply

  final case class Result(
      view: ExistingUntpdSingleParameterMethodView.View,
      replacementFamily: ReplacementFamily,
      structuralResult: ExistingUntpdMethodBodyRewriter.Result,
      positionedResult: ExistingUntpdMethodBodyRewriteOriginAdapter.Result
  )

  def rewrite(
      view: ExistingUntpdSingleParameterMethodView.View,
      replacementBody: untpd.Tree
  )(using Context): Either[ExistingUntpdSingleParameterMethodRhsRewriteError, Result] =
    for
      structural <- ExistingUntpdMethodBodyRewriter
        .rewriteSingleParameter(view, replacementBody)
        .left
        .map(problem => error(problem.code, problem.detail))
      family <- classify(structural.replacementBody)
      positioned <- family match
        case ReplacementFamily.SingleNode =>
          ExistingUntpdMethodBodyRewriteOriginAdapter.adapt(structural)
            .left.map(problem => error(problem.code, problem.detail))
        case ReplacementFamily.DirectIdentApply =>
          ExistingUntpdMethodBodyRewriteOriginAdapter.adaptApply(structural)
            .left.map(problem => error(problem.code, problem.detail))
        case ReplacementFamily.DirectIdentQualifiedSelectedApply =>
          ExistingUntpdMethodBodyRewriteOriginAdapter.adaptSelectedApply(structural)
            .left.map(problem => error(problem.code, problem.detail))
      _ <- validateResult(view, family, structural, positioned)
    yield Result(view, family, structural, positioned)

  private[dotty] def validateResult(
      view: ExistingUntpdSingleParameterMethodView.View,
      replacementFamily: ReplacementFamily,
      structural: ExistingUntpdMethodBodyRewriter.Result,
      positioned: ExistingUntpdMethodBodyRewriteOriginAdapter.Result
  )(using Context): Either[ExistingUntpdSingleParameterMethodRhsRewriteError, Unit] =
    val structuralParameter = exactParameter(Option(structural).map(_.rebuiltTarget))
    val positionedParameter = exactParameter(Option(positioned).map(_.positionedTarget))
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
        .exists(replacement => classify(replacement).contains(replacementFamily))
    val valid =
      Option(view).exists(value => ExistingUntpdSingleParameterMethodView.validate(value).isRight) &&
        Option(structural).exists(value =>
          value.originalRoot.eq(view.captured.originalRoot) &&
            value.originalTemplate.eq(view.captured.originalTemplate) &&
            value.originalTarget.eq(view.method) &&
            value.rebuiltTarget.tpt.eq(view.resultType) &&
            value.rebuiltTarget.rhs.eq(value.replacementBody)
        ) &&
        structuralParameter.exists(parameter =>
          parameter.eq(view.parameter) && parameter.tpt.eq(view.parameterType)
        ) &&
        Option(positioned).exists(value =>
          value.structuralResult.eq(structural) &&
            value.positionedTarget.tpt.eq(view.resultType) &&
            value.positionedTarget.rhs.eq(value.positionedReplacement)
        ) &&
        positionedParameter.exists(parameter =>
          parameter.eq(view.parameter) && parameter.tpt.eq(view.parameterType)
        ) &&
        bodyIdentityValid && familyValid
    Either.cond(
      valid,
      (),
      error(
        "FINAL_REWRITE_INVARIANT_FAILED",
        "the final U029 result did not preserve its validated view, replacement family, exact parameter/type identities, direct-member order, or positioned reconstruction linkage."
      )
    )

  private def exactParameter(
      method: Option[untpd.DefDef]
  ): Option[untpd.ValDef] =
    method
      .flatMap(value => Option(value.paramss))
      .filter(_.size == 1)
      .flatMap(_.headOption)
      .filter(_ != null)
      .filter(_.size == 1)
      .flatMap(_.headOption)
      .collect { case parameter: untpd.ValDef => parameter }

  private def classify(
      replacementBody: untpd.Tree
  )(using Context): Either[ExistingUntpdSingleParameterMethodRhsRewriteError, ReplacementFamily] =
    if ExistingUntpdClassMemberFilter.allTrees(replacementBody).size == 1 then
      Right(ReplacementFamily.SingleNode)
    else
      replacementBody match
        case replacement: untpd.Apply =>
          replacement.fun match
            case _: untpd.Ident => Right(ReplacementFamily.DirectIdentApply)
            case selection: untpd.Select
                if Option(selection.qualifier).exists(_.isInstanceOf[untpd.Ident]) =>
              Right(ReplacementFamily.DirectIdentQualifiedSelectedApply)
            case _ => unsupportedFamily(replacementBody)
        case _ => unsupportedFamily(replacementBody)

  private def unsupportedFamily(
      replacementBody: untpd.Tree
  ): Left[ExistingUntpdSingleParameterMethodRhsRewriteError, Nothing] =
    Left(
      error(
        "REPLACEMENT_FAMILY_UNSUPPORTED",
        s"U029 admits only U003 single-node, U005 direct-Ident Apply, or U013 direct-Ident-qualified selected Apply replacements; found ${replacementBody.getClass.getSimpleName}."
      )
    )

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdSingleParameterMethodRhsRewriteError =
    ExistingUntpdSingleParameterMethodRhsRewriteError(code, detail)
