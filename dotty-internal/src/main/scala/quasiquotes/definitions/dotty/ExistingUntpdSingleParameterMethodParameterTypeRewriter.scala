package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.terms.dotty.CompletedTypeUntypedLowerer
import quasiquotes.types.TypeNormalForm

/** Bounded primitive parameter-type rewrite of one exact U028 method view. */
private[quasiquotes] object ExistingUntpdSingleParameterMethodParameterTypeRewriter:
  final case class Result(
      view: ExistingUntpdSingleParameterMethodView.View,
      normalForm: TypeNormalForm,
      loweredParameterType: untpd.Tree,
      positionedParameterType: untpd.Tree,
      positionedParameter: untpd.ValDef,
      positionedMethod: untpd.DefDef,
      positionedTemplate: untpd.Template,
      positionedRoot: untpd.TypeDef
  )

  def rewrite(
      view: ExistingUntpdSingleParameterMethodView.View,
      normalForm: TypeNormalForm
  )(using Context): Either[ExistingUntpdSingleParameterMethodParameterTypeRewriteError, Result] =
    rewriteWithLowerer(
      view,
      normalForm,
      value => CompletedTypeUntypedLowerer.lower(value).left.map(_.message)
    )

  private[dotty] def rewriteWithLowerer(
      view: ExistingUntpdSingleParameterMethodView.View,
      normalForm: TypeNormalForm,
      lower: TypeNormalForm => Either[String, untpd.Tree]
  )(using Context): Either[ExistingUntpdSingleParameterMethodParameterTypeRewriteError, Result] =
    for
      presentView <- Option(view).toRight(
        error("VIEW_REQUIRED", "the U028 method view was null.")
      )
      oldParameter <- Option(presentView.parameter)
        .filterNot(_.isEmpty)
        .toRight(
          error(
            "OLD_PARAMETER_REQUIRED",
            "the old parameter transformation shell was null or EmptyTree."
          )
        )
      oldParameterType <- Option(presentView.parameterType)
        .filterNot(_.isEmpty)
        .toRight(
          error(
            "OLD_PARAMETER_TYPE_REQUIRED",
            "the old parameter-type transformation site was null or EmptyTree."
          )
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
      _ <- ExistingUntpdSingleParameterMethodView
        .validate(presentView)
        .left
        .map(problem => error("VIEW_INVALID", problem.detail))
      oldMethod <- Option(presentView.method).toRight(
        error("VIEW_INVALID", "the U028 method handle was null.")
      )
      _ <- requireSite(
        oldMethod,
        "OLD_METHOD_PROVENANCE_REQUIRED",
        "the old method shell"
      )
      presentNormalForm <- Option(normalForm).toRight(
        error("PARAMETER_TYPE_REQUIRED", "the semantic replacement parameter type was null.")
      )
      admitted <- admit(presentNormalForm)
      lowerer <- Option(lower).toRight(
        error("PARAMETER_TYPE_LOWERING_FAILED", "the bounded lowerer seam was null.")
      )
      lowering <- Option(lowerer(admitted)).toRight(
        error("PARAMETER_TYPE_LOWERING_FAILED", "the delegated lowerer returned null.")
      )
      lowered <- lowering.left.map(problem =>
        error("PARAMETER_TYPE_LOWERING_FAILED", problem)
      )
      _ <- validateLowered(lowered, primitiveName(admitted))
      result <- reconstruct(presentView, admitted, lowered)
      _ <- validateResult(result)
    yield result

  private[dotty] def validateResult(
      result: Result
  )(using Context): Either[ExistingUntpdSingleParameterMethodParameterTypeRewriteError, Unit] =
    Option(result).toRight(
      error("FINAL_REWRITE_INVARIANT_FAILED", "the U031 result was null.")
    ).flatMap { value =>
      Option(value.normalForm) match
        case Some(admitted @ TypeNormalForm.STypeIdent("Int" | "String" | "Boolean")) =>
          val expectedName = primitiveName(admitted)
          validateLowered(value.loweredParameterType, expectedName).flatMap { _ =>
            val view = Option(value.view).filter(candidate =>
              ExistingUntpdSingleParameterMethodView.validate(candidate).isRight
            )
            val positionedType = Option(value.positionedParameterType)
            val parameter = Option(value.positionedParameter)
            val method = Option(value.positionedMethod)
            val template = Option(value.positionedTemplate)
            val root = Option(value.positionedRoot)
            val exactMethodParameter = method
              .flatMap(candidate => Option(candidate.paramss))
              .filter(_.size == 1)
              .flatMap(_.headOption)
              .filter(_ != null)
              .filter(_.size == 1)
              .flatMap(_.headOption)
              .collect { case candidate: untpd.ValDef => candidate }
            val positionedTypeValid = positionedType.exists {
              case ident: untpd.Ident =>
                Option(ident.name).exists(_.toString == expectedName) &&
                  ExistingUntpdClassMemberFilter.allTrees(ident).size == 1
              case _ => false
            }
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
            val shellValid = view.exists { originalView =>
              positionedTypeValid &&
              value.normalForm == TypeNormalForm.STypeIdent(expectedName) &&
              !value.loweredParameterType.eq(value.positionedParameterType) &&
              !value.positionedParameterType.eq(originalView.parameterType) &&
              value.positionedParameterType.source == originalView.parameterType.source &&
              value.positionedParameterType.span == originalView.parameterType.span &&
              value.positionedParameterType.symbol == NoSymbol &&
              parameter.exists(rebuilt =>
                !rebuilt.eq(originalView.parameter) &&
                  rebuilt.tpt.eq(value.positionedParameterType) &&
                  rebuilt.rhs.isEmpty &&
                  rebuilt.name == originalView.parameter.name &&
                  rebuilt.mods.eq(originalView.parameter.mods) &&
                  rebuilt.source == originalView.parameter.source &&
                  rebuilt.span == originalView.parameter.span &&
                  rebuilt.symbol == NoSymbol
              ) &&
              exactMethodParameter.exists(exact => parameter.exists(_.eq(exact))) &&
              method.exists(rebuilt =>
                !rebuilt.eq(originalView.method) &&
                  rebuilt.tpt.eq(originalView.resultType) &&
                  rebuilt.rhs.eq(originalView.rhs) &&
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
            val graphValid = root.exists(candidate =>
              ExistingUntpdClassMemberFilter.allTrees(candidate).forall(tree =>
                tree != null && tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
              )
            )
            Either.cond(
              shellValid && bodyIdentityValid && graphValid,
              (),
              error(
                "FINAL_REWRITE_INVARIANT_FAILED",
                "the final U031 result violated exact view, fresh parameter/type, preserved result/RHS, member-order, shell, provenance, or pre-Typer invariants."
              )
            )
          }
        case _ =>
          Left(
            error(
              "FINAL_REWRITE_INVARIANT_FAILED",
              "the U031 result did not retain one admitted primitive semantic type."
            )
          )
    }

  private def reconstruct(
      view: ExistingUntpdSingleParameterMethodView.View,
      normalForm: TypeNormalForm,
      lowered: untpd.Tree
  )(using Context): Either[ExistingUntpdSingleParameterMethodParameterTypeRewriteError, Result] =
    given SourceFile = NoSource
    val positionedType = lowered
      .cloneIn(view.parameterType.source)
      .withSpan(view.parameterType.span)
    val sourceFreeParameter = untpd
      .ValDef(view.parameter.name, lowered, untpd.EmptyTree)
      .withMods(view.parameter.mods)
    val positionedParameter = untpd.cpy
      .ValDef(sourceFreeParameter)(
        sourceFreeParameter.name,
        positionedType,
        sourceFreeParameter.rhs
      )
      .cloneIn(view.parameter.source)
      .withSpan(view.parameter.span)
    val sourceFreeMethod = untpd
      .DefDef(
        view.method.name,
        List(List(sourceFreeParameter)),
        view.resultType,
        view.rhs
      )
      .withMods(view.method.mods)
    val positionedMethod = untpd.cpy
      .DefDef(sourceFreeMethod)(
        sourceFreeMethod.name,
        List(List(positionedParameter)),
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
      .left
      .map(problem => error("RECONSTRUCTION_FAILED", problem.message))
      .map(reconstructed =>
        Result(
          view,
          normalForm,
          lowered,
          positionedType,
          positionedParameter,
          positionedMethod,
          reconstructed.template,
          reconstructed.root
        )
      )

  private def admit(
      normalForm: TypeNormalForm
  ): Either[ExistingUntpdSingleParameterMethodParameterTypeRewriteError, TypeNormalForm] =
    normalForm match
      case value @ TypeNormalForm.STypeIdent("Int" | "String" | "Boolean") => Right(value)
      case value: TypeNormalForm.STypeIdent =>
        Left(
          error(
            "PARAMETER_TYPE_IDENTIFIER_UNSUPPORTED",
            s"U031 does not admit primitive identifier ${Option(value.name).getOrElse("null")}."
          )
        )
      case other =>
        Left(
          error(
            "PARAMETER_TYPE_FAMILY_UNSUPPORTED",
            s"U031 admits no non-primitive TypeNormalForm family; found ${other.getClass.getSimpleName}."
          )
        )

  private def primitiveName(normalForm: TypeNormalForm): String = normalForm match
    case TypeNormalForm.STypeIdent(name) => name
    case _ => ""

  private def validateLowered(
      lowered: untpd.Tree,
      expectedName: String
  )(using Context): Either[ExistingUntpdSingleParameterMethodParameterTypeRewriteError, Unit] =
    Option(lowered) match
      case Some(ident: untpd.Ident) if ident.source == null =>
        Left(error("LOWERED_PARAMETER_TYPE_PROVENANCE", "the lowered primitive type had a null source."))
      case Some(ident: untpd.Ident)
          if Option(ident.name).exists(_.toString == expectedName) &&
            ExistingUntpdClassMemberFilter.allTrees(ident).size == 1 =>
        if ident.source.exists || ident.span.exists then
          Left(error("LOWERED_PARAMETER_TYPE_PROVENANCE", "the lowered primitive type was not source/span-free."))
        else if ident.symbol != NoSymbol then
          Left(error("LOWERED_PARAMETER_TYPE_PRE_TYPER_REQUIRED", "the lowered primitive type carried a symbol."))
        else Right(())
      case _ =>
        Left(error("LOWERED_PARAMETER_TYPE_TOPOLOGY", "the lowerer did not return the exact expected primitive Ident leaf."))

  private def requireSite(
      tree: untpd.Tree,
      code: String,
      label: String
  ): Either[ExistingUntpdSingleParameterMethodParameterTypeRewriteError, Unit] =
    Either.cond(
      Option(tree.source).exists(_.exists) && tree.span.exists,
      (),
      error(code, s"$label must provide both source and span.")
    )

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdSingleParameterMethodParameterTypeRewriteError =
    ExistingUntpdSingleParameterMethodParameterTypeRewriteError(code, detail)
