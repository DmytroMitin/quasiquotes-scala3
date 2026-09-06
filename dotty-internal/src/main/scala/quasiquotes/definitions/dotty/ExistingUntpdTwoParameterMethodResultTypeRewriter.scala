package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.terms.dotty.CompletedTypeUntypedLowerer
import quasiquotes.types.TypeNormalForm

/** Bounded primitive result-type rewrite of one exact U033 two-parameter method view. */
private[quasiquotes] object ExistingUntpdTwoParameterMethodResultTypeRewriter:
  final case class Result(
      view: ExistingUntpdTwoParameterMethodView.View,
      normalForm: TypeNormalForm,
      loweredResultType: untpd.Tree,
      positionedResultType: untpd.Tree,
      positionedMethod: untpd.DefDef,
      positionedTemplate: untpd.Template,
      positionedRoot: untpd.TypeDef
  )

  def rewrite(
      view: ExistingUntpdTwoParameterMethodView.View,
      normalForm: TypeNormalForm
  )(using Context): Either[ExistingUntpdTwoParameterMethodResultTypeRewriteError, Result] =
    rewriteWithDependencies(
      view,
      normalForm,
      value => CompletedTypeUntypedLowerer.lower(value).left.map(_.message),
      (captured, body) => ExistingUntpdClassMemberFilter.reconstruct(captured, body)
    )

  private[dotty] def rewriteWithDependencies(
      view: ExistingUntpdTwoParameterMethodView.View,
      normalForm: TypeNormalForm,
      lower: TypeNormalForm => Either[String, untpd.Tree],
      reconstructOwner: (
          ExistingUntpdClassMemberFilter.Capture,
          Vector[untpd.Tree]
      ) => Either[
        ExistingUntpdClassMemberFilterError,
        ExistingUntpdClassMemberFilter.Reconstructed
      ]
  )(using Context): Either[ExistingUntpdTwoParameterMethodResultTypeRewriteError, Result] =
    for
      presentView <- Option(view).toRight(
        error("VIEW_REQUIRED", "the U033 method view was null.")
      )
      oldResultType <- Option(presentView.resultType)
        .filterNot(_.isEmpty)
        .toRight(
          error(
            "OLD_RESULT_TYPE_REQUIRED",
            "the old result-type transformation site was null or EmptyTree."
          )
        )
      _ <- requireSite(
        oldResultType,
        "OLD_RESULT_TYPE_PROVENANCE_REQUIRED",
        "the old result-type transformation site"
      )
      oldMethod <- Option(presentView.method).toRight(
        error("VIEW_INVALID", "the U033 method handle was null.")
      )
      _ <- requireSite(
        oldMethod,
        "OLD_METHOD_PROVENANCE_REQUIRED",
        "the old method shell"
      )
      _ <- validateView(presentView)
      presentNormalForm <- Option(normalForm).toRight(
        error("RESULT_TYPE_REQUIRED", "the semantic replacement result type was null.")
      )
      admitted <- admit(presentNormalForm)
      lowerer <- Option(lower).toRight(
        error("RESULT_TYPE_LOWERING_FAILED", "the bounded lowerer seam was null.")
      )
      lowering <- Option(lowerer(admitted)).toRight(
        error("RESULT_TYPE_LOWERING_FAILED", "the delegated lowerer returned null.")
      )
      lowered <- lowering.left.map(problem =>
        error("RESULT_TYPE_LOWERING_FAILED", problem)
      )
      _ <- validateLowered(lowered, primitiveName(admitted))
      reconstructor <- Option(reconstructOwner).toRight(
        error("RECONSTRUCTION_FAILED", "the bounded owner reconstructor seam was null.")
      )
      result <- reconstruct(presentView, admitted, lowered, reconstructor)
      _ <- validateResult(result)
    yield result

  private[dotty] def validateResult(
      result: Result
  )(using Context): Either[ExistingUntpdTwoParameterMethodResultTypeRewriteError, Unit] =
    Option(result).toRight(
      error("FINAL_REWRITE_INVARIANT_FAILED", "the U035 result was null.")
    ).flatMap { value =>
      Option(value.normalForm) match
        case Some(admitted @ TypeNormalForm.STypeIdent("Int" | "String" | "Boolean")) =>
          val expectedName = primitiveName(admitted)
          validateLowered(value.loweredResultType, expectedName).flatMap { _ =>
            val view = Option(value.view).filter(candidate =>
              ExistingUntpdTwoParameterMethodView.validate(candidate).isRight
            )
            val positionedType = Option(value.positionedResultType)
            val method = Option(value.positionedMethod)
            val template = Option(value.positionedTemplate)
            val root = Option(value.positionedRoot)
            val parameters = exactParameters(method)
            val positionedTypeValid = positionedType.exists {
              case ident: untpd.Ident =>
                Option(ident.name).exists(_.toString == expectedName) &&
                  safeAllTrees(ident).exists(_.size == 1)
              case _ => false
            }
            val bodyIdentityValid = view.exists { originalView =>
              Option(originalView.captured)
                .flatMap(captured => Option(captured.originalTemplate))
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
              !value.loweredResultType.eq(value.positionedResultType) &&
              !value.positionedResultType.eq(originalView.resultType) &&
              value.positionedResultType.source == originalView.resultType.source &&
              value.positionedResultType.span == originalView.resultType.span &&
              value.positionedResultType.symbol == NoSymbol &&
              parameters.exists { case (first, second) =>
                first.eq(originalView.firstParameter) &&
                first.tpt.eq(originalView.firstParameterType) &&
                second.eq(originalView.secondParameter) &&
                second.tpt.eq(originalView.secondParameterType)
              } &&
              method.exists(rebuilt =>
                !rebuilt.eq(originalView.method) &&
                  rebuilt.name == originalView.method.name &&
                  rebuilt.paramss.eq(originalView.method.paramss) &&
                  rebuilt.tpt.eq(value.positionedResultType) &&
                  rebuilt.rhs.eq(originalView.rhs) &&
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
              safeAllTrees(candidate).exists(_.forall(tree =>
                tree != null && tree.symbol == NoSymbol &&
                  !tree.isInstanceOf[untpd.TypedSplice]
              ))
            )
            Either.cond(
              shellValid && bodyIdentityValid && graphValid,
              (),
              error(
                "FINAL_REWRITE_INVARIANT_FAILED",
                "the final U035 result violated exact view, primitive result type, both parameter/type identities, preserved RHS, member-order, shell, provenance, or pre-Typer invariants."
              )
            )
          }
        case _ =>
          Left(
            error(
              "FINAL_REWRITE_INVARIANT_FAILED",
              "the U035 result did not retain one admitted primitive semantic type."
            )
          )
    }

  private def reconstruct(
      view: ExistingUntpdTwoParameterMethodView.View,
      normalForm: TypeNormalForm,
      lowered: untpd.Tree,
      reconstructOwner: (
          ExistingUntpdClassMemberFilter.Capture,
          Vector[untpd.Tree]
      ) => Either[
        ExistingUntpdClassMemberFilterError,
        ExistingUntpdClassMemberFilter.Reconstructed
      ]
  )(using Context): Either[ExistingUntpdTwoParameterMethodResultTypeRewriteError, Result] =
    given SourceFile = NoSource
    val positionedResultType = lowered
      .cloneIn(view.resultType.source)
      .withSpan(view.resultType.span)
    val sourceFreeMethod = untpd
      .DefDef(view.method.name, view.method.paramss, lowered, view.rhs)
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
    Option(reconstructOwner(view.captured, body)).toRight(
      error("RECONSTRUCTION_FAILED", "the delegated owner reconstructor returned null.")
    ).flatMap(
      _.left
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
    )

  private def admit(
      normalForm: TypeNormalForm
  ): Either[ExistingUntpdTwoParameterMethodResultTypeRewriteError, TypeNormalForm] =
    normalForm match
      case value @ TypeNormalForm.STypeIdent("Int" | "String" | "Boolean") => Right(value)
      case value: TypeNormalForm.STypeIdent =>
        Left(
          error(
            "RESULT_TYPE_IDENTIFIER_UNSUPPORTED",
            s"U035 does not admit primitive identifier ${Option(value.name).getOrElse("null")}."
          )
        )
      case other =>
        Left(
          error(
            "RESULT_TYPE_FAMILY_UNSUPPORTED",
            s"U035 admits no non-primitive TypeNormalForm family; found ${other.getClass.getSimpleName}."
          )
        )

  private def primitiveName(normalForm: TypeNormalForm): String = normalForm match
    case TypeNormalForm.STypeIdent(name) => name
    case _ => ""

  private def validateLowered(
      lowered: untpd.Tree,
      expectedName: String
  )(using Context): Either[ExistingUntpdTwoParameterMethodResultTypeRewriteError, Unit] =
    Option(lowered) match
      case Some(tree) if tree.source == null =>
        Left(
          error(
            "LOWERED_RESULT_TYPE_PROVENANCE",
            "the authoritative lowered primitive type had a null source field."
          )
        )
      case Some(tree) if tree.source.exists || tree.span.exists =>
        Left(
          error(
            "LOWERED_RESULT_TYPE_PROVENANCE",
            "the authoritative lowered primitive type was not source/span-free."
          )
        )
      case Some(tree)
          if tree.symbol != NoSymbol || tree.isInstanceOf[untpd.TypedSplice] =>
        Left(
          error(
            "LOWERED_RESULT_TYPE_PRE_TYPER_REQUIRED",
            "the authoritative lowered primitive type carried a symbol or TypedSplice."
          )
        )
      case Some(ident: untpd.Ident)
          if Option(ident.name).exists(_.toString == expectedName) &&
            safeAllTrees(ident).exists(_.size == 1) =>
        Right(())
      case _ =>
        Left(
          error(
            "LOWERED_RESULT_TYPE_TOPOLOGY",
            "the authoritative lowerer did not return the exact expected primitive Ident leaf."
          )
        )

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

  private def validateView(
      view: ExistingUntpdTwoParameterMethodView.View
  )(using Context): Either[ExistingUntpdTwoParameterMethodResultTypeRewriteError, Unit] =
    try
      ExistingUntpdTwoParameterMethodView
        .validate(view)
        .left
        .map(problem => error("VIEW_INVALID", problem.detail))
    catch
      case problem: NullPointerException =>
        Left(
          error(
            "VIEW_INVALID",
            s"the U033 method view could not be validated: ${problem.getClass.getSimpleName}."
          )
        )

  private def safeAllTrees(tree: untpd.Tree)(using Context): Option[Vector[untpd.Tree]] =
    try Option(tree).map(ExistingUntpdClassMemberFilter.allTrees)
    catch case _: NullPointerException => None

  private def requireSite(
      tree: untpd.Tree,
      code: String,
      label: String
  ): Either[ExistingUntpdTwoParameterMethodResultTypeRewriteError, Unit] =
    Either.cond(
      Option(tree.source).exists(_.exists) && tree.span.exists,
      (),
      error(code, s"$label must provide both source and span.")
    )

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdTwoParameterMethodResultTypeRewriteError =
    ExistingUntpdTwoParameterMethodResultTypeRewriteError(code, detail)
