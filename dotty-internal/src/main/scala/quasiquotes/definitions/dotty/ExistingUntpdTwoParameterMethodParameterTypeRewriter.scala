package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.terms.dotty.CompletedTypeUntypedLowerer
import quasiquotes.types.TypeNormalForm

/** Bounded selected-parameter primitive type rewrite of one exact U033 method view. */
private[quasiquotes] object ExistingUntpdTwoParameterMethodParameterTypeRewriter:
  final case class Result(
      view: ExistingUntpdTwoParameterMethodView.View,
      parameterIndex: Int,
      normalForm: TypeNormalForm,
      loweredParameterType: untpd.Tree,
      positionedParameterType: untpd.Tree,
      positionedParameter: untpd.ValDef,
      positionedMethod: untpd.DefDef,
      positionedTemplate: untpd.Template,
      positionedRoot: untpd.TypeDef
  )

  def rewrite(
      view: ExistingUntpdTwoParameterMethodView.View,
      parameterIndex: Int,
      normalForm: TypeNormalForm
  )(using Context): Either[ExistingUntpdTwoParameterMethodParameterTypeRewriteError, Result] =
    rewriteWithDependencies(
      view,
      parameterIndex,
      normalForm,
      value => CompletedTypeUntypedLowerer.lower(value).left.map(_.message),
      (captured, body) => ExistingUntpdClassMemberFilter.reconstruct(captured, body)
    )

  private[dotty] def rewriteWithDependencies(
      view: ExistingUntpdTwoParameterMethodView.View,
      parameterIndex: Int,
      normalForm: TypeNormalForm,
      lower: TypeNormalForm => Either[String, untpd.Tree],
      reconstructOwner: (
          ExistingUntpdClassMemberFilter.Capture,
          Vector[untpd.Tree]
      ) => Either[
        ExistingUntpdClassMemberFilterError,
        ExistingUntpdClassMemberFilter.Reconstructed
      ]
  )(using Context): Either[ExistingUntpdTwoParameterMethodParameterTypeRewriteError, Result] =
    for
      presentView <- Option(view).toRight(
        error("VIEW_REQUIRED", "the U033 method view was null.")
      )
      selectedIndex <- Either.cond(
        parameterIndex == 0 || parameterIndex == 1,
        parameterIndex,
        error(
          "PARAMETER_INDEX_UNSUPPORTED",
          s"U036 admits only parameter index 0 or 1; found $parameterIndex."
        )
      )
      oldParameter <- selectedParameter(presentView, selectedIndex)
      oldParameterType <- selectedParameterType(presentView, selectedIndex)
      _ <- requireSite(
        oldParameterType,
        "OLD_PARAMETER_TYPE_PROVENANCE_REQUIRED",
        "the selected old parameter-type transformation site"
      )
      _ <- requireSite(
        oldParameter,
        "OLD_PARAMETER_PROVENANCE_REQUIRED",
        "the selected old parameter shell"
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
      reconstructor <- Option(reconstructOwner).toRight(
        error("RECONSTRUCTION_FAILED", "the bounded owner reconstructor seam was null.")
      )
      result <- reconstruct(
        presentView,
        selectedIndex,
        admitted,
        lowered,
        reconstructor
      )
      _ <- validateResult(result)
    yield result

  private[dotty] def validateResult(
      result: Result
  )(using Context): Either[ExistingUntpdTwoParameterMethodParameterTypeRewriteError, Unit] =
    Option(result).toRight(
      error("FINAL_REWRITE_INVARIANT_FAILED", "the U036 result was null.")
    ).flatMap { value =>
      Option(value.normalForm) match
        case Some(admitted @ TypeNormalForm.STypeIdent("Int" | "String" | "Boolean"))
            if value.parameterIndex == 0 || value.parameterIndex == 1 =>
          validateLowered(value.loweredParameterType, primitiveName(admitted)).flatMap { _ =>
            validateFinalGraph(value, primitiveName(admitted))
          }
        case _ =>
          Left(
            error(
              "FINAL_REWRITE_INVARIANT_FAILED",
              "the U036 result did not retain one admitted parameter index and primitive semantic type."
            )
          )
    }

  private def validateFinalGraph(
      result: Result,
      expectedName: String
  )(using Context): Either[ExistingUntpdTwoParameterMethodParameterTypeRewriteError, Unit] =
    val view = Option(result.view).filter(candidate => validateView(candidate).isRight)
    val positionedType = Option(result.positionedParameterType)
    val positionedParameter = Option(result.positionedParameter)
    val method = Option(result.positionedMethod)
    val template = Option(result.positionedTemplate)
    val root = Option(result.positionedRoot)
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
      val selected = parameterAt(originalView, result.parameterIndex)
      val selectedType = parameterTypeAt(originalView, result.parameterIndex)
      val untouched = parameterAt(originalView, 1 - result.parameterIndex)
      val untouchedType = parameterTypeAt(originalView, 1 - result.parameterIndex)
      val parameterIdentityValid = parameters.exists { case (first, second) =>
        val rebuilt = Vector(first, second)
        rebuilt(result.parameterIndex).eq(result.positionedParameter) &&
        rebuilt(1 - result.parameterIndex).eq(untouched) &&
        rebuilt(1 - result.parameterIndex).tpt.eq(untouchedType)
      }
      positionedTypeValid &&
      result.normalForm == TypeNormalForm.STypeIdent(expectedName) &&
      !result.loweredParameterType.eq(result.positionedParameterType) &&
      !result.positionedParameterType.eq(selectedType) &&
      result.positionedParameterType.source == selectedType.source &&
      result.positionedParameterType.span == selectedType.span &&
      result.positionedParameterType.symbol == NoSymbol &&
      positionedParameter.exists(rebuilt =>
        !rebuilt.eq(selected) &&
          rebuilt.tpt.eq(result.positionedParameterType) &&
          rebuilt.rhs.isEmpty &&
          rebuilt.name == selected.name &&
          rebuilt.mods.eq(selected.mods) &&
          rebuilt.source == selected.source &&
          rebuilt.span == selected.span &&
          rebuilt.symbol == NoSymbol
      ) &&
      parameterIdentityValid &&
      method.exists(rebuilt =>
        !rebuilt.eq(originalView.method) &&
          rebuilt.name == originalView.method.name &&
          rebuilt.tpt.eq(originalView.resultType) &&
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
          rebuilt.rhs.eq(result.positionedTemplate) &&
          rebuilt.mods.eq(originalView.captured.originalRoot.mods) &&
          rebuilt.source == originalView.captured.originalRoot.source &&
          rebuilt.span == originalView.captured.originalRoot.span
      )
    }
    val graphValid = root.exists(candidate =>
      safeAllTrees(candidate).exists(_.forall(tree =>
        tree != null && tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
      ))
    )
    Either.cond(
      shellValid && bodyIdentityValid && graphValid,
      (),
      error(
        "FINAL_REWRITE_INVARIANT_FAILED",
        "the final U036 result violated selected/untouched parameter identity, preserved result/RHS, member order, shell, provenance, or pre-Typer invariants."
      )
    )

  private def reconstruct(
      view: ExistingUntpdTwoParameterMethodView.View,
      parameterIndex: Int,
      normalForm: TypeNormalForm,
      lowered: untpd.Tree,
      reconstructOwner: (
          ExistingUntpdClassMemberFilter.Capture,
          Vector[untpd.Tree]
      ) => Either[
        ExistingUntpdClassMemberFilterError,
        ExistingUntpdClassMemberFilter.Reconstructed
      ]
  )(using Context): Either[ExistingUntpdTwoParameterMethodParameterTypeRewriteError, Result] =
    val oldParameter = parameterAt(view, parameterIndex)
    val oldParameterType = parameterTypeAt(view, parameterIndex)
    val untouchedParameter = parameterAt(view, 1 - parameterIndex)
    given SourceFile = NoSource
    val positionedParameterType = lowered
      .cloneIn(oldParameterType.source)
      .withSpan(oldParameterType.span)
    val sourceFreeParameter = untpd
      .ValDef(oldParameter.name, lowered, untpd.EmptyTree)
      .withMods(oldParameter.mods)
    val positionedParameter = untpd.cpy
      .ValDef(sourceFreeParameter)(
        sourceFreeParameter.name,
        positionedParameterType,
        sourceFreeParameter.rhs
      )
      .cloneIn(oldParameter.source)
      .withSpan(oldParameter.span)
    val parameters =
      if parameterIndex == 0 then List(positionedParameter, untouchedParameter)
      else List(untouchedParameter, positionedParameter)
    val sourceFreeMethod = untpd
      .DefDef(view.method.name, List(parameters), view.resultType, view.rhs)
      .withMods(view.method.mods)
    val positionedMethod = untpd.cpy
      .DefDef(sourceFreeMethod)(
        sourceFreeMethod.name,
        List(parameters),
        sourceFreeMethod.tpt,
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
            parameterIndex,
            normalForm,
            lowered,
            positionedParameterType,
            positionedParameter,
            positionedMethod,
            reconstructed.template,
            reconstructed.root
          )
        )
    )

  private def selectedParameter(
      view: ExistingUntpdTwoParameterMethodView.View,
      parameterIndex: Int
  ): Either[ExistingUntpdTwoParameterMethodParameterTypeRewriteError, untpd.ValDef] =
    Option(parameterAt(view, parameterIndex)).filterNot(_.isEmpty).toRight(
      error(
        "OLD_PARAMETER_REQUIRED",
        "the selected old parameter shell was null or EmptyTree."
      )
    )

  private def selectedParameterType(
      view: ExistingUntpdTwoParameterMethodView.View,
      parameterIndex: Int
  ): Either[ExistingUntpdTwoParameterMethodParameterTypeRewriteError, untpd.Tree] =
    Option(parameterTypeAt(view, parameterIndex)).filterNot(_.isEmpty).toRight(
      error(
        "OLD_PARAMETER_TYPE_REQUIRED",
        "the selected old parameter-type transformation site was null or EmptyTree."
      )
    )

  private def parameterAt(
      view: ExistingUntpdTwoParameterMethodView.View,
      parameterIndex: Int
  ): untpd.ValDef =
    if parameterIndex == 0 then view.firstParameter else view.secondParameter

  private def parameterTypeAt(
      view: ExistingUntpdTwoParameterMethodView.View,
      parameterIndex: Int
  ): untpd.Tree =
    if parameterIndex == 0 then view.firstParameterType else view.secondParameterType

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

  private def admit(
      normalForm: TypeNormalForm
  ): Either[ExistingUntpdTwoParameterMethodParameterTypeRewriteError, TypeNormalForm] =
    normalForm match
      case value @ TypeNormalForm.STypeIdent("Int" | "String" | "Boolean") => Right(value)
      case value: TypeNormalForm.STypeIdent =>
        Left(
          error(
            "PARAMETER_TYPE_IDENTIFIER_UNSUPPORTED",
            s"U036 does not admit primitive identifier ${Option(value.name).getOrElse("null")}."
          )
        )
      case other =>
        Left(
          error(
            "PARAMETER_TYPE_FAMILY_UNSUPPORTED",
            s"U036 admits no non-primitive TypeNormalForm family; found ${other.getClass.getSimpleName}."
          )
        )

  private def primitiveName(normalForm: TypeNormalForm): String = normalForm match
    case TypeNormalForm.STypeIdent(name) => name
    case _ => ""

  private def validateLowered(
      lowered: untpd.Tree,
      expectedName: String
  )(using Context): Either[ExistingUntpdTwoParameterMethodParameterTypeRewriteError, Unit] =
    Option(lowered) match
      case Some(tree) if tree.source == null =>
        Left(
          error(
            "LOWERED_PARAMETER_TYPE_PROVENANCE",
            "the authoritative lowered primitive type had a null source field."
          )
        )
      case Some(tree) if tree.source.exists || tree.span.exists =>
        Left(
          error(
            "LOWERED_PARAMETER_TYPE_PROVENANCE",
            "the authoritative lowered primitive type was not source/span-free."
          )
        )
      case Some(tree)
          if tree.symbol != NoSymbol || tree.isInstanceOf[untpd.TypedSplice] =>
        Left(
          error(
            "LOWERED_PARAMETER_TYPE_PRE_TYPER_REQUIRED",
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
            "LOWERED_PARAMETER_TYPE_TOPOLOGY",
            "the authoritative lowerer did not return the exact expected primitive Ident leaf."
          )
        )

  private def validateView(
      view: ExistingUntpdTwoParameterMethodView.View
  )(using Context): Either[ExistingUntpdTwoParameterMethodParameterTypeRewriteError, Unit] =
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
  ): Either[ExistingUntpdTwoParameterMethodParameterTypeRewriteError, Unit] =
    Either.cond(
      Option(tree.source).exists(_.exists) && tree.span.exists,
      (),
      error(code, s"$label must provide both source and span.")
    )

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdTwoParameterMethodParameterTypeRewriteError =
    ExistingUntpdTwoParameterMethodParameterTypeRewriteError(code, detail)
