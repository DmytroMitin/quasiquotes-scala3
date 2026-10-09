package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.types.TypeNormalForm
import quasiquotes.types.dotty.TypeUntypedLowering

/** Preparation-only Type fragments for a later same-descriptor reconstruction. */
private[quasiquotes] object ExistingUntpdOrdinaryMethodTypeEditPreparation:
  import ExistingUntpdOrdinaryMethodDescriptor.{Descriptor, Parameter}

  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  private[dotty] final class ParameterTypeIdentity private[ExistingUntpdOrdinaryMethodTypeEditPreparation] (
      val outer: untpd.Tree,
      val byNameInner: Option[untpd.Tree],
      val repeatedElement: Option[untpd.Tree],
      val repeatedMarker: Option[untpd.Tree]
  )

  private[dotty] final case class ParameterEvidence(
      descriptor: Descriptor,
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int,
      method: untpd.DefDef,
      parameter: Parameter,
      oldParameter: untpd.ValDef,
      oldParameterType: untpd.Tree,
      semanticReplacement: TypeNormalForm,
      loweredType: untpd.Tree,
      positionedType: untpd.Tree,
      positionedTypeIdentity: ParameterTypeIdentity,
      positionedParameter: untpd.ValDef
  )

  final case class PreparedParameterType private[dotty] (
      descriptor: Descriptor,
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int,
      method: untpd.DefDef,
      parameter: Parameter,
      oldParameter: untpd.ValDef,
      oldParameterType: untpd.Tree,
      semanticReplacement: TypeNormalForm,
      loweredType: untpd.Tree,
      positionedType: untpd.Tree,
      positionedParameter: untpd.ValDef,
      private[dotty] evidence: ParameterEvidence
  )

  private[dotty] final case class ResultEvidence(
      descriptor: Descriptor,
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int,
      method: untpd.DefDef,
      oldResultType: untpd.Tree,
      semanticReplacement: TypeNormalForm,
      loweredType: untpd.Tree,
      positionedType: untpd.Tree
  )

  final case class PreparedResultType private[dotty] (
      descriptor: Descriptor,
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int,
      method: untpd.DefDef,
      oldResultType: untpd.Tree,
      semanticReplacement: TypeNormalForm,
      loweredType: untpd.Tree,
      positionedType: untpd.Tree,
      private[dotty] evidence: ResultEvidence
  )

  def prepareParameterType(
      descriptor: Descriptor,
      parameter: Parameter,
      replacement: TypeNormalForm
  )(using Context): Either[Error, PreparedParameterType] =
    for
      validDescriptor <- validateDescriptor(descriptor)
      selected <- selectParameter(validDescriptor, parameter)
      oldParameter <- Option(selected.tree)
        .filterNot(_.isEmpty)
        .toRight(error("OLD_PARAMETER_SITE_REQUIRED", "the selected old parameter was null or EmptyTree."))
      oldParameterType <- Option(selected.tpt)
        .filterNot(_.isEmpty)
        .toRight(error("OLD_PARAMETER_TYPE_SITE_REQUIRED", "the selected old parameter Type was null or EmptyTree."))
      _ <- validateOriginalParameterType(validDescriptor, selected, oldParameterType)
      _ <- requireSite(oldParameter, "OLD_PARAMETER_SITE_REQUIRED", "the selected old parameter")
      _ <- requireSite(oldParameterType, "OLD_PARAMETER_TYPE_SITE_REQUIRED", "the selected old parameter Type")
      admitted <- admit(replacement)
      lowered <- lower(admitted)
      _ <- validateLowered(lowered, primitiveName(admitted))
      positioned <- positionParameter(oldParameter, oldParameterType, lowered)
      _ <- validatePositionedParameterType(positioned._1, lowered, oldParameterType, primitiveName(admitted))
        .left.map(problem => error("ORIGIN_POSITIONING_FAILED", problem.detail))
      _ <- validatePositionedParameter(positioned._2, positioned._1, oldParameter)
        .left.map(problem => error("ORIGIN_POSITIONING_FAILED", problem.detail))
      evidence = ParameterEvidence(
        validDescriptor,
        validDescriptor.captured,
        validDescriptor.memberIndex,
        validDescriptor.method,
        selected,
        oldParameter,
        oldParameterType,
        admitted,
        lowered,
        positioned._1,
        parameterTypeIdentity(positioned._1),
        positioned._2
      )
      prepared = PreparedParameterType(
        validDescriptor,
        validDescriptor.captured,
        validDescriptor.memberIndex,
        validDescriptor.method,
        selected,
        oldParameter,
        oldParameterType,
        admitted,
        lowered,
        positioned._1,
        positioned._2,
        evidence
      )
      _ <- validatePreparedParameterType(prepared, validDescriptor)
    yield prepared

  def prepareResultType(
      descriptor: Descriptor,
      replacement: TypeNormalForm
  )(using Context): Either[Error, PreparedResultType] =
    for
      validDescriptor <- validateDescriptor(descriptor)
      oldResultType <- Option(validDescriptor.resultType)
        .filterNot(_.isEmpty)
        .toRight(error("OLD_RESULT_TYPE_SITE_REQUIRED", "the old result Type was null or EmptyTree."))
      _ <- requireSite(oldResultType, "OLD_RESULT_TYPE_SITE_REQUIRED", "the old result Type")
      admitted <- admit(replacement)
      lowered <- lower(admitted)
      _ <- validateLowered(lowered, primitiveName(admitted))
      positioned = positionType(lowered, oldResultType)
      _ <- validatePositionedType(positioned, lowered, oldResultType, primitiveName(admitted))
        .left.map(problem => error("ORIGIN_POSITIONING_FAILED", problem.detail))
      evidence = ResultEvidence(
        validDescriptor,
        validDescriptor.captured,
        validDescriptor.memberIndex,
        validDescriptor.method,
        oldResultType,
        admitted,
        lowered,
        positioned
      )
      prepared = PreparedResultType(
        validDescriptor,
        validDescriptor.captured,
        validDescriptor.memberIndex,
        validDescriptor.method,
        oldResultType,
        admitted,
        lowered,
        positioned,
        evidence
      )
      _ <- validatePreparedResultType(prepared, validDescriptor)
    yield prepared

  private[dotty] def validatePreparedParameterType(
      prepared: PreparedParameterType,
      expectedDescriptor: Descriptor
  )(using Context): Either[Error, Unit] =
    val failure = error(
      "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED",
      "the prepared parameter Type no longer matches its exact descriptor wrapper, selected parameter, original sites, semantic value, or fresh fragment evidence."
    )
    (for
      value <- Option(prepared).toRight(failure)
      expected <- Option(expectedDescriptor).toRight(failure)
      evidence <- Option(value.evidence).toRight(failure)
      _ <- validateDescriptor(expected).left.map(_ => failure)
      _ <- validateDescriptor(value.descriptor).left.map(_ => failure)
      _ <- Either.cond(value.descriptor.eq(expected), (), failure)
      _ <- Either.cond(parameterEvidenceMatches(value, evidence), (), failure)
      _ <- Either.cond(
        value.captured.eq(expected.captured) &&
          value.memberIndex == expected.memberIndex &&
          value.method.eq(expected.method),
        (),
        failure
      )
      _ <- Either.cond(exactParameterOccurrences(expected, value.parameter) == 1, (), failure)
      _ <- Either.cond(
        value.oldParameter.eq(value.parameter.tree) &&
          value.oldParameterType.eq(value.parameter.tpt) &&
          value.oldParameter.tpt.eq(value.oldParameterType),
        (),
        failure
      )
      _ <- validateOriginalParameterType(
        expected,
        value.parameter,
        value.oldParameterType
      ).left.map(_ => failure)
      _ <- requireSite(value.oldParameter, failure.code, "the retained old parameter").left.map(_ => failure)
      _ <- requireSite(value.oldParameterType, failure.code, "the retained old parameter Type").left.map(_ => failure)
      admitted <- admit(value.semanticReplacement).left.map(_ => failure)
      _ <- validateLowered(value.loweredType, primitiveName(admitted)).left.map(_ => failure)
      _ <- validatePositionedParameterType(
        value.positionedType,
        value.loweredType,
        value.oldParameterType,
        primitiveName(admitted)
      ).left.map(_ => failure)
      _ <- validatePositionedParameter(
        value.positionedParameter,
        value.positionedType,
        value.oldParameter
      ).left.map(_ => failure)
    yield ()).left.map(_ => failure)

  private[dotty] def validatePreparedResultType(
      prepared: PreparedResultType,
      expectedDescriptor: Descriptor
  )(using Context): Either[Error, Unit] =
    val failure = error(
      "FINAL_RESULT_PREPARATION_INVARIANT_FAILED",
      "the prepared result Type no longer matches its exact descriptor wrapper, original site, semantic value, or fresh fragment evidence."
    )
    (for
      value <- Option(prepared).toRight(failure)
      expected <- Option(expectedDescriptor).toRight(failure)
      evidence <- Option(value.evidence).toRight(failure)
      _ <- validateDescriptor(expected).left.map(_ => failure)
      _ <- validateDescriptor(value.descriptor).left.map(_ => failure)
      _ <- Either.cond(value.descriptor.eq(expected), (), failure)
      _ <- Either.cond(resultEvidenceMatches(value, evidence), (), failure)
      _ <- Either.cond(
        value.captured.eq(expected.captured) &&
          value.memberIndex == expected.memberIndex &&
          value.method.eq(expected.method) &&
          value.oldResultType.eq(expected.resultType),
        (),
        failure
      )
      _ <- requireSite(value.oldResultType, failure.code, "the retained old result Type").left.map(_ => failure)
      admitted <- admit(value.semanticReplacement).left.map(_ => failure)
      _ <- validateLowered(value.loweredType, primitiveName(admitted)).left.map(_ => failure)
      _ <- validatePositionedType(
        value.positionedType,
        value.loweredType,
        value.oldResultType,
        primitiveName(admitted)
      ).left.map(_ => failure)
    yield ()).left.map(_ => failure)

  private def validateDescriptor(
      descriptor: Descriptor
  )(using Context): Either[Error, Descriptor] =
    Option(descriptor)
      .toRight(error("INVALID_DESCRIPTOR", "the U037 descriptor was null."))
      .flatMap { present =>
        ExistingUntpdOrdinaryMethodDescriptor
          .validate(present)
          .left.map(problem =>
            error("INVALID_DESCRIPTOR", s"U037 descriptor validation failed before preparation: ${problem.message}")
          )
          .map(_ => present)
      }

  private def selectParameter(
      descriptor: Descriptor,
      parameter: Parameter
  ): Either[Error, Parameter] =
    Option(parameter)
      .toRight(error("PARAMETER_REQUIRED", "the capture-local U037 parameter wrapper was null."))
      .flatMap { selected =>
        exactParameterOccurrences(descriptor, selected) match
          case 1 => Right(selected)
          case 0 => Left(error("PARAMETER_NOT_CAPTURED", "the supplied parameter wrapper does not belong to the exact descriptor wrapper."))
          case count => Left(error("PARAMETER_OCCURRENCE_NOT_UNIQUE", s"the supplied parameter wrapper occurred $count times; exactly one occurrence is required."))
      }

  private def exactParameterOccurrences(
      descriptor: Descriptor,
      parameter: Parameter
  ): Int =
    Option(descriptor.parameterClauses).iterator
      .flatMap(_.iterator)
      .flatMap(clause => Option(clause).iterator.flatMap(_.iterator))
      .count(candidate => candidate != null && candidate.eq(parameter))

  private def admit(replacement: TypeNormalForm): Either[Error, TypeNormalForm] =
    Option(replacement)
      .toRight(error("SEMANTIC_TYPE_REQUIRED", "the semantic replacement Type was null."))
      .flatMap {
        case admitted @ TypeNormalForm.STypeIdent("Int" | "String" | "Boolean") => Right(admitted)
        case identifier: TypeNormalForm.STypeIdent =>
          Left(error(
            "SEMANTIC_TYPE_IDENTIFIER_UNSUPPORTED",
            s"U040 does not admit primitive identifier ${Option(identifier.name).getOrElse("null")}."
          ))
        case other =>
          Left(error(
            "SEMANTIC_TYPE_FAMILY_UNSUPPORTED",
            s"U040 admits no non-primitive TypeNormalForm family; found ${other.getClass.getSimpleName}."
          ))
      }

  private def validateOriginalParameterType(
      descriptor: Descriptor,
      parameter: Parameter,
      oldParameterType: untpd.Tree
  )(using Context): Either[Error, Unit] =
    oldParameterType match
      case wrapper: untpd.ByNameTypeTree =>
        for
          inner <- Option(wrapper.result)
            .filterNot(_.isEmpty)
            .toRight(error(
              "OLD_PARAMETER_TYPE_SITE_REQUIRED",
              "the selected old by-name parameter had no inner Type site."
            ))
          _ <- requireSite(
            inner,
            "OLD_PARAMETER_TYPE_SITE_REQUIRED",
            "the selected old by-name parameter inner Type"
          )
        yield ()
      case wrapper: untpd.PostfixOp =>
        wrapper match
          case untpd.PostfixOp(element, marker) =>
            val repeatedParameters =
              descriptor.parameterClauses.flatten.filter(candidate =>
                candidate != null && candidate.tpt.isInstanceOf[untpd.PostfixOp]
              )
            for
              presentElement <- Option(element)
                .filterNot(_.isEmpty)
                .toRight(error(
                  "REPEATED_PARAMETER_TYPE_INVALID",
                  "the selected repeated parameter had no element Type site."
                ))
              presentMarker <- Option(marker)
                .filterNot(_.isEmpty)
                .toRight(error(
                  "REPEATED_PARAMETER_TYPE_INVALID",
                  "the selected repeated parameter had no star marker."
                ))
              _ <- requireSite(
                presentElement,
                "REPEATED_PARAMETER_TYPE_INVALID",
                "the selected repeated parameter element Type"
              )
              _ <- requireSite(
                presentMarker,
                "REPEATED_PARAMETER_TYPE_INVALID",
                "the selected repeated parameter star marker"
              )
              _ <- validateRepeatedMarker(presentMarker)
              _ <- Either.cond(
                repeatedParameters.size == 1 &&
                  repeatedParameters.head.eq(parameter) &&
                  descriptor.parameterClauses.lastOption
                    .flatMap(_.lastOption)
                    .exists(_.eq(parameter)),
                (),
                error(
                  "REPEATED_PARAMETER_POSITION_INVALID",
                  "the exact repeated parameter must be the sole PostfixOp parameter and remain last in its ordinary clause."
                )
              )
            yield ()
      case _ => Right(())

  private def validateRepeatedMarker(marker: untpd.Tree)(using Context): Either[Error, Unit] =
    val valid = Option(marker).exists {
      case ident: untpd.Ident =>
        Option(ident.name).exists(_.toString == "*") &&
          ident.symbol == NoSymbol &&
          safeAllTrees(ident).exists(_.size == 1)
      case _ => false
    }
    Either.cond(
      valid,
      (),
      error(
        "REPEATED_PARAMETER_TYPE_INVALID",
        "the selected PostfixOp operator was not the exact clean repeated star Ident marker."
      )
    )

  private def lower(replacement: TypeNormalForm): Either[Error, untpd.Tree] =
    TypeUntypedLowering
      .lower(replacement)
      .left.map(problem => error("TYPE_LOWERING_FAILED", problem.message))

  private def positionType(
      lowered: untpd.Tree,
      oldSite: untpd.Tree
  ): untpd.Tree =
    lowered.cloneIn(oldSite.source).withSpan(oldSite.span)

  private def positionParameter(
      oldParameter: untpd.ValDef,
      oldParameterType: untpd.Tree,
      lowered: untpd.Tree
  )(using Context): Either[Error, (untpd.Tree, untpd.ValDef)] =
    for
      positionedType <- positionParameterType(oldParameterType, lowered)
    yield
      given SourceFile = NoSource
      val sourceFreeParameter = untpd
        .ValDef(oldParameter.name, lowered, untpd.EmptyTree)
        .withMods(oldParameter.mods)
      val positionedParameter = untpd.cpy
        .ValDef(sourceFreeParameter)(
          sourceFreeParameter.name,
          positionedType,
          sourceFreeParameter.rhs
        )
        .cloneIn(oldParameter.source)
        .withSpan(oldParameter.span)
      (positionedType, positionedParameter)

  private def positionParameterType(
      oldParameterType: untpd.Tree,
      lowered: untpd.Tree
  )(using Context): Either[Error, untpd.Tree] =
    oldParameterType match
      case oldWrapper: untpd.ByNameTypeTree =>
        for
          oldInner <- Option(oldWrapper.result)
            .filterNot(_.isEmpty)
            .toRight(error(
              "OLD_PARAMETER_TYPE_SITE_REQUIRED",
              "the selected old by-name parameter had no inner Type site."
            ))
          _ <- requireSite(
            oldInner,
            "OLD_PARAMETER_TYPE_SITE_REQUIRED",
            "the selected old by-name parameter inner Type"
          )
        yield
          given SourceFile = NoSource
          val positionedInner = positionType(lowered, oldInner)
          untpd.ByNameTypeTree(positionedInner)
            .cloneIn(oldWrapper.source)
            .withSpan(oldWrapper.span)
      case oldWrapper: untpd.PostfixOp =>
        oldWrapper match
          case untpd.PostfixOp(oldElement, oldMarker) =>
            for
              element <- Option(oldElement)
                .filterNot(_.isEmpty)
                .toRight(error(
                  "REPEATED_PARAMETER_TYPE_INVALID",
                  "the selected repeated parameter had no element Type site."
                ))
              marker <- Option(oldMarker)
                .filterNot(_.isEmpty)
                .toRight(error(
                  "REPEATED_PARAMETER_TYPE_INVALID",
                  "the selected repeated parameter had no star marker."
                ))
              _ <- validateRepeatedMarker(marker)
            yield
              given SourceFile = NoSource
              val positionedElement = positionType(lowered, element)
              untpd.PostfixOp(positionedElement, marker)
                .cloneIn(oldWrapper.source)
                .withSpan(oldWrapper.span)
      case _ =>
        Right(positionType(lowered, oldParameterType))

  private def validateLowered(
      lowered: untpd.Tree,
      expectedName: String
  )(using Context): Either[Error, Unit] =
    Option(lowered) match
      case None => Left(error("LOWERED_TYPE_INVALID", "the delegated Type lowerer returned null."))
      case Some(tree) if tree.source == null =>
        Left(error("LOWERED_TYPE_INVALID", "the delegated Type lowerer returned a null source handle."))
      case Some(tree) if tree.source.exists || tree.span.exists =>
        Left(error("LOWERED_TYPE_INVALID", "the delegated Type lowerer did not return a source/span-free tree."))
      case Some(tree) if tree.symbol != NoSymbol || tree.isInstanceOf[untpd.TypedSplice] =>
        Left(error("LOWERED_TYPE_CONTAMINATED", "the delegated Type lowerer returned a symbol-bearing or TypedSplice tree."))
      case Some(ident: untpd.Ident)
          if Option(ident.name).exists(_.toString == expectedName) &&
            safeAllTrees(ident).exists(_.size == 1) =>
        Right(())
      case Some(_) =>
        Left(error("LOWERED_TYPE_INVALID", "the delegated Type lowerer did not return the exact admitted primitive Ident leaf."))

  private def validatePositionedType(
      positioned: untpd.Tree,
      lowered: untpd.Tree,
      oldSite: untpd.Tree,
      expectedName: String
  )(using Context): Either[Error, Unit] =
    val valid = Option(positioned).exists {
      case ident: untpd.Ident =>
        Option(ident.name).exists(_.toString == expectedName) &&
          safeAllTrees(ident).exists(_.size == 1) &&
          !ident.eq(lowered) &&
          !ident.eq(oldSite) &&
          ident.source == oldSite.source &&
          ident.span == oldSite.span &&
          ident.symbol == NoSymbol
      case _ => false
    }
    Either.cond(
      valid,
      (),
      error("ORIGIN_POSITIONING_FAILED", "the fresh positioned Type did not exactly occupy the old transformation site.")
    )

  private def validatePositionedParameterType(
      positioned: untpd.Tree,
      lowered: untpd.Tree,
      oldSite: untpd.Tree,
      expectedName: String
  )(using Context): Either[Error, Unit] =
    oldSite match
      case oldWrapper: untpd.ByNameTypeTree =>
        (positioned, Option(oldWrapper.result)) match
          case (wrapper: untpd.ByNameTypeTree, Some(oldInner))
              if !oldInner.isEmpty =>
            val graph = safeAllTrees(wrapper)
            val outerValid =
              !wrapper.eq(lowered) &&
                !wrapper.eq(oldWrapper) &&
                wrapper.result != null &&
                !wrapper.result.isEmpty &&
                wrapper.source == oldWrapper.source &&
                wrapper.span == oldWrapper.span &&
                wrapper.symbol == NoSymbol &&
                graph.exists(nodes =>
                  nodes.size == 2 && nodes.forall(node =>
                    node != null &&
                      node.symbol == NoSymbol &&
                      !node.isInstanceOf[untpd.TypedSplice]
                  )
                )
            for
              _ <- requireSite(
                oldInner,
                "ORIGIN_POSITIONING_FAILED",
                "the retained old by-name parameter inner Type"
              )
              _ <- Either.cond(
                outerValid,
                (),
                error(
                  "ORIGIN_POSITIONING_FAILED",
                  "the fresh by-name wrapper drifted from the old outer site or clean two-node topology."
                )
              )
              _ <- validatePositionedType(
                wrapper.result,
                lowered,
                oldInner,
                expectedName
              )
            yield ()
          case _ =>
            Left(error(
              "ORIGIN_POSITIONING_FAILED",
              "the positioned by-name parameter Type did not retain a nonempty ByNameTypeTree wrapper."
            ))
      case oldWrapper: untpd.PostfixOp =>
        (positioned, oldWrapper) match
          case (
                wrapper: untpd.PostfixOp,
                untpd.PostfixOp(oldElement, oldMarker)
              ) =>
            wrapper match
              case untpd.PostfixOp(positionedElement, positionedMarker) =>
                val graph = safeAllTrees(wrapper)
                val outerValid =
                  !wrapper.eq(lowered) &&
                    !wrapper.eq(oldWrapper) &&
                    positionedElement != null &&
                    !positionedElement.isEmpty &&
                    positionedMarker != null &&
                    !positionedMarker.isEmpty &&
                    positionedMarker.eq(oldMarker) &&
                    wrapper.source == oldWrapper.source &&
                    wrapper.span == oldWrapper.span &&
                    wrapper.symbol == NoSymbol &&
                    graph.exists(nodes =>
                      nodes.size == 3 && nodes.forall(node =>
                        node != null &&
                          node.symbol == NoSymbol &&
                          !node.isInstanceOf[untpd.TypedSplice]
                      )
                    )
                for
                  _ <- requireSite(
                    oldElement,
                    "ORIGIN_POSITIONING_FAILED",
                    "the retained old repeated parameter element Type"
                  )
                  _ <- requireSite(
                    oldMarker,
                    "ORIGIN_POSITIONING_FAILED",
                    "the retained old repeated parameter star marker"
                  )
                  _ <- validateRepeatedMarker(oldMarker)
                    .left.map(problem => error("ORIGIN_POSITIONING_FAILED", problem.detail))
                  _ <- Either.cond(
                    outerValid,
                    (),
                    error(
                      "ORIGIN_POSITIONING_FAILED",
                      "the fresh repeated wrapper drifted from the old outer site, exact marker identity, or clean three-node topology."
                    )
                  )
                  _ <- validatePositionedType(
                    positionedElement,
                    lowered,
                    oldElement,
                    expectedName
                  )
                yield ()
          case _ =>
            Left(error(
              "ORIGIN_POSITIONING_FAILED",
              "the positioned repeated parameter Type did not retain an exact star PostfixOp wrapper."
            ))
      case _ =>
        validatePositionedType(positioned, lowered, oldSite, expectedName)

  private def validatePositionedParameter(
      positioned: untpd.ValDef,
      positionedType: untpd.Tree,
      oldParameter: untpd.ValDef
  )(using Context): Either[Error, Unit] =
    val graph = safeAllTrees(positioned)
    val valid = Option(positioned).exists { parameter =>
      !parameter.eq(oldParameter) &&
        parameter.tpt.eq(positionedType) &&
        parameter.rhs.isEmpty &&
        parameter.name == oldParameter.name &&
        parameter.mods.eq(oldParameter.mods) &&
        parameter.source == oldParameter.source &&
        parameter.span == oldParameter.span &&
        parameter.symbol == NoSymbol
    } && graph.exists(_.forall(tree =>
      tree != null && tree.symbol == NoSymbol &&
        !tree.isInstanceOf[untpd.TypedSplice] &&
        !tree.isInstanceOf[untpd.DefDef] &&
        !tree.isInstanceOf[untpd.Template] &&
        !tree.isInstanceOf[untpd.TypeDef]
    ))
    Either.cond(
      valid,
      (),
      error("ORIGIN_POSITIONING_FAILED", "the fresh parameter shell drifted from the old parameter site, name, modifiers, empty RHS, or pre-Typer contract.")
    )

  private def parameterTypeIdentity(
      positioned: untpd.Tree
  ): ParameterTypeIdentity =
    val (byNameInner, repeatedElement, repeatedMarker) = positioned match
      case wrapper: untpd.ByNameTypeTree =>
        (Option(wrapper.result), None, None)
      case untpd.PostfixOp(element, marker) =>
        (None, Option(element), Option(marker))
      case _ =>
        (None, None, None)
    new ParameterTypeIdentity(positioned, byNameInner, repeatedElement, repeatedMarker)

  private def parameterTypeIdentityMatches(
      positioned: untpd.Tree,
      identity: ParameterTypeIdentity
  ): Boolean =
    Option(positioned).exists { present =>
      Option(identity).exists { snapshot =>
        val shapeMatches =
          (Option(snapshot.byNameInner),
            Option(snapshot.repeatedElement),
            Option(snapshot.repeatedMarker)) match
            case (Some(Some(inner)), Some(None), Some(None)) =>
              present match
                case wrapper: untpd.ByNameTypeTree =>
                  Option(wrapper.result).exists(_.eq(inner))
                case _ => false
            case (Some(None), Some(Some(element)), Some(Some(marker))) =>
              present match
                case untpd.PostfixOp(actualElement, actualMarker) =>
                  Option(actualElement).exists(_.eq(element)) &&
                    Option(actualMarker).exists(_.eq(marker))
                case _ => false
            case (Some(None), Some(None), Some(None)) =>
              !present.isInstanceOf[untpd.ByNameTypeTree] &&
                !present.isInstanceOf[untpd.PostfixOp]
            case _ => false
        present.eq(snapshot.outer) && shapeMatches
      }
    }

  private def parameterEvidenceMatches(
      value: PreparedParameterType,
      evidence: ParameterEvidence
  ): Boolean =
    value.descriptor.eq(evidence.descriptor) &&
      value.captured.eq(evidence.captured) &&
      value.memberIndex == evidence.memberIndex &&
      value.method.eq(evidence.method) &&
      value.parameter.eq(evidence.parameter) &&
      value.oldParameter.eq(evidence.oldParameter) &&
      value.oldParameterType.eq(evidence.oldParameterType) &&
      value.semanticReplacement.eq(evidence.semanticReplacement) &&
      value.loweredType.eq(evidence.loweredType) &&
      Option(value.positionedType).exists(_.eq(evidence.positionedType)) &&
      parameterTypeIdentityMatches(value.positionedType, evidence.positionedTypeIdentity) &&
      value.positionedParameter.eq(evidence.positionedParameter)

  private def resultEvidenceMatches(
      value: PreparedResultType,
      evidence: ResultEvidence
  ): Boolean =
    value.descriptor.eq(evidence.descriptor) &&
      value.captured.eq(evidence.captured) &&
      value.memberIndex == evidence.memberIndex &&
      value.method.eq(evidence.method) &&
      value.oldResultType.eq(evidence.oldResultType) &&
      value.semanticReplacement.eq(evidence.semanticReplacement) &&
      value.loweredType.eq(evidence.loweredType) &&
      value.positionedType.eq(evidence.positionedType)

  private def primitiveName(replacement: TypeNormalForm): String = replacement match
    case TypeNormalForm.STypeIdent(name) => name
    case _ => ""

  private def requireSite(
      tree: untpd.Tree,
      code: String,
      label: String
  ): Either[Error, Unit] =
    Either.cond(
      Option(tree).exists(value =>
        Option(value.source).exists(_.exists) && value.span.exists
      ),
      (),
      error(code, s"$label must provide both source and span.")
    )

  private def safeAllTrees(tree: untpd.Tree)(using Context): Option[Vector[untpd.Tree]] =
    try Option(tree).map(ExistingUntpdClassMemberFilter.allTrees)
    catch case _: NullPointerException => None

  private def error(code: String, detail: String): Error =
    Error(code, detail)
