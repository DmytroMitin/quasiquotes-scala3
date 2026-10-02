package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import scala.util.control.NonFatal

/** One fresh method shell assembled only from accepted same-descriptor field preparations. */
private[quasiquotes] object ExistingUntpdOrdinaryMethodReconstruction:
  import ExistingUntpdOrdinaryMethodDescriptor.{Descriptor, Parameter}
  import ExistingUntpdOrdinaryMethodRhsEditPreparation.PreparedBody
  import ExistingUntpdOrdinaryMethodTypeEditPreparation.{PreparedParameterType, PreparedResultType}

  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  final case class ReconstructedMethod private[dotty] (
      descriptor: Descriptor,
      parameterTypes: Vector[PreparedParameterType],
      editedParameters: Vector[Parameter],
      resultType: Option[PreparedResultType],
      body: Option[PreparedBody],
      positionedMethod: untpd.DefDef
  )

  def reconstructMethod(
      descriptor: Descriptor,
      parameterTypes: Vector[PreparedParameterType],
      resultType: Option[PreparedResultType],
      body: Option[PreparedBody]
  )(using Context): Either[Error, ReconstructedMethod] =
    for
      validDescriptor <- validateDescriptor(descriptor)
      parameters <- Option(parameterTypes).toRight(
        error("PARAMETER_PREPARATIONS_REQUIRED", "the parameter preparation collection was null.")
      )
      result <- Option(resultType).toRight(
        error("RESULT_PREPARATION_OPTION_REQUIRED", "the optional result preparation carrier was null.")
      )
      rhs <- Option(body).toRight(
        error("BODY_PREPARATION_OPTION_REQUIRED", "the optional body preparation carrier was null.")
      )
      _ <- Either.cond(
        parameters.nonEmpty || result.nonEmpty || rhs.nonEmpty,
        (),
        error("EMPTY_EDIT_SET", "at least one accepted prepared field edit is required.")
      )
      checkedParameters <- validateParameterPreparations(parameters, validDescriptor)
      _ <- validateResultPreparation(result, validDescriptor)
      _ <- validateBodyPreparation(rhs, validDescriptor)
      oldMethod <- Option(validDescriptor.method).filterNot(_.isEmpty).toRight(
        error("MALFORMED_ORIGINAL_METHOD_SITE", "the original method was null or EmptyTree.")
      )
      _ <- Either.cond(
        Option(oldMethod.source).exists(_.exists) && oldMethod.span.exists,
        (),
        error("MALFORMED_ORIGINAL_METHOD_SITE", "the original method must provide source and span.")
      )
      positioned <- constructMethod(validDescriptor, checkedParameters, result, rhs)
      edited = validDescriptor.parameterClauses.flatten.filter(parameter =>
        checkedParameters.exists(_.parameter.eq(parameter))
      )
      reconstructed = ReconstructedMethod(
        validDescriptor,
        checkedParameters,
        edited,
        result,
        rhs,
        positioned
      )
      _ <- validateReconstructedMethod(reconstructed, validDescriptor)
    yield reconstructed

  private[dotty] def validateReconstructedMethod(
      reconstructed: ReconstructedMethod,
      expectedDescriptor: Descriptor
  )(using Context): Either[Error, Unit] =
    val failure = error(
      "FINAL_METHOD_INVARIANT_FAILED",
      "the reconstructed method no longer matches its exact descriptor, accepted prepared fields, selected child identities, or original method-site shell."
    )
    for
      value <- Option(reconstructed).toRight(failure)
      expected <- Option(expectedDescriptor).toRight(failure)
      _ <- validateDescriptor(expected).left.map(_ => failure)
      _ <- Either.cond(value.descriptor.eq(expected), (), failure)
      parameters <- Option(value.parameterTypes).toRight(failure)
      edited <- Option(value.editedParameters).toRight(failure)
      result <- Option(value.resultType).toRight(failure)
      rhs <- Option(value.body).toRight(failure)
      _ <- Either.cond(
        parameters.nonEmpty || result.nonEmpty || rhs.nonEmpty,
        (),
        failure
      )
      checkedParameters <- validateParameterPreparations(parameters, expected).left.map(_ => failure)
      _ <- validateResultPreparation(result, expected).left.map(_ => failure)
      _ <- validateBodyPreparation(rhs, expected).left.map(_ => failure)
      expectedEdited = expected.parameterClauses.flatten.filter(parameter =>
        checkedParameters.exists(_.parameter.eq(parameter))
      )
      _ <- Either.cond(
        edited.size == expectedEdited.size &&
          edited.zip(expectedEdited).forall((actual, original) => actual.eq(original)),
        (),
        failure
      )
      method <- Option(value.positionedMethod).filterNot(_.isEmpty).toRight(failure)
      selectedClauses = selectParameterClauses(expected, checkedParameters)
      selectedResult = result.fold(expected.resultType)(_.positionedType)
      selectedBody = rhs.fold(expected.rhs)(_.positionedReplacement)
      _ <- validateMethodShell(method, expected, selectedClauses, selectedResult, selectedBody, failure)
    yield ()

  private def validateMethodShell(
      method: untpd.DefDef,
      expected: Descriptor,
      selectedClauses: List[List[untpd.ValDef]],
      selectedResult: untpd.Tree,
      selectedBody: untpd.Tree,
      failure: Error
  )(using Context): Either[Error, Unit] =
    try
      val actualClauses = method.paramss
      Either.cond(
        actualClauses != null &&
          !method.eq(expected.method) &&
          method.name == expected.method.name &&
          method.mods != null &&
          expected.method.mods != null &&
          method.mods.eq(expected.method.mods) &&
          method.source == expected.method.source &&
          method.span == expected.method.span &&
          method.symbol == NoSymbol &&
          clausesMatchByIdentity(actualClauses, selectedClauses) &&
          method.tpt != null &&
          selectedResult != null &&
          method.tpt.eq(selectedResult) &&
          method.rhs != null &&
          selectedBody != null &&
          method.rhs.eq(selectedBody) &&
          ExistingUntpdClassMemberFilter.allTrees(method).forall(tree =>
            tree != null && tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
          ),
        (),
        failure
      )
    catch
      case NonFatal(_) => Left(failure)

  private def validateDescriptor(
      descriptor: Descriptor
  )(using Context): Either[Error, Descriptor] =
    Option(descriptor)
      .toRight(error("INVALID_DESCRIPTOR", "the U037 descriptor was null."))
      .flatMap { present =>
        ExistingUntpdOrdinaryMethodDescriptor.validate(present)
          .left.map(problem =>
            error("INVALID_DESCRIPTOR", s"U037 descriptor validation failed before method reconstruction: ${problem.message}")
          )
          .map(_ => present)
      }

  private def validateParameterPreparations(
      preparations: Vector[PreparedParameterType],
      descriptor: Descriptor
  )(using Context): Either[Error, Vector[PreparedParameterType]] =
    preparations.foldLeft[Either[Error, Vector[PreparedParameterType]]](Right(Vector.empty)) {
      case (collected, preparation) =>
        for
          accepted <- collected
          value <- Option(preparation).toRight(
            error("PARAMETER_PREPARATION_REQUIRED", "a parameter Type preparation was null.")
          )
          _ <- Either.cond(
            value.descriptor.eq(descriptor),
            (),
            error("FOREIGN_PARAMETER_PREPARATION", "the parameter preparation belongs to a different descriptor wrapper.")
          )
          _ <- ExistingUntpdOrdinaryMethodTypeEditPreparation
            .validatePreparedParameterType(value, descriptor)
            .left.map(problem => error("INVALID_PARAMETER_PREPARATION", problem.message))
          _ <- Either.cond(
            !accepted.exists(_.parameter.eq(value.parameter)),
            (),
            error("DUPLICATE_PARAMETER_EDIT", "the same exact parameter wrapper was edited more than once.")
          )
        yield accepted :+ value
    }

  private def validateResultPreparation(
      preparation: Option[PreparedResultType],
      descriptor: Descriptor
  )(using Context): Either[Error, Unit] =
    preparation match
      case None => Right(())
      case Some(null) =>
        Left(error("RESULT_PREPARATION_REQUIRED", "the result Type preparation was null."))
      case Some(value) =>
        for
          _ <- Either.cond(
            value.descriptor.eq(descriptor),
            (),
            error("FOREIGN_RESULT_PREPARATION", "the result Type preparation belongs to a different descriptor wrapper.")
          )
          _ <- ExistingUntpdOrdinaryMethodTypeEditPreparation
            .validatePreparedResultType(value, descriptor)
            .left.map(problem => error("INVALID_RESULT_PREPARATION", problem.message))
        yield ()

  private def validateBodyPreparation(
      preparation: Option[PreparedBody],
      descriptor: Descriptor
  )(using Context): Either[Error, Unit] =
    preparation match
      case None => Right(())
      case Some(null) =>
        Left(error("BODY_PREPARATION_REQUIRED", "the body preparation was null."))
      case Some(value) =>
        for
          _ <- Either.cond(
            value.descriptor.eq(descriptor),
            (),
            error("FOREIGN_BODY_PREPARATION", "the body preparation belongs to a different descriptor wrapper.")
          )
          _ <- ExistingUntpdOrdinaryMethodRhsEditPreparation
            .validatePreparedBody(value, descriptor)
            .left.map(problem => error("INVALID_BODY_PREPARATION", problem.message))
        yield ()

  private def constructMethod(
      descriptor: Descriptor,
      parameterTypes: Vector[PreparedParameterType],
      resultType: Option[PreparedResultType],
      body: Option[PreparedBody]
  )(using Context): Either[Error, untpd.DefDef] =
    val selectedParamss = selectParameterClauses(descriptor, parameterTypes)
    val selectedResult = resultType.fold(descriptor.resultType)(_.positionedType)
    val selectedBody = body.fold(descriptor.rhs)(_.positionedReplacement)
    given SourceFile = NoSource
    val sourceFreeMethod = untpd
      .DefDef(descriptor.method.name, selectedParamss, selectedResult, selectedBody)
      .withMods(descriptor.method.mods)
    val positionedMethod = untpd.cpy
      .DefDef(sourceFreeMethod)(
        sourceFreeMethod.name,
        sourceFreeMethod.paramss,
        sourceFreeMethod.tpt,
        sourceFreeMethod.rhs
      )
      .cloneIn(descriptor.method.source)
      .withSpan(descriptor.method.span)
    Either.cond(
      !positionedMethod.eq(descriptor.method),
      positionedMethod,
      error("METHOD_CONSTRUCTION_FAILED", "method construction did not allocate a fresh shell.")
    )

  private def selectParameterClauses(
      descriptor: Descriptor,
      preparations: Vector[PreparedParameterType]
  ): List[List[untpd.ValDef]] =
    descriptor.parameterClauses.map { clause =>
      clause.map { parameter =>
        preparations.find(_.parameter.eq(parameter))
          .fold(parameter.tree)(_.positionedParameter)
      }.toList
    }.toList

  private def clausesMatchByIdentity(
      actual: List[untpd.ParamClause],
      expected: List[List[untpd.ValDef]]
  ): Boolean =
    Option(actual).exists { clauses =>
      clauses.size == expected.size &&
        clauses.zip(expected).forall { (actualClause, expectedClause) =>
          Option(actualClause).exists { clause =>
            clause.size == expectedClause.size &&
              clause.zip(expectedClause).forall { (left, right) =>
                left != null && right != null && left.eq(right)
              }
          }
        }
    }

  private def error(code: String, detail: String): Error = Error(code, detail)
