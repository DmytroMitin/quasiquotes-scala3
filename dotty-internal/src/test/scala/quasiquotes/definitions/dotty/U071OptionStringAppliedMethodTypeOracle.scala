package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.types.TypeNormalForm
import quasiquotes.types.dotty.TypeUntypedLowering
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

/** Test-only oracle for the exact U071 Option[String] feasibility question. */
private[dotty] object U071OptionStringAppliedMethodTypeOracle:
  import ExistingClassUntypedRewrite.*

  final case class Error(code: String, detail: String):
    def message: String = s"$code: $detail"

  final case class PositionedType(
      lowered: untpd.AppliedTypeTree,
      positioned: untpd.AppliedTypeTree,
      oldSite: untpd.Tree
  )

  final case class Proof(
      reconstructed: ExistingUntpdClassMemberFilter.Reconstructed,
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor,
      oldMethod: untpd.DefDef,
      positionedMethod: untpd.DefDef,
      parameterType: Option[PositionedType],
      resultType: Option[PositionedType],
      bodyEdited: Boolean,
      untouchedIdentity: Boolean,
      orderPreserved: Boolean
  )

  val optionString: TypeNormalForm =
    TypeNormalForm.STypeApply(
      TypeNormalForm.STypeIdent("Option"),
      List(TypeNormalForm.STypeIdent("String"))
    )

  def apply(capture: Capture, plan: EditPlan)(using Context): Either[Error, Proof] =
    for
      publicCapture <- Option(capture).toRight(error("CAPTURE_REQUIRED", "the public Capture was null."))
      publicPlan <- Option(plan).toRight(error("PLAN_REQUIRED", "the public EditPlan was null."))
      _ <- Either.cond(
        publicPlan.captureIdentity.eq(publicCapture.exact),
        (),
        error("FOREIGN_PLAN", "the EditPlan belongs to a different public Capture.")
      )
      _ <- Either.cond(
        publicPlan.omissions.isEmpty && publicPlan.appends.isEmpty,
        (),
        error("UNSUPPORTED_PLAN", "U071 admits no omission or append intent.")
      )
      indices = (
        publicPlan.parameterTypes.map(_.parameter.methodIdentity.memberIndex) ++
          publicPlan.resultTypes.map(_.method.memberIndex) ++
          publicPlan.bodies.map(_.method.memberIndex)
      ).distinct
      _ <- Either.cond(
        indices.size == 1,
        (),
        error("EXACT_METHOD_REQUIRED", "U071 requires edits for exactly one captured method.")
      )
      memberIndex = indices.head
      descriptor <- ExistingUntpdOrdinaryMethodDescriptor
        .capture(publicCapture.exact, memberIndex)
        .left.map(problem => error("DESCRIPTOR_REJECTED", problem.message))
      _ <- validateDescriptorShape(descriptor)
      _ <- validateNoVisibleTypeShadow(publicCapture.exact, descriptor)
      parameterIntent <- atMostOne(publicPlan.parameterTypes, "parameter Type")
      resultIntent <- atMostOne(publicPlan.resultTypes, "result Type")
      bodyIntent <- atMostOne(publicPlan.bodies, "body")
      _ <- Either.cond(
        parameterIntent.nonEmpty || resultIntent.nonEmpty,
        (),
        error("TYPE_EDIT_REQUIRED", "U071 requires a parameter and/or result Type edit.")
      )
      _ <- validateParameterIntent(parameterIntent, descriptor)
      _ <- validateResultIntent(resultIntent, descriptor)
      _ <- validateBodyIntent(bodyIntent, descriptor)
      preparedParameter <- parameterIntent match
        case None => Right(None)
        case Some(_) =>
          positionOptionString(descriptor.parameterClauses.head.head.tpt)
            .flatMap(positioned => positionParameter(descriptor.parameterClauses.head.head.tree, positioned)
              .map(parameter => Some(positioned -> parameter)))
      preparedResult <- resultIntent match
        case None => Right(None)
        case Some(_) => positionOptionString(descriptor.resultType).map(Some(_))
      preparedBody <- bodyIntent match
        case None => Right(None)
        case Some(intent) =>
          ExistingUntpdOrdinaryMethodRhsEditPreparation
            .prepareBody(descriptor) { target =>
              intent.method.scopeIdentity.adaptForPreparation(
                intent.replacement,
                descriptor,
                target
              )
            }
            .left.map(problem => error("BODY_PREPARATION_REJECTED", problem.message))
            .map(Some(_))
      positionedMethod <- positionMethod(descriptor, preparedParameter.map(_._2), preparedResult, preparedBody)
      finalMembers = publicCapture.exact.originalTemplate.body.toVector.updated(memberIndex, positionedMethod)
      reconstructed <- ExistingUntpdClassMemberFilter
        .reconstruct(publicCapture.exact, finalMembers)
        .left.map(problem => error("OWNER_RECONSTRUCTION_REJECTED", problem.message))
      _ <- validateFinalGraph(publicCapture.exact, descriptor, reconstructed, positionedMethod)
    yield Proof(
      reconstructed,
      descriptor,
      descriptor.method,
      positionedMethod,
      preparedParameter.map(_._1),
      preparedResult,
      preparedBody.nonEmpty,
      finalMembers.indices.forall(index => index == memberIndex || finalMembers(index).eq(publicCapture.exact.originalTemplate.body(index))),
      finalMembers.size == publicCapture.exact.originalTemplate.body.size
    )

  private[dotty] def positionOptionString(oldSite: untpd.Tree)(using Context): Either[Error, PositionedType] =
    for
      site <- Option(oldSite).filterNot(_.isEmpty).toRight(error("OLD_SITE_REQUIRED", "the old direct Type site was absent."))
      _ <- Either.cond(
        Option(site.source).exists(_.exists) && site.span.exists,
        (),
        error("OLD_SITE_REQUIRED", "the old direct Type site has no source/span.")
      )
      loweredTree <- TypeUntypedLowering.lower(optionString)
        .left.map(problem => error("LOWERING_FAILED", problem.message))
      lowered <- loweredTree match
        case value: untpd.AppliedTypeTree => Right(value)
        case other => Left(error("LOWERED_GRAPH_INVALID", s"expected AppliedTypeTree, found ${other.getClass.getSimpleName}."))
      _ <- validateLowered(lowered)
      positioned <- freshPositioned(site)
      _ <- validatePositioned(lowered, positioned, site)
    yield PositionedType(lowered, positioned, site)

  private[dotty] def validatePositionedForTest(
      lowered: untpd.AppliedTypeTree,
      positioned: untpd.AppliedTypeTree,
      oldSite: untpd.Tree
  )(using Context): Either[Error, Unit] =
    validatePositioned(lowered, positioned, oldSite)

  private def freshPositioned(oldSite: untpd.Tree)(using Context): Either[Error, untpd.AppliedTypeTree] =
    for
      raw <- TypeUntypedLowering.lower(optionString)
        .left.map(problem => error("LOWERING_FAILED", problem.message))
      fragment <- GeneratedOriginFragmentSupport.planType(optionString)
        .left.map(problem => error("GENERATED_ORIGIN_PLAN_FAILED", problem.message))
      generatedSource = SourceFile.virtual("generated/U071OptionString.scala", fragment.source)
      generated <- GeneratedOriginFragmentSupport.positionType(raw, fragment, generatedSource, 0)
        .left.map(problem => error("GENERATED_ORIGIN_POSITION_FAILED", problem.message))
      applied <- generated match
        case value: untpd.AppliedTypeTree => Right(value)
        case other => Left(error("GENERATED_ORIGIN_POSITION_FAILED", s"expected AppliedTypeTree, found ${other.getClass.getSimpleName}."))
    yield applied.cloneIn(oldSite.source).withSpan(oldSite.span)

  private def validateLowered(lowered: untpd.AppliedTypeTree)(using Context): Either[Error, Unit] =
    val nodes = allTrees(lowered)
    val valid = optionTopology(lowered) &&
      nodes.size == 3 &&
      nodes.forall(node =>
        node != null && node.source != null && !node.source.exists && !node.span.exists &&
          node.symbol == NoSymbol && !node.isInstanceOf[untpd.TypedSplice]
      )
    Either.cond(valid, (), error("LOWERED_GRAPH_INVALID", "the lowered graph was not exact source-free AppliedTypeTree(Ident(Option), Ident(String))."))

  private def validatePositioned(
      lowered: untpd.AppliedTypeTree,
      positioned: untpd.AppliedTypeTree,
      oldSite: untpd.Tree
  )(using Context): Either[Error, Unit] =
    val loweredNodes = allTrees(lowered)
    val positionedNodes = allTrees(positioned)
    val children = positionedNodes.drop(1)
    val childSources = children.map(_.source)
    val valid = optionTopology(positioned) &&
      positionedNodes.size == 3 &&
      !positioned.eq(lowered) && !positioned.eq(oldSite) &&
      positioned.source == oldSite.source && positioned.span == oldSite.span &&
      childSources.nonEmpty && childSources.forall(source => source != null && source.exists) &&
      childSources.forall(_.path == "generated/U071OptionString.scala") &&
      children.forall(_.span.exists) &&
      children.map(_.span).distinct.size == children.size &&
      positionedNodes.forall(node => node.symbol == NoSymbol && !node.isInstanceOf[untpd.TypedSplice]) &&
      positionedNodes.forall(positionedNode => loweredNodes.forall(loweredNode => !positionedNode.eq(loweredNode)))
    Either.cond(
      valid,
      (),
      error("POSITION_EVIDENCE_INVALID", "the fresh outer applied Type must occupy the old primitive site while fresh recursive children carry exact generated-origin source/spans.")
    )

  private def optionTopology(tree: untpd.AppliedTypeTree): Boolean = tree match
    case untpd.AppliedTypeTree(constructor: untpd.Ident, List(argument: untpd.Ident)) =>
      constructor.name.toString == "Option" && argument.name.toString == "String"
    case _ => false

  private def positionParameter(
      oldParameter: untpd.ValDef,
      positioned: PositionedType
  )(using Context): Either[Error, untpd.ValDef] =
    given SourceFile = NoSource
    val sourceFree = untpd.ValDef(oldParameter.name, positioned.positioned, untpd.EmptyTree).withMods(oldParameter.mods)
    val result = untpd.cpy.ValDef(sourceFree)(
      sourceFree.name,
      positioned.positioned,
      sourceFree.rhs
    ).cloneIn(oldParameter.source).withSpan(oldParameter.span)
    Either.cond(
      !result.eq(oldParameter) && result.tpt.eq(positioned.positioned) && result.rhs.isEmpty &&
        result.name == oldParameter.name && result.mods.eq(oldParameter.mods) &&
        result.source == oldParameter.source && result.span == oldParameter.span && result.symbol == NoSymbol,
      result,
      error("PARAMETER_POSITION_INVALID", "the fresh parameter shell drifted from the exact old parameter site.")
    )

  private def positionMethod(
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor,
      parameter: Option[untpd.ValDef],
      result: Option[PositionedType],
      body: Option[ExistingUntpdOrdinaryMethodRhsEditPreparation.PreparedBody]
  )(using Context): Either[Error, untpd.DefDef] =
    val selectedParameter = parameter.getOrElse(descriptor.parameterClauses.head.head.tree)
    val selectedResult = result.fold(descriptor.resultType)(_.positioned)
    val selectedBody = body.fold(descriptor.rhs)(_.positionedReplacement)
    given SourceFile = NoSource
    val sourceFree = untpd.DefDef(
      descriptor.method.name,
      List(List(selectedParameter)),
      selectedResult,
      selectedBody
    ).withMods(descriptor.method.mods)
    val positioned = untpd.cpy.DefDef(sourceFree)(
      sourceFree.name,
      sourceFree.paramss,
      sourceFree.tpt,
      sourceFree.rhs
    ).cloneIn(descriptor.method.source).withSpan(descriptor.method.span)
    Either.cond(
      !positioned.eq(descriptor.method) && positioned.name == descriptor.method.name &&
        positioned.mods.eq(descriptor.method.mods) && positioned.source == descriptor.method.source &&
        positioned.span == descriptor.method.span && positioned.symbol == NoSymbol,
      positioned,
      error("METHOD_POSITION_INVALID", "the fresh method shell drifted from the exact old method site.")
    )

  private def validateDescriptorShape(
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor
  )(using Context): Either[Error, Unit] =
    for
      kinds <- ExistingUntpdOrdinaryMethodDescriptor.clauseKinds(descriptor)
        .left.map(problem => error("DESCRIPTOR_REJECTED", problem.message))
      _ <- Either.cond(
        descriptor.opaqueTypeClause.isEmpty && descriptor.localTypeDependency.isEmpty,
        (),
        error("LOCAL_TYPE_BINDER_UNSUPPORTED", "U071 does not promote G1/G2 method Type binders into applied-Type replacement authority.")
      )
      _ <- Either.cond(
        descriptor.parameterClauses.size == 1 && descriptor.parameterClauses.head.size == 1 &&
          kinds == Vector(ExistingUntpdOrdinaryMethodDescriptor.ClauseKind.Ordinary),
        (),
        error("STRICT_UNARY_ORDINARY_REQUIRED", "U071 admits exactly one strict unary ordinary term clause.")
      )
      parameter = descriptor.parameterClauses.head.head
      _ <- Either.cond(
        !parameter.tpt.isInstanceOf[untpd.ByNameTypeTree] && !parameter.tpt.isInstanceOf[untpd.PostfixOp],
        (),
        error("PARAMETER_WRAPPER_UNSUPPORTED", "by-name and repeated wrappers remain outside U071 applied-Type support.")
      )
    yield ()

  private def validateNoVisibleTypeShadow(
      capture: ExistingUntpdClassMemberFilter.Capture,
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor
  ): Either[Error, Unit] =
    val shadowedNames = Set("Option", "String")
    val directShadow = capture.members.exists(_.tree match
      case member: untpd.TypeDef => shadowedNames(member.name.toString) && !member.eq(capture.originalRoot)
      case _ => false
    )
    val methodShadow = descriptor.method.paramss.flatten.exists {
      case parameter: untpd.TypeDef => shadowedNames(parameter.name.toString)
      case _ => false
    }
    Either.cond(
      !directShadow && !methodShadow,
      (),
      error("OPTION_SHADOWING_UNSUPPORTED", "a class member or method-local Type binder shadows unqualified Option or String.")
    )

  private def validateParameterIntent(
      intent: Option[ParameterTypeIntent],
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor
  ): Either[Error, Unit] = intent match
    case None => Right(())
    case Some(value) =>
      val expected = descriptor.parameterClauses.head.head
      Either.cond(
        exactOptionString(value.replacement) &&
          value.parameter.methodIdentity.memberIndex == descriptor.memberIndex &&
          value.parameter.methodIdentity.methodIdentity.eq(descriptor.method) &&
          value.parameter.clauseOrdinal == 0 && value.parameter.parameterOrdinal == 0 &&
          value.parameter.parameterIdentity.eq(expected.tree) && value.parameter.typeIdentity.eq(expected.tpt),
        (),
        error("PARAMETER_INTENT_UNSUPPORTED", "the parameter intent was stale, foreign, or not exact direct Option[String].")
      )

  private def validateResultIntent(
      intent: Option[ResultTypeIntent],
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor
  ): Either[Error, Unit] = intent match
    case None => Right(())
    case Some(value) =>
      Either.cond(
        exactOptionString(value.replacement) && value.method.memberIndex == descriptor.memberIndex &&
          value.method.methodIdentity.eq(descriptor.method),
        (),
        error("RESULT_INTENT_UNSUPPORTED", "the result intent was stale, foreign, or not exact direct Option[String].")
      )

  private def validateBodyIntent(
      intent: Option[BodyIntent],
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor
  ): Either[Error, Unit] = intent match
    case None => Right(())
    case Some(value) =>
      Either.cond(
        value.method.memberIndex == descriptor.memberIndex && value.method.methodIdentity.eq(descriptor.method),
        (),
        error("BODY_INTENT_UNSUPPORTED", "the body intent was stale or foreign.")
      )

  private def validateFinalGraph(
      capture: ExistingUntpdClassMemberFilter.Capture,
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor,
      reconstructed: ExistingUntpdClassMemberFilter.Reconstructed,
      positionedMethod: untpd.DefDef
  )(using Context): Either[Error, Unit] =
    val body = reconstructed.template.body.toVector
    val clean = allTrees(reconstructed.root).forall(node =>
      node != null && node.symbol == NoSymbol && !node.isInstanceOf[untpd.TypedSplice]
    )
    Either.cond(
      body.size == capture.originalTemplate.body.size &&
        body(descriptor.memberIndex).eq(positionedMethod) && clean,
      (),
      error("FINAL_GRAPH_INVALID", "the reconstructed owner lost exact order, selected method identity, or pre-Typer cleanliness.")
    )

  private def exactOptionString(value: TypeNormalForm): Boolean = value match
    case TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("Option"), List(TypeNormalForm.STypeIdent("String"))) => true
    case _ => false

  private def atMostOne[A](values: Vector[A], label: String): Either[Error, Option[A]] =
    Option(values).toRight(error("MALFORMED_PLAN", s"the $label intent collection was null.")).flatMap {
      case Vector() => Right(None)
      case Vector(value) if value != null => Right(Some(value))
      case entries => Left(error("UNSUPPORTED_PLAN", s"U071 requires at most one $label intent; found ${entries.size}."))
    }

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ExistingUntpdClassMemberFilter.allTrees(tree)

  private def error(code: String, detail: String): Error = Error(code, detail)
