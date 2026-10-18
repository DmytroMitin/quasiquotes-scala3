package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.types.{
  ResolvedTypeNameId,
  ResolvedTypeOwnerKind,
  ResolvedTypeOwnerSegment,
  TypeNormalForm
}

/** Test-scope model for the authority a future exact U073 read projector would
  * need. It deliberately does not participate in the public capture or edit
  * implementation.
  */
private[dotty] object ExistingRootedOptionSemanticProjectionAuthorityPrototype:
  import ExistingClassUntypedRewrite.*
  import ExistingUntpdOrdinaryMethodDescriptor.{Descriptor, Parameter}

  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  private final case class SlotCertificate(
      resultRoot: untpd.TypeDef,
      memberIndex: Int,
      method: untpd.DefDef,
      parameter: Option[untpd.ValDef],
      originalOuter: untpd.Tree,
      rootedOuter: untpd.Tree,
      rootedNodes: Vector[untpd.Tree],
      generatedSource: dotty.tools.dotc.util.SourceFile
  )

  final class CertifiedRewrite private[ExistingRootedOptionSemanticProjectionAuthorityPrototype] (
      val result: Result,
      private val parameterCertificate: SlotCertificate,
      private val resultCertificate: SlotCertificate
  ):
    def validateParameterCandidate(
        candidate: untpd.Tree
    )(using Context): Either[Error, Unit] =
      validateCandidateIdentity(parameterCertificate, candidate)

    def projectParameter(
        capture: Capture,
        method: MethodRef,
        parameter: ParameterRef
    )(using Context): Either[Error, TypeNormalForm] =
      for
        selectedCapture <- requireCapture(capture, parameterCertificate)
        selectedMethod <- requireMethod(selectedCapture, method, parameterCertificate)
        descriptor <- descriptorFor(selectedCapture, parameterCertificate.memberIndex)
        selectedParameter <- selectParameter(descriptor, parameter, selectedMethod)
        expected <- parameterCertificate.parameter.toRight(
          error("CERTIFICATE_SLOT_MISMATCH", "the parameter certificate had no parameter identity.")
        )
        _ <- Either.cond(
          selectedParameter.tree.eq(expected) && selectedParameter.tpt.eq(parameterCertificate.rootedOuter),
          (),
          error("CERTIFICATE_SLOT_MISMATCH", "the recaptured parameter slot did not match the certified U073 output identity.")
        )
        _ <- validateCertificate(parameterCertificate)
      yield canonicalOptionString

    def projectResult(
        capture: Capture,
        method: MethodRef
    )(using Context): Either[Error, TypeNormalForm] =
      for
        selectedCapture <- requireCapture(capture, resultCertificate)
        _ <- requireMethod(selectedCapture, method, resultCertificate)
        descriptor <- descriptorFor(selectedCapture, resultCertificate.memberIndex)
        _ <- Either.cond(
          descriptor.resultType.eq(resultCertificate.rootedOuter),
          (),
          error("CERTIFICATE_SLOT_MISMATCH", "the recaptured result slot did not match the certified U073 output identity.")
        )
        _ <- validateCertificate(resultCertificate)
      yield canonicalOptionString

  def rewriteAndCertify(
      capture: Capture,
      plan: EditPlan
  )(using Context): Either[Error, CertifiedRewrite] =
    for
      selectedCapture <- Option(capture).toRight(error("CAPTURE_REQUIRED", "the original public capture was null."))
      selectedPlan <- Option(plan).toRight(error("PLAN_REQUIRED", "the public edit plan was null."))
      _ <- Either.cond(
        selectedPlan.captureIdentity.eq(selectedCapture.exact),
        (),
        error("PLAN_CAPTURE_MISMATCH", "the edit plan did not belong to the supplied original capture.")
      )
      parameterIntent <- selectedPlan.parameterTypes match
        case Vector(value) => Right(value)
        case other => Left(error("CANONICAL_PLAN_REQUIRED", s"expected one parameter Type intent, found ${other.size}."))
      resultIntent <- selectedPlan.resultTypes match
        case Vector(value) => Right(value)
        case other => Left(error("CANONICAL_PLAN_REQUIRED", s"expected one result Type intent, found ${other.size}."))
      _ <- Either.cond(
        parameterIntent.parameter.methodIdentity.memberIndex == resultIntent.method.memberIndex &&
          parameterIntent.replacement == canonicalOptionString &&
          resultIntent.replacement == canonicalOptionString,
        (),
        error("CANONICAL_PLAN_REQUIRED", "both Type intents must target one method with the exact C080 resolved Option[String] value.")
      )
      originalDescriptor <- descriptorFor(selectedCapture, resultIntent.method.memberIndex)
      originalParameter <- parameterAt(originalDescriptor, parameterIntent.parameter)
      rewritten <- ExistingClassUntypedRewrite(selectedCapture, selectedPlan)
        .left.map(problem => error("PUBLIC_REWRITE_FAILED", problem.message))
      resultCapture <- ExistingUntpdClassMemberFilter.capture(rewritten.tree)
        .left.map(problem => error("RESULT_CAPTURE_FAILED", problem.message))
      resultDescriptor <- ExistingUntpdOrdinaryMethodDescriptor
        .capture(resultCapture, resultIntent.method.memberIndex)
        .left.map(problem => error("RESULT_DESCRIPTOR_FAILED", problem.message))
      resultParameter <- resultDescriptor.parameterClauses match
        case Vector(Vector(value)) if value != null => Right(value)
        case _ => Left(error("RESULT_DESCRIPTOR_FAILED", "the rewritten method was no longer strict unary ordinary."))
      parameterCertificate <- certifySlot(
        rewritten.tree,
        resultIntent.method.memberIndex,
        resultDescriptor.method,
        Some(resultParameter.tree),
        originalParameter.tpt,
        resultParameter.tpt,
        "parameter"
      )
      resultCertificate <- certifySlot(
        rewritten.tree,
        resultIntent.method.memberIndex,
        resultDescriptor.method,
        None,
        originalDescriptor.resultType,
        resultDescriptor.resultType,
        "result"
      )
      _ <- Either.cond(
        parameterCertificate.rootedNodes.tail.forall(node =>
          resultCertificate.rootedNodes.forall(other => !node.eq(other))
        ),
        (),
        error("GENERATED_GRAPH_REUSED", "parameter and result certificates shared a generated child node.")
      )
    yield new CertifiedRewrite(rewritten, parameterCertificate, resultCertificate)

  def inspectUncertifiedParameter(
      capture: Capture,
      method: MethodRef,
      parameter: ParameterRef
  )(using Context): Either[Error, TypeNormalForm] =
    for
      selectedCapture <- Option(capture).toRight(error("CAPTURE_REQUIRED", "the public capture was null."))
      selectedMethod <- requireLocalMethod(selectedCapture, method)
      descriptor <- descriptorFor(selectedCapture, selectedMethod.memberIndex)
      selectedParameter <- selectParameter(descriptor, parameter, selectedMethod)
      projected <- classifyUncertified(selectedParameter.tpt, "parameter")
    yield projected

  def inspectUncertifiedResult(
      capture: Capture,
      method: MethodRef
  )(using Context): Either[Error, TypeNormalForm] =
    for
      selectedCapture <- Option(capture).toRight(error("CAPTURE_REQUIRED", "the public capture was null."))
      selectedMethod <- requireLocalMethod(selectedCapture, method)
      descriptor <- descriptorFor(selectedCapture, selectedMethod.memberIndex)
      projected <- classifyUncertified(descriptor.resultType, "result")
    yield projected

  def validateGeneratedCandidate(
      rootedOuter: untpd.Tree,
      originalOuter: untpd.Tree
  )(using Context): Either[Error, Unit] =
    for
      rooted <- Option(rootedOuter).toRight(
        error("CERTIFICATE_INPUT_MALFORMED", "the candidate rooted outer node was null.")
      )
      original <- Option(originalOuter).toRight(
        error("CERTIFICATE_INPUT_MALFORMED", "the original outer node was null.")
      )
      nodes = ExistingUntpdClassMemberFilter.allTrees(rooted)
      _ <- validateRootedGraph(rooted, nodes, "candidate")
      _ <- Either.cond(
        sameSourceHandle(rooted, original) && rooted.span == original.span,
        (),
        error("OUTER_SITE_MISMATCH", "the candidate outer source/span did not match the original Type site.")
      )
    yield ()

  private def requireCapture(
      capture: Capture,
      certificate: SlotCertificate
  ): Either[Error, Capture] =
    Option(capture)
      .filter(selected => selected.exact.originalRoot.eq(certificate.resultRoot))
      .toRight(error("CERTIFICATE_CAPTURE_MISMATCH", "the capture did not belong to the certified rewritten owner."))

  private def requireMethod(
      capture: Capture,
      method: MethodRef,
      certificate: SlotCertificate
  ): Either[Error, MethodRef] =
    Option(method)
      .filter(selected =>
        selected.captureIdentity.eq(capture.exact) &&
          selected.memberIndex == certificate.memberIndex &&
          selected.methodIdentity.eq(certificate.method)
      )
      .toRight(error("CERTIFICATE_METHOD_MISMATCH", "the method reference did not belong to the certified recapture."))

  private def requireLocalMethod(
      capture: Capture,
      method: MethodRef
  ): Either[Error, MethodRef] =
    Option(method)
      .filter(selected =>
        selected.captureIdentity.eq(capture.exact) &&
          capture.exact.members.lift(selected.memberIndex).exists(_.tree.eq(selected.methodIdentity))
      )
      .toRight(error("METHOD_NOT_CAPTURED", "the method reference did not belong to the supplied capture."))

  private def descriptorFor(
      capture: Capture,
      memberIndex: Int
  )(using Context): Either[Error, Descriptor] =
    ExistingUntpdOrdinaryMethodDescriptor
      .capture(capture.exact, memberIndex)
      .left.map(problem => error("DESCRIPTOR_INVALID", problem.message))

  private def selectParameter(
      descriptor: Descriptor,
      parameter: ParameterRef,
      method: MethodRef
  ): Either[Error, Parameter] =
    Option(parameter).toRight(error("PARAMETER_REQUIRED", "the parameter reference was null.")).flatMap { selected =>
      descriptor.parameterClauses.lift(selected.clauseOrdinal)
        .flatMap(_.lift(selected.parameterOrdinal))
        .filter(candidate =>
          selected.methodIdentity.eq(method) &&
            candidate.tree.eq(selected.parameterIdentity) &&
            candidate.tpt.eq(selected.typeIdentity)
        )
        .toRight(error("CERTIFICATE_PARAMETER_MISMATCH", "the parameter reference did not belong to the certified method slot."))
    }

  private def parameterAt(
      descriptor: Descriptor,
      parameter: ParameterRef
  ): Either[Error, Parameter] =
    descriptor.parameterClauses.lift(parameter.clauseOrdinal)
      .flatMap(_.lift(parameter.parameterOrdinal))
      .filter(candidate =>
        candidate.tree.eq(parameter.parameterIdentity) && candidate.tpt.eq(parameter.typeIdentity)
      )
      .toRight(error("CANONICAL_PLAN_REQUIRED", "the parameter intent did not identify the original descriptor slot."))

  private def certifySlot(
      resultRoot: untpd.TypeDef,
      memberIndex: Int,
      method: untpd.DefDef,
      parameter: Option[untpd.ValDef],
      originalOuter: untpd.Tree,
      rootedOuter: untpd.Tree,
      label: String
  )(using Context): Either[Error, SlotCertificate] =
    for
      _ <- Either.cond(
        resultRoot != null && method != null && originalOuter != null && rootedOuter != null,
        (),
        error("CERTIFICATE_INPUT_MALFORMED", s"$label certificate input contained null.")
      )
      nodes = ExistingUntpdClassMemberFilter.allTrees(rootedOuter)
      _ <- validateRootedGraph(rootedOuter, nodes, label)
      _ <- Either.cond(
        sameSourceHandle(rootedOuter, originalOuter) && rootedOuter.span == originalOuter.span,
        (),
        error("OUTER_SITE_MISMATCH", s"$label rooted outer node did not retain the exact original source/span site.")
      )
      _ <- Either.cond(
        nodes.forall(node => !node.eq(originalOuter)),
        (),
        error("GENERATED_GRAPH_REUSED", s"$label rooted graph reused the original outer Type node.")
      )
      source <- nodes.drop(1).headOption.map(_.source).toRight(
        error("GENERATED_PROVENANCE_INVALID", s"$label rooted graph had no generated children.")
      )
      certificate = SlotCertificate(
        resultRoot,
        memberIndex,
        method,
        parameter,
        originalOuter,
        rootedOuter,
        nodes,
        source
      )
      _ <- validateCertificate(certificate)
    yield certificate

  private def validateCertificate(
      certificate: SlotCertificate
  )(using Context): Either[Error, Unit] =
    for
      currentNodes <- Option(certificate.rootedOuter)
        .map(ExistingUntpdClassMemberFilter.allTrees)
        .toRight(error("CERTIFICATE_GRAPH_MISMATCH", "the certified rooted outer node was null."))
      _ <- Either.cond(
        currentNodes.size == certificate.rootedNodes.size &&
          currentNodes.zip(certificate.rootedNodes).forall((current, expected) => current.eq(expected)),
        (),
        error("CERTIFICATE_GRAPH_MISMATCH", "the current rooted graph did not preserve every certified node identity.")
      )
      _ <- validateRootedGraph(certificate.rootedOuter, currentNodes, "certified")
      _ <- Either.cond(
        sameSourceHandle(certificate.rootedOuter, certificate.originalOuter) &&
          certificate.rootedOuter.span == certificate.originalOuter.span,
        (),
        error("OUTER_SITE_MISMATCH", "the certified rooted outer source/span no longer matched the original site.")
      )
      _ <- Either.cond(
        currentNodes.drop(1).forall(node => node.source.asInstanceOf[AnyRef] eq certificate.generatedSource.asInstanceOf[AnyRef]),
        (),
        error("GENERATED_PROVENANCE_INVALID", "a generated child no longer used the certified virtual source handle.")
      )
    yield ()

  private def validateCandidateIdentity(
      certificate: SlotCertificate,
      candidate: untpd.Tree
  )(using Context): Either[Error, Unit] =
    for
      present <- Option(candidate).toRight(
        error("CERTIFICATE_GRAPH_MISMATCH", "the candidate Type graph was null.")
      )
      nodes = ExistingUntpdClassMemberFilter.allTrees(present)
      _ <- Either.cond(
        present.eq(certificate.rootedOuter) &&
          nodes.size == certificate.rootedNodes.size &&
          nodes.zip(certificate.rootedNodes).forall((current, expected) => current.eq(expected)),
        (),
        error("CERTIFICATE_GRAPH_MISMATCH", "the candidate did not preserve the certified rooted node identities.")
      )
      _ <- validateGeneratedCandidate(present, certificate.originalOuter)
    yield ()

  private def validateRootedGraph(
      rootedOuter: untpd.Tree,
      nodes: Vector[untpd.Tree],
      label: String
  )(using Context): Either[Error, Unit] =
    val expectedSpans = Vector(
      (0, 19, 13),
      (0, 12, 7),
      (0, 6, 0),
      (20, 43, 37),
      (20, 36, 32),
      (20, 31, 27),
      (20, 26, 20)
    )
    val clean = nodes.forall(node => node.symbol == NoSymbol && !node.isInstanceOf[untpd.TypedSplice])
    val generated = nodes.drop(1)
    val provenance = generated.nonEmpty &&
      generated.forall(_.source.asInstanceOf[AnyRef] eq generated.head.source.asInstanceOf[AnyRef]) &&
      generated.head.source.path == "generated/CanonicalResolvedOptionStringMethodType.scala" &&
      generated.head.source.content.mkString == "_root_.scala.Option[_root_.java.lang.String]" &&
      generated.map(node => (node.span.start, node.span.end, node.span.point)) == expectedSpans
    Either.cond(
      isCanonicalRootedPair(rootedOuter) && nodes.size == 8 && clean && provenance,
      (),
      error("GENERATED_PROVENANCE_INVALID", s"$label slot was not the exact clean eight-node U073 rooted graph.")
    )

  private def classifyUncertified(
      tree: untpd.Tree,
      label: String
  )(using Context): Either[Error, TypeNormalForm] =
    if !isCanonicalRootedPair(tree) then
      Left(error("CANONICAL_SHAPE_MISMATCH", s"$label slot was not the exact rooted Option[java.lang.String] raw shape."))
    else
      val nodes = ExistingUntpdClassMemberFilter.allTrees(tree)
      validateRootedGraph(tree, nodes, label) match
        case Right(_) =>
          Left(error(
            "GENERATED_AUTHORITY_REQUIRED",
            s"$label slot has the generated U073 shape but no trusted rewrite certificate."
          ))
        case Left(_) =>
          Left(error(
            "ORIGINAL_ROOTED_SOURCE_UNCERTIFIED",
            s"$label slot has canonical rooted lexical spelling but no U073 generated-origin authority."
          ))

  private def isCanonicalRootedPair(tree: untpd.Tree): Boolean = tree match
    case untpd.AppliedTypeTree(
          untpd.Select(untpd.Select(untpd.Ident(rootOption), scalaName), optionName),
          List(
            untpd.Select(
              untpd.Select(untpd.Select(untpd.Ident(rootString), javaName), langName),
              stringName
            )
          )
        ) =>
      rootOption.toString == "_root_" && scalaName.toString == "scala" &&
        optionName.toString == "Option" && rootString.toString == "_root_" &&
        javaName.toString == "java" && langName.toString == "lang" &&
        stringName.toString == "String"
    case _ => false

  private def sameSourceHandle(left: untpd.Tree, right: untpd.Tree): Boolean =
    left.source.asInstanceOf[AnyRef] eq right.source.asInstanceOf[AnyRef]

  private val canonicalOptionString: TypeNormalForm =
    val pkg = (name: String) => ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, name)
    TypeNormalForm.STypeApply(
      TypeNormalForm.STypeResolved(ResolvedTypeNameId(Vector(pkg("scala")), "Option")),
      List(
        TypeNormalForm.STypeResolved(
          ResolvedTypeNameId(Vector(pkg("java"), pkg("lang")), "String")
        )
      )
    )

  private def error(code: String, detail: String): Error = Error(code, detail)
