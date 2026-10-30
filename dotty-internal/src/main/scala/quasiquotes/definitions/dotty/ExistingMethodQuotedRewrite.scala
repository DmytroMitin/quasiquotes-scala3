package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import quasiquotes.definitions.DefinitionParameterClauseKind
import quasiquotes.neutral.ScalametaDefinitionProjection
import quasiquotes.parser.TermShape
import quasiquotes.terms.{TermBinder, TermShapeBindingView}
import quasiquotes.types.TypeNormalForm
import scala.meta.{Defn, Member, Term, Type}

/** A capture-owned authoring view for a bounded rooted existing-method rewrite. */
object ExistingMethodQuotedRewrite:
  import ExistingClassUntypedRewrite.*
  private val IntType = TypeNormalForm.STypeIdent("Int")

  final class Selection private[dotty] (
      private[dotty] val capture: Capture,
      private[dotty] val method: MethodView,
      private[dotty] val parameter: ParameterView,
      private val definition: Defn.Def,
      private val original: ExistingQuotedRootedBody.Snapshot,
      private val context: Context
  ):
    def projected: Defn.Def = definition

    private[dotty] def validate()(using Context): Either[Failure, Unit] =
      for
        _ <- check(context.eq(summon[Context]), "QUOTED_CONTEXT_MISMATCH", "the original capture Context is required")
        _ <- check(original.valid(ExistingQuotedRootedBody.originalGraph(capture.exact.originalRoot)),
          "QUOTED_STALE_SELECTION", "the captured graph, source bytes, payload or sites changed")
        _ <- ExistingUntpdClassMemberFilter.validateCaptured(capture.exact)
          .left.map(p => Failure("QUOTED_STALE_SELECTION",p.message))
        descriptor <- ExistingUntpdOrdinaryMethodDescriptor.capture(capture.exact,method.ref.memberIndex)
          .left.map(p => Failure("QUOTED_STALE_SELECTION",p.message))
        _ <- check(descriptor.method.eq(method.ref.methodIdentity) &&
          descriptor.parameterClauses.head.head.tree.eq(parameter.ref.parameterIdentity) &&
          descriptor.parameterClauses.head.head.tpt.eq(parameter.ref.typeIdentity),
          "QUOTED_STALE_SELECTION", "the exact selected parameter topology changed")
      yield ()

    def replace(plan: EditPlan, replacement: Defn.Def)(using Context): Either[Failure, EditPlan] =
      for
        _ <- validate()
        _ <- check(plan != null && plan.captureIdentity.eq(capture.exact),
          "SELECTION_FAILED", "the EditPlan must belong to the exact selected Capture")
        quoted <- Option(replacement).toRight(Failure("QUOTED_OUTPUT_REQUIRED","a replacement method is required"))
        _ <- check(quoted.mods.isEmpty && quoted.tparams.isEmpty,
          "QUOTED_OUTPUT_SHAPE_UNSUPPORTED", "replacement modifiers and generic parameters are unsupported")
        projection <- ScalametaDefinitionProjection.project(quoted)
          .left.map(p => Failure("QUOTED_OUTPUT_PROJECTION_FAILED",p.message))
        output <- projection.definition.asMethod.toRight(Failure("QUOTED_OUTPUT_SHAPE_UNSUPPORTED","a concrete method is required"))
        _ <- check(projection.definition.name.source == method.diagnosticName &&
          output.parameterClauses.size == 1 &&
          output.parameterClauses.head.kind == DefinitionParameterClauseKind.Ordinary &&
          output.parameterClauses.head.parameters.size == 1 &&
          output.parameterClauses.head.parameters.head.name.source == parameter.diagnosticName &&
          output.parameterClauses.head.parameters.head.declaredType == IntType && output.resultType == IntType,
          "QUOTED_OUTPUT_SHAPE_UNSUPPORTED", "matching strict ordinary unary Int declarations are required")
        body <- output.body.toRight(Failure("QUOTED_OUTPUT_SHAPE_UNSUPPORTED","a method body is required"))
        binder <- output.parameterScope.binder(0,0).left.map(p => Failure("QUOTED_OUTPUT_BINDER_FAILED",p.message))
        reference <- method.parameterScope.reference(parameter.ref)
        mapped <- transport(body,binder,reference)
        parameterPlan <- plan.replaceParameterType(parameter.ref,IntType)
        resultPlan <- parameterPlan.replaceResultType(method.ref,IntType)
        finalPlan <- resultPlan.replaceQuotedBody(this,mapped)
      yield finalPlan

  def select(capture: Capture, member: MemberRef)(using Context): Either[Failure, Selection] =
    for
      captured <- Option(capture).toRight(Failure("MISSING_INPUT","a Capture is required"))
      _ <- check(captured.hasContext(summon[Context]), "QUOTED_CONTEXT_MISMATCH", "the original capture Context is required")
      _ <- check(captured.quotedOriginalUnchanged,"QUOTED_STALE_CAPTURE","the original captured graph or source changed")
      method <- captured.method(member)
      _ <- check(captured.members.count(_.diagnosticName.contains(method.diagnosticName)) == 1,
        "QUOTED_SELECTION_AMBIGUOUS","the first quoted slice requires exactly one direct member with the selected name")
      descriptor <- ExistingUntpdOrdinaryMethodDescriptor.capture(captured.exact,method.ref.memberIndex)
        .left.map(p => Failure("QUOTED_INPUT_DESCRIPTOR_UNSUPPORTED",p.message))
      _ <- check(descriptor.opaqueTypeClause.isEmpty && descriptor.localTypeDependency.isEmpty,
        "QUOTED_INPUT_HIDDEN_TYPE_CLAUSE","generic Type clauses cannot be projected away")
      _ <- check(descriptor.method.paramss match
        case List(List(p: untpd.ValDef)) =>
          !p.tpt.isInstanceOf[untpd.ByNameTypeTree] && !p.tpt.isInstanceOf[untpd.PostfixOp] && p.rhs.isEmpty
        case _ => false,
        "QUOTED_INPUT_STRICT_TOPOLOGY","one strict ordinary unary term clause is required")
      _ <- check(method.parameterClauses.size == 1 &&
        method.parameterClauses.head.kind == ParameterClauseKind.Ordinary &&
        method.parameterClauses.head.parameters.size == 1,
        "QUOTED_INPUT_STRICT_TOPOLOGY","one strict ordinary unary term clause is required")
      parameter = method.parameterClauses.head.parameters.head
      parameterType <- parameter.declaredType.semantic.flatMap(simpleType)
      resultType <- method.resultType.semantic.flatMap(simpleType)
      body <- method.body.semantic
      own <- method.parameterScope.binder(parameter.ref)
      bodyView <- TermShapeBindingView.inspect(body).left.map(p => Failure("QUOTED_INPUT_BODY_MISMATCH",p.message))
      _ <- check(bodyView.boundReference.exists(_.binder == own),
        "QUOTED_INPUT_BODY_MISMATCH","the original body must be its own parameter reference")
      definition = Defn.Def(Nil,Term.Name(method.diagnosticName),List(Member.ParamClauseGroup(
        Type.ParamClause(Nil),List(Term.ParamClause(List(Term.Param(Nil,Term.Name(parameter.diagnosticName),Some(parameterType),None))))
      )),Some(resultType),Term.Name(parameter.diagnosticName))
      snapshot = new ExistingQuotedRootedBody.Snapshot(ExistingQuotedRootedBody.originalGraph(captured.exact.originalRoot))
    yield new Selection(captured,method,parameter,definition,snapshot,summon[Context])

  private[dotty] def transport(body: TermShape, own: TermBinder, reference: TermShape): Either[Failure,TermShape] =
    for
      _ <- check(ExistingQuotedRootedBody.canonical(body),"QUOTED_ROOTED_TARGET_UNSUPPORTED",
        "only explicit _root_.java.lang.Math.abs or _root_.java.lang.StrictMath.abs is supported")
      application = body.asInstanceOf[TermShape.Apply]
      view <- TermShapeBindingView.inspect(application.arguments.head).left.map(p => Failure("QUOTED_OUTPUT_BINDER_FAILED",p.message))
      _ <- check(view.boundReference.exists(_.binder == own),"QUOTED_OUTPUT_BINDER_MISMATCH",
        "the call argument must be the quoted output method's own parameter binder")
    yield TermShape.Apply(application.function,List(reference))

  private def simpleType(value: TypeNormalForm): Either[Failure, Type] = value match
    case TypeNormalForm.STypeIdent(name) if Set("AnyVal","Int","Long","Double","Float","Short","Byte","Char","Boolean","String")(name) => Right(Type.Name(name))
    case _ => Left(Failure("QUOTED_INPUT_TYPE_UNSUPPORTED","a simple admitted primitive or AnyVal Type is required"))

  private def check(valid: Boolean,code: String,detail: String): Either[Failure,Unit] =
    Either.cond(valid,(),Failure(code,detail))
