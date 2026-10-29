// Test-only adaptation of the consumer-proven semantic adapter; no production/public bridge.
package quasiquotes.definitions.dotty

import _root_.dotty.tools.dotc.core.Contexts.Context

import _root_.quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.*
import _root_.quasiquotes.types.TypeNormalForm
import scala.meta.*

private[dotty] final class QuotedUnaryProjection private (
    val capture: Capture,
    val member: MemberView,
    val method: MethodView,
    val parameter: ParameterView,
    val tree: Defn.Def
)

private[dotty] object QuotedUnaryProjection:
  def capture(
      captured: Capture,
      selectedName: String
  )(using Context): Either[Failure, QuotedUnaryProjection] =
    for
      candidates <- Right(captured.members.filter(member =>
        member.kind == MemberKind.Method &&
          member.diagnosticName.contains(selectedName)
      ))
      _ <- Either.cond(
        candidates.size == 1,
        (),
        Failure(
          "QUOTED_SELECTION_FAILED",
          s"expected exactly one direct `$selectedName` method; found ${candidates.size}."
        )
      )
      member = candidates.head
      descriptor <- ExistingUntpdOrdinaryMethodDescriptor.capture(captured.exact, member.index)
        .left.map(p => Failure("QUOTED_INPUT_DESCRIPTOR_UNSUPPORTED", p.message))
      _ <- Either.cond(
        descriptor.opaqueTypeClause.isEmpty && descriptor.localTypeDependency.isEmpty,
        (), Failure("QUOTED_INPUT_HIDDEN_TYPE_CLAUSE", "generic Type clauses may not disappear from a literal quote view.")
      )
      _ <- Either.cond(
        descriptor.method.paramss match
          case List(List(p: _root_.dotty.tools.dotc.ast.untpd.ValDef)) =>
            !p.tpt.isInstanceOf[_root_.dotty.tools.dotc.ast.untpd.ByNameTypeTree] &&
              !p.tpt.isInstanceOf[_root_.dotty.tools.dotc.ast.untpd.PostfixOp]
          case _ => false,
        (), Failure("QUOTED_INPUT_STRICT_TOPOLOGY", "one raw strict ordinary unary term clause is required")
      )
      method <- captured.method(member.ref)
      _ <- Either.cond(
        method.parameterClauses.size == 1 &&
          method.parameterClauses.head.kind == ParameterClauseKind.Ordinary &&
          method.parameterClauses.head.parameters.size == 1,
        (),
        Failure(
          "QUOTED_INPUT_SHAPE_UNSUPPORTED",
          s"`$selectedName` must have one ordinary unary parameter clause."
        )
      )
      parameter = method.parameterClauses.head.parameters.head
      parameterType <- parameter.declaredType.semantic
      resultType <- method.resultType.semantic
      body <- method.body.semantic
      parameterReference <- method.parameterScope.reference(parameter.ref)
      _ <- Either.cond(
        body == parameterReference,
        (),
        Failure(
          "QUOTED_INPUT_BODY_MISMATCH",
          s"`$selectedName` must return its selected parameter directly."
        )
      )
      parameterTypeTree <- simpleType(parameterType, "parameter")
      resultTypeTree <- simpleType(resultType, "result")
      definition = Defn.Def(
        Nil,
        Term.Name(method.diagnosticName),
        List(Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          List(Term.ParamClause(List(Term.Param(
            Nil,
            Term.Name(parameter.diagnosticName),
            Some(parameterTypeTree),
            None
          ))))
        )),
        Some(resultTypeTree),
        Term.Name(parameter.diagnosticName)
      )
    yield new QuotedUnaryProjection(
      captured,
      member,
      method,
      parameter,
      definition
    )

  private def simpleType(
      normalForm: TypeNormalForm,
      role: String
  ): Either[Failure, Type] =
    normalForm match
      case TypeNormalForm.STypeIdent(name) => Right(Type.Name(name))
      case other => Left(Failure(
        "QUOTED_INPUT_TYPE_UNSUPPORTED",
        s"quoted input $role type must be one simple identifier; found ${other.render}."
      ))


import _root_.dotty.tools.dotc.core.Contexts.Context
import _root_.quasiquotes.definitions.*
import _root_.quasiquotes.definitions.dotty.ExistingClassUntypedRewrite
import _root_.quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.*
import _root_.quasiquotes.neutral.ScalametaDefinitionProjection
import _root_.quasiquotes.parser.TermShape
import _root_.quasiquotes.terms.{TermBinder, TermShapeBindingView}
import scala.meta.Defn

private[dotty] object CaptureLocalQuoteRewrite:
  def apply(
      selected: QuotedUnaryProjection,
      replacement: Defn.Def
  )(using Context): Either[Failure, Result] =
    for
      projected <- ScalametaDefinitionProjection.project(replacement).left.map(
        problem => Failure("QUOTED_OUTPUT_PROJECTION_FAILED", problem.message)
      )
      definition = projected.definition
      _ <- Either.cond(
        definition.name.source == selected.method.diagnosticName,
        (),
        Failure(
          "QUOTED_OUTPUT_METHOD_MISMATCH",
          s"quoted output `${definition.name.source}` does not name the selected method `${selected.method.diagnosticName}`."
        )
      )
      outputMethod <- definition.asMethod.toRight(Failure(
        "QUOTED_OUTPUT_SHAPE_UNSUPPORTED",
        "quoted output must be one concrete method."
      ))
      outputParameter <- outputMethod.parameterClauses match
        case Vector(clause)
            if clause.kind == DefinitionParameterClauseKind.Ordinary &&
              clause.parameters.size == 1 =>
          Right(clause.parameters.head)
        case clauses => Left(Failure(
          "QUOTED_OUTPUT_SHAPE_UNSUPPORTED",
          s"quoted output must have one ordinary unary clause; found ${clauses.size} clauses."
        ))
      _ <- Either.cond(
        outputParameter.name.source == selected.parameter.diagnosticName,
        (),
        Failure(
          "QUOTED_OUTPUT_PARAMETER_MISMATCH",
          s"quoted output parameter `${outputParameter.name.source}` does not preserve selected parameter `${selected.parameter.diagnosticName}`."
        )
      )
      outputBody <- outputMethod.body.toRight(Failure(
        "QUOTED_OUTPUT_SHAPE_UNSUPPORTED",
        "quoted output must have a body."
      ))
      outputBinder <- outputMethod.parameterScope.binder(0, 0).left.map(
        problem => Failure("QUOTED_OUTPUT_BINDER_FAILED", problem.message)
      )
      existingReference <-
        selected.method.parameterScope.reference(selected.parameter.ref)
      mappedBody <- mapQualifiedUnaryCall(
        outputBody,
        outputBinder,
        existingReference
      )
      parameterPlan <- selected.capture.emptyPlan.replaceParameterType(
        selected.parameter.ref,
        outputParameter.declaredType
      )
      resultPlan <- parameterPlan.replaceResultType(
        selected.method.ref,
        outputMethod.resultType
      )
      plan <- resultPlan.replaceBody(selected.method.ref, mappedBody)
      result <- ExistingClassUntypedRewrite(selected.capture, plan)
    yield result

  def mapQualifiedUnaryCall(
      body: TermShape,
      outputBinder: TermBinder,
      existingReference: TermShape
  ): Either[Failure, TermShape] =
    body match
      case TermShape.Apply(
            TermShape.Select(qualifier, member),
            List(argument)
          ) =>
        for
          _ <- Either.cond(
            qualifiedPath(qualifier),
            (),
            Failure(
              "QUOTED_OUTPUT_SHAPE_UNSUPPORTED",
              "quoted output call qualifier must be a stable identifier/select path."
            )
          )
          view <- TermShapeBindingView.inspect(argument).left.map(
            problem => Failure("QUOTED_OUTPUT_BINDER_FAILED", problem.message)
          )
          reference <- view.boundReference.toRight(Failure(
            "QUOTED_OUTPUT_BINDER_MISMATCH",
            "quoted output call argument must be the output method parameter reference."
          ))
          _ <- Either.cond(
            reference.binder == outputBinder,
            (),
            Failure(
              "QUOTED_OUTPUT_BINDER_MISMATCH",
              "quoted output call argument belongs to a different binder."
            )
          )
        yield TermShape.Apply(
          TermShape.Select(qualifier, member),
          List(existingReference)
        )
      case _ => Left(Failure(
        "QUOTED_OUTPUT_SHAPE_UNSUPPORTED",
        "quoted output body must be one qualified unary call."
      ))

  private def qualifiedPath(shape: TermShape): Boolean =
    shape match
      case TermShape.Identifier(name, false) => name.nonEmpty
      case TermShape.Select(qualifier, name) =>
        name.nonEmpty && qualifiedPath(qualifier)
      case _ => false


/** NONPUBLIC feasibility model: this test oracle is not a delivered foreign API. */
private[dotty] object QuotedExistingMethodAuthorityModel:
  import _root_.dotty.tools.dotc.ast.untpd
  import scala.meta.*
  import ExistingClassUntypedRewrite.*
  type Selected = QuotedUnaryProjection
  def select(capture: Capture, name: String)(using Context): Either[Failure, Selected] =
    QuotedUnaryProjection.capture(capture, name)
  def quotedReplacement(selected: Selected): Either[Failure, Defn.Def] = selected.tree match
    case q"def echo($value: AnyVal): AnyVal = $body" => value match
      case term: Term.Name => Right(q"def echo($term: Int): Int = Math.abs($term)")
      case _ => Left(Failure("QUOTE_NAME_NARROWING", "parameter hole is not a Term.Name"))
    case q"def magnitude($number: AnyVal): AnyVal = $body" => number match
      case term: Term.Name => Right(q"def magnitude($term: Int): Int = StrictMath.abs($term)")
      case _ => Left(Failure("QUOTE_NAME_NARROWING", "parameter hole is not a Term.Name"))
    case _ => Left(Failure("QUOTE_PATTERN_MISMATCH", "expected an admitted real method quote"))
  def replace(selected: Selected, replacement: Defn.Def)(using Context): Either[Failure, Result] =
    CaptureLocalQuoteRewrite(selected, replacement)
  def rewrite(root: untpd.TypeDef, name: String)(using Context): Either[Failure, Result] =
    for
      captured <- ExistingClassUntypedRewrite.capture(root)
      selected <- select(captured, name)
      replacement <- quotedReplacement(selected)
      result <- replace(selected, replacement)
    yield result
