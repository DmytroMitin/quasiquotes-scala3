package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol

/** Read-only ordered handles over the accepted ordinary method view families. */
private[quasiquotes] object ExistingUntpdOrdinaryMethodDescriptor:
  final case class Parameter private[dotty] (
      tree: untpd.ValDef,
      tpt: untpd.Tree,
      diagnosticName: String
  )

  final case class Descriptor private[dotty] (
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int,
      method: untpd.DefDef,
      diagnosticMethodName: String,
      parameterClauses: Vector[Vector[Parameter]],
      resultType: untpd.Tree,
      rhs: untpd.Tree
  )

  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def capture(
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int
  )(using Context): Either[Error, Descriptor] =
    for
      descriptor <- build(captured, memberIndex)
      _ <- validate(descriptor)
    yield descriptor

  private[dotty] def validate(descriptor: Descriptor)(using Context): Either[Error, Unit] =
    val failure = Error(
      "DESCRIPTOR_IDENTITY_INVARIANT_FAILED",
      "the descriptor no longer matches its original capture, direct member, ordered parameters, or exact raw slots."
    )
    Option(descriptor).toRight(failure).flatMap { value =>
      build(value.captured, value.memberIndex).left.map(_ => failure).flatMap { expected =>
        val parametersMatch = Option(value.parameterClauses).exists { clauses =>
          clauses.size == expected.parameterClauses.size &&
          clauses.zip(expected.parameterClauses).forall { (actual, original) =>
            Option(actual).exists { parameters =>
              parameters.size == original.size &&
              parameters.zip(original).forall { (parameter, site) =>
                Option(parameter).exists(p => site.tree.eq(p.tree) && site.tpt.eq(p.tpt))
              }
            }
          }
        }
        Either.cond(
          expected.captured.eq(value.captured) && expected.memberIndex == value.memberIndex &&
            expected.method.eq(value.method) && expected.resultType.eq(value.resultType) &&
            expected.rhs.eq(value.rhs) && parametersMatch,
          (),
          failure
        )
      }
    }

  private def build(
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int
  )(using Context): Either[Error, Descriptor] =
    for
      owner <- Option(captured).toRight(Error("CAPTURE_REQUIRED", "the original class capture was null."))
      // Presence precedes compiler traversal. This checks raw seams, not slot semantics.
      _ <- Either.cond(
        !rawGraphHasUnavailableField(owner.originalRoot),
        (),
        Error("MALFORMED_OWNER_GRAPH", "the original owner has a null or deferred raw field; descriptor capture cannot force compiler lazy values.")
      )
      // Reject foreign linkage before U023 can read the supplied Template body.
      _ <- Either.cond(
        owner.originalRoot.rhs.eq(owner.originalTemplate),
        (),
        Error("CAPTURE_INVARIANT_FAILED", "the supplied Template is not the exact original class RHS.")
      )
      _ <- ExistingUntpdClassMemberFilter.validateCaptured(owner)
        .left.map(e => Error("CAPTURE_INVARIANT_FAILED", e.detail))
      member <- owner.members.lift(memberIndex).toRight(
        Error("MEMBER_INDEX_NOT_CAPTURED", s"direct-member index $memberIndex is outside the original capture.")
      )
      method <- member.tree match
        case value: untpd.DefDef => Right(value)
        case _ => Left(Error("SELECTED_MEMBER_NOT_METHOD", "the selected direct member is not a method."))
      descriptor <- method.paramss match
        // Classify cardinality only; the accepted authority validates the actual family.
        case clause :: Nil if clause.size == 1 =>
          ExistingUntpdSingleParameterMethodView.capture(owner, memberIndex)
            .left.map(e => Error(e.code, e.detail)).map { view =>
              Descriptor(view.captured, view.memberIndex, view.method, view.methodName,
                Vector(Vector(Parameter(view.parameter, view.parameterType, view.parameterName))),
                view.resultType, view.rhs)
            }
        case clause :: Nil if clause.size == 2 =>
          ExistingUntpdTwoParameterMethodView.capture(owner, memberIndex)
            .left.map(e => Error(e.code, e.detail)).map { view =>
              Descriptor(view.captured, view.memberIndex, view.method, view.methodName,
                Vector(Vector(
                  Parameter(view.firstParameter, view.firstParameterType, view.firstParameterName),
                  Parameter(view.secondParameter, view.secondParameterType, view.secondParameterName)
                )), view.resultType, view.rhs)
            }
        case _ => Left(Error("UNSUPPORTED_PARAMETER_TOPOLOGY", "exactly one clause of one or two ordinary parameters is required."))
      // The older single-parameter authority predates the explicit erased/owner guards.
      // Keep that authority unchanged while enforcing this descriptor's pre-Typer boundary.
      _ <- Either.cond(
        !descriptor.parameterClauses.flatten.exists(_.tree.mods.is(Flags.Erased)),
        (),
        Error("CONTEXTUAL_PARAMETER_UNSUPPORTED", "erased parameters are outside ordinary method capture.")
      )
      _ <- validatePreTyperOwner(owner.originalRoot)
    yield descriptor

  private def rawGraphHasUnavailableField(value: Any): Boolean = value match
    case null => true
    case _: dotty.tools.dotc.ast.Trees.Lazy[?] => true
    case tree: untpd.Tree => tree.source == null || rawChildren(tree).exists(rawGraphHasUnavailableField)
    case modifiers: untpd.Modifiers => modifiers.productIterator.exists(rawGraphHasUnavailableField)
    case modifier: untpd.Mod => modifier.source == null
    case values: Iterable[?] => values.iterator.exists(rawGraphHasUnavailableField)
    case _ => false

  private def validatePreTyperOwner(root: untpd.TypeDef)(using Context): Either[Error, Unit] =
    val builder = Vector.newBuilder[untpd.Tree]
    def visit(value: Any): Unit = value match
      case tree: untpd.Tree =>
        builder += tree
        rawChildren(tree).foreach(visit)
      case modifiers: untpd.Modifiers => modifiers.productIterator.foreach(visit)
      case values: Iterable[?] => values.foreach(visit)
      case _ => ()
    visit(root)
    val graph = builder.result()
    if graph.exists(_.isInstanceOf[untpd.TypedSplice]) then
      Left(Error("TYPED_SPLICE_OWNER_GRAPH", "the original owner contains TypedSplice."))
    else if graph.exists(_.symbol != NoSymbol) then
      Left(Error("SYMBOL_BEARING_OWNER_GRAPH", "the original owner contains a symbol-bearing tree."))
    else Right(())

  // Compiler tree products omit definition/function modifiers. Inspect their raw
  // children explicitly; do not descend arbitrary Products such as Constant(null).
  private def rawChildren(tree: untpd.Tree): Iterator[Any] =
    val extra = tree match
      case function: untpd.FunctionWithMods => Iterator(function.mods, function.erasedParams)
      case definition: untpd.DefTree => Iterator(definition.mods)
      case _ => Iterator.empty
    tree.productIterator ++ extra
