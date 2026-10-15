package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.StdNames.nme

/** Read-only ordered handles over the accepted ordinary method view families. */
private[quasiquotes] object ExistingUntpdOrdinaryMethodDescriptor:
  final case class Parameter private[dotty] (
      tree: untpd.ValDef,
      tpt: untpd.Tree,
      diagnosticName: String
  )

  final case class OpaqueTypeClause private[dotty] (
      rawClause: untpd.ParamClause,
      rawClauseOrdinal: Int,
      typeParameter: untpd.TypeDef,
      bounds: untpd.TypeBoundsTree,
      rawTermClauseOrdinal: Int
  )

  final case class Descriptor private[dotty] (
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int,
      method: untpd.DefDef,
      diagnosticMethodName: String,
      parameterClauses: Vector[Vector[Parameter]],
      resultType: untpd.Tree,
      rhs: untpd.Tree,
      opaqueTypeClause: Option[OpaqueTypeClause] = None
  )

  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  final class ClauseKind private (val code: String) derives CanEqual

  object ClauseKind:
    val Ordinary: ClauseKind = new ClauseKind("ORDINARY")
    val Contextual: ClauseKind = new ClauseKind("CONTEXTUAL")

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
        val opaqueTypeClauseMatches = (value.opaqueTypeClause, expected.opaqueTypeClause) match
          case (None, None) => true
          case (Some(actual), Some(original)) =>
            actual.rawClauseOrdinal == original.rawClauseOrdinal &&
              actual.rawTermClauseOrdinal == original.rawTermClauseOrdinal &&
              actual.rawClause.eq(original.rawClause) &&
              actual.typeParameter.eq(original.typeParameter) &&
              actual.bounds.eq(original.bounds)
          case _ => false
        Either.cond(
          expected.captured.eq(value.captured) && expected.memberIndex == value.memberIndex &&
            expected.method.eq(value.method) && expected.resultType.eq(value.resultType) &&
            expected.rhs.eq(value.rhs) && parametersMatch && opaqueTypeClauseMatches,
          (),
          failure
        )
      }
    }

  private[dotty] def clauseKinds(
      descriptor: Descriptor
  )(using Context): Either[Error, Vector[ClauseKind]] =
    validate(descriptor).flatMap { _ =>
      descriptor.parameterClauses.foldLeft[Either[Error, Vector[ClauseKind]]](Right(Vector.empty)) {
        case (result, clause) =>
          for
            kinds <- result
            kind <- clauseKind(clause)
          yield kinds :+ kind
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
        case Nil =>
          buildZeroParameterDescriptor(owner, memberIndex, method, Vector.empty)
        case clause :: Nil if clause != null && clause.isEmpty =>
          buildZeroParameterDescriptor(owner, memberIndex, method, Vector(Vector.empty))
        case List(List(parameter: untpd.ValDef))
            if hasExactFlags(parameter, Flags.Param | Flags.Given) =>
          buildContextualUnaryDescriptor(owner, memberIndex, method, None, parameter)
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
        case List(List(first: untpd.ValDef), List(second: untpd.ValDef))
            if first.mods != null && first.mods.flags == Flags.Param &&
              second.mods != null && second.mods.flags == Flags.Param =>
          buildTwoUnaryOrdinaryClauseDescriptor(
            owner,
            memberIndex,
            method,
            first,
            second
          )
        case List(List(first: untpd.ValDef), List(second: untpd.ValDef))
            if hasExactFlags(first, Flags.Param) &&
              hasExactFlags(second, Flags.Param | Flags.Given) =>
          buildContextualUnaryDescriptor(
            owner,
            memberIndex,
            method,
            Some(first),
            second
          )
        case List(typeClause @ List(typeParameter: untpd.TypeDef), List(termParameter: untpd.ValDef)) =>
          buildGenericG1Descriptor(
            owner,
            memberIndex,
            method,
            typeClause,
            typeParameter,
            termParameter
          )
        case _ => Left(Error(
          "UNSUPPORTED_PARAMETER_TOPOLOGY",
          "a parameterless method, one clause of zero, one, or two ordinary parameters, exactly two ordinary unary clauses, one contextual unary clause, or one ordinary unary clause followed by one contextual unary clause is required."
        ))
      // The older single-parameter authority predates the explicit erased/owner guards.
      // Keep that authority unchanged while enforcing this descriptor's pre-Typer boundary.
      _ <- Either.cond(
        !descriptor.parameterClauses.flatten.exists(_.tree.mods.is(Flags.Erased)),
        (),
        Error("CONTEXTUAL_PARAMETER_UNSUPPORTED", "erased parameters are outside ordinary method capture.")
      )
      _ <- validatePreTyperOwner(owner.originalRoot)
    yield descriptor

  private def buildGenericG1Descriptor(
      owner: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int,
      method: untpd.DefDef,
      typeClause: untpd.ParamClause,
      typeParameter: untpd.TypeDef,
      termParameterTree: untpd.Tree
  ): Either[Error, Descriptor] =
    val unsupportedRole =
      method.name == nme.CONSTRUCTOR ||
        !method.mods.is(Flags.Method) ||
        method.mods.is(Flags.Synthetic) ||
        method.mods.is(Flags.Artifact) ||
        method.mods.is(Flags.Accessor) ||
        method.mods.is(Flags.ExtensionMethod) ||
        method.mods.is(Flags.Given) ||
        method.mods.is(Flags.Implicit)
    for
      _ <- Either.cond(
        !unsupportedRole,
        (),
        Error(
          "UNSUPPORTED_METHOD_ROLE",
          "constructors, contextual, synthetic/artifact/accessor, extension, and non-method DefDefs are outside the admitted generic G1 method role."
        )
      )
      _ <- Either.cond(
        typeParameter.mods != null && typeParameter.mods.flags == Flags.Param &&
          typeParameter.name != null && typeParameter.name.isTypeName &&
          typeParameter.name.toString.nonEmpty && hasSourceSite(typeParameter),
        (),
        Error(
          "UNSUPPORTED_GENERIC_TYPE_CLAUSE",
          "the generic G1 method must contain one exact source-positioned Type parameter declaration."
        )
      )
      bounds <- typeParameter.rhs match
        case value: untpd.TypeBoundsTree => Right(value)
        case _ => Left(Error(
          "UNSUPPORTED_GENERIC_TYPE_BOUNDS",
          "the generic G1 Type parameter must retain one exact TypeBoundsTree."
        ))
      _ <- Either.cond(
        bounds.lo.isEmpty && bounds.hi.isEmpty && bounds.alias.isEmpty && hasSourceSite(bounds),
        (),
        Error(
          "UNSUPPORTED_GENERIC_TYPE_BOUNDS",
          "the generic G1 Type parameter must use the exact unbounded parser form."
        )
      )
      _ <- Either.cond(
        termParameterTree.isInstanceOf[untpd.ValDef] &&
          hasExactFlags(termParameterTree.asInstanceOf[untpd.ValDef], Flags.Param) &&
          Option(termParameterTree.asInstanceOf[untpd.ValDef].unforcedRhs.asInstanceOf[untpd.Tree])
            .exists(_.isEmpty),
        (),
        Error(
          "UNSUPPORTED_GENERIC_TERM_CLAUSE",
          "the generic G1 family requires one exact ordinary unary term clause."
        )
      )
      termParameter <- ordinaryUnaryParameter(termParameterTree, "generic G1 term")
      _ <- Either.cond(
        termParameter.name.toString != typeParameter.name.toString,
        (),
        Error(
          "GENERIC_BINDER_ALIAS_UNSUPPORTED",
          "the generic G1 Type and term binders must remain distinct."
        )
      )
      _ <- directAdmittedPrimitiveType(
        termParameter.tpt,
        typeParameter.name.toString,
        "generic G1 term parameter type"
      )
      _ <- directAdmittedPrimitiveType(
        method.tpt,
        typeParameter.name.toString,
        "generic G1 result type"
      )
      rhs <- Option(method.unforcedRhs.asInstanceOf[untpd.Tree]).filterNot(_.isEmpty).filter(hasSourceSite).toRight(
        Error("RHS_REQUIRED", "the generic G1 method RHS must be nonempty and retain its source site.")
      )
    yield Descriptor(
      owner,
      memberIndex,
      method,
      method.name.toString,
      Vector(Vector(Parameter(termParameter, termParameter.tpt, termParameter.name.toString))),
      method.tpt,
      rhs,
      Some(OpaqueTypeClause(typeClause, 0, typeParameter, bounds, 1))
    )

  private def directAdmittedPrimitiveType(
      tree: untpd.Tree,
      localTypeBinderName: String,
      path: String
  ): Either[Error, Unit] =
    Option(tree).filterNot(_.isEmpty) match
      case Some(untpd.Ident(name))
          if name.isTypeName && name.toString != localTypeBinderName &&
            Set("Int", "String", "Boolean", "AnyVal").contains(name.toString) =>
        Right(())
      case _ => Left(Error(
        "GENERIC_TYPE_DEPENDENCY_UNSUPPORTED",
        s"$path must be one direct admitted primitive Type independent of the local method Type binder."
      ))

  private def buildTwoUnaryOrdinaryClauseDescriptor(
      owner: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int,
      method: untpd.DefDef,
      firstTree: untpd.Tree,
      secondTree: untpd.Tree
  ): Either[Error, Descriptor] =
    val unsupportedRole =
      method.name == nme.CONSTRUCTOR ||
        !method.mods.is(Flags.Method) ||
        method.mods.is(Flags.Synthetic) ||
        method.mods.is(Flags.Artifact) ||
        method.mods.is(Flags.Accessor) ||
        method.mods.is(Flags.ExtensionMethod) ||
        method.mods.is(Flags.Given) ||
        method.mods.is(Flags.Implicit)
    for
      _ <- Either.cond(
        !unsupportedRole,
        (),
        Error(
          "UNSUPPORTED_METHOD_ROLE",
          "constructors, contextual, synthetic/artifact/accessor, extension, and non-method DefDefs are outside the admitted ordinary-method role."
        )
      )
      first <- ordinaryUnaryParameter(firstTree, "first")
      second <- ordinaryUnaryParameter(secondTree, "second")
      _ <- Either.cond(
        first.name != second.name,
        (),
        Error(
          "DUPLICATE_PARAMETER_NAME",
          "the two ordinary unary clauses must use distinct parameter names for unambiguous lexical capture."
        )
      )
      resultType <- Option(method.tpt).filterNot(_.isEmpty).filter(hasSourceSite).toRight(
        Error("RESULT_TYPE_REQUIRED", "the method result type must be nonempty and retain its source site.")
      )
      rhs <- Option(method.unforcedRhs.asInstanceOf[untpd.Tree]).filterNot(_.isEmpty).filter(hasSourceSite).toRight(
        Error("RHS_REQUIRED", "the method RHS must be nonempty and retain its source site.")
      )
    yield Descriptor(
      owner,
      memberIndex,
      method,
      method.name.toString,
      Vector(
        Vector(Parameter(first, first.tpt, first.name.toString)),
        Vector(Parameter(second, second.tpt, second.name.toString))
      ),
      resultType,
      rhs
    )

  private def buildContextualUnaryDescriptor(
      owner: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int,
      method: untpd.DefDef,
      ordinaryTree: Option[untpd.ValDef],
      contextualTree: untpd.ValDef
  ): Either[Error, Descriptor] =
    val unsupportedRole =
      method.name == nme.CONSTRUCTOR ||
        !method.mods.is(Flags.Method) ||
        method.mods.is(Flags.Synthetic) ||
        method.mods.is(Flags.Artifact) ||
        method.mods.is(Flags.Accessor) ||
        method.mods.is(Flags.ExtensionMethod) ||
        method.mods.is(Flags.Given) ||
        method.mods.is(Flags.Implicit)
    for
      _ <- Either.cond(
        !unsupportedRole,
        (),
        Error(
          "UNSUPPORTED_METHOD_ROLE",
          "constructors, contextual, synthetic/artifact/accessor, extension, and non-method DefDefs are outside the admitted ordinary-method role."
        )
      )
      ordinary <- ordinaryTree match
        case Some(parameter) => ordinaryUnaryParameter(parameter, "ordinary")
          .map(Some(_))
        case None => Right(None)
      contextual <- contextualUnaryParameter(contextualTree)
      _ <- Either.cond(
        ordinary.forall(_.name != contextual.name),
        (),
        Error(
          "DUPLICATE_PARAMETER_NAME",
          "ordinary and contextual unary clauses must use distinct parameter names for unambiguous lexical capture."
        )
      )
      resultType <- Option(method.tpt).filterNot(_.isEmpty).filter(hasSourceSite).toRight(
        Error("RESULT_TYPE_REQUIRED", "the method result type must be nonempty and retain its source site.")
      )
      rhs <- Option(method.unforcedRhs.asInstanceOf[untpd.Tree]).filterNot(_.isEmpty).filter(hasSourceSite).toRight(
        Error("RHS_REQUIRED", "the method RHS must be nonempty and retain its source site.")
      )
    yield Descriptor(
      owner,
      memberIndex,
      method,
      method.name.toString,
      ordinary.toVector.map(parameter => Vector(Parameter(parameter, parameter.tpt, parameter.name.toString))) :+
        Vector(Parameter(contextual, contextual.tpt, contextual.name.toString)),
      resultType,
      rhs
    )

  private def ordinaryUnaryParameter(
      tree: untpd.Tree,
      position: String
  ): Either[Error, untpd.ValDef] =
    for
      parameter <- Option(tree).filterNot(_.isEmpty) match
        case Some(value: untpd.ValDef) => Right(value)
        case _ => Left(Error("PARAMETER_REQUIRED", s"the $position unary-clause parameter was absent or not a value parameter."))
      _ <- Either.cond(
        parameter.mods != null && parameter.mods.flags == Flags.Param,
        (),
        Error("UNSUPPORTED_PARAMETER_MODIFIERS", s"the $position unary-clause parameter was not one unmodified ordinary value parameter.")
      )
      parameterType <- Option(parameter.tpt).filterNot(_.isEmpty).filter(hasSourceSite).toRight(
        Error("PARAMETER_TYPE_REQUIRED", s"the $position unary-clause parameter type must be nonempty and retain its source site.")
      )
      _ <- Either.cond(
        !parameterType.isInstanceOf[untpd.ByNameTypeTree] &&
          !parameterType.isInstanceOf[untpd.PostfixOp],
        (),
        Error("UNSUPPORTED_PARAMETER_MODE", s"the $position unary-clause parameter may not be by-name or repeated.")
      )
      parameterRhs <- Option(parameter.unforcedRhs.asInstanceOf[untpd.Tree]).toRight(
        Error("PARAMETER_REQUIRED", s"the $position unary-clause parameter RHS field was absent.")
      )
      _ <- Either.cond(
        parameterRhs.isEmpty,
        (),
        Error("PARAMETER_RHS_UNEXPECTED", s"the $position unary-clause parameter carried a default or other RHS.")
      )
      _ <- Either.cond(
        parameter.name != null && parameter.name.toString.nonEmpty && hasSourceSite(parameter),
        (),
        Error("PARAMETER_SITE_REQUIRED", s"the $position unary-clause parameter must retain a name, source, and span.")
      )
    yield parameter

  private def contextualUnaryParameter(
      tree: untpd.Tree
  ): Either[Error, untpd.ValDef] =
    for
      parameter <- Option(tree).filterNot(_.isEmpty) match
        case Some(value: untpd.ValDef) => Right(value)
        case _ => Left(Error("PARAMETER_REQUIRED", "the contextual unary-clause parameter was absent or not a value parameter."))
      _ <- Either.cond(
        hasExactFlags(parameter, Flags.Param | Flags.Given),
        (),
        Error("UNSUPPORTED_PARAMETER_MODIFIERS", "the contextual unary-clause parameter was not one exact Given value parameter.")
      )
      parameterType <- Option(parameter.tpt).filterNot(_.isEmpty).filter(hasSourceSite).toRight(
        Error("PARAMETER_TYPE_REQUIRED", "the contextual unary-clause parameter type must be nonempty and retain its source site.")
      )
      _ <- Either.cond(
        !parameterType.isInstanceOf[untpd.ByNameTypeTree] &&
          !parameterType.isInstanceOf[untpd.PostfixOp],
        (),
        Error("UNSUPPORTED_PARAMETER_MODE", "the contextual unary-clause parameter may not be by-name or repeated.")
      )
      parameterRhs <- Option(parameter.unforcedRhs.asInstanceOf[untpd.Tree]).toRight(
        Error("PARAMETER_REQUIRED", "the contextual unary-clause parameter RHS field was absent.")
      )
      _ <- Either.cond(
        parameterRhs.isEmpty,
        (),
        Error("PARAMETER_RHS_UNEXPECTED", "the contextual unary-clause parameter carried a default or other RHS.")
      )
      _ <- Either.cond(
        parameter.name != null && parameter.name.toString.nonEmpty && hasSourceSite(parameter),
        (),
        Error("PARAMETER_SITE_REQUIRED", "the contextual unary-clause parameter must retain a name, source, and span.")
      )
    yield parameter

  private def clauseKind(clause: Vector[Parameter]): Either[Error, ClauseKind] =
    if clause.forall(parameter => hasExactFlags(parameter.tree, Flags.Param)) then
      Right(ClauseKind.Ordinary)
    else if clause.nonEmpty && clause.forall(parameter =>
        hasExactFlags(parameter.tree, Flags.Param | Flags.Given)
      ) then
      Right(ClauseKind.Contextual)
    else
      Left(Error(
        "DESCRIPTOR_IDENTITY_INVARIANT_FAILED",
        "the descriptor parameter clause no longer has one homogeneous validated calling mode."
      ))

  private def hasExactFlags(parameter: untpd.ValDef, expected: Flags.FlagSet): Boolean =
    parameter.mods != null && parameter.mods.flags == expected

  private def hasSourceSite(tree: untpd.Tree): Boolean =
    tree.source != null && tree.source.exists && tree.span.exists

  private def buildZeroParameterDescriptor(
      owner: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int,
      method: untpd.DefDef,
      parameterClauses: Vector[Vector[Parameter]]
  ): Either[Error, Descriptor] =
    val unsupportedRole =
      method.name == nme.CONSTRUCTOR ||
        !method.mods.is(Flags.Method) ||
        method.mods.is(Flags.Synthetic) ||
        method.mods.is(Flags.Artifact) ||
        method.mods.is(Flags.Accessor) ||
        method.mods.is(Flags.ExtensionMethod) ||
        method.mods.is(Flags.Given) ||
        method.mods.is(Flags.Implicit)
    for
      _ <- Either.cond(
        !unsupportedRole,
        (),
        Error(
          "UNSUPPORTED_METHOD_ROLE",
          "constructors, contextual, synthetic/artifact/accessor, extension, and non-method DefDefs are outside the admitted ordinary-method role."
        )
      )
      resultType <- Option(method.tpt).filterNot(_.isEmpty).toRight(
        Error("RESULT_TYPE_REQUIRED", "the method result type was null or EmptyTree.")
      )
      rhs <- Option(method.unforcedRhs.asInstanceOf[untpd.Tree]).filterNot(_.isEmpty).toRight(
        Error("RHS_REQUIRED", "the method RHS was null or EmptyTree.")
      )
    yield Descriptor(
      owner,
      memberIndex,
      method,
      method.name.toString,
      parameterClauses,
      resultType,
      rhs
    )

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
