package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.terms.{ConstructedTerm, TermBinder, TermBindingFailure, TermBindingInternals}
import quasiquotes.terms.dotty.ConstructedTermUntypedBackend

/** Preparation-only semantic RHS fragments for a later same-descriptor reconstruction. */
private[quasiquotes] object ExistingUntpdOrdinaryMethodRhsEditPreparation:
  import ExistingUntpdOrdinaryMethodDescriptor.{Descriptor, Parameter}
  import ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily

  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  final class ReplacementScope private[dotty] (
      private[dotty] val descriptorIdentity: Descriptor,
      private[dotty] val parameters: TermBindingInternals.PersistentParameters,
      private[dotty] val parameterNames: Vector[Vector[String]],
      private[dotty] val binders: Vector[Vector[TermBinder]]
  ):
    private var active = true

    def parameterBinders: Vector[Vector[TermBinder]] = binders

    def reference(parameter: Parameter): Either[Error, TermShape] =
      if !active then
        Left(error("REPLACEMENT_SCOPE_INACTIVE", "the replacement scope is no longer active."))
      else
        parameterPosition(descriptorIdentity, parameter).flatMap { case (clause, index) =>
          parameters.referenceAt(clause, index)
            .left.map(coreFailure("CORE_PARAMETER_REFERENCE_FAILED", _))
        }

    private[dotty] def close(): Unit = active = false

  private[dotty] final case class Evidence(
      descriptor: Descriptor,
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int,
      method: untpd.DefDef,
      oldRhs: untpd.Tree,
      scope: ReplacementScope,
      semanticReplacement: TermShape,
      parameterBinders: Vector[Vector[TermBinder]],
      loweredReplacement: untpd.Tree,
      replacementFamily: ReplacementFamily,
      positionedReplacement: untpd.Tree
  )

  final case class PreparedBody private[dotty] (
      descriptor: Descriptor,
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int,
      method: untpd.DefDef,
      oldRhs: untpd.Tree,
      semanticReplacement: TermShape,
      parameterBinders: Vector[Vector[TermBinder]],
      loweredReplacement: untpd.Tree,
      replacementFamily: ReplacementFamily,
      positionedReplacement: untpd.Tree,
      private[dotty] val replacementScope: ReplacementScope,
      private[dotty] val evidence: Evidence
  )

  def prepareBody(
      descriptor: Descriptor
  )(
      build: ReplacementScope => Either[Error, TermShape]
  )(using Context): Either[Error, PreparedBody] =
    for
      validDescriptor <- validateDescriptor(descriptor)
      oldRhs <- Option(validDescriptor.rhs).filterNot(_.isEmpty).toRight(
        error("OLD_RHS_SITE_REQUIRED", "the old RHS was null or EmptyTree.")
      )
      _ <- requireSite(oldRhs)
      names <- exactParameterNames(validDescriptor)
      _ <- rejectDuplicateNames(names)
      parameters <- TermBindingInternals.persistentParameters(names)
        .left.map(coreFailure("CORE_PARAMETER_SCOPE_FAILED", _))
      binders <- parameterBinders(parameters, names)
        .left.map(coreFailure("CORE_PARAMETER_SCOPE_FAILED", _))
      scope = new ReplacementScope(validDescriptor, parameters, names, binders)
      built <- invokeBuilder(build, scope)
      admitted <- admitReplacement(built, names.flatten.toSet)
      completed <- parameters.complete(admitted)
        .left.map(bodyFailure)
      checked <- parameters.validateDefinitionBody(names.map(_.size), completed)
        .left.map(bodyFailure)
      bindings <- binderBindings(parameters, names)
      constructed <- ConstructedTerm.fromShapeInScope(checked, bindings.map(_._1))
        .left.map(problem => error("TERM_CONSTRUCTION_FAILED", problem.message))
      lowered <- ConstructedTermUntypedBackend.lowerInScopes(constructed, bindings)
        .left.map(problem => error("SOURCE_FREE_TERM_LOWERING_FAILED", problem.message))
      _ <- validateLowered(lowered, checked)
      family <- ExistingUntpdSingleParameterMethodRhsRewriter.classify(lowered, "U041")
        .left.map(problem => error(problem.code, problem.detail))
      positioned <- ExistingUntpdMethodBodyRewriteOriginAdapter
        .prepareReplacement(lowered, oldRhs, family)
        .left.map(problem => error("ORIGIN_POSITIONING_FAILED", problem.message))
      _ <- validatePositioned(positioned, lowered, checked, oldRhs)
      evidence = Evidence(
        validDescriptor,
        validDescriptor.captured,
        validDescriptor.memberIndex,
        validDescriptor.method,
        oldRhs,
        scope,
        checked,
        binders,
        lowered,
        family,
        positioned
      )
      prepared = PreparedBody(
        validDescriptor,
        validDescriptor.captured,
        validDescriptor.memberIndex,
        validDescriptor.method,
        oldRhs,
        checked,
        binders,
        lowered,
        family,
        positioned,
        scope,
        evidence
      )
      _ <- validatePreparedBody(prepared, validDescriptor)
    yield prepared

  private[dotty] def validatePreparedBody(
      prepared: PreparedBody,
      expectedDescriptor: Descriptor
  )(using Context): Either[Error, Unit] =
    val failure = error(
      "FINAL_BODY_PREPARATION_INVARIANT_FAILED",
      "the prepared body no longer matches its exact descriptor, persistent parameter graph, semantic fragment, source-free lowering, family, or positioned replacement evidence."
    )
    (for
      value <- Option(prepared).toRight(failure)
      expected <- Option(expectedDescriptor).toRight(failure)
      evidence <- Option(value.evidence).toRight(failure)
      scope <- Option(value.replacementScope).toRight(failure)
      _ <- validateDescriptor(expected).left.map(_ => failure)
      _ <- validateDescriptor(value.descriptor).left.map(_ => failure)
      _ <- Either.cond(value.descriptor.eq(expected), (), failure)
      names <- exactParameterNames(expected).left.map(_ => failure)
      expectedBinders <- parameterBinders(scope.parameters, names).left.map(_ => failure)
      _ <- Either.cond(
        scope.descriptorIdentity.eq(expected) &&
          scope.parameterNames == names &&
          scope.binders == expectedBinders &&
          value.parameterBinders == expectedBinders,
        (),
        failure
      )
      _ <- scope.parameters.validateDefinitionBody(names.map(_.size), value.semanticReplacement)
        .left.map(_ => failure)
      _ <- admitReplacement(value.semanticReplacement, names.flatten.toSet).left.map(_ => failure)
      _ <- validateLowered(value.loweredReplacement, value.semanticReplacement).left.map(_ => failure)
      family <- ExistingUntpdSingleParameterMethodRhsRewriter
        .classify(value.loweredReplacement, "U041")
        .left.map(_ => failure)
      _ <- Either.cond(family == value.replacementFamily, (), failure)
      _ <- validatePositioned(
        value.positionedReplacement,
        value.loweredReplacement,
        value.semanticReplacement,
        value.oldRhs
      ).left.map(_ => failure)
      _ <- Either.cond(carrierMatches(value, evidence), (), failure)
      _ <- Either.cond(
        value.captured.eq(expected.captured) &&
          value.memberIndex == expected.memberIndex &&
          value.method.eq(expected.method) &&
          value.oldRhs.eq(expected.rhs),
        (),
        failure
      )
    yield ()).left.map(_ => failure)

  private def invokeBuilder(
      build: ReplacementScope => Either[Error, TermShape],
      scope: ReplacementScope
  ): Either[Error, TermShape] =
    if build == null then
      scope.close()
      Left(error("REPLACEMENT_BUILDER_REQUIRED", "the replacement builder was null."))
    else
      try
        Option(build(scope))
          .toRight(error("REPLACEMENT_BUILDER_RESULT_REQUIRED", "the replacement builder returned no result."))
          .flatMap(identity)
          .flatMap(shape => Option(shape).toRight(
            error("SEMANTIC_REPLACEMENT_REQUIRED", "the replacement builder returned a null TermShape.")
          ))
      finally scope.close()

  private def admitReplacement(
      shape: TermShape,
      parameterNames: Set[String]
  ): Either[Error, TermShape] =
    Option(shape).toRight(
      error("SEMANTIC_REPLACEMENT_REQUIRED", "the semantic replacement was null.")
    ).flatMap {
      case leaf @ (_: TermShape.BoundReference | _: TermShape.Identifier | _: TermShape.Literal) =>
        admitLeaf(leaf, parameterNames)
      case apply @ TermShape.Apply(function, arguments) =>
        Option(arguments).toRight(
          error("SEMANTIC_APPLY_ARGUMENTS_REQUIRED", "the semantic Apply argument sequence was null.")
        ).flatMap { presentArguments =>
          if presentArguments.isEmpty || presentArguments.size > 3 then
            Left(error(
              "SEMANTIC_APPLY_ARGUMENT_COUNT_UNSUPPORTED",
              s"U041 admits 1..3 Apply arguments; found ${presentArguments.size}."
            ))
          else
            for
              _ <- function match
                case identifier: TermShape.Identifier =>
                  admitFreeIdentifier(identifier, parameterNames, "Apply function").map(_ => ())
                case TermShape.Select(qualifier: TermShape.Identifier, selectedName) =>
                  for
                    _ <- admitFreeIdentifier(qualifier, parameterNames, "selected Apply qualifier")
                    _ <- validateIdentifierName(selectedName, "selected member")
                  yield ()
                case other => Left(error(
                  "SEMANTIC_APPLY_FUNCTION_UNSUPPORTED",
                  s"U041 requires a direct Identifier or direct-Identifier-qualified Select; found ${nodeKind(other)}."
                ))
              _ <- presentArguments.zipWithIndex.foldLeft[Either[Error, Unit]](Right(())) {
                case (result, (argument, index)) =>
                  result.flatMap(_ => admitLeaf(argument, parameterNames).left.map(problem =>
                    if problem.code == "SEMANTIC_REPLACEMENT_FAMILY_UNSUPPORTED" then
                      error("SEMANTIC_APPLY_LEAF_REQUIRED", s"argument $index must be an admitted leaf; ${problem.detail}")
                    else problem
                  ).map(_ => ()))
              }
            yield apply
        }
      case other => Left(error(
        "SEMANTIC_REPLACEMENT_FAMILY_UNSUPPORTED",
        s"U041 admits only a leaf, direct-Identifier Apply, or direct-Identifier-qualified selected Apply; found ${nodeKind(other)}."
      ))
    }

  private def admitLeaf(
      shape: TermShape,
      parameterNames: Set[String]
  ): Either[Error, TermShape] =
    shape match
      case reference: TermShape.BoundReference => Right(reference)
      case identifier: TermShape.Identifier =>
        admitFreeIdentifier(identifier, parameterNames, "free identifier")
      case literal @ TermShape.Literal(value) =>
        Option(value) match
          case Some("true" | "false") => Right(literal)
          case Some(number) if number.matches("-?[0-9]+") => Right(literal)
          case Some(string) if string.length >= 2 && string.head == '"' && string.last == '"' =>
            Right(literal)
          case _ => Left(error(
            "SEMANTIC_LITERAL_UNSUPPORTED",
            s"U041 admits decimal integer, Boolean, and String literals; found ${String.valueOf(value)}."
          ))
      case other => Left(error(
        "SEMANTIC_REPLACEMENT_FAMILY_UNSUPPORTED",
        s"the semantic node ${nodeKind(other)} is not an admitted leaf."
      ))

  private def admitFreeIdentifier(
      identifier: TermShape.Identifier,
      parameterNames: Set[String],
      label: String
  ): Either[Error, TermShape.Identifier] =
    if identifier.isPlaceholder then
      Left(error("PLACEHOLDER_IDENTIFIER_UNSUPPORTED", s"the $label cannot be a placeholder."))
    else
      validateIdentifierName(identifier.name, label).flatMap { _ =>
        Either.cond(
          !parameterNames(identifier.name),
          identifier,
          error(
            "FREE_IDENTIFIER_PARAMETER_CAPTURE",
            s"free unqualified identifier `${identifier.name}` would be captured by the existing method parameter."
          )
        )
      }

  private def validateIdentifierName(value: String, label: String): Either[Error, Unit] =
    Option(value).toRight(error("SEMANTIC_IDENTIFIER_INVALID", s"the $label name was null."))
      .flatMap(name =>
        quasiquotes.definitions.DefinitionName.fromSource(name)
          .left.map(_ => error("SEMANTIC_IDENTIFIER_INVALID", s"the $label name `$name` is not admitted."))
          .map(_ => ())
      )

  private def validateDescriptor(
      descriptor: Descriptor
  )(using Context): Either[Error, Descriptor] =
    Option(descriptor)
      .toRight(error("INVALID_DESCRIPTOR", "the U037 descriptor was null."))
      .flatMap { present =>
        ExistingUntpdOrdinaryMethodDescriptor.validate(present)
          .left.map(problem => error(
            "INVALID_DESCRIPTOR",
            s"U037 descriptor validation failed before body preparation: ${problem.message}"
          ))
          .map(_ => present)
      }

  private def exactParameterNames(
      descriptor: Descriptor
  ): Either[Error, Vector[Vector[String]]] =
    Option(descriptor.parameterClauses)
      .toRight(error("MALFORMED_PARAMETER_TOPOLOGY", "the descriptor parameter clauses were null."))
      .flatMap { clauses =>
        clauses.zipWithIndex.foldLeft[Either[Error, Vector[Vector[String]]]](Right(Vector.empty)) {
          case (collectedClauses, (clause, clauseIndex)) =>
            for
              collected <- collectedClauses
              presentClause <- Option(clause).toRight(error(
                "MALFORMED_PARAMETER_TOPOLOGY",
                s"parameter clause $clauseIndex was null."
              ))
              names <- presentClause.zipWithIndex.foldLeft[Either[Error, Vector[String]]](Right(Vector.empty)) {
                case (collectedNames, (parameter, parameterIndex)) =>
                  for
                    existing <- collectedNames
                    tree <- Option(parameter).flatMap(value => Option(value.tree)).toRight(error(
                      "MALFORMED_PARAMETER_TOPOLOGY",
                      s"parameter $clauseIndex/$parameterIndex was null or had no ValDef."
                    ))
                    name = tree.name.toString
                    _ <- Either.cond(
                      name.nonEmpty,
                      (),
                      error("MALFORMED_PARAMETER_TOPOLOGY", s"parameter $clauseIndex/$parameterIndex had an empty spelling.")
                    )
                  yield existing :+ name
              }
            yield collected :+ names
        }
      }

  private def rejectDuplicateNames(
      names: Vector[Vector[String]]
  ): Either[Error, Unit] =
    val duplicates = names.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
      .collect { case (name, count) if count > 1 => name }
      .toVector.sorted
    Either.cond(
      duplicates.isEmpty,
      (),
      error(
        "AMBIGUOUS_PARAMETER_NAME",
        s"duplicate captured parameter spelling(s) ${duplicates.mkString(", ")} cannot define one lexical replacement scope."
      )
    )

  private def parameterPosition(
      descriptor: Descriptor,
      parameter: Parameter
  ): Either[Error, (Int, Int)] =
    Option(parameter).toRight(error(
      "PARAMETER_REQUIRED",
      "the capture-local U037 parameter wrapper was null."
    )).flatMap { selected =>
      val positions = descriptor.parameterClauses.zipWithIndex.flatMap { (clause, clauseIndex) =>
        clause.zipWithIndex.collect {
          case (candidate, parameterIndex) if candidate != null && candidate.eq(selected) =>
            clauseIndex -> parameterIndex
        }
      }
      positions match
        case Vector(position) => Right(position)
        case Vector() => Left(error(
          "PARAMETER_NOT_CAPTURED",
          "the supplied parameter wrapper does not belong to the exact descriptor wrapper."
        ))
        case other => Left(error(
          "PARAMETER_OCCURRENCE_NOT_UNIQUE",
          s"the supplied parameter wrapper occurred ${other.size} times; exactly one occurrence is required."
        ))
    }

  private def parameterBinders(
      parameters: TermBindingInternals.PersistentParameters,
      names: Vector[Vector[String]]
  ): Either[TermBindingFailure, Vector[Vector[TermBinder]]] =
    names.zipWithIndex.foldLeft[Either[TermBindingFailure, Vector[Vector[TermBinder]]]](
      Right(Vector.empty)
    ) { case (collectedClauses, (clause, clauseIndex)) =>
      for
        collected <- collectedClauses
        binders <- clause.indices.foldLeft[Either[TermBindingFailure, Vector[TermBinder]]](
          Right(Vector.empty)
        ) { (collectedBinders, parameterIndex) =>
          for
            existing <- collectedBinders
            binder <- parameters.binderAt(clauseIndex, parameterIndex)
          yield existing :+ binder
        }
      yield collected :+ binders
    }

  private def binderBindings(
      parameters: TermBindingInternals.PersistentParameters,
      names: Vector[Vector[String]]
  ): Either[Error, Vector[(BinderId, String)]] =
    names.zipWithIndex.flatMap { (clause, clauseIndex) =>
      clause.zipWithIndex.map { (name, parameterIndex) =>
        parameters.referenceAt(clauseIndex, parameterIndex)
          .left.map(coreFailure("CORE_PARAMETER_REFERENCE_FAILED", _))
          .flatMap {
            case TermShape.BoundReference(id, _) => Right(id -> name)
            case _ => Left(error(
              "CORE_PARAMETER_REFERENCE_FAILED",
              "the Core persistent graph returned a non-bound parameter reference."
            ))
          }
      }
    }.foldLeft[Either[Error, Vector[(BinderId, String)]]](Right(Vector.empty)) {
      case (collected, next) =>
        for
          values <- collected
          value <- next
        yield values :+ value
    }

  private def validateLowered(
      lowered: untpd.Tree,
      semantic: TermShape
  )(using Context): Either[Error, Unit] =
    val nodes = safeAllTrees(lowered)
    val valid = Option(lowered).exists(tree =>
      nodes.exists(_.forall(node =>
        node != null && !node.source.exists && !node.span.exists &&
          node.symbol == NoSymbol && !node.isInstanceOf[untpd.TypedSplice]
      )) && semanticMatchesRaw(semantic, tree)
    )
    Either.cond(
      valid,
      (),
      error("SOURCE_FREE_REPLACEMENT_INVALID", "the scoped lowerer did not return the exact admitted source-free semantic topology.")
    )

  private def validatePositioned(
      positioned: untpd.Tree,
      lowered: untpd.Tree,
      semantic: TermShape,
      oldRhs: untpd.Tree
  )(using Context): Either[Error, Unit] =
    val loweredNodes = safeAllTrees(lowered)
    val positionedNodes = safeAllTrees(positioned)
    val valid = for
      before <- loweredNodes
      after <- positionedNodes
    yield before.size == after.size &&
      before.zip(after).forall((left, right) => !left.eq(right)) &&
      semanticMatchesRaw(semantic, positioned) &&
      after.forall(tree =>
        tree.source == oldRhs.source && tree.span == oldRhs.span &&
          tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
      )
    Either.cond(
      valid.contains(true),
      (),
      error("ORIGIN_POSITIONING_FAILED", "the positioned replacement did not freshly and uniformly occupy the exact old RHS site.")
    )

  private def semanticMatchesRaw(shape: TermShape, raw: untpd.Tree): Boolean =
    (shape, raw) match
      case (TermShape.BoundReference(_, displayName), ident: untpd.Ident) =>
        ident.name.toString == displayName
      case (TermShape.Identifier(name, false), ident: untpd.Ident) =>
        ident.name.toString == name
      case (TermShape.Literal(value), number: untpd.Number) =>
        number.digits == value
      case (TermShape.Literal("true"), literal: untpd.Literal) =>
        literal.const.value == true
      case (TermShape.Literal("false"), literal: untpd.Literal) =>
        literal.const.value == false
      case (TermShape.Literal(value), literal: untpd.Literal)
          if value.length >= 2 && value.head == '"' && value.last == '"' =>
        literal.const.value == value.substring(1, value.length - 1)
      case (TermShape.Select(qualifier, name), selection: untpd.Select) =>
        selection.name.toString == name && semanticMatchesRaw(qualifier, selection.qualifier)
      case (TermShape.Apply(function, arguments), application: untpd.Apply) =>
        Option(arguments).exists(args =>
          args.size == application.args.size &&
            semanticMatchesRaw(function, application.fun) &&
            args.zip(application.args).forall(semanticMatchesRaw)
        )
      case _ => false

  private def carrierMatches(
      value: PreparedBody,
      evidence: Evidence
  ): Boolean =
    value.descriptor.eq(evidence.descriptor) &&
      value.captured.eq(evidence.captured) &&
      value.memberIndex == evidence.memberIndex &&
      value.method.eq(evidence.method) &&
      value.oldRhs.eq(evidence.oldRhs) &&
      value.replacementScope.eq(evidence.scope) &&
      value.semanticReplacement.asInstanceOf[AnyRef].eq(evidence.semanticReplacement.asInstanceOf[AnyRef]) &&
      value.parameterBinders == evidence.parameterBinders &&
      value.loweredReplacement.eq(evidence.loweredReplacement) &&
      value.replacementFamily == evidence.replacementFamily &&
      value.positionedReplacement.eq(evidence.positionedReplacement)

  private def requireSite(oldRhs: untpd.Tree): Either[Error, Unit] =
    Either.cond(
      Option(oldRhs.source).exists(_.exists) && oldRhs.span.exists,
      (),
      error("OLD_RHS_SITE_REQUIRED", "the old RHS must provide both source and span.")
    )

  private def safeAllTrees(tree: untpd.Tree)(using Context): Option[Vector[untpd.Tree]] =
    try Option(tree).map(ExistingUntpdClassMemberFilter.allTrees)
    catch case _: NullPointerException => None

  private def coreFailure(code: String, problem: TermBindingFailure): Error =
    error(code, problem.message)

  private def bodyFailure(problem: TermBindingFailure): Error =
    if problem.code == "TERM_BINDER_SCOPE_MISMATCH" then
      error("FOREIGN_BINDER_GRAPH", problem.message)
    else coreFailure("CORE_BODY_VALIDATION_FAILED", problem)

  private def nodeKind(value: Any): String =
    Option(value).fold("null")(_.getClass.getSimpleName)

  private def error(code: String, detail: String): Error = Error(code, detail)
