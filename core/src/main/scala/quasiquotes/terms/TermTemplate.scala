package quasiquotes.terms

import quasiquotes.parser.TermShape
import quasiquotes.source.GeneratedHoleIndex
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

private[quasiquotes] final case class TermHoleOccurrence(
    name: String,
    identifierOrdinal: Int
) derives CanEqual

private sealed trait SemanticTermKey derives CanEqual

private object SemanticTermKey:
  final case class Identifier(name: String) extends SemanticTermKey
  final case class TermHole(name: String) extends SemanticTermKey
  final case class Literal(value: String) extends SemanticTermKey
  final case class Select(qualifier: SemanticTermKey, name: String)
      extends SemanticTermKey
  final case class Apply(
      function: SemanticTermKey,
      arguments: Vector[SemanticTermKey]
  ) extends SemanticTermKey
  final case class Infix(
      left: SemanticTermKey,
      operator: String,
      right: SemanticTermKey
  ) extends SemanticTermKey
  final case class Unary(operator: String, operand: SemanticTermKey)
      extends SemanticTermKey
  final case class InterpolatedString(
      prefix: String,
      parts: Vector[String],
      arguments: Vector[SemanticTermKey]
  ) extends SemanticTermKey
  final case class Typed(
      expression: SemanticTermKey,
      ascription: TypeTemplate
  ) extends SemanticTermKey
  final case class Tuple(elements: Vector[SemanticTermKey])
      extends SemanticTermKey
  final case class If(
      condition: SemanticTermKey,
      thenBranch: SemanticTermKey,
      elseBranch: SemanticTermKey
  ) extends SemanticTermKey
  final case class Parenthesized(expression: SemanticTermKey)
      extends SemanticTermKey
  final case class Unsupported(nodeKind: String, detail: String)
      extends SemanticTermKey

private[quasiquotes] final class TermTemplate private (
    val root: TermShape,
    val termHoleIndex: GeneratedHoleIndex,
    val termHoleOccurrences: Vector[TermHoleOccurrence],
    val typeHoleIndex: GeneratedHoleIndex,
    val ascriptionTypes: Vector[TypeTemplate]
) derives CanEqual:
  private lazy val occurrenceByOrdinal: Map[Int, String] =
    termHoleOccurrences.map(occurrence =>
      occurrence.identifierOrdinal -> occurrence.name
    ).toMap

  private lazy val semanticKey: SemanticTermKey =
    semanticShapeKey(root, 0, 0)._1

  /** Logical term-hole names in first identifier-occurrence order.
    *
    * This is deterministic internal traversal evidence, not a stable public
    * ordering promise.
    */
  def requiredTermBindings: Vector[String] =
    termHoleOccurrences.map(_.name).distinct

  /** Logical type-hole names in first typed-sidecar occurrence order.
    *
    * This is deterministic internal traversal evidence, not a stable public
    * ordering promise.
    */
  def requiredTypeBindings: Vector[String] =
    ascriptionTypes.flatMap(TypeTemplate.requiredBindings).distinct

  def complete(
      termBindings: Map[String, ConstructedTerm],
      typeBindings: Map[String, TypeNormalForm]
  ): Either[TermConstructionError, ConstructedTerm] =
    for
      _ <- validateBindingSets(termBindings, typeBindings)
      _ <- validateTypeBindings(typeBindings)
      completed <- completeSubtree(root, 0, 0, termBindings, typeBindings)
      _ <- Either.cond(
        completed.nextIdentifierOrdinal ==
          TermShapeTraversal.identifierEntries(root).size,
        (),
        TermConstructionError.CompletionInvariantFailure(
          "identifier preorder was not consumed exactly once"
        )
      )
      _ <- Either.cond(
        completed.nextTypedOrdinal == ascriptionTypes.size,
        (),
        TermConstructionError.CompletionInvariantFailure(
          "typed-node preorder was not consumed exactly once"
        )
      )
      result <- ConstructedTerm.create(completed.shape, completed.ascriptions)
    yield result

  def render: String =
    val termHoles =
      termHoleOccurrences
        .map(occurrence => s"${occurrence.name}@${occurrence.identifierOrdinal}")
        .mkString(", ")
    val typeSidecars =
      ascriptionTypes.map(TermShapeTraversal.renderLogicalTypeTemplate).mkString(", ")
    s"TermTemplate(root=${root.render}, termHoles=[$termHoles], typeSidecars=[$typeSidecars])"

  override def equals(other: Any): Boolean =
    other match
      case that: TermTemplate =>
        semanticKey == that.semanticKey
      case _ =>
        false

  override def hashCode: Int =
    semanticKey.hashCode

  override def toString: String =
    render

  private final case class CompletedSubtree(
      shape: TermShape,
      ascriptions: Vector[TypeNormalForm],
      nextIdentifierOrdinal: Int,
      nextTypedOrdinal: Int
  )

  private def completeSubtree(
      shape: TermShape,
      identifierOrdinal: Int,
      typedOrdinal: Int,
      termBindings: Map[String, ConstructedTerm],
      typeBindings: Map[String, TypeNormalForm]
  ): Either[TermConstructionError, CompletedSubtree] =
    shape match
      case TermShape.Identifier(name, _) =>
        occurrenceByOrdinal.get(identifierOrdinal) match
          case Some(holeName) =>
            termBindings.get(holeName) match
              case Some(binding) =>
                Right(
                  CompletedSubtree(
                    binding.root,
                    binding.ascriptionTypes,
                    identifierOrdinal + 1,
                    typedOrdinal
                  )
                )
              case None =>
                Left(TermConstructionError.IncompleteBoundTerm(holeName))
          case None =>
            Right(
              CompletedSubtree(
                TermShape.Identifier(name, false),
                Vector.empty,
                identifierOrdinal + 1,
                typedOrdinal
              )
            )
      case literal: TermShape.Literal =>
        Right(
          CompletedSubtree(
            literal,
            Vector.empty,
            identifierOrdinal,
            typedOrdinal
          )
        )
      case TermShape.Select(qualifier, name) =>
        completeSubtree(
          qualifier,
          identifierOrdinal,
          typedOrdinal,
          termBindings,
          typeBindings
        ).map(completed =>
          completed.copy(shape = TermShape.Select(completed.shape, name))
        )
      case TermShape.Apply(function, arguments) =>
        for
          completedFunction <- completeSubtree(
            function,
            identifierOrdinal,
            typedOrdinal,
            termBindings,
            typeBindings
          )
          completedArguments <- completeChildren(
            arguments,
            completedFunction.nextIdentifierOrdinal,
            completedFunction.nextTypedOrdinal,
            termBindings,
            typeBindings
          )
        yield CompletedSubtree(
          TermShape.Apply(completedFunction.shape, completedArguments.shapes),
          completedFunction.ascriptions ++ completedArguments.ascriptions,
          completedArguments.nextIdentifierOrdinal,
          completedArguments.nextTypedOrdinal
        )
      case TermShape.Infix(left, operator, right) =>
        for
          completedLeft <- completeSubtree(
            left,
            identifierOrdinal,
            typedOrdinal,
            termBindings,
            typeBindings
          )
          completedRight <- completeSubtree(
            right,
            completedLeft.nextIdentifierOrdinal,
            completedLeft.nextTypedOrdinal,
            termBindings,
            typeBindings
          )
        yield CompletedSubtree(
          TermShape.Infix(completedLeft.shape, operator, completedRight.shape),
          completedLeft.ascriptions ++ completedRight.ascriptions,
          completedRight.nextIdentifierOrdinal,
          completedRight.nextTypedOrdinal
        )
      case TermShape.Unary(operator, operand) =>
        completeSubtree(
          operand,
          identifierOrdinal,
          typedOrdinal,
          termBindings,
          typeBindings
        ).map(completed =>
          completed.copy(shape = TermShape.Unary(operator, completed.shape))
        )
      case TermShape.InterpolatedString(prefix, parts, arguments) =>
        completeChildren(
          arguments,
          identifierOrdinal,
          typedOrdinal,
          termBindings,
          typeBindings
        ).map(completed =>
          CompletedSubtree(
            TermShape.InterpolatedString(prefix, parts, completed.shapes),
            completed.ascriptions,
            completed.nextIdentifierOrdinal,
            completed.nextTypedOrdinal
          )
        )
      case TermShape.Typed(expression, _) =>
        val template = ascriptionTypes(typedOrdinal)
        val relevantBindings =
          typeBindings.view
            .filterKeys(TypeTemplate.holeNames(template))
            .toMap
        for
          normalForm <- TypeTemplate
            .construct(template, relevantBindings)
            .left
            .map(error =>
              TermConstructionError.InvalidTypeTemplateSidecar(
                typedOrdinal,
                error.message
              )
            )
          _ <- TypeTemplate
            .validateConstructed(normalForm)
            .left
            .map(error =>
              TermConstructionError.InvalidTypeTemplateSidecar(
                typedOrdinal,
                error.message
              )
            )
          completedExpression <- completeSubtree(
            expression,
            identifierOrdinal,
            typedOrdinal + 1,
            termBindings,
            typeBindings
          )
        yield CompletedSubtree(
          TermShape.Typed(
            completedExpression.shape,
            TermShapeTraversal.renderNormalForm(normalForm)
          ),
          normalForm +: completedExpression.ascriptions,
          completedExpression.nextIdentifierOrdinal,
          completedExpression.nextTypedOrdinal
        )
      case TermShape.Tuple(elements) =>
        completeChildren(
          elements,
          identifierOrdinal,
          typedOrdinal,
          termBindings,
          typeBindings
        ).map(completed =>
          CompletedSubtree(
            TermShape.Tuple(completed.shapes),
            completed.ascriptions,
            completed.nextIdentifierOrdinal,
            completed.nextTypedOrdinal
          )
        )
      case TermShape.If(condition, thenBranch, elseBranch) =>
        for
          completedCondition <- completeSubtree(
            condition,
            identifierOrdinal,
            typedOrdinal,
            termBindings,
            typeBindings
          )
          completedThen <- completeSubtree(
            thenBranch,
            completedCondition.nextIdentifierOrdinal,
            completedCondition.nextTypedOrdinal,
            termBindings,
            typeBindings
          )
          completedElse <- completeSubtree(
            elseBranch,
            completedThen.nextIdentifierOrdinal,
            completedThen.nextTypedOrdinal,
            termBindings,
            typeBindings
          )
        yield CompletedSubtree(
          TermShape.If(
            completedCondition.shape,
            completedThen.shape,
            completedElse.shape
          ),
          completedCondition.ascriptions ++
            completedThen.ascriptions ++
            completedElse.ascriptions,
          completedElse.nextIdentifierOrdinal,
          completedElse.nextTypedOrdinal
        )
      case TermShape.Parenthesized(expression) =>
        completeSubtree(
          expression,
          identifierOrdinal,
          typedOrdinal,
          termBindings,
          typeBindings
        ).map(completed =>
          completed.copy(
            shape = TermShape.Parenthesized(completed.shape)
          )
        )
      case TermShape.Unsupported(_, _) =>
        Left(TermConstructionError.UnsupportedTermShape())

  private final case class CompletedChildren(
      shapes: List[TermShape],
      ascriptions: Vector[TypeNormalForm],
      nextIdentifierOrdinal: Int,
      nextTypedOrdinal: Int
  )

  private def completeChildren(
      shapes: List[TermShape],
      identifierOrdinal: Int,
      typedOrdinal: Int,
      termBindings: Map[String, ConstructedTerm],
      typeBindings: Map[String, TypeNormalForm]
  ): Either[TermConstructionError, CompletedChildren] =
    shapes.foldLeft[
      Either[TermConstructionError, CompletedChildren]
    ](
      Right(
        CompletedChildren(
          Nil,
          Vector.empty,
          identifierOrdinal,
          typedOrdinal
        )
      )
    ) { (result, child) =>
      result.flatMap { completed =>
        completeSubtree(
          child,
          completed.nextIdentifierOrdinal,
          completed.nextTypedOrdinal,
          termBindings,
          typeBindings
        ).map(next =>
          CompletedChildren(
            completed.shapes :+ next.shape,
            completed.ascriptions ++ next.ascriptions,
            next.nextIdentifierOrdinal,
            next.nextTypedOrdinal
          )
        )
      }
    }

  private def validateBindingSets(
      termBindings: Map[String, ConstructedTerm],
      typeBindings: Map[String, TypeNormalForm]
  ): Either[TermConstructionError, Unit] =
    val requiredTerms = termHoleIndex.semanticNames
    val requiredTypes = typeHoleIndex.semanticNames
    firstSorted(requiredTerms -- termBindings.keySet)
      .map(name => Left(TermConstructionError.MissingTermBinding(name)))
      .orElse(
        firstSorted(termBindings.keySet -- requiredTerms)
          .map(name => Left(TermConstructionError.ExtraTermBinding(name)))
      )
      .orElse(
        firstSorted(requiredTypes -- typeBindings.keySet)
          .map(name => Left(TermConstructionError.MissingTypeBinding(name)))
      )
      .orElse(
        firstSorted(typeBindings.keySet -- requiredTypes)
          .map(name => Left(TermConstructionError.ExtraTypeBinding(name)))
      )
      .getOrElse(Right(()))

  private def validateTypeBindings(
      typeBindings: Map[String, TypeNormalForm]
  ): Either[TermConstructionError, Unit] =
    typeBindings.toVector.sortBy(_._1).foldLeft[
      Either[TermConstructionError, Unit]
    ](Right(())) { case (result, (name, normalForm)) =>
      result.flatMap { _ =>
        TypeTemplate
          .validateConstructed(normalForm)
          .left
          .map(error =>
            TermConstructionError.TypeBindingConstructionFailure(
              name,
              error.message
            )
          )
      }
    }

  private def firstSorted(values: Set[String]): Option[String] =
    values.toVector.sorted.headOption

  private def semanticShapeKey(
      shape: TermShape,
      identifierOrdinal: Int,
      typedOrdinal: Int
  ): (SemanticTermKey, Int, Int) =
    shape match
      case TermShape.Identifier(name, _) =>
        val key = occurrenceByOrdinal
          .get(identifierOrdinal)
          .fold[SemanticTermKey](SemanticTermKey.Identifier(name))(
            SemanticTermKey.TermHole.apply
          )
        (key, identifierOrdinal + 1, typedOrdinal)
      case TermShape.Literal(value) =>
        (SemanticTermKey.Literal(value), identifierOrdinal, typedOrdinal)
      case TermShape.Select(qualifier, name) =>
        val (qualifierKey, nextIdentifier, nextTyped) =
          semanticShapeKey(qualifier, identifierOrdinal, typedOrdinal)
        (
          SemanticTermKey.Select(qualifierKey, name),
          nextIdentifier,
          nextTyped
        )
      case TermShape.Apply(function, arguments) =>
        val (functionKey, afterFunctionIdentifier, afterFunctionTyped) =
          semanticShapeKey(function, identifierOrdinal, typedOrdinal)
        val (argumentKeys, nextIdentifier, nextTyped) =
          semanticChildrenKey(
            arguments,
            afterFunctionIdentifier,
            afterFunctionTyped
          )
        (
          SemanticTermKey.Apply(functionKey, argumentKeys),
          nextIdentifier,
          nextTyped
        )
      case TermShape.Infix(left, operator, right) =>
        val (leftKey, afterLeftIdentifier, afterLeftTyped) =
          semanticShapeKey(left, identifierOrdinal, typedOrdinal)
        val (rightKey, nextIdentifier, nextTyped) =
          semanticShapeKey(right, afterLeftIdentifier, afterLeftTyped)
        (
          SemanticTermKey.Infix(leftKey, operator, rightKey),
          nextIdentifier,
          nextTyped
        )
      case TermShape.Unary(operator, operand) =>
        val (operandKey, nextIdentifier, nextTyped) =
          semanticShapeKey(operand, identifierOrdinal, typedOrdinal)
        (
          SemanticTermKey.Unary(operator, operandKey),
          nextIdentifier,
          nextTyped
        )
      case TermShape.InterpolatedString(prefix, parts, arguments) =>
        val (argumentKeys, nextIdentifier, nextTyped) =
          semanticChildrenKey(arguments, identifierOrdinal, typedOrdinal)
        (
          SemanticTermKey.InterpolatedString(prefix, parts.toVector, argumentKeys),
          nextIdentifier,
          nextTyped
        )
      case TermShape.Typed(expression, _) =>
        val (expressionKey, nextIdentifier, nextTyped) =
          semanticShapeKey(expression, identifierOrdinal, typedOrdinal + 1)
        (
          SemanticTermKey.Typed(
            expressionKey,
            ascriptionTypes(typedOrdinal)
          ),
          nextIdentifier,
          nextTyped
        )
      case TermShape.Tuple(elements) =>
        val (elementKeys, nextIdentifier, nextTyped) =
          semanticChildrenKey(elements, identifierOrdinal, typedOrdinal)
        (
          SemanticTermKey.Tuple(elementKeys),
          nextIdentifier,
          nextTyped
        )
      case TermShape.If(condition, thenBranch, elseBranch) =>
        val (conditionKey, afterConditionIdentifier, afterConditionTyped) =
          semanticShapeKey(condition, identifierOrdinal, typedOrdinal)
        val (thenKey, afterThenIdentifier, afterThenTyped) =
          semanticShapeKey(
            thenBranch,
            afterConditionIdentifier,
            afterConditionTyped
          )
        val (elseKey, nextIdentifier, nextTyped) =
          semanticShapeKey(
            elseBranch,
            afterThenIdentifier,
            afterThenTyped
          )
        (
          SemanticTermKey.If(conditionKey, thenKey, elseKey),
          nextIdentifier,
          nextTyped
        )
      case TermShape.Parenthesized(expression) =>
        val (expressionKey, nextIdentifier, nextTyped) =
          semanticShapeKey(expression, identifierOrdinal, typedOrdinal)
        (
          SemanticTermKey.Parenthesized(expressionKey),
          nextIdentifier,
          nextTyped
        )
      case TermShape.Unsupported(nodeKind, detail) =>
        (
          SemanticTermKey.Unsupported(nodeKind, detail),
          identifierOrdinal,
          typedOrdinal
        )

  private def semanticChildrenKey(
      shapes: List[TermShape],
      identifierOrdinal: Int,
      typedOrdinal: Int
  ): (Vector[SemanticTermKey], Int, Int) =
    shapes.foldLeft(
      (Vector.empty[SemanticTermKey], identifierOrdinal, typedOrdinal)
    ) {
      case ((keys, nextIdentifier, nextTyped), child) =>
        val (key, afterIdentifier, afterTyped) =
          semanticShapeKey(child, nextIdentifier, nextTyped)
        (keys :+ key, afterIdentifier, afterTyped)
    }

private[quasiquotes] object TermTemplate:
  def create(
      root: TermShape,
      termHoleIndex: GeneratedHoleIndex,
      termHoleOccurrences: Vector[TermHoleOccurrence],
      typeHoleIndex: GeneratedHoleIndex,
      ascriptionTypes: Vector[TypeTemplate]
  ): Either[TermConstructionError, TermTemplate] =
    for
      _ <- TermShapeTraversal.validateSupported(root)
      _ <- validateHoleNames(termHoleIndex.semanticNames)
      _ <- validateHoleNames(typeHoleIndex.semanticNames)
      _ <- validateGeneratedCategorySeparation(termHoleIndex, typeHoleIndex)
      _ <- validateTermHolePositions(root, termHoleIndex)
      _ <- validateTermOccurrences(
        root,
        termHoleIndex,
        typeHoleIndex,
        termHoleOccurrences
      )
      _ <- validateTypeSidecars(
        root,
        typeHoleIndex,
        ascriptionTypes
      )
      canonicalRoot = TermShapeTraversal.canonicalizePlaceholders(root)
    yield new TermTemplate(
      canonicalRoot,
      termHoleIndex,
      termHoleOccurrences,
      typeHoleIndex,
      ascriptionTypes
    )

  private def validateHoleNames(
      names: Set[String]
  ): Either[TermConstructionError, Unit] =
    names.toVector.sorted
      .find(name => !isValidHoleName(name))
      .toLeft(())
      .left
      .map(TermConstructionError.InvalidTermHoleName.apply)

  private def isValidHoleName(name: String): Boolean =
    name.nonEmpty &&
      isIdentifierStart(name.head) &&
      name.tail.forall(isIdentifierPart)

  private def isIdentifierStart(char: Char): Boolean =
    char == '_' ||
      ('A' <= char && char <= 'Z') ||
      ('a' <= char && char <= 'z')

  private def isIdentifierPart(char: Char): Boolean =
    isIdentifierStart(char) || ('0' <= char && char <= '9')

  private def validateGeneratedCategorySeparation(
      termHoleIndex: GeneratedHoleIndex,
      typeHoleIndex: GeneratedHoleIndex
  ): Either[TermConstructionError, Unit] =
    (termHoleIndex.generatedNames intersect typeHoleIndex.generatedNames)
      .toVector
      .sorted
      .headOption
      .toLeft(())
      .left
      .map(TermConstructionError.DuplicateGeneratedIdentifier.apply)

  private def validateTermOccurrences(
      root: TermShape,
      termHoleIndex: GeneratedHoleIndex,
      typeHoleIndex: GeneratedHoleIndex,
      occurrences: Vector[TermHoleOccurrence]
  ): Either[TermConstructionError, Unit] =
    val identifiers = TermShapeTraversal.identifierEntries(root)
    val ordinals = occurrences.map(_.identifierOrdinal)
    for
      _ <- Either.cond(
        ordinals == ordinals.sorted,
        (),
        TermConstructionError.CompletionInvariantFailure(
          "term-hole occurrence ordinals must be stored in ascending order"
        )
      )
      _ <- ordinals
        .groupBy(identity)
        .collectFirst { case (ordinal, values) if values.size > 1 => ordinal }
        .toLeft(())
        .left
        .map(TermConstructionError.DuplicateTermOccurrenceAddress.apply)
      _ <- occurrences.foldLeft[Either[TermConstructionError, Unit]](
        Right(())
      ) { (result, occurrence) =>
        result.flatMap { _ =>
          validateOneOccurrence(
            occurrence,
            identifiers,
            termHoleIndex,
            typeHoleIndex
          )
        }
      }
      occurrenceByOrdinal =
        occurrences.map(occurrence =>
          occurrence.identifierOrdinal -> occurrence.name
        ).toMap
      _ <- identifiers.foldLeft[Either[TermConstructionError, Unit]](
        Right(())
      ) { (result, identifier) =>
        result.flatMap { _ =>
          val exactOwnedTransport =
            termHoleIndex.semanticNameFor(identifier.name)
          if identifier.isPlaceholder &&
              !occurrenceByOrdinal.contains(identifier.ordinal)
          then
            Left(
              TermConstructionError.UnownedGeneratedMarker(
                identifier.name,
                identifier.ordinal
              )
            )
          else if exactOwnedTransport.nonEmpty &&
              !occurrenceByOrdinal.contains(identifier.ordinal)
          then
            Left(
              TermConstructionError.UnownedGeneratedMarker(
                identifier.name,
                identifier.ordinal
              )
            )
          else if typeHoleIndex.semanticNameFor(identifier.name).nonEmpty &&
              !occurrenceByOrdinal.contains(identifier.ordinal)
          then
            Left(
              TermConstructionError.TypeHoleMarkerInTermPosition(
                identifier.ordinal
              )
            )
          else Right(())
        }
      }
      occurrenceNames = occurrences.map(_.name).toSet
      _ <- (termHoleIndex.semanticNames -- occurrenceNames)
        .toVector
        .sorted
        .headOption
        .toLeft(())
        .left
        .map(TermConstructionError.MissingTermOccurrence.apply)
    yield ()

  private def validateOneOccurrence(
      occurrence: TermHoleOccurrence,
      identifiers: Vector[TermShapeTraversal.IdentifierEntry],
      termHoleIndex: GeneratedHoleIndex,
      typeHoleIndex: GeneratedHoleIndex
  ): Either[TermConstructionError, Unit] =
    if !termHoleIndex.semanticNames(occurrence.name) then
      Left(
        TermConstructionError.UnknownTermOccurrence(
          occurrence.name,
          occurrence.identifierOrdinal
        )
      )
    else
      identifiers.lift(occurrence.identifierOrdinal) match
        case None =>
          Left(
            TermConstructionError.UnknownTermOccurrence(
              occurrence.name,
              occurrence.identifierOrdinal
            )
          )
        case Some(identifier)
            if typeHoleIndex.semanticNameFor(identifier.name).nonEmpty =>
          Left(
            TermConstructionError.TermOccurrenceCategoryMismatch(
              occurrence.name,
              occurrence.identifierOrdinal
            )
          )
        case Some(identifier) =>
          val expected = termHoleIndex.generatedNameFor(occurrence.name)
          Either.cond(
            expected.contains(identifier.name),
            (),
            TermConstructionError.InvalidTermHolePosition(occurrence.name)
          )

  private def validateTermHolePositions(
      root: TermShape,
      termHoleIndex: GeneratedHoleIndex
  ): Either[TermConstructionError, Unit] =
    val invalidGeneratedName =
      TermShapeTraversal
        .nonIdentifierFields(root)
        .find(termHoleIndex.generatedNames)
    invalidGeneratedName
      .flatMap(termHoleIndex.semanticNameFor)
      .toLeft(())
      .left
      .map(TermConstructionError.InvalidTermHolePosition.apply)

  private def validateTypeSidecars(
      root: TermShape,
      typeHoleIndex: GeneratedHoleIndex,
      ascriptionTypes: Vector[TypeTemplate]
  ): Either[TermConstructionError, Unit] =
    val typedNames = TermShapeTraversal.typedNames(root)
    for
      _ <- Either.cond(
        typedNames.size == ascriptionTypes.size,
        (),
        TermConstructionError.TypedSidecarCountMismatch(
          typedNames.size,
          ascriptionTypes.size
        )
      )
      _ <- ascriptionTypes.zip(typedNames).zipWithIndex.foldLeft[
        Either[TermConstructionError, Unit]
      ](Right(())) { case (result, ((template, actual), typedOrdinal)) =>
        result.flatMap { _ =>
          for
            _ <- TermShapeTraversal
              .validateTypeTemplate(template)
              .left
              .map(detail =>
                TermConstructionError.InvalidTypeTemplateSidecar(
                  typedOrdinal,
                  detail
                )
              )
            templateHoles =
              TermShapeTraversal.typeHoleOccurrences(template).toSet
            _ <- (templateHoles -- typeHoleIndex.semanticNames)
              .toVector
              .sorted
              .headOption match
              case Some(unknownHole) =>
                Left(
                  TermConstructionError.InvalidTypeTemplateSidecar(
                    typedOrdinal,
                    s"unknown type hole `$unknownHole`"
                  )
                )
              case None =>
                Right(())
          yield ()
        }.flatMap { _ =>
          TermShapeTraversal
            .renderTypeTemplate(
              ascriptionTypes(typedOrdinal),
              typeHoleIndex.generatedNameFor
            )
            .left
            .map(detail =>
                TermConstructionError.InvalidTypeTemplateSidecar(
                  typedOrdinal,
                  detail
                )
              )
            .flatMap { expected =>
              Either.cond(
                actual == expected,
                (),
                TermConstructionError.TypedSidecarRenderingMismatch(
                  typedOrdinal,
                  expected,
                  actual
                )
              )
            }
        }
      }
      usedTypeHoles =
        ascriptionTypes.flatMap(TermShapeTraversal.typeHoleOccurrences).toSet
      _ <- (typeHoleIndex.semanticNames -- usedTypeHoles)
        .toVector
        .sorted
        .headOption
        .toLeft(())
        .left
        .map(name =>
          TermConstructionError.InvalidTypeTemplateSidecar(
            0,
            s"registered type hole `$name` has no sidecar occurrence"
          )
        )
    yield ()
