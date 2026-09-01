package quasiquotes.matching

import scala.quoted.Quotes
import quasiquotes.parser.BinderId

object TermMatcher:
  private[matching] final case class RankedResult[T](
      scalarBindings: Map[String, T],
      sequenceBindings: Map[String, Seq[T]]
  )

  def matchTermRaw(using q: Quotes)(
      pattern: TermPattern,
      target: q.reflect.Term
  ): Either[MatchFailure, MatchResult[q.reflect.Term]] =
    matchViews(pattern, target, normalized = false)

  def matchTerm(using q: Quotes)(
      pattern: TermPattern,
      target: q.reflect.Term
  ): Either[MatchFailure, MatchResult[q.reflect.Term]] =
    matchViews(pattern, target, normalized = true)

  private[matching] def matchTermRanked(using q: Quotes)(
      pattern: TermPattern,
      sequenceHoleName: String,
      target: q.reflect.Term
  ): Either[MatchFailure, RankedResult[q.reflect.Term]] =
    matchViewsRanked(pattern, target, normalized = true, Some(sequenceHoleName))

  private def matchViews(using q: Quotes)(
      pattern: TermPattern,
      target: q.reflect.Term,
      normalized: Boolean
  ): Either[MatchFailure, MatchResult[q.reflect.Term]] =
    matchViewsRanked(pattern, target, normalized, None)
      .map(result => MatchResult[q.reflect.Term](result.scalarBindings))

  private def matchViewsRanked(using q: Quotes)(
      pattern: TermPattern,
      target: q.reflect.Term,
      normalized: Boolean,
      sequenceHoleName: Option[String]
  ): Either[MatchFailure, RankedResult[q.reflect.Term]] =
    import q.reflect.*
    val preparedPattern =
      if normalized then MatchNormalizer.normalizePattern(pattern)
      else pattern

    final case class Captured(
        term: Term,
        ambientScope: List[(BinderId, Symbol)]
    )

    final case class Bindings(
        scalars: Map[String, Captured],
        sequences: Map[String, Vector[Term]]
    )

    def scopedEquivalent(
        left: TargetTermView[Term],
        leftScope: List[BinderId],
        right: TargetTermView[Term],
        rightScope: List[BinderId]
    ): Boolean =
      (left, right) match
        case (TargetTermView.BoundReference(leftId, _, _), TargetTermView.BoundReference(rightId, _, _)) =>
          val leftDistance = leftScope.indexOf(leftId)
          val rightDistance = rightScope.indexOf(rightId)
          leftDistance >= 0 && leftDistance == rightDistance
        case (TargetTermView.Identifier(leftName, leftOriginal), TargetTermView.Identifier(rightName, rightOriginal)) =>
          if leftOriginal.symbol.exists && rightOriginal.symbol.exists then
            leftOriginal.symbol == rightOriginal.symbol
          else leftName == rightName
        case (TargetTermView.Literal(leftValue, _), TargetTermView.Literal(rightValue, _)) =>
          leftValue == rightValue
        case (TargetTermView.Lambda1(leftId, _, leftType, _, leftBody, _), TargetTermView.Lambda1(rightId, _, rightType, _, rightBody, _)) =>
          leftType == rightType && scopedEquivalent(
            leftBody,
            leftId :: leftScope,
            rightBody,
            rightId :: rightScope
          )
        case (TargetTermView.Select(leftQualifier, leftName, _), TargetTermView.Select(rightQualifier, rightName, _)) =>
          leftName == rightName && scopedEquivalent(leftQualifier, leftScope, rightQualifier, rightScope)
        case (TargetTermView.Apply(leftFunction, leftArguments, _), TargetTermView.Apply(rightFunction, rightArguments, _)) =>
          leftArguments.size == rightArguments.size &&
            scopedEquivalent(leftFunction, leftScope, rightFunction, rightScope) &&
            leftArguments.zip(rightArguments).forall((l, r) => scopedEquivalent(l, leftScope, r, rightScope))
        case (TargetTermView.New(leftConstructor, leftArguments, _), TargetTermView.New(rightConstructor, rightArguments, _)) =>
          leftConstructor == rightConstructor && leftArguments.size == rightArguments.size &&
            leftArguments.zip(rightArguments).forall((l, r) => scopedEquivalent(l, leftScope, r, rightScope))
        case (TargetTermView.Infix(leftLeft, leftOperator, leftRight, _), TargetTermView.Infix(rightLeft, rightOperator, rightRight, _)) =>
          leftOperator == rightOperator &&
            scopedEquivalent(leftLeft, leftScope, rightLeft, rightScope) &&
            scopedEquivalent(leftRight, leftScope, rightRight, rightScope)
        case (TargetTermView.Unary(leftOperator, leftOperand, _), TargetTermView.Unary(rightOperator, rightOperand, _)) =>
          leftOperator == rightOperator && scopedEquivalent(leftOperand, leftScope, rightOperand, rightScope)
        case (TargetTermView.InterpolatedString(leftPrefix, leftParts, leftArguments, _), TargetTermView.InterpolatedString(rightPrefix, rightParts, rightArguments, _)) =>
          leftPrefix == rightPrefix && leftParts == rightParts && leftArguments.size == rightArguments.size &&
            leftArguments.zip(rightArguments).forall((l, r) => scopedEquivalent(l, leftScope, r, rightScope))
        case (TargetTermView.Typed(leftExpression, leftType, _), TargetTermView.Typed(rightExpression, rightType, _)) =>
          leftType == rightType && scopedEquivalent(leftExpression, leftScope, rightExpression, rightScope)
        case (TargetTermView.Tuple(leftElements, _), TargetTermView.Tuple(rightElements, _)) =>
          leftElements.size == rightElements.size &&
            leftElements.zip(rightElements).forall((l, r) => scopedEquivalent(l, leftScope, r, rightScope))
        case (TargetTermView.If(leftCondition, leftThen, leftElse, _), TargetTermView.If(rightCondition, rightThen, rightElse, _)) =>
          scopedEquivalent(leftCondition, leftScope, rightCondition, rightScope) &&
            scopedEquivalent(leftThen, leftScope, rightThen, rightScope) &&
            scopedEquivalent(leftElse, leftScope, rightElse, rightScope)
        case (TargetTermView.Block(leftPrefix, leftResult, _), TargetTermView.Block(rightPrefix, rightResult, _)) =>
          (leftPrefix, rightPrefix) match
            case (
                  List(TargetBlockStatementView.LocalVal(leftId, _, leftType, _, leftInitializer, _)),
                  List(TargetBlockStatementView.LocalVal(rightId, _, rightType, _, rightInitializer, _))
                ) =>
              leftType == rightType &&
                scopedEquivalent(leftInitializer, leftScope, rightInitializer, rightScope) &&
                scopedEquivalent(leftResult, leftId :: leftScope, rightResult, rightId :: rightScope)
            case _ if leftPrefix.forall(_.isInstanceOf[TargetTermView[?]]) &&
                rightPrefix.forall(_.isInstanceOf[TargetTermView[?]]) =>
              leftPrefix.size == rightPrefix.size &&
                leftPrefix.zip(rightPrefix).forall { (left, right) =>
                  scopedEquivalent(
                    left.asInstanceOf[TargetTermView[Term]],
                    leftScope,
                    right.asInstanceOf[TargetTermView[Term]],
                    rightScope
                  )
                } && scopedEquivalent(leftResult, leftScope, rightResult, rightScope)
            case _ => false
        case _ => false

    def normalizedEquality(
        left: Captured,
        right: Captured
    ): Either[MatchFailure, Boolean] =
      for
        leftView <- TargetTermView
          .fromTermInScope(left.term, left.ambientScope)
          .map(MatchNormalizer.normalizeTarget)
        rightView <- TargetTermView
          .fromTermInScope(right.term, right.ambientScope)
          .map(MatchNormalizer.normalizeTarget)
      yield scopedEquivalent(
        leftView,
        left.ambientScope.map(_._1),
        rightView,
        right.ambientScope.map(_._1)
      )

    def loop(
        pattern: TermPattern,
        target: TargetTermView[Term],
        bindings: Bindings,
        patternScope: List[BinderId],
        targetScope: List[(BinderId, Symbol)]
    ): Either[MatchFailure, Bindings] =
      pattern match
        case TermPattern.Hole(name) =>
          bindings.scalars.get(name) match
            case None =>
              Right(
                bindings.copy(
                  scalars = bindings.scalars.updated(name, Captured(target.original, targetScope))
                )
              )
            case Some(previous) =>
              val current = Captured(target.original, targetScope)
              normalizedEquality(previous, current).flatMap { equal =>
                if equal then Right(bindings)
                else
                  Left(
                    MatchFailure.RepeatedHoleMismatch(
                      name = name,
                      previous = MatchNormalizer.normalizedTreeStructure(previous.term),
                      current = MatchNormalizer.normalizedTreeStructure(current.term)
                    )
                  )
              }
        case TermPattern.BoundReference(binderId, _) =>
          target match
            case TargetTermView.BoundReference(targetBinderId, _, _)
                if patternScope.indexOf(binderId) == targetScope.map(_._1).indexOf(targetBinderId) &&
                  patternScope.indexOf(binderId) >= 0 =>
              Right(bindings)
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Lambda1(binderId, _, parameterType, body) =>
          target match
            case TargetTermView.Lambda1(targetBinderId, _, targetParameterType, binderSymbol, targetBody, _)
                if parameterType == targetParameterType =>
              loop(
                body,
                targetBody,
                bindings,
                binderId :: patternScope,
                (targetBinderId -> binderSymbol.asInstanceOf[Symbol]) :: targetScope
              )
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Identifier(name) =>
          target match
            case TargetTermView.Identifier(targetName, _) if targetName == name => Right(bindings)
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Literal(value) =>
          target match
            case TargetTermView.Literal(targetValue, _) if targetValue == value => Right(bindings)
            case _ => Left(shapeMismatch(pattern, target))
        case TermPattern.Select(qualifier, name) =>
          target match
            case TargetTermView.Select(targetQualifier, targetName, _) if targetName == name =>
              loop(qualifier, targetQualifier, bindings, patternScope, targetScope)
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Apply(function, arguments) =>
          target match
            case targetApply @ TargetTermView.Apply(targetFunction, targetArguments, _) =>
              val sequenceIndex = sequenceHoleName.flatMap { name =>
                arguments.zipWithIndex.collectFirst {
                  case (TermPattern.Hole(`name`), index) => index
                }
              }
              val admittedLength = sequenceIndex match
                case None => targetArguments.length == arguments.length
                case Some(_) => targetArguments.length >= arguments.length - 1
              if !admittedLength then Left(shapeMismatch(pattern, targetApply))
              else
                for
                  functionBindings <- loop(function, targetFunction, bindings, patternScope, targetScope)
                  argumentBindings <- matchImmediateChildren(
                    arguments,
                    targetArguments,
                    functionBindings,
                    patternScope,
                    targetScope,
                    shapeMismatch(pattern, targetApply)
                  )
                yield argumentBindings
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.New(constructor, arguments) =>
          target match
            case targetNew @ TargetTermView.New(targetConstructor, targetArguments, _)
                if targetConstructor == constructor =>
              val sequenceIndex = sequenceHoleName.flatMap { name =>
                arguments.zipWithIndex.collectFirst {
                  case (TermPattern.Hole(`name`), index) => index
                }
              }
              val admittedLength = sequenceIndex match
                case None => targetArguments.length == arguments.length
                case Some(_) => targetArguments.length >= arguments.length - 1
              if !admittedLength then Left(shapeMismatch(pattern, targetNew))
              else
                matchImmediateChildren(
                  arguments,
                  targetArguments,
                  bindings,
                  patternScope,
                  targetScope,
                  shapeMismatch(pattern, targetNew)
                )
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Infix(left, operator, right) =>
          target match
            case TargetTermView.Infix(targetLeft, targetOperator, targetRight, _) if targetOperator == operator =>
              for
                leftBindings <- loop(left, targetLeft, bindings, patternScope, targetScope)
                rightBindings <- loop(right, targetRight, leftBindings, patternScope, targetScope)
              yield rightBindings
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Unary(operator, operand) =>
          target match
            case TargetTermView.Unary(targetOperator, targetOperand, _) if targetOperator == operator =>
              loop(operand, targetOperand, bindings, patternScope, targetScope)
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.InterpolatedString(prefix, parts, arguments) =>
          target match
            case TargetTermView.InterpolatedString(targetPrefix, targetParts, targetArguments, _)
                if targetPrefix == prefix && targetParts == parts && targetArguments.length == arguments.length =>
              arguments.zip(targetArguments).foldLeft(Right(bindings): Either[MatchFailure, Bindings]) {
                case (acc, (patternArgument, targetArgument)) =>
                  acc.flatMap(loop(patternArgument, targetArgument, _, patternScope, targetScope))
              }
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Typed(expression, typeName) =>
          target match
            case TargetTermView.Typed(targetExpression, targetTypeName, _) if targetTypeName == typeName =>
              loop(expression, targetExpression, bindings, patternScope, targetScope)
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Tuple(elements) =>
          target match
            case TargetTermView.Tuple(targetElements, _) if targetElements.length == elements.length =>
              elements.zip(targetElements).foldLeft(Right(bindings): Either[MatchFailure, Bindings]) {
                case (acc, (patternElement, targetElement)) =>
                  acc.flatMap(loop(patternElement, targetElement, _, patternScope, targetScope))
              }
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.If(condition, thenBranch, elseBranch) =>
          target match
            case TargetTermView.If(targetCondition, targetThenBranch, targetElseBranch, _) =>
              for
                conditionBindings <- loop(condition, targetCondition, bindings, patternScope, targetScope)
                thenBindings <- loop(thenBranch, targetThenBranch, conditionBindings, patternScope, targetScope)
                elseBindings <- loop(elseBranch, targetElseBranch, thenBindings, patternScope, targetScope)
              yield elseBindings
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Block(prefix, result) =>
          target match
            case TargetTermView.Block(targetPrefix, targetResult, _) =>
              (prefix, targetPrefix) match
                case (
                      List(BlockPatternStatement.LocalVal(patternBinderId, _, patternType, patternInitializer)),
                      List(TargetBlockStatementView.LocalVal(targetBinderId, _, targetType, binderSymbol, targetInitializer, _))
                    ) if patternType == targetType =>
                  for
                    initializerBindings <- loop(
                      patternInitializer,
                      targetInitializer,
                      bindings,
                      patternScope,
                      targetScope
                    )
                    resultBindings <- loop(
                      result,
                      targetResult,
                      initializerBindings,
                      patternBinderId :: patternScope,
                      (targetBinderId -> binderSymbol.asInstanceOf[Symbol]) :: targetScope
                    )
                  yield resultBindings
                case _ if prefix.length == targetPrefix.length &&
                    prefix.forall(_.isInstanceOf[TermPattern]) &&
                    targetPrefix.forall(_.isInstanceOf[TargetTermView[?]]) =>
                  for
                    prefixBindings <- prefix.zip(targetPrefix).foldLeft(
                      Right(bindings): Either[MatchFailure, Bindings]
                    ) { case (acc, (patternChild, targetChild)) =>
                      acc.flatMap(
                        loop(
                          patternChild.asInstanceOf[TermPattern],
                          targetChild.asInstanceOf[TargetTermView[Term]],
                          _,
                          patternScope,
                          targetScope
                        )
                      )
                    }
                    resultBindings <- loop(result, targetResult, prefixBindings, patternScope, targetScope)
                  yield resultBindings
                case _ => Left(shapeMismatch(pattern, target))
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Parenthesized(inner) =>
          if normalized then loop(inner, target, bindings, patternScope, targetScope)
          else Left(shapeMismatch(pattern, target))

    def matchImmediateChildren(
        arguments: List[TermPattern],
        targets: List[TargetTermView[Term]],
        bindings: Bindings,
        patternScope: List[BinderId],
        targetScope: List[(BinderId, Symbol)],
        mismatch: MatchFailure
    ): Either[MatchFailure, Bindings] =
      val sequenceSlot = sequenceHoleName.flatMap { name =>
        arguments.zipWithIndex.collectFirst {
          case (TermPattern.Hole(`name`), index) => name -> index
        }
      }
      sequenceSlot match
        case None =>
          arguments.zip(targets).foldLeft(Right(bindings): Either[MatchFailure, Bindings]) {
            case (acc, (patternArgument, targetArgument)) =>
              acc.flatMap(loop(patternArgument, targetArgument, _, patternScope, targetScope))
          }
        case Some((sequenceName, index)) =>
          val prefix = arguments.take(index)
          val suffix = arguments.drop(index + 1)
          if targets.size < prefix.size + suffix.size then
            Left(mismatch)
          else
            val prefixTargets = targets.take(prefix.size)
            val suffixTargets = targets.takeRight(suffix.size)
            val middle = targets.slice(prefix.size, targets.size - suffix.size).map(_.original).toVector
            for
              prefixBindings <- prefix.zip(prefixTargets).foldLeft(
                Right(bindings): Either[MatchFailure, Bindings]
              ) { case (acc, (patternArgument, targetArgument)) =>
                acc.flatMap(loop(patternArgument, targetArgument, _, patternScope, targetScope))
              }
              withSequence = prefixBindings.copy(
                sequences = prefixBindings.sequences.updated(sequenceName, middle)
              )
              suffixBindings <- suffix.zip(suffixTargets).foldLeft(
                Right(withSequence): Either[MatchFailure, Bindings]
              ) { case (acc, (patternArgument, targetArgument)) =>
                acc.flatMap(loop(patternArgument, targetArgument, _, patternScope, targetScope))
              }
            yield suffixBindings

    for
      rawView <- TargetTermView.fromTerm(target)
      preparedTarget = if normalized then MatchNormalizer.normalizeTarget(rawView) else rawView
      bindings <- loop(preparedPattern, preparedTarget, Bindings(Map.empty, Map.empty), Nil, Nil)
    yield RankedResult[q.reflect.Term](
      bindings.scalars.view.mapValues(_.term).toMap,
      bindings.sequences
    )

  private def shapeMismatch(pattern: TermPattern, target: TargetTermView[?]): MatchFailure =
    MatchFailure.ShapeMismatch(pattern.render, target.render)
