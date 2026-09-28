package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.{BooleanTag, StringTag}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.parser.{Placeholder, TermShape}
import quasiquotes.terms.{TermBinder, TermBindingFailure, TermBindingInternals}

/** Read-only binder-aware projection of the exact RHS retained by U037. */
private[quasiquotes] object ExistingUntpdOrdinaryMethodRhsTermProjection:
  import ExistingUntpdOrdinaryMethodDescriptor.Descriptor

  private val AdmittedInfixOperators = Set(
    "+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">="
  )
  private val AdmittedUnaryOperators = Set("+", "-", "!", "~")

  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  private[dotty] final class PrivateScope private[dotty] (
      val descriptorIdentity: Descriptor,
      val shapeIdentity: TermShape,
      val parameterNames: Vector[Vector[String]],
      val parameters: TermBindingInternals.PersistentParameters
  )

  final case class Projection private[dotty] (
      descriptor: Descriptor,
      shape: TermShape,
      parameterBinders: Vector[Vector[TermBinder]],
      private[dotty] val privateScope: PrivateScope
  )

  def projectRhs(descriptor: Descriptor)(using Context): Either[Error, Projection] =
    for
      valid <- validateDescriptor(descriptor)
      names <- exactParameterNames(valid)
      _ <- rejectAmbiguousNames(names)
      parameters <- TermBindingInternals.persistentParameters(names)
        .left.map(coreFailure("CORE_PARAMETER_SCOPE_FAILED", _))
      binders <- parameterBinders(parameters, names)
        .left.map(coreFailure("CORE_PARAMETER_SCOPE_FAILED", _))
      positions = parameterPositions(names)
      decoded <- decode(valid.rhs, "method RHS", positions, parameters)
      completed <- parameters.complete(decoded)
        .left.map(coreFailure("CORE_BODY_VALIDATION_FAILED", _))
      checked <- parameters.validateDefinitionBody(names.map(_.size), completed)
        .left.map(coreFailure("CORE_BODY_VALIDATION_FAILED", _))
      scope = new PrivateScope(valid, checked, names, parameters)
      projection = Projection(valid, checked, binders, scope)
      _ <- validateProjection(projection)
    yield projection

  private[dotty] def validateProjection(
      projection: Projection
  )(using Context): Either[Error, Unit] =
    val failed = finalInvariant(
      "the projection no longer retains its exact descriptor, semantic shape, persistent scope, or ordered public parameter binders."
    )
    Option(projection).toRight(failed).flatMap { value =>
      Option(value.privateScope).toRight(failed).flatMap { scope =>
        validateDescriptor(value.descriptor).left.map(_ => failed).flatMap { descriptor =>
          exactParameterNames(descriptor).left.map(_ => failed).flatMap { names =>
            if !(descriptor.eq(scope.descriptorIdentity)) || names != scope.parameterNames then Left(failed)
            else if !(value.shape.asInstanceOf[AnyRef] eq scope.shapeIdentity.asInstanceOf[AnyRef]) then
              scope.parameters.validateDefinitionBody(names.map(_.size), value.shape) match
                case Left(problem) if problem.code == "TERM_BINDER_SCOPE_MISMATCH" =>
                  Left(error(
                    "FOREIGN_BINDER_GRAPH",
                    "the supplied semantic shape belongs to a different persistent parameter graph."
                  ))
                case _ => Left(failed)
            else
              for
                expectedBinders <- parameterBinders(scope.parameters, names)
                  .left.map(_ => failed)
                _ <- Either.cond(
                  Option(value.parameterBinders).contains(expectedBinders),
                  (),
                  failed
                )
                _ <- scope.parameters.validateDefinitionBody(names.map(_.size), value.shape)
                  .left.map(problem => finalInvariant(
                    s"the retained persistent parameter graph rejected its semantic shape: ${problem.message}"
                  ))
              yield ()
          }
        }
      }
    }

  private def validateDescriptor(
      descriptor: Descriptor
  )(using Context): Either[Error, Descriptor] =
    ExistingUntpdOrdinaryMethodDescriptor.validate(descriptor)
      .left.map(problem => error(
        "INVALID_DESCRIPTOR",
        s"U037 descriptor validation failed before RHS decoding: ${problem.message}"
      ))
      .map(_ => descriptor)

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
                    present <- Option(parameter).flatMap(p => Option(p.tree)).toRight(error(
                      "MALFORMED_PARAMETER_TOPOLOGY",
                      s"parameter $clauseIndex/$parameterIndex was null or had no raw ValDef."
                    ))
                    name = present.name.toString
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

  private def rejectAmbiguousNames(names: Vector[Vector[String]]): Either[Error, Unit] =
    val duplicates = names.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
      .collect { case (name, count) if count > 1 => name }
      .toVector.sorted
    Either.cond(
      duplicates.isEmpty,
      (),
      error(
        "AMBIGUOUS_PARAMETER_NAME",
        s"duplicate captured parameter spelling(s) ${duplicates.mkString(", ")} cannot be resolved lexically."
      )
    )

  private def parameterPositions(
      names: Vector[Vector[String]]
  ): Map[String, (Int, Int)] =
    names.zipWithIndex.flatMap { (clause, clauseIndex) =>
      clause.zipWithIndex.map { (name, parameterIndex) =>
        name -> (clauseIndex, parameterIndex)
      }
    }.toMap

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

  private def decode(
      tree: untpd.Tree,
      path: String,
      positions: Map[String, (Int, Int)],
      parameters: TermBindingInternals.PersistentParameters
  )(using Context): Either[Error, TermShape] =
    Option(tree)
      .filterNot(_.isEmpty)
      .toRight(error("MALFORMED_RHS", s"$path was null or EmptyTree."))
      .flatMap { present =>
        if present.isInstanceOf[untpd.TypedSplice] then
          Left(error("TYPED_SPLICE_RHS", s"$path contains TypedSplice."))
        else if present.symbol != NoSymbol then
          Left(error("SYMBOL_BEARING_RHS", s"$path contains a symbol-bearing ${nodeKind(present)}."))
        else
          present match
            case untpd.Ident(name) if name.isTermName =>
              val spelling = name.toString
              positions.get(spelling) match
                case Some((clauseIndex, parameterIndex)) =>
                  parameters.referenceAt(clauseIndex, parameterIndex)
                    .left.map(coreFailure("CORE_PARAMETER_REFERENCE_FAILED", _))
                case None => Right(TermShape.Identifier(spelling, Placeholder.isPlaceholder(spelling)))
            case _: untpd.Ident =>
              Left(error("UNSUPPORTED_IDENTIFIER", s"$path used a type-name identifier in Term position."))
            case untpd.Number(digits, untpd.NumberKind.Whole(10)) =>
              Right(TermShape.Literal(digits))
            case untpd.Number(_, kind) =>
              Left(error("UNSUPPORTED_LITERAL", s"$path number kind $kind is outside decimal whole integers."))
            case untpd.Literal(constant) if constant.tag == BooleanTag =>
              Right(TermShape.Literal(constant.booleanValue.toString))
            case untpd.Literal(constant) if constant.tag == StringTag =>
              Right(TermShape.Literal("\"" + constant.value.asInstanceOf[String] + "\""))
            case untpd.Literal(constant) =>
              Left(error("UNSUPPORTED_LITERAL", s"$path constant tag ${constant.tag} is outside Boolean and String literals."))
            case untpd.Select(qualifier, name) =>
              decode(qualifier, s"$path select qualifier", positions, parameters)
                .map(TermShape.Select(_, name.toString))
            case untpd.Apply(function, arguments) =>
              decodeApply(function, arguments, path, positions, parameters)
            case untpd.InfixOp(left, operator, right) =>
              decodeInfix(left, operator, right, path, positions, parameters)
            case untpd.PrefixOp(operator, operand) =>
              decodeUnary(operator, operand, path, positions, parameters)
            case untpd.Tuple(elements) =>
              decodeTuple(elements, path, positions, parameters)
            case untpd.If(condition, thenBranch, elseBranch) =>
              for
                projectedCondition <- decode(condition, s"$path condition", positions, parameters)
                projectedThen <- decode(thenBranch, s"$path then branch", positions, parameters)
                projectedElse <- decode(elseBranch, s"$path else branch", positions, parameters)
              yield TermShape.If(projectedCondition, projectedThen, projectedElse)
            case untpd.Parens(inner) =>
              decode(inner, s"$path parenthesized", positions, parameters)
                .map(TermShape.Parenthesized(_))
            case unsupported =>
              Left(error(
                "UNSUPPORTED_RHS_TOPOLOGY",
                s"$path raw topology ${nodeKind(unsupported)} is outside the bounded non-binder-introducing Term family."
              ))
      }

  private def decodeApply(
      function: untpd.Tree,
      arguments: List[untpd.Tree],
      path: String,
      positions: Map[String, (Int, Int)],
      parameters: TermBindingInternals.PersistentParameters
  )(using Context): Either[Error, TermShape] =
    for
      presentFunction <- Option(function).filterNot(_.isEmpty).toRight(error(
        "MALFORMED_RHS", s"$path application function was null or EmptyTree."
      ))
      _ <- Either.cond(
        !presentFunction.isInstanceOf[untpd.Apply],
        (),
        error("UNSUPPORTED_APPLICATION_SHAPE", s"$path contains more than one ordinary Apply list.")
      )
      presentArguments <- Option(arguments).toRight(error(
        "MALFORMED_RHS", s"$path application argument sequence was null."
      ))
      projectedFunction <- decode(presentFunction, s"$path application function", positions, parameters)
      projectedArguments <- decodeAll(presentArguments, s"$path application argument", positions, parameters)
    yield TermShape.Apply(projectedFunction, projectedArguments)

  private def decodeInfix(
      left: untpd.Tree,
      operator: untpd.Tree,
      right: untpd.Tree,
      path: String,
      positions: Map[String, (Int, Int)],
      parameters: TermBindingInternals.PersistentParameters
  )(using Context): Either[Error, TermShape] =
    for
      operatorName <- Option(operator).filterNot(_.isEmpty).toRight(error(
        "MALFORMED_RHS", s"$path infix operator was null or EmptyTree."
      )).flatMap {
        case untpd.Ident(name) if name.isTermName => Right(name.toString)
        case other => Left(error(
          "UNSUPPORTED_INFIX_SHAPE",
          s"$path infix operator ${nodeKind(other)} is not an unqualified Term identifier."
        ))
      }
      _ <- Either.cond(
        AdmittedInfixOperators(operatorName),
        (),
        error("UNSUPPORTED_INFIX_SHAPE", s"$path infix operator $operatorName is outside the admitted exact operator policy.")
      )
      projectedLeft <- decode(left, s"$path infix left", positions, parameters)
      projectedRight <- decode(right, s"$path infix right", positions, parameters)
    yield TermShape.Infix(projectedLeft, operatorName, projectedRight)

  private def decodeUnary(
      operator: untpd.Tree,
      operand: untpd.Tree,
      path: String,
      positions: Map[String, (Int, Int)],
      parameters: TermBindingInternals.PersistentParameters
  )(using Context): Either[Error, TermShape] =
    for
      operatorName <- Option(operator).filterNot(_.isEmpty).toRight(error(
        "MALFORMED_RHS", s"$path unary operator was null or EmptyTree."
      )).flatMap {
        case untpd.Ident(name) if name.isTermName => Right(name.toString)
        case other => Left(error(
          "UNSUPPORTED_UNARY_SHAPE",
          s"$path unary operator ${nodeKind(other)} is not an unqualified Term identifier."
        ))
      }
      _ <- Either.cond(
        AdmittedUnaryOperators(operatorName),
        (),
        error("UNSUPPORTED_UNARY_SHAPE", s"$path unary operator $operatorName is outside the admitted subset.")
      )
      projectedOperand <- decode(operand, s"$path unary operand", positions, parameters)
    yield TermShape.Unary(operatorName, projectedOperand)

  private def decodeTuple(
      elements: List[untpd.Tree],
      path: String,
      positions: Map[String, (Int, Int)],
      parameters: TermBindingInternals.PersistentParameters
  )(using Context): Either[Error, TermShape] =
    Option(elements).toRight(error("MALFORMED_RHS", s"$path tuple element sequence was null."))
      .flatMap { present =>
        if present.size != 2 && present.size != 3 then
          Left(error(
            "UNSUPPORTED_TUPLE_ARITY",
            s"$path tuple arity ${present.size} is outside the characterized arities 2 and 3."
          ))
        else decodeAll(present, s"$path tuple element", positions, parameters).map(TermShape.Tuple(_))
      }

  private def decodeAll(
      trees: List[untpd.Tree],
      path: String,
      positions: Map[String, (Int, Int)],
      parameters: TermBindingInternals.PersistentParameters
  )(using Context): Either[Error, List[TermShape]] =
    trees.zipWithIndex.foldRight[Either[Error, List[TermShape]]](Right(Nil)) {
      case ((tree, index), accumulated) =>
        for
          projected <- Option(tree).toRight(error("MALFORMED_RHS", s"$path $index was null."))
            .flatMap(decode(_, s"$path $index", positions, parameters))
          rest <- accumulated
        yield projected :: rest
    }

  private def coreFailure(code: String, problem: TermBindingFailure): Error =
    error(code, problem.message)

  private def finalInvariant(detail: String): Error =
    error("FINAL_PROJECTION_INVARIANT_FAILED", detail)

  private def nodeKind(tree: untpd.Tree): String =
    Option(tree).fold("null")(_.getClass.getSimpleName)

  private def error(code: String, detail: String): Error = Error(code, detail)
