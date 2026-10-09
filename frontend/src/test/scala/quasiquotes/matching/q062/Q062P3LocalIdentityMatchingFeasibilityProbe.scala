package quasiquotes.matching.q062

import scala.quoted.*

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.{Flags as DottyFlags}

import quasiquotes.matching.*
import quasiquotes.parser.{BinderId, BlockStatement, SourceOwnedLocalDefAdmission, TermShape, TinyTermParser}
import quasiquotes.source.ReflectedPositionProvenance
import quasiquotes.terms.TermShapeTraversal
import quasiquotes.types.{TargetTypeReprInspector, TypeNormalForm}

/** Test-scope stand-in for the smallest M1 P3 pattern node.
  *
  * It deliberately lives outside production. Q062 uses it only to prove that
  * the existing two-binder Core vocabulary, current target views, and matcher
  * scopes can express the first local-identity-method matching slice.
  */
object Q062P3LocalIdentityMatchingFeasibilityProbe:
  final case class Evidence(
      matched: Boolean,
      exactArgumentIdentity: Boolean,
      distinctBinderSymbols: Boolean,
      bodyUsesParameterBinder: Boolean,
      resultUsesMethodBinder: Boolean,
      argumentAvoidsLocalBinders: Boolean,
      patternMethodName: String,
      targetMethodName: String,
      patternParameterName: String,
      targetParameterName: String,
      patternTypes: String,
      targetTypes: String,
      detail: String
  )

  private[q062] final case class LocalDefPattern(
      methodBinderId: BinderId,
      methodDisplayName: String,
      parameterBinderId: BinderId,
      parameterDisplayName: String,
      parameterType: TypeNormalForm,
      resultType: TypeNormalForm,
      body: TermPattern
  )

  private[q062] final case class Compiled(
      statement: LocalDefPattern,
      result: TermPattern,
      holeNames: Vector[String]
  )

  inline def analyze[T](inline target: T, inline pattern: String): Evidence =
    ${ analyzeImpl('target, 'pattern) }

  private[q062] def compile(pattern: String): Either[String, Compiled] =
    for
      mapped <- PatternSource.synthesizeMapped(pattern).left.map(_.message)
      parsed <- TinyTermParser.parse(mapped.patternSource.source).left.map(_.summary)
      _ <- validateRawPattern(parsed.rawTree)
      _ <- SourceOwnedLocalDefAdmission.validate(parsed.shape).left.map(_.message)
      compiled <- parsed.shape match
        case TermShape.Block(
              List(
                BlockStatement.LocalDef(
                  methodBinderId,
                  methodDisplayName,
                  parameterBinderId,
                  parameterDisplayName,
                  parameterType,
                  resultType,
                  body
                )
              ),
              result
            ) =>
          for
            parameterNormalForm <- fixedNormalForm(parameterType)
            resultNormalForm <- fixedNormalForm(resultType)
            _ <- Either.cond(
              parameterNormalForm == resultNormalForm,
              (),
              "P3 pattern parameter and result Types must be the same fixed semantic Type"
            )
            compiledBody <- compileTerm(body, mapped.generatedHoleIndex.semanticNameFor)
            compiledResult <- compileTerm(result, mapped.generatedHoleIndex.semanticNameFor)
            _ <- compiledBody match
              case TermPattern.BoundReference(`parameterBinderId`, _) => Right(())
              case _ => Left("P3 pattern body must be its parameter bound reference")
            _ <- compiledResult match
              case TermPattern.Apply(
                    TermPattern.BoundReference(`methodBinderId`, _),
                    List(_)
                  ) => Right(())
              case _ => Left("P3 pattern result must call its local method with one argument")
          yield Compiled(
            LocalDefPattern(
              methodBinderId,
              methodDisplayName,
              parameterBinderId,
              parameterDisplayName,
              parameterNormalForm,
              resultNormalForm,
              compiledBody
            ),
            compiledResult,
            mapped.patternSource.holes
          )
        case _ => Left("P3 pattern requires one local method statement and one result")
    yield compiled

  private def validateRawPattern(tree: untpd.Tree): Either[String, Unit] =
    tree match
      case untpd.Block(List(definition: untpd.DefDef), _) =>
        if definition.mods.flags != DottyFlags.Method ||
            definition.mods.hasAnnotations || definition.mods.hasPrivateWithin
        then Left("P3 pattern method modifiers are unsupported")
        else
          definition.paramss match
            case List(List(parameter: untpd.ValDef))
                if parameter.mods.flags == DottyFlags.Param &&
                  parameter.unforcedRhs.asInstanceOf[untpd.Tree].isEmpty &&
                  !parameter.tpt.isEmpty && !definition.tpt.isEmpty =>
              Right(())
            case List(List(_: untpd.ValDef)) =>
              Left("P3 pattern requires explicit parameter and result Types")
            case _ => Left("P3 pattern requires one ordinary parameter in one clause")
      case _ => Left("P3 pattern requires exactly one local method")

  private def fixedNormalForm(shape: quasiquotes.parser.TypeShape): Either[String, TypeNormalForm] =
    TypeNormalForm.fromShape(shape).left.map(_.toString).flatMap {
      case normal @ TypeNormalForm.STypeIdent("Int" | "String" | "Boolean") => Right(normal)
      case other => Left(s"unsupported P3 fixed Type: ${TermShapeTraversal.renderNormalForm(other)}")
    }

  private def compileTerm(
      shape: TermShape,
      semanticHoleName: String => Option[String]
  ): Either[String, TermPattern] =
    def children(values: List[TermShape]): Either[String, List[TermPattern]] =
      values.foldRight(Right(Nil): Either[String, List[TermPattern]]) { (next, acc) =>
        for
          head <- compileTerm(next, semanticHoleName)
          tail <- acc
        yield head :: tail
      }

    shape match
      case TermShape.Identifier(name, _) =>
        Right(semanticHoleName(name).map(TermPattern.Hole.apply).getOrElse(TermPattern.Identifier(name)))
      case TermShape.BoundReference(binderId, displayName) =>
        Right(TermPattern.BoundReference(binderId, displayName))
      case TermShape.Literal(value) => Right(TermPattern.Literal(value))
      case TermShape.Select(qualifier, name) =>
        compileTerm(qualifier, semanticHoleName).map(TermPattern.Select(_, name))
      case TermShape.Apply(function, arguments) =>
        for
          compiledFunction <- compileTerm(function, semanticHoleName)
          compiledArguments <- children(arguments)
        yield TermPattern.Apply(compiledFunction, compiledArguments)
      case TermShape.New(constructor, arguments) =>
        children(arguments).map(TermPattern.New(constructor, _))
      case TermShape.Infix(left, operator, right) =>
        for
          compiledLeft <- compileTerm(left, semanticHoleName)
          compiledRight <- compileTerm(right, semanticHoleName)
        yield TermPattern.Infix(compiledLeft, operator, compiledRight)
      case TermShape.Unary(operator, operand) =>
        compileTerm(operand, semanticHoleName).map(TermPattern.Unary(operator, _))
      case TermShape.Typed(expression, typeName) =>
        compileTerm(expression, semanticHoleName).map(TermPattern.Typed(_, typeName))
      case TermShape.Tuple(elements) => children(elements).map(TermPattern.Tuple.apply)
      case TermShape.If(condition, thenBranch, elseBranch) =>
        for
          compiledCondition <- compileTerm(condition, semanticHoleName)
          compiledThen <- compileTerm(thenBranch, semanticHoleName)
          compiledElse <- compileTerm(elseBranch, semanticHoleName)
        yield TermPattern.If(compiledCondition, compiledThen, compiledElse)
      case TermShape.Parenthesized(expression) =>
        compileTerm(expression, semanticHoleName).map(TermPattern.Parenthesized.apply)
      case other => Left(s"unsupported P3 test-pattern child: ${other.render}")

  private def analyzeImpl[T: Type](
      targetExpression: Expr[T],
      patternExpression: Expr[String]
  )(using q: Quotes): Expr[Evidence] =
    import q.reflect.*

    final case class Target(
        methodBinderId: BinderId,
        methodName: String,
        methodSymbol: Symbol,
        parameterBinderId: BinderId,
        parameterName: String,
        parameterSymbol: Symbol,
        parameterType: TypeNormalForm,
        resultType: TypeNormalForm,
        body: TargetTermView[Term],
        result: TargetTermView[Term],
        argument: Term
    )

    def unwrap(term: Term): Term =
      term match
        case Inlined(_, _, inner) => unwrap(inner)
        case q.reflect.Block(Nil, inner: Term) => unwrap(inner)
        case other => other

    def fixedType(typeRepr: TypeRepr): Either[String, TypeNormalForm] =
      TargetTypeReprInspector.inspect(typeRepr).left.map(_.toString).flatMap {
        case normal @ TypeNormalForm.STypeIdent("Int" | "String" | "Boolean") => Right(normal)
        case other => Left(s"unsupported target fixed Type: ${TermShapeTraversal.renderNormalForm(other)}")
      }

    def sourceBackedInferred(definition: Definition, typeTree: TypeTree): Boolean =
      ReflectedPositionProvenance.sourceCode(definition.pos).nonEmpty &&
        ReflectedPositionProvenance.sourceCode(typeTree.pos).forall(_.trim.isEmpty)

    def extractTarget(term: Term): Either[String, Target] =
      unwrap(term) match
        case block @ q.reflect.Block(List(definition: DefDef), result) =>
          definition.paramss match
            case List(clause: TermParamClause)
                if !clause.isGiven && !clause.isImplicit &&
                  !clause.params.exists(_.symbol.flags.is(q.reflect.Flags.Erased)) &&
                  clause.params.size == 1 =>
              val parameter = clause.params.head
              if definition.symbol.flags.is(q.reflect.Flags.Inline) ||
                  parameter.symbol.flags.is(q.reflect.Flags.HasDefault)
              then Left("target P3 modifiers/defaults are unsupported")
              else if sourceBackedInferred(definition, definition.returnTpt) ||
                  sourceBackedInferred(parameter, parameter.tpt)
              then Left("target P3 requires explicit parameter and result Types")
              else
                val methodBinderId = BinderId(0)
                val parameterBinderId = BinderId(1)
                for
                  parameterType <- fixedType(parameter.tpt.tpe)
                  resultType <- fixedType(definition.returnTpt.tpe)
                  bodyTerm <- definition.rhs.toRight("target P3 method body is missing")
                  bodyView <- TargetTermView
                    .fromTermInScope(bodyTerm, List(parameterBinderId -> parameter.symbol))
                    .left.map(_.toString)
                  resultView <- TargetTermView
                    .fromTermInScope(result, List(methodBinderId -> definition.symbol))
                    .left.map(_.toString)
                  argument <- resultView match
                    case TargetTermView.Apply(
                          TargetTermView.BoundReference(`methodBinderId`, _, _),
                          List(argument),
                          _
                        ) => Right(argument.original)
                    case _ => Left("target P3 result is not a one-argument call to its local method")
                yield Target(
                  methodBinderId,
                  definition.name,
                  definition.symbol,
                  parameterBinderId,
                  parameter.name,
                  parameter.symbol,
                  parameterType,
                  resultType,
                  bodyView,
                  resultView,
                  argument
                )
            case _ => Left("target P3 requires one nongeneric ordinary one-parameter clause")
        case _ => Left("target P3 requires exactly one local method statement")

    def matchTarget(compiled: Compiled, target: Target): Either[String, MatchResult[Term]] =
      val patternStatement = compiled.statement
      val bodyMatches = target.body match
        case TargetTermView.BoundReference(id, _, _) if id == target.parameterBinderId => true
        case _ => false
      val resultArgumentPattern = compiled.result match
        case TermPattern.Apply(
              TermPattern.BoundReference(methodBinderId, _),
              List(argumentPattern)
            ) if methodBinderId == patternStatement.methodBinderId => Some(argumentPattern)
        case _ => None

      if patternStatement.parameterType != target.parameterType ||
          patternStatement.resultType != target.resultType
      then Left("P3 fixed semantic Types differ")
      else if !bodyMatches then Left("P3 body does not bind to its parameter")
      else
        resultArgumentPattern
          .toRight("P3 result does not bind to its method")
          .flatMap(pattern => TermMatcher.matchTerm(pattern, target.argument).left.map(_.toString))

    val pattern = patternExpression.valueOrAbort
    val compiled = compile(pattern)
    val target = extractTarget(targetExpression.asTerm)
    val matched = for
      compiledPattern <- compiled
      targetView <- target
      bindings <- matchTarget(compiledPattern, targetView)
    yield (compiledPattern, targetView, bindings)

    val evidence = matched match
      case Right((compiledPattern, targetView, bindings)) =>
        val distinct = targetView.methodSymbol != targetView.parameterSymbol
        val bodyBound = targetView.body match
          case TargetTermView.BoundReference(id, _, _) => id == targetView.parameterBinderId
          case _ => false
        val resultBound = targetView.result match
          case TargetTermView.Apply(TargetTermView.BoundReference(id, _, _), _, _) =>
            id == targetView.methodBinderId
          case _ => false
        val directHole = compiledPattern.result match
          case TermPattern.Apply(_, List(TermPattern.Hole(name))) => Some(name)
          case _ => None
        val captured = directHole.flatMap(bindings.bindings.get)
        val exact = captured.exists(_.asInstanceOf[AnyRef].eq(targetView.argument.asInstanceOf[AnyRef]))
        val argumentAvoidsLocals =
          !targetView.argument.symbol.exists ||
            (targetView.argument.symbol != targetView.methodSymbol &&
              targetView.argument.symbol != targetView.parameterSymbol)
        Evidence(
          matched = true,
          exactArgumentIdentity = exact,
          distinctBinderSymbols = distinct,
          bodyUsesParameterBinder = bodyBound,
          resultUsesMethodBinder = resultBound,
          argumentAvoidsLocalBinders = argumentAvoidsLocals,
          patternMethodName = compiledPattern.statement.methodDisplayName,
          targetMethodName = targetView.methodName,
          patternParameterName = compiledPattern.statement.parameterDisplayName,
          targetParameterName = targetView.parameterName,
          patternTypes = typePair(compiledPattern.statement.parameterType, compiledPattern.statement.resultType),
          targetTypes = typePair(targetView.parameterType, targetView.resultType),
          detail = "matched"
        )
      case Left(detail) =>
        val compiledNames = compiled.toOption
        val targetNames = target.toOption
        Evidence(
          matched = false,
          exactArgumentIdentity = false,
          distinctBinderSymbols = targetNames.exists(value => value.methodSymbol != value.parameterSymbol),
          bodyUsesParameterBinder = false,
          resultUsesMethodBinder = false,
          argumentAvoidsLocalBinders = false,
          patternMethodName = compiledNames.map(_.statement.methodDisplayName).getOrElse(""),
          targetMethodName = targetNames.map(_.methodName).getOrElse(""),
          patternParameterName = compiledNames.map(_.statement.parameterDisplayName).getOrElse(""),
          targetParameterName = targetNames.map(_.parameterName).getOrElse(""),
          patternTypes = compiledNames
            .map(value => typePair(value.statement.parameterType, value.statement.resultType))
            .getOrElse(""),
          targetTypes = targetNames.map(value => typePair(value.parameterType, value.resultType)).getOrElse(""),
          detail = detail
        )

    '{
      Evidence(
        ${ Expr(evidence.matched) },
        ${ Expr(evidence.exactArgumentIdentity) },
        ${ Expr(evidence.distinctBinderSymbols) },
        ${ Expr(evidence.bodyUsesParameterBinder) },
        ${ Expr(evidence.resultUsesMethodBinder) },
        ${ Expr(evidence.argumentAvoidsLocalBinders) },
        ${ Expr(evidence.patternMethodName) },
        ${ Expr(evidence.targetMethodName) },
        ${ Expr(evidence.patternParameterName) },
        ${ Expr(evidence.targetParameterName) },
        ${ Expr(evidence.patternTypes) },
        ${ Expr(evidence.targetTypes) },
        ${ Expr(evidence.detail) }
      )
    }

  private def typePair(parameterType: TypeNormalForm, resultType: TypeNormalForm): String =
    s"${TermShapeTraversal.renderNormalForm(parameterType)}/${TermShapeTraversal.renderNormalForm(resultType)}"
