package quasiquotes.matching

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.{Flags as DottyFlags}
import quasiquotes.parser.{BinderId, ConstructorNamePolicy, DottySourceSpanAdapter, InterpolatedStringSegments, Lambda1DiagnosticMessages, TermShapeInspector, TypeShapeInspector}
import quasiquotes.parser.P1BlockDiagnosticMessages
import quasiquotes.parser.SourceOwnedLocalDefAdmission
import quasiquotes.parser.P2LocalValDiagnosticMessages
import quasiquotes.parser.P2LocalValUntypedAdmission
import quasiquotes.source.{GeneratedHoleIndex, SourceSpan}
import quasiquotes.types.TypeNormalForm

private[matching] final case class PatternCompileFailure(
    error: PatternError,
    generatedSpan: Option[SourceSpan]
)

object PatternCompiler:
  private val SupportedUnaryOperators = Set("+", "-", "!", "~")

  def compile(tree: untpd.Tree): Either[PatternError, TermPattern] =
    validateAdmission(tree).flatMap(_ => compileLocatedUsing(tree, PatternSource.extractHoleName)).left.map(_.error)

  /** Compatibility path for direct low-level callers that lack rewrite metadata. */
  private[matching] def compileLocated(tree: untpd.Tree): Either[PatternCompileFailure, TermPattern] =
    validateAdmission(tree).flatMap(_ => compileLocatedUsing(tree, PatternSource.extractHoleName))

  private[matching] def compileLocated(
      tree: untpd.Tree,
      generatedHoles: GeneratedHoleIndex
  ): Either[PatternCompileFailure, TermPattern] =
    validateAdmission(tree).flatMap(_ => compileLocatedUsing(tree, generatedHoles.semanticNameFor))

  private def validateAdmission(tree: untpd.Tree): Either[PatternCompileFailure, Unit] =
    SourceOwnedLocalDefAdmission
      .validate(TermShapeInspector.inspect(tree))
      .left
      .map { violation =>
        PatternCompileFailure(
          PatternError.UnsupportedPatternShape("Block", violation.message),
          DottySourceSpanAdapter.fromTree(tree).filter(!_.isEmpty)
        )
      }
      .flatMap(_ =>
        P2LocalValUntypedAdmission.validate(tree).left.map { violation =>
          PatternCompileFailure(
            PatternError.UnsupportedPatternShape("Block", violation.message),
            DottySourceSpanAdapter.fromTree(tree).filter(!_.isEmpty)
          )
        }
      )

  private def compileLocatedUsing(
      tree: untpd.Tree,
      semanticHoleName: String => Option[String],
      scope: List[(String, BinderId)] = Nil,
      lambdaDepth: Int = 0
  ): Either[PatternCompileFailure, TermPattern] =
    def compileChild(child: untpd.Tree): Either[PatternCompileFailure, TermPattern] =
      compileLocatedUsing(child, semanticHoleName, scope, lambdaDepth)

    tree match
      case untpd.Ident(name) =>
        val text = name.toString
        semanticHoleName(text) match
          case Some(holeName) => Right(TermPattern.Hole(holeName))
          case None =>
            scope.collectFirst { case (`text`, binderId) => binderId } match
              case Some(binderId) => Right(TermPattern.BoundReference(binderId, text))
              case None => Right(TermPattern.Identifier(text))
      case function @ untpd.Function(parameters, body) =>
        if lambdaDepth > 0 then
          unsupportedLambda(function, Lambda1DiagnosticMessages.NestedLambda)
        else
          parameters match
            case (parameter: untpd.ValDef) :: Nil if !parameter.tpt.isEmpty =>
              val binderId = BinderId(scope.size)
              compileLocatedUsing(
                body,
                semanticHoleName,
                (parameter.name.toString -> binderId) :: scope,
                lambdaDepth + 1
              ).map(
                TermPattern.Lambda1(
                  binderId,
                  parameter.name.toString,
                  renderType(parameter.tpt),
                  _
                )
              )
            case _ :: Nil =>
              unsupportedLambda(function, Lambda1DiagnosticMessages.ExplicitParameterType)
            case _ =>
              unsupportedLambda(function, Lambda1DiagnosticMessages.ExactlyOneParameter)
      case untpd.Literal(constant) =>
        Right(TermPattern.Literal(renderConstant(constant.value)))
      case untpd.Number(digits, _) =>
        Right(TermPattern.Literal(digits))
      case untpd.Select(qualifier, name) =>
        compileChild(qualifier).map(TermPattern.Select(_, name.toString))
      case untpd.Apply(untpd.Apply(untpd.Select(_: untpd.New, init), _), _)
          if init.toString == "<init>" =>
        unsupportedConstructor(tree, "multiple constructor argument lists are not supported")
      case untpd.Apply(untpd.Select(untpd.New(typeTree), init), arguments)
          if init.toString == "<init>" =>
        compileNew(tree, typeTree, arguments, semanticHoleName, scope, lambdaDepth)
      case untpd.Apply(function, arguments) =>
        for
          compiledFunction <- compileChild(function)
          compiledArguments <- sequence(arguments.map(compileChild))
        yield TermPattern.Apply(compiledFunction, compiledArguments)
      case untpd.InfixOp(left, op, right) =>
        for
          compiledLeft <- compileChild(left)
          compiledRight <- compileChild(right)
        yield TermPattern.Infix(compiledLeft, op.name.toString, compiledRight)
      case untpd.PrefixOp(untpd.Ident(operator), operand) if SupportedUnaryOperators(operator.toString) =>
        compileChild(operand).map(TermPattern.Unary(operator.toString, _))
      case interpolation @ untpd.InterpolatedString(prefix, segments) =>
        if prefix.toString != "s" then unsupportedInterpolation(interpolation, s"unsupported prefix: ${prefix.toString}")
        else
          InterpolatedStringSegments.decode(segments) match
            case Left(detail) => unsupportedInterpolation(interpolation, detail)
            case Right(decoded) =>
              sequence(decoded.arguments.map(compileChild))
                .map(TermPattern.InterpolatedString("s", decoded.parts, _))
      case untpd.Typed(expression, typeTree) =>
        compileChild(expression).map(TermPattern.Typed(_, renderType(typeTree)))
      case untpd.Tuple(elements) =>
        sequence(elements.map(compileChild)).map(TermPattern.Tuple.apply)
      case untpd.If(condition, thenBranch, elseBranch) =>
        for
          compiledCondition <- compileChild(condition)
          compiledThenBranch <- compileChild(thenBranch)
          compiledElseBranch <- compileChild(elseBranch)
        yield TermPattern.If(compiledCondition, compiledThenBranch, compiledElseBranch)
      case untpd.Block(Nil, result) =>
        compileChild(result)
      case block @ untpd.Block(statements, result) =>
        statements match
          case (value: untpd.ValDef) :: Nil =>
            compileLocalVal(value, result, semanticHoleName, scope, lambdaDepth)
          case (definition: untpd.DefDef) :: Nil =>
            compileLocalDef(definition, result, semanticHoleName, scope, lambdaDepth)
          case values if values.exists(_.isInstanceOf[untpd.ValDef]) =>
            unsupportedBlock(block, P2LocalValDiagnosticMessages.ExactlyOne)
          case definitions if definitions.exists(_.isInstanceOf[untpd.DefDef]) =>
            unsupportedBlock(
              definitions.find(_.isInstanceOf[untpd.DefDef]).get,
              P2LocalValDiagnosticMessages.LocalDef
            )
          case patternDefinitions if patternDefinitions.exists(isPatternDefinition) =>
            unsupportedBlock(
              patternDefinitions.find(isPatternDefinition).get,
              P2LocalValDiagnosticMessages.Pattern
            )
          case expressionStatements if expressionStatements.forall(_.isTerm) =>
            for
              compiledStatements <- sequence(expressionStatements.map(compileChild))
              compiledResult <- compileChild(result)
            yield TermPattern.Block(compiledStatements, compiledResult)
          case statement :: _ =>
            unsupportedBlock(
              statement,
              P1BlockDiagnosticMessages.UnsupportedStatement(statement.getClass.getSimpleName)
            )
          case Nil => compileChild(result)
      case untpd.Parens(inner) =>
        compileChild(inner).map(TermPattern.Parenthesized.apply)
      case untpd.TypedSplice(inner) =>
        compileChild(inner)
      case other =>
        Left(
          PatternCompileFailure(
            PatternError.UnsupportedPatternShape(other.getClass.getSimpleName, other.toString),
            DottySourceSpanAdapter.fromTree(other).filter(!_.isEmpty)
          )
        )

  private def sequence[A](values: List[Either[PatternCompileFailure, A]]): Either[PatternCompileFailure, List[A]] =
    values.foldRight(Right(Nil): Either[PatternCompileFailure, List[A]]) { (next, acc) =>
      for
        head <- next
        tail <- acc
      yield head :: tail
    }

  private def renderConstant(value: Any): String =
    value match
      case string: String => "\"" + string + "\""
      case other => String.valueOf(other)

  private def unsupportedInterpolation(
      tree: untpd.Tree,
      detail: String
  ): Either[PatternCompileFailure, Nothing] =
    Left(
      PatternCompileFailure(
        PatternError.UnsupportedPatternShape("InterpolatedString", detail),
        DottySourceSpanAdapter.fromTree(tree).filter(!_.isEmpty)
      )
    )

  private def unsupportedLambda(
      tree: untpd.Tree,
      detail: String
  ): Either[PatternCompileFailure, Nothing] =
    Left(
      PatternCompileFailure(
        PatternError.UnsupportedPatternShape("Lambda1", detail),
        DottySourceSpanAdapter.fromTree(tree).filter(!_.isEmpty)
      )
    )

  private def unsupportedBlock(
      tree: untpd.Tree,
      detail: String
  ): Either[PatternCompileFailure, Nothing] =
    Left(
      PatternCompileFailure(
        PatternError.UnsupportedPatternShape("Block", detail),
        DottySourceSpanAdapter.fromTree(tree).filter(!_.isEmpty)
      )
    )

  private def compileLocalVal(
      value: untpd.ValDef,
      result: untpd.Tree,
      semanticHoleName: String => Option[String],
      scope: List[(String, BinderId)],
      lambdaDepth: Int
  ): Either[PatternCompileFailure, TermPattern] =
    val displayName = value.name.toString
    if value.mods.is(dotty.tools.dotc.core.Flags.Mutable) then
      unsupportedBlock(value, P2LocalValDiagnosticMessages.Mutable)
    else if value.mods.is(dotty.tools.dotc.core.Flags.Lazy) then
      unsupportedBlock(value, P2LocalValDiagnosticMessages.Lazy)
    else if !isSimpleBinderName(displayName) then
      unsupportedBlock(value, P2LocalValDiagnosticMessages.Pattern)
    else if value.tpt.isEmpty then
      unsupportedBlock(value, P2LocalValDiagnosticMessages.MissingExplicitType)
    else
      TypeNormalForm.fromShape(TypeShapeInspector.inspect(value.tpt)) match
        case Left(_) => unsupportedBlock(value.tpt, P2LocalValDiagnosticMessages.UnsupportedType)
        case Right(normalForm) =>
          val binderId = BinderId(scope.size)
          for
            initializer <- compileLocatedUsing(
              value.unforcedRhs.asInstanceOf[untpd.Tree],
              semanticHoleName,
              scope,
              lambdaDepth
            )
            compiledResult <- compileLocatedUsing(
              result,
              semanticHoleName,
              (displayName -> binderId) :: scope,
              lambdaDepth
            )
          yield TermPattern.Block(
            List(
              BlockPatternStatement.LocalVal(
                binderId,
                displayName,
                quasiquotes.terms.TermShapeTraversal.renderNormalForm(normalForm),
                initializer
              )
            ),
            compiledResult
          )

  private def compileLocalDef(
      definition: untpd.DefDef,
      result: untpd.Tree,
      semanticHoleName: String => Option[String],
      scope: List[(String, BinderId)],
      lambdaDepth: Int
  ): Either[PatternCompileFailure, TermPattern] =
    val methodName = definition.name.toString
    if definition.mods.flags != DottyFlags.Method ||
        definition.mods.hasAnnotations || definition.mods.hasPrivateWithin
    then unsupportedBlock(definition, "P3 local method modifiers are unsupported")
    else if !isSimpleBinderName(methodName) then
      unsupportedBlock(definition, "P3 requires a simple local method binder")
    else if definition.tpt.isEmpty then
      unsupportedBlock(definition, "P3 requires an explicit result Type")
    else
      definition.paramss match
        case List(List(parameter: untpd.ValDef)) =>
          val parameterName = parameter.name.toString
          val parameterRhs = parameter.unforcedRhs.asInstanceOf[untpd.Tree]
          if parameter.mods.flags != DottyFlags.Param ||
              parameter.mods.hasAnnotations || parameter.mods.hasPrivateWithin
          then unsupportedBlock(parameter, "P3 requires one unmodified ordinary parameter")
          else if !parameterRhs.isEmpty then
            unsupportedBlock(parameter, "P3 does not support default parameters")
          else if !isSimpleBinderName(parameterName) then
            unsupportedBlock(parameter, "P3 requires a simple parameter binder")
          else if parameter.tpt.isEmpty then
            unsupportedBlock(parameter, "P3 requires an explicit parameter Type")
          else
            for
              parameterType <- fixedP3Type(parameter.tpt)
              resultType <- fixedP3Type(definition.tpt)
              _ <-
                if parameterType == resultType then Right(())
                else unsupportedBlock(definition, "P3 parameter and result Types must be identical")
              bodyTree <-
                val body = definition.unforcedRhs.asInstanceOf[untpd.Tree]
                if body.isEmpty then unsupportedBlock(definition, "P3 local method body is required")
                else Right(body)
              methodBinderId = BinderId(scope.size)
              parameterBinderId = BinderId(scope.size + 1)
              body <- compileLocatedUsing(
                bodyTree,
                semanticHoleName,
                (parameterName -> parameterBinderId) :: scope,
                lambdaDepth
              )
              _ <- body match
                case TermPattern.BoundReference(`parameterBinderId`, _) => Right(())
                case _ => unsupportedBlock(bodyTree, "P3 method body must be its own parameter")
              compiledResult <- compileLocatedUsing(
                result,
                semanticHoleName,
                (methodName -> methodBinderId) :: scope,
                lambdaDepth
              )
              argumentPattern <- compiledResult match
                case TermPattern.Apply(
                      TermPattern.BoundReference(`methodBinderId`, _),
                      List(argument)
                    ) => Right(argument)
                case _ =>
                  unsupportedBlock(
                    result,
                    "P3 block result must call its local method with one ordinary argument"
                  )
              _ <-
                if p3HoleNames(argumentPattern).size == 1 then Right(())
                else
                  unsupportedBlock(
                    result,
                    "P3 call argument requires exactly one semantic scalar hole name"
                  )
            yield TermPattern.Block(
              List(
                BlockPatternStatement.LocalDef(
                  methodBinderId,
                  methodName,
                  parameterBinderId,
                  parameterName,
                  parameterType,
                  resultType,
                  body
                )
              ),
              compiledResult
            )
        case _ =>
          unsupportedBlock(definition, "P3 requires one ordinary parameter in one clause")

  private def fixedP3Type(tree: untpd.Tree): Either[PatternCompileFailure, TypeNormalForm] =
    TypeNormalForm.fromShape(TypeShapeInspector.inspect(tree)) match
      case Right(normal @ TypeNormalForm.STypeIdent("Int" | "String" | "Boolean")) =>
        Right(normal)
      case _ =>
        unsupportedBlock(tree, "P3 supports only fixed Int, String, or Boolean Types")

  private def p3HoleNames(pattern: TermPattern): Set[String] =
    pattern match
      case TermPattern.Hole(name) => Set(name)
      case TermPattern.Lambda1(_, _, _, body) => p3HoleNames(body)
      case TermPattern.Select(qualifier, _) => p3HoleNames(qualifier)
      case TermPattern.Apply(function, arguments) =>
        p3HoleNames(function) ++ arguments.flatMap(p3HoleNames)
      case TermPattern.New(_, arguments) => arguments.flatMap(p3HoleNames).toSet
      case TermPattern.Infix(left, _, right) => p3HoleNames(left) ++ p3HoleNames(right)
      case TermPattern.Unary(_, operand) => p3HoleNames(operand)
      case TermPattern.InterpolatedString(_, _, arguments) =>
        arguments.flatMap(p3HoleNames).toSet
      case TermPattern.Typed(expression, _) => p3HoleNames(expression)
      case TermPattern.Tuple(elements) => elements.flatMap(p3HoleNames).toSet
      case TermPattern.If(condition, thenBranch, elseBranch) =>
        p3HoleNames(condition) ++ p3HoleNames(thenBranch) ++ p3HoleNames(elseBranch)
      case TermPattern.Block(statements, result) =>
        statements.flatMap {
          case term: TermPattern => p3HoleNames(term)
          case BlockPatternStatement.LocalVal(_, _, _, initializer) => p3HoleNames(initializer)
          case BlockPatternStatement.LocalDef(_, _, _, _, _, _, body) => p3HoleNames(body)
        }.toSet ++ p3HoleNames(result)
      case TermPattern.Parenthesized(expression) => p3HoleNames(expression)
      case _ => Set.empty
  private def isSimpleBinderName(name: String): Boolean =
    name != "_" && name.matches("[A-Za-z_$][A-Za-z0-9_$]*")

  private def isPatternDefinition(tree: untpd.Tree): Boolean =
    val kind = tree.getClass.getSimpleName
    kind.contains("PatDef") || kind.contains("Pattern") || kind.contains("Thicket")

  private def renderType(tree: untpd.Tree): String =
    normalizeTypeName(tree match
      case untpd.Ident(name) => name.toString
      case untpd.Select(qualifier, name) => s"${renderType(qualifier)}.${name.toString}"
      case other => other.toString
    )

  private def normalizeTypeName(typeName: String): String =
    typeName match
      case "scala.Int" => "Int"
      case "scala.Predef.String" | "java.lang.String" | "scala.String" => "String"
      case "scala.Boolean" => "Boolean"
      case other => other

  private def compileNew(
      tree: untpd.Tree,
      typeTree: untpd.Tree,
      arguments: List[untpd.Tree],
      semanticHoleName: String => Option[String],
      scope: List[(String, BinderId)],
      lambdaDepth: Int
  ): Either[PatternCompileFailure, TermPattern] =
    if arguments.exists(_.isInstanceOf[untpd.NamedArg]) then
      unsupportedConstructor(tree, "named constructor arguments are not supported")
    else
      constructorName(typeTree).flatMap(ConstructorNamePolicy.validate) match
        case Left(detail) => unsupportedConstructor(tree, detail)
        case Right(name) =>
          sequence(arguments.map(compileLocatedUsing(_, semanticHoleName, scope, lambdaDepth)))
            .map(TermPattern.New(name, _))

  private def constructorName(tree: untpd.Tree): Either[String, String] =
    tree match
      case untpd.Ident(name) => Right(name.toString)
      case untpd.Select(qualifier, name) => constructorName(qualifier).map(_ + "." + name.toString)
      case _: untpd.AppliedTypeTree => Left("constructor type arguments are not supported")
      case other => Left(s"unsupported constructor type syntax: ${other.getClass.getSimpleName}")

  private def unsupportedConstructor(
      tree: untpd.Tree,
      detail: String
  ): Either[PatternCompileFailure, Nothing] =
    Left(
      PatternCompileFailure(
        PatternError.UnsupportedPatternShape("ConstructorNew", detail),
        DottySourceSpanAdapter.fromTree(tree).filter(!_.isEmpty)
      )
    )
