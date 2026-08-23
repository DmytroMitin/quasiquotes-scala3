package quasiquotes.matching

import dotty.tools.dotc.ast.untpd
import quasiquotes.parser.{BinderId, ConstructorNamePolicy, DottySourceSpanAdapter, InterpolatedStringSegments, Lambda1DiagnosticMessages, TypeShapeInspector}
import quasiquotes.parser.P1BlockDiagnosticMessages
import quasiquotes.parser.P2LocalValDiagnosticMessages
import quasiquotes.source.{GeneratedHoleIndex, SourceSpan}
import quasiquotes.types.TypeNormalForm

private[matching] final case class PatternCompileFailure(
    error: PatternError,
    generatedSpan: Option[SourceSpan]
)

object PatternCompiler:
  private val SupportedUnaryOperators = Set("+", "-", "!", "~")

  def compile(tree: untpd.Tree): Either[PatternError, TermPattern] =
    compileLocatedUsing(tree, PatternSource.extractHoleName).left.map(_.error)

  /** Compatibility path for direct low-level callers that lack rewrite metadata. */
  private[matching] def compileLocated(tree: untpd.Tree): Either[PatternCompileFailure, TermPattern] =
    compileLocatedUsing(tree, PatternSource.extractHoleName)

  private[matching] def compileLocated(
      tree: untpd.Tree,
      generatedHoles: GeneratedHoleIndex
  ): Either[PatternCompileFailure, TermPattern] =
    compileLocatedUsing(tree, generatedHoles.semanticNameFor)

  private def compileLocatedUsing(
      tree: untpd.Tree,
      semanticHoleName: String => Option[String],
      scope: List[(String, BinderId)] = Nil
  ): Either[PatternCompileFailure, TermPattern] =
    def compileChild(child: untpd.Tree): Either[PatternCompileFailure, TermPattern] =
      compileLocatedUsing(child, semanticHoleName, scope)

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
        if scope.nonEmpty then
          unsupportedLambda(function, Lambda1DiagnosticMessages.NestedLambda)
        else
          parameters match
            case (parameter: untpd.ValDef) :: Nil if !parameter.tpt.isEmpty =>
              val binderId = BinderId(0)
              compileLocatedUsing(
                body,
                semanticHoleName,
                (parameter.name.toString -> binderId) :: scope
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
        compileNew(tree, typeTree, arguments, semanticHoleName, scope)
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
            compileLocalVal(value, result, semanticHoleName, scope)
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
      scope: List[(String, BinderId)]
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
              scope
            )
            compiledResult <- compileLocatedUsing(
              result,
              semanticHoleName,
              (displayName -> binderId) :: scope
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
      scope: List[(String, BinderId)]
  ): Either[PatternCompileFailure, TermPattern] =
    if arguments.exists(_.isInstanceOf[untpd.NamedArg]) then
      unsupportedConstructor(tree, "named constructor arguments are not supported")
    else
      constructorName(typeTree).flatMap(ConstructorNamePolicy.validate) match
        case Left(detail) => unsupportedConstructor(tree, detail)
        case Right(name) =>
          sequence(arguments.map(compileLocatedUsing(_, semanticHoleName, scope)))
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
