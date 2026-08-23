package quasiquotes.matching.hybrid

import scala.meta.*

import _root_.quasiquotes.construct.hybrid.ScalametaTermFrontend
import _root_.quasiquotes.matching.{BlockPatternStatement, PatternSource, TermPattern}
import _root_.quasiquotes.parser.{BinderId, ConstructorNamePolicy, Lambda1DiagnosticMessages}
import _root_.quasiquotes.parser.P1BlockDiagnosticMessages
import _root_.quasiquotes.parser.P2LocalValDiagnosticMessages
import _root_.quasiquotes.terms.TermShapeTraversal
import _root_.quasiquotes.types.TypeNormalFormSource
import _root_.quasiquotes.hybrid.TermQ3DialectPolicy
import _root_.quasiquotes.source.GeneratedHoleIndex

/** Scalameta syntax-to-existing-pattern-IR compiler for the bounded term slice. */
private[quasiquotes] object ScalametaPatternFrontend:
  type Failure = ScalametaTermFrontend.Failure
  private val SupportedUnaryOperators = Set("+", "-", "!", "~")

  def compile(
      pattern: String,
      dialect: Dialect = TermQ3DialectPolicy.selected
  ): Either[Failure, TermPattern] =
    PatternSource.synthesizeMapped(pattern)
      .left.map(error => ScalametaTermFrontend.Failure.template(error.message))
      .flatMap { mapped =>
        val parserSource = mapped.occurrences
          .sortBy(_.generatedSpan.start)
          .reverse
          .foldLeft(mapped.patternSource.source) { (source, occurrence) =>
            val span = occurrence.generatedSpan
            if span.start > 0 && source.charAt(span.start - 1) == '$' then
              source.substring(0, span.start) + "{" +
                source.substring(span.start, span.end) + "}" +
                source.substring(span.end)
            else source
          }
        for
          tree <- ScalametaTermFrontend.parse(parserSource, dialect)
          _ <- ScalametaTermFrontend.validateExactCompiler(mapped.patternSource.source)
          compiled <- compileTree(tree, mapped.generatedHoleIndex)
        yield compiled
      }

  def compileTree(
      tree: scala.meta.Term,
      holes: GeneratedHoleIndex
  ): Either[Failure, TermPattern] =
    def unsupported(tree: scala.meta.Tree, detail: String): Either[Failure, Nothing] =
      Left(
        ScalametaTermFrontend.Failure(
          "SCALAMETA_PATTERN_LOWERING_UNSUPPORTED",
          tree.pos.start,
          tree.pos.end,
          s"${tree.productPrefix}: $detail"
        )
      )

    def sequence[A](values: List[Either[Failure, A]]): Either[Failure, List[A]] =
      values.foldRight(Right(Nil): Either[Failure, List[A]]) { (next, accumulated) =>
        for
          head <- next
          tail <- accumulated
        yield head :: tail
      }

    def renderType(tpe: scala.meta.Type): Either[Failure, String] =
      tpe match
        case name: scala.meta.Type.Name => Right(normalizeType(name.value))
        case select: scala.meta.Type.Select => Right(normalizeType(select.syntax))
        case _ => unsupported(tpe, "ascription type must be a stable name")

    def renderP2DeclaredType(tpe: scala.meta.Type): Either[Failure, String] =
      renderType(tpe).orElse(
        TypeNormalFormSource
          .fromSource(tpe.syntax)
          .left.map(error => ScalametaTermFrontend.Failure.lowering(error.message))
          .map(TermShapeTraversal.renderNormalForm)
      )

    def constructorName(tpe: scala.meta.Type): Either[Failure, String] =
      tpe match
        case name: scala.meta.Type.Name => Right(name.value)
        case select: scala.meta.Type.Select => Right(select.syntax)
        case _ => unsupported(tpe, "constructor type arguments are not supported")

    def literal(lit: Lit): Either[Failure, TermPattern] =
      lit match
        case Lit.Int(value) => Right(TermPattern.Literal(value.toString))
        case Lit.String(value) => Right(TermPattern.Literal("\"" + value + "\""))
        case Lit.Boolean(value) => Right(TermPattern.Literal(value.toString))
        case other => unsupported(other, "unsupported literal")

    def loop(
        current: scala.meta.Term,
        scope: List[(String, BinderId)] = Nil
    ): Either[Failure, TermPattern] =
      current match
        case name: scala.meta.Term.Name =>
          holes.semanticNameFor(name.value) match
            case Some(semanticName) => Right(TermPattern.Hole(semanticName))
            case None =>
              scope.collectFirst { case (boundName, id) if boundName == name.value => id } match
                case Some(id) => Right(TermPattern.BoundReference(id, name.value))
                case None => Right(TermPattern.Identifier(name.value))
        case function: scala.meta.Term.Function =>
          if scope.nonEmpty then unsupported(function, Lambda1DiagnosticMessages.NestedLambda)
          else
            function.paramClause.values match
              case parameter :: Nil if parameter.decltpe.nonEmpty && parameter.mods.isEmpty =>
                val binderId = BinderId(0)
                val parameterName = parameter.name.value
                for
                  parameterType <- renderType(parameter.decltpe.get)
                  body <- loop(function.body, (parameterName -> binderId) :: scope)
                yield TermPattern.Lambda1(binderId, parameterName, parameterType, body)
              case _ :: Nil => unsupported(function, Lambda1DiagnosticMessages.ExplicitParameterType)
              case _ => unsupported(function, Lambda1DiagnosticMessages.ExactlyOneParameter)
        case value: Lit => literal(value)
        case select: scala.meta.Term.Select =>
          loop(select.qual, scope).map(TermPattern.Select(_, select.name.value))
        case unary: scala.meta.Term.ApplyUnary if SupportedUnaryOperators(unary.op.value) =>
          loop(unary.arg, scope).map(TermPattern.Unary(unary.op.value, _))
        case fresh: scala.meta.Term.New =>
          val argumentLists = fresh.init.argss
          if argumentLists.size != 1 then unsupported(fresh, "multiple constructor argument lists are not supported")
          else if argumentLists.head.exists(_.isInstanceOf[scala.meta.Term.Assign]) then
            unsupported(fresh, "named constructor arguments are not supported")
          else
            for
              name <- constructorName(fresh.init.tpe)
              _ <- ConstructorNamePolicy.validate(name).left.map(ScalametaTermFrontend.Failure.lowering)
              arguments <- sequence(argumentLists.head.map(loop(_, scope)))
            yield TermPattern.New(name, arguments)
        case application: scala.meta.Term.Apply =>
          for
            function <- loop(application.fun, scope)
            arguments <- sequence(application.args.map(loop(_, scope)))
          yield TermPattern.Apply(function, arguments)
        case infix: scala.meta.Term.ApplyInfix if infix.argClause.values.size == 1 =>
          for
            left <- loop(infix.lhs, scope)
            right <- loop(infix.argClause.values.head, scope)
          yield TermPattern.Infix(left, infix.op.value, right)
        case tuple: scala.meta.Term.Tuple =>
          sequence(tuple.args.map(loop(_, scope))).map(TermPattern.Tuple.apply)
        case conditional: scala.meta.Term.If =>
          for
            condition <- loop(conditional.cond, scope)
            thenBranch <- loop(conditional.thenp, scope)
            elseBranch <- loop(conditional.elsep, scope)
          yield TermPattern.If(condition, thenBranch, elseBranch)
        case block: scala.meta.Term.Block =>
          block.stats match
            case (result: scala.meta.Term) :: Nil => loop(result, scope)
            case (definition: scala.meta.Defn.Val) :: (result: scala.meta.Term) :: Nil =>
              compileLocalVal(definition, result, scope)
            case stats if stats.size >= 2 && stats.forall(_.isInstanceOf[scala.meta.Term]) =>
              val terms = stats.map(_.asInstanceOf[scala.meta.Term])
              for
                prefix <- sequence(terms.init.map(loop(_, scope)))
                result <- loop(terms.last, scope)
              yield TermPattern.Block(prefix, result)
            case stats =>
              stats.collectFirst {
                case _: scala.meta.Defn.Val => P2LocalValDiagnosticMessages.ExactlyOne
                case _: scala.meta.Defn.Var => P2LocalValDiagnosticMessages.Mutable
                case _: scala.meta.Defn.Def => P2LocalValDiagnosticMessages.LocalDef
                case stat if !stat.isInstanceOf[scala.meta.Term] =>
                  P1BlockDiagnosticMessages.UnsupportedStatement(stat.productPrefix)
              } match
                case Some(detail) => unsupported(block, detail)
                case None => unsupported(block, "P1 block requires at least one expression statement and a final result")
        case ascription: scala.meta.Term.Ascribe =>
          for
            expression <- loop(ascription.expr, scope)
            tpe <- renderType(ascription.tpe)
          yield TermPattern.Typed(expression, tpe)
        case interpolation: scala.meta.Term.Interpolate if interpolation.prefix.value == "s" =>
          def interpolationArgument(argument: scala.meta.Term): Either[Failure, TermPattern] =
            argument match
              case block: scala.meta.Term.Block =>
                block.stats match
                  case (term: scala.meta.Term) :: Nil => loop(term, scope)
                  case _ => unsupported(block, "interpolation argument blocks must contain one admitted term")
              case term => loop(term, scope)
          for
            arguments <- sequence(interpolation.args.map(interpolationArgument))
            parts <- sequence(interpolation.parts.map {
              case Lit.String(value) => Right(value)
              case other => unsupported(other, "non-string interpolation segment")
            })
          yield TermPattern.InterpolatedString("s", parts, arguments)
        case other => unsupported(other, "outside the bounded side-by-side term-pattern tranche")

    def compileLocalVal(
        definition: scala.meta.Defn.Val,
        result: scala.meta.Term,
        scope: List[(String, BinderId)]
    ): Either[Failure, TermPattern] =
      definition.pats match
        case scala.meta.Pat.Var(name) :: Nil if definition.mods.exists(_.isInstanceOf[scala.meta.Mod.Lazy]) =>
          unsupported(definition, P2LocalValDiagnosticMessages.Lazy)
        case scala.meta.Pat.Var(name) :: Nil if definition.mods.nonEmpty =>
          unsupported(definition, P2LocalValDiagnosticMessages.Pattern)
        case scala.meta.Pat.Var(name) :: Nil =>
          definition.decltpe match
            case None => unsupported(definition, P2LocalValDiagnosticMessages.MissingExplicitType)
            case Some(declaredType) =>
              val binderId = BinderId(scope.size)
              for
                renderedType <- renderP2DeclaredType(declaredType)
                initializer <- loop(definition.rhs, scope)
                compiledResult <- loop(result, (name.value -> binderId) :: scope)
              yield TermPattern.Block(
                List(
                  BlockPatternStatement.LocalVal(
                    binderId,
                    name.value,
                    renderedType,
                    initializer
                  )
                ),
                compiledResult
              )
        case _ => unsupported(definition, P2LocalValDiagnosticMessages.Pattern)

    loop(tree)

  private def normalizeType(name: String): String =
    name match
      case "scala.Int" => "Int"
      case "scala.Predef.String" | "java.lang.String" | "scala.String" => "String"
      case "scala.Boolean" => "Boolean"
      case other => other
