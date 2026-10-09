package quasiquotes.q064

import scala.meta.*
import scala.quoted.*

import _root_.quasiquotes.construct.hybrid.ScalametaTermFrontend
import _root_.quasiquotes.hybrid.{P2LocalValScalametaAdmission, TermQ3DialectPolicy}
import _root_.quasiquotes.matching.*
import _root_.quasiquotes.matching.hybrid.ScalametaPatternFrontend
import _root_.quasiquotes.parser.BinderId
import _root_.quasiquotes.source.GeneratedHoleIndex
import _root_.quasiquotes.types.{TypeNormalForm, TypeNormalFormSource}

/** Q064-only stand-in for the future typed-Scalameta P3 source compiler.
  *
  * It intentionally stays in test scope. The output is the already accepted
  * private Core LocalDef pattern consumed by the real shared matcher.
  */
object Q064ScalametaP3PatternProbe:
  final case class SyntaxEvidence(
      rootKind: String,
      statementKinds: List[String],
      methodName: String,
      parameterName: String,
      parameterType: String,
      resultType: String,
      bodySyntax: String,
      resultSyntax: String,
      exactCompilerAccepted: Boolean,
      p2AdmissionAccepted: Boolean,
      p2AdmissionDetail: String
  )

  final case class SyntaxGates(
      scalametaParsed: Boolean,
      exactCompilerAccepted: Boolean,
      rootKind: String
  )

  final case class Compiled(
      pattern: TermPattern,
      parameterType: TypeNormalForm,
      resultType: TypeNormalForm,
      semanticHoleNames: Vector[String],
      generatedHoleNames: Vector[String]
  )

  private final case class Parsed(
      mapped: MappedPatternSource,
      tree: scala.meta.Term
  )

  def inspectSyntax(source: String): Either[String, SyntaxEvidence] =
    parseMapped(source).flatMap { parsed =>
      parsed.tree match
        case block: scala.meta.Term.Block =>
          block.stats match
            case (definition: scala.meta.Defn.Def) :: (result: scala.meta.Term) :: Nil =>
              for
                group <- definition.paramClauseGroups match
                  case value :: Nil => Right(value)
                  case _ => Left("expected exactly one Scalameta parameter-clause group")
                clause <- group.paramClauses match
                  case value :: Nil => Right(value)
                  case _ => Left("expected exactly one Scalameta term-parameter clause")
                parameter <- clause.values match
                  case value :: Nil => Right(value)
                  case _ => Left("expected exactly one Scalameta term parameter")
                parameterType <- parameter.decltpe.toRight("Scalameta parameter Type is absent")
                resultType <- definition.decltpe.toRight("Scalameta result Type is absent")
              yield
                val p2 = P2LocalValScalametaAdmission.validate(parsed.tree)
                SyntaxEvidence(
                  parsed.tree.productPrefix,
                  block.stats.map(_.productPrefix),
                  definition.name.value,
                  parameter.name.value,
                  parameterType.syntax,
                  resultType.syntax,
                  definition.body.syntax,
                  result.syntax,
                  ScalametaTermFrontend
                    .validateExactCompiler(parsed.mapped.patternSource.source)
                    .isRight,
                  p2.isRight,
                  p2.left.toOption.map(_.message).getOrElse("")
                )
            case _ => Left("Scalameta block is not exactly Defn.Def plus Term result")
        case other => Left(s"Scalameta parsed ${other.productPrefix} instead of Term.Block")
    }

  def syntaxGates(source: String): Either[String, SyntaxGates] =
    parseMapped(source).map { parsed =>
      SyntaxGates(
        scalametaParsed = true,
        exactCompilerAccepted = ScalametaTermFrontend
          .validateExactCompiler(parsed.mapped.patternSource.source)
          .isRight,
        rootKind = parsed.tree.productPrefix
      )
    }

  def compile(source: String): Either[String, Compiled] =
    for
      parsed <- parseMapped(source)
      _ <- ScalametaTermFrontend
        .validateExactCompiler(parsed.mapped.patternSource.source)
        .left.map(_.message)
      pair <- parsed.tree match
        case block: scala.meta.Term.Block =>
          block.stats match
            case (definition: scala.meta.Defn.Def) :: (result: scala.meta.Term) :: Nil =>
              Right(definition -> result)
            case _ => Left("P3 requires exactly one local Defn.Def and one following Term")
        case _ => Left("P3 requires a Scalameta Term.Block root")
      (definition, result) = pair
      _ <- requireCondition(definition.mods.isEmpty, "P3 local method modifiers are unsupported")
      _ <- requireCondition(
        definition.name.syntax == definition.name.value,
        "P3 requires a simple local method binder"
      )
      group <- definition.paramClauseGroups match
        case value :: Nil => Right(value)
        case _ => Left("P3 requires one parameter-clause group")
      _ <- requireCondition(group.tparamClause.values.isEmpty, "P3 type parameters are unsupported")
      clause <- group.paramClauses match
        case value :: Nil if value.mod.isEmpty => Right(value)
        case _ => Left("P3 requires one ordinary parameter clause")
      parameter <- clause.values match
        case value :: Nil => Right(value)
        case _ => Left("P3 requires exactly one ordinary parameter")
      _ <- requireCondition(
        parameter.mods.isEmpty && parameter.default.isEmpty,
        "P3 requires one unmodified strict parameter without a default"
      )
      _ <- requireCondition(
        parameter.name.syntax == parameter.name.value,
        "P3 requires a simple parameter binder"
      )
      parameterTypeTree <- parameter.decltpe.toRight("P3 requires an explicit parameter Type")
      resultTypeTree <- definition.decltpe.toRight("P3 requires an explicit result Type")
      parameterType <- fixedType(parameterTypeTree)
      resultType <- fixedType(resultTypeTree)
      _ <- requireCondition(
        parameterType == resultType,
        "P3 parameter and result Types must be identical"
      )
      _ <- definition.body match
        case body: scala.meta.Term.Name if body.value == parameter.name.value => Right(())
        case _ => Left("P3 method body must be its own parameter reference")
      argument <- result match
        case application: scala.meta.Term.Apply =>
          application.fun match
            case callee: scala.meta.Term.Name
                if callee.value == definition.name.value && application.args.size == 1 =>
              Right(application.args.head)
            case _ => Left("P3 result must call its local method with one ordinary argument")
        case _ => Left("P3 result must be one ordinary Apply")
      compiledArgument <- ScalametaPatternFrontend
        .compileTree(argument, parsed.mapped.generatedHoleIndex)
        .left.map(_.message)
      distinctHoleNames = parsed.mapped.patternSource.holes.distinct
      _ <- requireCondition(
        distinctHoleNames.size == 1,
        "P3 call argument requires exactly one semantic scalar hole name"
      )
      methodId = BinderId(0)
      parameterId = BinderId(1)
      pattern = TermPattern.Block(
        List(
          BlockPatternStatement.LocalDef(
            methodId,
            definition.name.value,
            parameterId,
            parameter.name.value,
            parameterType,
            resultType,
            TermPattern.BoundReference(parameterId, parameter.name.value)
          )
        ),
        TermPattern.Apply(
          TermPattern.BoundReference(methodId, definition.name.value),
          List(compiledArgument)
        )
      )
    yield Compiled(
      pattern,
      parameterType,
      resultType,
      parsed.mapped.patternSource.holes,
      parsed.mapped.occurrences.map(_.generatedName)
    )

  private def parseMapped(source: String): Either[String, Parsed] =
    for
      mapped <- PatternSource.synthesizeMapped(source).left.map(_.message)
      tree <- ScalametaTermFrontend
        .parse(mapped.patternSource.source, TermQ3DialectPolicy.selected)
        .left.map(_.message)
    yield Parsed(mapped, tree)

  private def fixedType(tpe: scala.meta.Type): Either[String, TypeNormalForm] =
    TypeNormalFormSource.fromSource(tpe.syntax).left.map(_.message).flatMap {
      case normal @ TypeNormalForm.STypeIdent("Int" | "String" | "Boolean") => Right(normal)
      case other => Left(s"unsupported P3 fixed Type: $other")
    }

  private def requireCondition(condition: Boolean, detail: String): Either[String, Unit] =
    Either.cond(condition, (), detail)

private object Q064ScalametaP3Target:
  val ambient: Int = 41

object Q064ScalametaP3MatchingMacros:
  inline def positiveObservations: List[String] = ${ positiveObservationsImpl }
  inline def negativeObservations: List[String] = ${ negativeObservationsImpl }
  inline def repeatedHoleMatches: Boolean = ${ repeatedHoleMatchesImpl }

  private def positiveObservationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    val intTarget = '{
      def different(x: Int): Int = x
      different(41)
    }.asTerm
    val stringTarget = '{
      def different(x: String): String = x
      different("value")
    }.asTerm
    val booleanTarget = '{
      def different(x: Boolean): Boolean = x
      different(true)
    }.asTerm
    val q063rTarget = '{
      def renamed(v: Int): Int = v
      renamed((((m: Int) => renamed(m)), 41)._2)
    }.asTerm

    val observations = List(
      positive("Int", "{ def id(value: Int): Int = value; id($argument) }", intTarget),
      positive("String", "{ def id(value: String): String = value; id($argument) }", stringTarget),
      positive("Boolean", "{ def id(value: Boolean): Boolean = value; id($argument) }", booleanTarget),
      positive("q063r", "{ def id(value: Int): Int = value; id($argument) }", q063rTarget)
    )
    Expr.ofList(observations.map(Expr(_)))

  private def negativeObservationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    val pattern = "{ def id(value: Int): Int = value; id($argument) }"
    val observations = List(
      "wrong-body" -> '{
        def different(x: Int): Int = 1
        different(41)
      }.asTerm,
      "wrong-call" -> '{
        def different(x: Int): Int = x
        identity(41)
      }.asTerm,
      "wrong-type" -> '{
        def different(x: String): String = x
        different("value")
      }.asTerm,
      "lambda-body" -> '{
        def different(x: Int): Int = ((y: Int) => y)(x)
        different(41)
      }.asTerm,
      "non-block" -> '{ 41 }.asTerm
    ).map { case (label, target) => s"$label:${matches(pattern, target)}" }
    Expr.ofList(observations.map(Expr(_)))

  private def repeatedHoleMatchesImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    val pattern = "{ def id(value: Int): Int = value; id(($same, $same)._1) }"
    val target = '{
      def different(x: Int): Int = x
      different((Q064ScalametaP3Target.ambient, Q064ScalametaP3Target.ambient)._1)
    }.asTerm
    Expr(matches(pattern, target))

  private def matches(using q: Quotes)(source: String, target: q.reflect.Term): Boolean =
    Q064ScalametaP3PatternProbe
      .compile(source)
      .flatMap(compiled => TermMatcher.matchTerm(compiled.pattern, target).left.map(_.toString))
      .isRight

  private def positive(using q: Quotes)(
      label: String,
      source: String,
      target: q.reflect.Term
  ): String =
    import q.reflect.*

    def unwrap(term: Term): Term =
      term match
        case Inlined(_, _, inner) => unwrap(inner)
        case Typed(inner, _) => unwrap(inner)
        case q.reflect.Block(Nil, inner: Term) => unwrap(inner)
        case other => other

    val result = for
      compiled <- Q064ScalametaP3PatternProbe.compile(source)
      bindings <- TermMatcher.matchTerm(compiled.pattern, target).left.map(_.toString)
      captured <- bindings.bindings.get("argument").toRight("argument was not captured")
      rawArgument <- unwrap(target) match
        case q.reflect.Block(List(_: DefDef), q.reflect.Apply(_, List(argument))) => Right(argument)
        case other => Left(s"unexpected target: ${Printer.TreeStructure.show(other)}")
      view <- TargetTermView.fromTerm(target).left.map(_.toString)
    yield
      val exact = captured.asInstanceOf[AnyRef].eq(rawArgument.asInstanceOf[AnyRef])
      val (distinctSymbols, roleOrReservation) = view match
        case TargetTermView.Block(
              List(
                TargetBlockStatementView.LocalDef(
                  methodId,
                  _,
                  methodSymbol,
                  parameterId,
                  _,
                  parameterSymbol,
                  _,
                  _,
                  _,
                  _
                )
              ),
              targetResult,
              _
            ) =>
          val distinct = methodSymbol != parameterSymbol && methodId != parameterId
          val nestedIds = collectLambdaIds(targetResult)
          val roleOk =
            if label == "q063r" then
              nestedIds.nonEmpty && nestedIds.forall(id => id != methodId && id != parameterId)
            else true
          distinct -> roleOk
        case _ => false -> false
      (exact, distinctSymbols, roleOrReservation)

    result match
      case Right((exact, distinct, role)) => s"$label:true:$exact:$distinct:$role"
      case Left(_) => s"$label:false:false:false:false"

  private def collectLambdaIds[T](view: TargetTermView[T]): List[BinderId] =
    view match
      case TargetTermView.Lambda1(id, _, _, _, body, _) => id :: collectLambdaIds(body)
      case TargetTermView.Select(qualifier, _, _) => collectLambdaIds(qualifier)
      case TargetTermView.Apply(function, arguments, _) =>
        collectLambdaIds(function) ++ arguments.flatMap(collectLambdaIds)
      case TargetTermView.New(_, arguments, _) => arguments.flatMap(collectLambdaIds)
      case TargetTermView.Infix(left, _, right, _) => collectLambdaIds(left) ++ collectLambdaIds(right)
      case TargetTermView.Unary(_, operand, _) => collectLambdaIds(operand)
      case TargetTermView.InterpolatedString(_, _, arguments, _) => arguments.flatMap(collectLambdaIds)
      case TargetTermView.Typed(expression, _, _) => collectLambdaIds(expression)
      case TargetTermView.Tuple(elements, _) => elements.flatMap(collectLambdaIds)
      case TargetTermView.If(condition, thenBranch, elseBranch, _) =>
        collectLambdaIds(condition) ++ collectLambdaIds(thenBranch) ++ collectLambdaIds(elseBranch)
      case TargetTermView.Block(statements, result, _) =>
        statements.flatMap {
          case TargetBlockStatementView.LocalVal(_, _, _, _, initializer, _) => collectLambdaIds(initializer)
          case TargetBlockStatementView.LocalDef(_, _, _, _, _, _, _, _, body, _) => collectLambdaIds(body)
          case term: TargetTermView[?] =>
            collectLambdaIds(term.asInstanceOf[TargetTermView[T]])
        } ++ collectLambdaIds(result)
      case _ => Nil
