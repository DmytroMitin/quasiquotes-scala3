package quasiquotes.q021

import scala.quoted.*

import quasiquotes.matching.RankedDefinitionPatternExtractor

private[q021] object Q021DefinitionTypeParameterCaptureMatcher:
  def typeDefs(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(String, Seq[q.reflect.TypeDef], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)] =
    admitted(target).map { (tparams, clauses, body) =>
      (target.name, tparams, clauses.map(_.params), target.returnTpt.tpe, body)
    }

  def symbols(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(String, Seq[q.reflect.Symbol], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)] =
    admitted(target).map { (tparams, clauses, body) =>
      (target.name, tparams.map(_.symbol), clauses.map(_.params), target.returnTpt.tpe, body)
    }

  def nameBounds(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(String, Seq[(String, q.reflect.TypeBounds)], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)] =
    admitted(target).flatMap { (tparams, clauses, body) =>
      val products = tparams.foldLeft(Option(List.empty[(String, q.reflect.TypeBounds)])) {
        case (Some(values), tparam) =>
          import q.reflect.*
          tparam.rhs match
            case bounds: TypeBoundsTree => Some(values :+ (tparam.name, bounds.tpe))
            case _ => None
        case (None, _) => None
      }
      products.map(values =>
        (target.name, values, clauses.map(_.params), target.returnTpt.tpe, body)
      )
    }

  private def admitted(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(List[q.reflect.TypeDef], List[q.reflect.TermParamClause], q.reflect.Term)] =
    import q.reflect.*

    if target == null ||
        target.symbol == Symbol.noSymbol ||
        !target.symbol.isDefDef ||
        target.symbol.isClassConstructor ||
        target.symbol.flags.is(Flags.ExtensionMethod) ||
        target.symbol.flags.is(Flags.FieldAccessor) ||
        target.symbol.flags.is(Flags.ParamAccessor) ||
        target.symbol.flags.is(Flags.CaseAccessor) ||
        target.symbol.flags.is(Flags.Given)
    then None
    else
      target.paramss match
        case (typeClause: TypeParamClause) :: rawTermClauses if typeClause.params.nonEmpty =>
          val tparams = typeClause.params
          val termClauses = rawTermClauses.collect { case clause: TermParamClause => clause }
          val typeSymbols = tparams.map(_.symbol)
          val termParameters = termClauses.flatMap(_.params)
          val termSymbols = termParameters.map(_.symbol)
          val nestedSymbols = typeSymbols :: termClauses.map(_.params.map(_.symbol))
          val admittedTypeParameters =
            typeSymbols.forall(_ != Symbol.noSymbol) &&
              typeSymbols.distinct.size == typeSymbols.size &&
              tparams.forall(parameter => parameter.symbol.owner == target.symbol)
          val admittedTermClauses =
            termClauses.size == rawTermClauses.size &&
              termClauses.forall(clause =>
                !clause.isImplicit && !clause.isGiven && !clause.isErased
              )
          val admittedTermParameters =
            termSymbols.forall(_ != Symbol.noSymbol) &&
              termSymbols.distinct.size == termSymbols.size &&
              termParameters.forall(parameter =>
                parameter.symbol.owner == target.symbol &&
                  !parameter.symbol.flags.is(Flags.HasDefault) &&
                  !parameter.symbol.flags.is(Flags.Erased) &&
                  !parameter.symbol.flags.is(Flags.Implicit) &&
                  !parameter.symbol.flags.is(Flags.Given)
              )
          Option
            .when(
              admittedTypeParameters &&
                admittedTermClauses &&
                admittedTermParameters &&
                target.symbol.paramSymss == nestedSymbols
            )(())
            .flatMap(_ => target.rhs.map(body => (tparams, termClauses, body)))
        case _ => None

object Q021CandidateFactory:
  def typeDefs(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (String, Seq[q.reflect.TypeDef], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
  ] = new RankedDefinitionPatternExtractor(Q021DefinitionTypeParameterCaptureMatcher.typeDefs(_))

  def symbols(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (String, Seq[q.reflect.Symbol], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
  ] = new RankedDefinitionPatternExtractor(Q021DefinitionTypeParameterCaptureMatcher.symbols(_))

  def nameBounds(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (String, Seq[(String, q.reflect.TypeBounds)], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
  ] = new RankedDefinitionPatternExtractor(Q021DefinitionTypeParameterCaptureMatcher.nameBounds(_))

object Q021DefinitionTypeParameterCapturePatternMacro:
  def typeDefs(context: Expr[StringContext], callerQuotes: Expr[Quotes])(using Quotes): Expr[Any] =
    validate(context)
    '{ Q021CandidateFactory.typeDefs(using $callerQuotes) }

  def symbols(context: Expr[StringContext], callerQuotes: Expr[Quotes])(using Quotes): Expr[Any] =
    validate(context)
    '{ Q021CandidateFactory.symbols(using $callerQuotes) }

  def nameBounds(context: Expr[StringContext], callerQuotes: Expr[Quotes])(using Quotes): Expr[Any] =
    validate(context)
    '{ Q021CandidateFactory.nameBounds(using $callerQuotes) }

  private def validate(context: Expr[StringContext])(using Quotes): Unit =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        val values = partExpressions.toList.map(_.value)
        Option.when(values.forall(_.nonEmpty))(values.flatten)
      case _ => None

    parts match
      case Some(values) if exactLayout(values) => ()
      case Some(values) =>
        quotes.reflect.report.errorAndAbort(
          s"Invalid Q021 standard dqq Definition template: ${diagnostic(values)}.",
          context
        )
      case None =>
        quotes.reflect.report.errorAndAbort(
          "Q021 standard Definition type-parameter capture template must be statically known.",
          context
        )

  private def exactLayout(parts: List[String]): Boolean =
    parts match
      case List(prefix, beforeTparams, beforeParamss, beforeResult, beforeBody, suffix) =>
        prefix.matches("(?s)\\s*def\\s+") &&
          beforeTparams.matches("(?s)\\s*\\[\\s*\\.\\.\\s*") &&
          beforeParamss.matches("(?s)\\s*\\]\\s*\\(\\s*\\.\\.\\.\\s*") &&
          beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
          beforeBody.matches("(?s)\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private def diagnostic(parts: List[String]): String =
    val literal = parts.mkString("<capture>")
    if !literal.contains("[..") then "a rank-2 type-parameter capture is required"
    else if !literal.contains("(...") then "a rank-3 term-parameter-clause capture is required"
    else "name, complete type parameters, complete paramss, result, and body captures are required"

object Q021TypeDefStandardPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q021DefinitionTypeParameterCapturePatternMacro.typeDefs('context, 'q) }

object Q021SymbolStandardPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q021DefinitionTypeParameterCapturePatternMacro.symbols('context, 'q) }

object Q021NameBoundsStandardPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q021DefinitionTypeParameterCapturePatternMacro.nameBounds('context, 'q) }
