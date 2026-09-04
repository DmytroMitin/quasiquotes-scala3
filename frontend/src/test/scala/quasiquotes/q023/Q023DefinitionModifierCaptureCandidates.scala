package quasiquotes.q023

import scala.quoted.*

import quasiquotes.matching.RankedDefinitionPatternExtractor

/** Test-only model of the smallest complete public-reflection modifier view. */
final class DefinitionModifiers[FlagSet, Within, Annotation](
    val flags: FlagSet,
    val privateWithin: Option[Within],
    val protectedWithin: Option[Within],
    val annotations: List[Annotation]
)

object Q023CandidateFactory:
  def flags(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      q.reflect.Flags,
      String,
      Seq[q.reflect.TypeDef],
      Seq[Seq[q.reflect.ValDef]],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] = candidate(target => target.symbol.flags)

  def structured(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.TypeDef],
      Seq[Seq[q.reflect.ValDef]],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    candidate(target =>
      new DefinitionModifiers(
        target.symbol.flags,
        target.symbol.privateWithin,
        target.symbol.protectedWithin,
        target.symbol.annotations
      )
    )

  def symbol(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      q.reflect.Symbol,
      String,
      Seq[q.reflect.TypeDef],
      Seq[Seq[q.reflect.ValDef]],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] = candidate(_.symbol)

  private def candidate[Mods](using q: Quotes)(
      capture: q.reflect.DefDef => Mods
  ): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      Mods,
      String,
      Seq[q.reflect.TypeDef],
      Seq[Seq[q.reflect.ValDef]],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    import q.reflect.*

    new RankedDefinitionPatternExtractor(target =>
      admitted(target).map { (tparams, clauses, result, body) =>
        (capture(target), target.name, tparams, clauses.map(_.params), result, body)
      }
    )

  private def admitted(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[
    (
      List[q.reflect.TypeDef],
      List[q.reflect.TermParamClause],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
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
          val clauses = rawTermClauses.collect { case clause: TermParamClause => clause }
          val typeSymbols = tparams.map(_.symbol)
          val parameters = clauses.flatMap(_.params)
          val parameterSymbols = parameters.map(_.symbol)
          val nestedSymbols = typeSymbols :: clauses.map(_.params.map(_.symbol))
          val admittedTypeParameters =
            typeSymbols.forall(_ != Symbol.noSymbol) &&
              typeSymbols.distinct.size == typeSymbols.size &&
              tparams.forall(parameter =>
                parameter.symbol.owner == target.symbol &&
                  parameter.symbol.annotations.isEmpty &&
                  !parameter.symbol.flags.is(Flags.Covariant) &&
                  !parameter.symbol.flags.is(Flags.Contravariant) &&
                  (parameter.rhs match
                    case _: TypeBoundsTree => true
                    case _ => false)
              )
          val admittedClauses =
            clauses.size == rawTermClauses.size &&
              clauses.forall(clause =>
                !clause.isImplicit && !clause.isGiven && !clause.isErased
              )
          val admittedParameters =
            parameterSymbols.forall(_ != Symbol.noSymbol) &&
              parameterSymbols.distinct.size == parameterSymbols.size &&
              parameters.forall(parameter =>
                parameter.symbol.owner == target.symbol &&
                  !parameter.symbol.flags.is(Flags.HasDefault) &&
                  !parameter.symbol.flags.is(Flags.Erased) &&
                  !parameter.symbol.flags.is(Flags.Implicit) &&
                  !parameter.symbol.flags.is(Flags.Given)
              ) &&
              target.symbol.paramSymss == nestedSymbols

          Option
            .when(admittedTypeParameters && admittedClauses && admittedParameters)(
              target.returnTpt.tpe
            )
            .flatMap(result => target.rhs.map(body => (tparams, clauses, result, body)))
        case _ => None

object Q023DefinitionModifierCapturePatternMacro:
  def flags(context: Expr[StringContext], callerQuotes: Expr[Quotes])(using Quotes): Expr[Any] =
    validate(context)
    '{ Q023CandidateFactory.flags(using $callerQuotes) }

  def structured(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    validate(context)
    '{ Q023CandidateFactory.structured(using $callerQuotes) }

  def symbol(context: Expr[StringContext], callerQuotes: Expr[Quotes])(using Quotes): Expr[Any] =
    validate(context)
    '{ Q023CandidateFactory.symbol(using $callerQuotes) }

  private def validate(context: Expr[StringContext])(using Quotes): Unit =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        val values = partExpressions.toList.map(_.value)
        Option.when(values.forall(_.nonEmpty))(values.flatten)
      case _ => None

    parts match
      case Some(
            List(
              beforeMods,
              beforeName,
              beforeTparams,
              beforeParamss,
              beforeResult,
              beforeBody,
              suffix
            )
          )
          if beforeMods.trim.isEmpty &&
            beforeName.matches("(?s)\\s+def\\s+") &&
            beforeTparams.matches("(?s)\\s*\\[\\s*\\.\\.\\s*") &&
            beforeParamss.matches("(?s)\\s*\\]\\s*\\(\\s*\\.\\.\\.\\s*") &&
            beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
            beforeBody.matches("(?s)\\s*=\\s*") &&
            suffix.trim.isEmpty => ()
      case Some(_) =>
        quotes.reflect.report.errorAndAbort(
          "Invalid Q023 dqq Definition template: expected exactly `$mods def $name[..$tparams](...$paramss): $result = $body`.",
          context
        )
      case None =>
        quotes.reflect.report.errorAndAbort(
          "Q023 Definition modifier-capture template must be statically known.",
          context
        )

object Q023FlagsStandardPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q023DefinitionModifierCapturePatternMacro.flags('context, 'q) }

object Q023StructuredStandardPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q023DefinitionModifierCapturePatternMacro.structured('context, 'q) }

object Q023SymbolStandardPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q023DefinitionModifierCapturePatternMacro.symbol('context, 'q) }

final class Q023InlineFixture:
  inline def inlineMethod[A](value: A): A = value
  transparent inline def transparentInlineMethod[A](value: A): A = value
  inline def nongenericInlineMethod(value: Int): Int = value
  transparent inline def nongenericTransparentInlineMethod(value: Int): Int = value

object Q023InlineProbe:
  inline def evidence: (Boolean, Boolean) = ${ evidenceImpl }
  inline def details: List[String] = ${ detailsImpl }

  private def evidenceImpl(using q: Quotes): Expr[(Boolean, Boolean)] =
    import q.reflect.*

    def definition(name: String): DefDef =
      TypeRepr.of[Q023InlineFixture].typeSymbol.declaredMethod(name).head.tree match
        case value: DefDef => value
        case other => report.errorAndAbort(s"Expected DefDef for $name, received ${other.show}")

    val inline = definition("inlineMethod")
    val transparent = definition("transparentInlineMethod")
    Expr(
      (
        inline.symbol.flags.is(Flags.Inline),
        transparent.symbol.flags.is(Flags.Inline) && transparent.symbol.flags.is(Flags.Transparent)
      )
    )

  private def detailsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    def definition(name: String): DefDef =
      TypeRepr.of[Q023InlineFixture].typeSymbol.declaredMethod(name).head.tree.asInstanceOf[DefDef]

    def describe(name: String): String =
      val target = definition(name)
      val clauses = target.paramss.collect { case clause: TermParamClause => clause }
      val tparams = target.paramss.collect { case clause: TypeParamClause => clause }.flatMap(_.params)
      val params = clauses.flatMap(_.params)
      val treeSymss = target.paramss.map(_.params.map(_.symbol))
      s"$name rhs=${target.rhs.nonEmpty} paramss=${target.paramss.map(_.getClass.getSimpleName)} " +
        s"symss=${target.symbol.paramSymss.map(_.map(_.name))} trees=${target.paramss.map(_.params.map(_.name))} symmatch=${target.symbol.paramSymss == treeSymss} " +
      s"towners=${tparams.map(_.symbol.owner == target.symbol)} tanns=${tparams.map(_.symbol.annotations.size)} " +
        s"trhs=${tparams.map(_.rhs.getClass.getSimpleName)} " +
        s"powners=${params.map(_.symbol.owner == target.symbol)} defaults=${params.map(_.symbol.flags.is(Flags.HasDefault))} " +
        s"isDef=${target.symbol.isDefDef} ctor=${target.symbol.isClassConstructor} ext=${target.symbol.flags.is(Flags.ExtensionMethod)} " +
        s"access=${target.symbol.flags.is(Flags.FieldAccessor)}/${target.symbol.flags.is(Flags.ParamAccessor)}/${target.symbol.flags.is(Flags.CaseAccessor)} " +
        s"given=${target.symbol.flags.is(Flags.Given)} clauses=${clauses.map(c => (c.isImplicit, c.isGiven, c.isErased))} " +
        s"pflags=${params.map(p => (p.symbol.flags.is(Flags.Erased), p.symbol.flags.is(Flags.Implicit), p.symbol.flags.is(Flags.Given)))}"

    Expr.ofList(
      List("inlineMethod", "transparentInlineMethod", "nongenericInlineMethod", "nongenericTransparentInlineMethod")
        .map(name => Expr(describe(name)))
    )
