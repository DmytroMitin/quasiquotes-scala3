package quasiquotes.construct.q068

import scala.quoted.*

import quasiquotes.matching.{MatchNormalizer, QuasiPattern, TargetTermView, TermMatcher}
import quasiquotes.parser.{SourceOwnedLocalDefAdmission, TinyTermParser}
import quasiquotes.scalameta.TermFrontend
import quasiquotes.source.ReflectedPositionProvenance

object Q068P3ProvenanceMacros:
  inline def fourOrigins(inline explicit: Int, inline inferred: Int, inline caller: Int): List[String] =
    ${ fourOriginsImpl('explicit, 'inferred, 'caller) }

  private def fourOriginsImpl(explicit: Expr[Int], inferred: Expr[Int], caller: Expr[Int])(
      using q: Quotes
  ): Expr[List[String]] =
    import q.reflect.*

    def unwrap(term: Term): Term = term match
      case Inlined(_, _, inner) => unwrap(inner)
      case Typed(inner, _) => unwrap(inner)
      case q.reflect.Block(Nil, inner: Term) => unwrap(inner)
      case other => other

    def publicStandard(term: Term): Boolean =
      import quasiquotes.matching.QuasiPattern.qq
      term match
        case qq"""{ def id(value: Int): Int = value; id($argument) }""" => true
        case _ => false

    def publicHybrid(term: Term): Boolean =
      import quasiquotes.scalameta.ScalametaQuasiPattern.qq
      term match
        case qq"""{ def id(value: Int): Int = value; id($argument) }""" => true
        case _ => false

    val argument = caller.asTerm
    val parts = Seq("{ def id(value: Int): Int = value; id(", ") }")
    def standardQr: Term =
      import quasiquotes.construct.Quasiquotes.qr
      StringContext(parts*).qr(argument)
    def hybridQr: Term =
      import quasiquotes.scalameta.ScalametaQuasiquotes.qr
      StringContext(parts*).qr(argument)
    val standard = standardQr
    val hybrid = hybridQr
    val standardPattern = QuasiPattern.termOrThrow("{ def id(value: Int): Int = value; id($argument) }")
    val hybridPattern = TermFrontend.compile("{ def id(value: Int): Int = value; id($argument) }")
      .fold(error => report.errorAndAbort(error.message), _.pattern)

    def sourceAdmission(sourceText: String): String =
      TinyTermParser.parse(sourceText).fold(
        error => s"PARSE:${error.summary}",
        parsed => SourceOwnedLocalDefAdmission.validate(parsed.shape)
          .fold(error => s"SOURCE_REJECT:${error.message}", _ => "SOURCE_ADMIT")
      )

    def source(position: Position): String =
      ReflectedPositionProvenance.sourceCode(position).map(_.replace('\n', ' ').trim).getOrElse("<none>")
    def bounds(position: Position): String =
      ReflectedPositionProvenance.usableBounds(position).fold("<none>")((start, end) => s"$start:$end")
    def symbol(symbol: Symbol): String =
      if symbol.exists then s"${symbol.name}@${symbol.owner.name}" else "<none>"

    def rows(label: String, original: Term): List[String] =
      val term = unwrap(original)
      val (definition, parameter, function, callArgument) = term match
        case q.reflect.Block(List(definition: DefDef), Apply(function, List(callArgument))) =>
          (definition, definition.termParamss.flatMap(_.params).head, function, callArgument)
        case other => report.errorAndAbort(s"$label unexpected tree: ${Printer.TreeStructure.show(other)}")
      val privateView = TargetTermView.fromTerm(term).fold(_.toString, _.render)
      val normalizedView = MatchNormalizer.normalizedView(term).fold(_.toString, _.render)
      val rawStandard = TermMatcher.matchTermRaw(standardPattern.pattern, term).fold(_.toString, _ => "MATCHED")
      val normalizedStandard = TermMatcher.matchTerm(standardPattern.pattern, term).fold(_.toString, _ => "MATCHED")
      val rawHybrid = TermMatcher.matchTermRaw(hybridPattern, term).fold(_.toString, _ => "MATCHED")
      val normalizedHybrid = TermMatcher.matchTerm(hybridPattern, term).fold(_.toString, _ => "MATCHED")
      List(
        s"$label|positions|def=${bounds(definition.pos)}:${source(definition.pos)}|paramType=${bounds(parameter.tpt.pos)}:${source(parameter.tpt.pos)}|resultType=${bounds(definition.returnTpt.pos)}:${source(definition.returnTpt.pos)}",
        s"$label|symbols|method=${symbol(definition.symbol)}|parameter=${symbol(parameter.symbol)}|bodyIsParam=${definition.rhs.exists(rhs => unwrap(rhs).symbol == parameter.symbol)}|callIsMethod=${unwrap(function).symbol == definition.symbol}|argIdentity=${callArgument.asInstanceOf[AnyRef].eq(argument.asInstanceOf[AnyRef])}",
        s"$label|types|parameter=${parameter.tpt.tpe.show}|result=${definition.returnTpt.tpe.show}|equal=${parameter.tpt.tpe =:= definition.returnTpt.tpe}",
        s"$label|match|publicStandard=${publicStandard(term)}|publicHybrid=${publicHybrid(term)}|private=$privateView|normalizedView=$normalizedView|rawStandard=$rawStandard|normalizedStandard=$normalizedStandard|rawHybrid=$rawHybrid|normalizedHybrid=$normalizedHybrid",
        s"$label|tree|${term.show(using Printer.TreeStructure).replace('\n', ' ')}",
        s"$label|normalizedTree|${MatchNormalizer.normalizedTreeStructure(term)}"
      )

    Expr.ofList((
      List(
        s"source-admission|explicit|${sourceAdmission("{ def id(value: Int): Int = value; id(41) }")}",
        s"source-admission|inferred|${sourceAdmission("{ def id(value: Int) = value; id(41) }")}",
        s"source-admission|nested|${sourceAdmission("{ val x = { def id(value: Int): Int = value; id(41) }; x }")}"
      ) ++
      rows("source-explicit", explicit.asTerm) ++
      rows("source-inferred", inferred.asTerm) ++
      rows("standard-synthetic", standard) ++
      rows("hybrid-synthetic", hybrid)
    ).map(Expr(_)))
