package quasiquotes.matching

import scala.quoted.*
import quasiquotes.construct.SelectedMemberName

/** Supplemental boundary control: direct typed unapply, never a flat-pattern success claim. */
object Q079ScalametaDirectProbe:
  inline def observations: List[String] = ${ observationsImpl }

  private def observationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    def strip(value: Term): Term = value match
      case Inlined(_, Nil, body) => strip(body)
      case other => other
    def original(value: Term, count: Int): Term =
      if count == 0 then value
      else value match
        case Select(prefix, _) => original(prefix, count - 1)
        case _ => report.errorAndAbort("Q079 direct malformed target")
    def publicPeel(value: Term, count: Int): Option[(Term, List[SelectedMemberName])] =
      import quasiquotes.scalameta.ScalametaQuasiPattern.*
      if count == 0 then Some(value -> Nil)
      else value match
        case qq"$prefix.$name" => publicPeel(prefix, count - 1).map((root, names) => root -> (names :+ name))
        case _ => None
    type Roles = ProductTermCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: EmptyTuple
    val source = "$receiver" + (1 to 24).map(i => s".$$slot$i").mkString
    val parts = List("") ++ List.fill(24)(".") ++ List("")
    val plan = Q077StandardFlatModel.planSource(source, parts).fold(report.errorAndAbort(_), identity)
    val compiled = quasiquotes.scalameta.TermFrontend.compile(plan.effectiveSource).fold(e => report.errorAndAbort(e.message), identity)
    if compiled.engine != quasiquotes.scalameta.TermFrontend.Engine.Scalameta || compiled.primaryFailure.nonEmpty then
      report.errorAndAbort("Q079 direct requires Scalameta primary")
    val actual = plan.copy(pattern = compiled.pattern)
    val target = strip('{ Q079ChainTarget.root.alpha.beta.gamma.`+`.`type`.`safe spaced name`.alpha.beta.gamma.`+`.`type`.`safe spaced name`.alpha.beta.gamma.`+`.`type`.`safe spaced name`.alpha.beta.gamma.`+`.`type`.`safe spaced name` }.asTerm)
    val before = (0 to 24).map(i => original(target, i)).map(t => (t, t.symbol, t.symbol.owner))
    val extractor: TermPatternProductExtractor[Term, Term *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: EmptyTuple] = Q078TupleFactory.extractor[Roles](actual)
    val tuple: Term *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: SelectedMemberName *: EmptyTuple = extractor.unapply(target).getOrElse(report.errorAndAbort("Q079 direct unapply failed"))
    val receiver: Term = tuple.head
    val first: SelectedMemberName = tuple(1)
    val middle: SelectedMemberName = tuple(12)
    val last: SelectedMemberName = tuple(24)
    val names: List[SelectedMemberName] = tuple.tail.toList
    val expected = List.fill(4)(List("alpha", "beta", "gamma", "+", "type", "safe spaced name")).flatten
    val unchanged = before.zipWithIndex.forall { case ((t, symbol, owner), i) =>
      val after = original(target, i)
      (t eq after) && symbol == after.symbol && owner == after.symbol.owner
    }
    val equivalent = publicPeel(target, 24).exists((base, selected) => (base eq receiver) && selected == names)
    val valid = tuple.productArity == 25 && (receiver eq original(target, 24)) &&
      names.map(_.decoded) == expected && first.decoded == "alpha" &&
      middle.decoded == "safe spaced name" && last.decoded == "safe spaced name" && unchanged && equivalent
    if !valid then report.errorAndAbort("Q079 direct25 static/identity/public check failed")
    val tooFew = Q078TupleFactory.checkedTuple[Term, Roles](receiver, names.dropRight(1).toVector).isEmpty
    val tooMany = Q078TupleFactory.checkedTuple[Term, Roles](receiver, (names :+ first).toVector).isEmpty
    // Receiver holes match suffixes; anchoring remains an explicit consumer predicate.
    val suffixTarget = strip('{ Q079ChainTarget.root.alpha.beta.gamma.`+`.`type`.`safe spaced name`.alpha.beta.gamma.`+`.`type`.`safe spaced name`.alpha.beta.gamma.`+`.`type`.`safe spaced name`.alpha.beta.gamma.`+`.`type`.`safe spaced name`.alpha }.asTerm)
    val suffix = extractor.unapply(suffixTarget).getOrElse(report.errorAndAbort("Q079 suffix failed"))
    val rootSymbol = strip('{ Q079ChainTarget.root }.asTerm).symbol
    val anchored = receiver.symbol == rootSymbol && suffix.head.symbol != rootSymbol &&
      (suffix.head eq original(suffixTarget, 24)) && suffix.head.symbol.name == "alpha"
    Expr.ofList(List("direct24" -> valid, "count-short" -> tooFew, "count-surplus" -> tooMany,
      "suffix-vs-root" -> anchored).map((label, value) => Expr(s"$label:$value")))
