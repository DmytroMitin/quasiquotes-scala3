package quasiquotes.matching

import scala.compiletime.erasedValue
import scala.quoted.*
import quasiquotes.construct.SelectedMemberName
import quasiquotes.source.ReflectedPositionProvenance

/** Test-only static product construction. No erased tuple conversion or casts. */
private[matching] object Q078TupleFactory:
  type Captures[T, Kinds <: Tuple] <: Tuple = Kinds match
    case EmptyTuple => EmptyTuple
    case ProductTermCapture *: tail => T *: Captures[T, tail]
    case ProductSelectedNameCapture *: tail => SelectedMemberName *: Captures[T, tail]

  private inline def tuple[T, Kinds <: Tuple](
      receiver: T,
      names: Vector[SelectedMemberName],
      index: Int = 0
  ): Captures[T, Kinds] =
    inline erasedValue[Kinds] match
      case _: EmptyTuple => EmptyTuple
      case _: (ProductTermCapture *: tail) =>
        receiver *: tuple[T, tail](receiver, names, index)
      case _: (ProductSelectedNameCapture *: tail) =>
        names(index) *: tuple[T, tail](receiver, names, index + 1)

  inline def nameCount[Kinds <: Tuple]: Int =
    inline erasedValue[Kinds] match
      case _: EmptyTuple => 0
      case _: (ProductTermCapture *: tail) => nameCount[tail]
      case _: (ProductSelectedNameCapture *: tail) => 1 + nameCount[tail]

  inline def checkedTuple[T, Kinds <: Tuple](receiver: T, names: Vector[SelectedMemberName]): Option[Captures[T, Kinds]] =
    Option.when(names.size == nameCount[Kinds])(tuple[T, Kinds](receiver, names))

  transparent inline def extractor[Kinds <: Tuple](using q: Quotes)(
      plan: Q077StandardFlatModel.Plan
  ): TermPatternProductExtractor[q.reflect.Term, Captures[q.reflect.Term, Kinds]] =
    TermPatternProductExtractorFactory.productExtractor(target =>
      Q077StandardFlatModel.matchTarget(plan, target).flatMap(result =>
        checkedTuple[q.reflect.Term, Kinds](result.receiver, result.names)
      )
    )

private[matching] object Q078GenericMacro:
  def extractor(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes],
      compile: String => Either[String, TermPattern]
  )(using Quotes): Expr[Any] =
    import quotes.reflect.*
    val parts = context match
      case '{ StringContext(${ Varargs(values) }*) } => values.toList.map(_.valueOrAbort)
      case _ => report.errorAndAbort("Q078 requires literal StringContext parts", context)
    // Candidate first: no position access for an unrelated template.
    val depth = parts.size - 2
    if depth < 1 || parts != List("") ++ List.fill(depth)(".") ++ List("") then
      report.errorAndAbort("Q078 requires a direct flat selected chain", context)
    val source = ReflectedPositionProvenance.sourceCode(context.asTerm.pos).getOrElse(
      report.errorAndAbort("Q078 selected chain requires recoverable original source", context)
    )
    val plan = Q077StandardFlatModel.planSource(source, parts).fold(report.errorAndAbort(_), identity)
    val compiled = compile(plan.effectiveSource).fold(report.errorAndAbort(_), identity)
    Q077StandardFlatModel.validate(compiled, plan.receiverName, plan.selectedOccurrences)
      .fold(report.errorAndAbort(_), identity)
    val cons = TypeRepr.of[Any *: EmptyTuple] match
      case AppliedType(constructor, _) => constructor
      case other => report.errorAndAbort(s"Q078 missing tuple constructor: ${other.show}")
    val roles = TypeRepr.of[ProductTermCapture] :: List.fill(depth)(TypeRepr.of[ProductSelectedNameCapture])
    val kinds = roles.foldRight(TypeRepr.of[EmptyTuple])((head, tail) => AppliedType(cons, List(head, tail)))
    val pattern = RankedTermPatternBridge.patternExpr(compiled)
    val occurrences = Expr.ofSeq(plan.selectedOccurrences.map { occurrence =>
      '{ SelectedNamePatternOccurrence(${Expr(occurrence.name)}, ${Expr(occurrence.selectOrdinal)}, ${Expr(occurrence.transport)}) }
    })
    val starts = Expr.ofSeq(plan.sourceStarts.map(Expr(_)))
    val partList = Expr.ofList(parts.map(Expr(_)))
    val emittedPlan = '{ Q077StandardFlatModel.Plan(
      ${Expr(source)}, $partList, ${Expr(plan.effectiveSource)}, $pattern,
      ${Expr(plan.receiverName)}, $occurrences.toVector, $starts.toVector
    ) }
    kinds.asType match
      case '[captureKinds] =>
        '{ Q078TupleFactory.extractor[captureKinds & Tuple](using $callerQuotes)($emittedPlan) }

object Q078StandardInterpolator:
  extension (inline sc: StringContext)
    transparent inline def q078(using q: Quotes) =
      ${ Q078StandardMacro.extractor('sc, 'q) }

private[matching] object Q078StandardMacro:
  def extractor(context: Expr[StringContext], callerQuotes: Expr[Quotes])(using Quotes): Expr[Any] =
    Q078GenericMacro.extractor(context, callerQuotes,
      source => QuasiPattern.term(source).left.map(_.message).map(_.pattern))
