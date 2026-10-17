package quasiquotes.matching

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName

final class Q073Operand(val label: String):
  infix def +(other: Q073Operand): Q073Operand = this
  infix def -(other: Q073Operand): Q073Operand = this
  infix def combine(other: Q073Operand): Q073Operand = this
  infix def __qq_selected_name_placeholder(other: Q073Operand): Q073Operand = this
  infix def ::(other: Q073Operand): Q073Operand = this
  def unary_- : Q073Operand = this
  def generic[A](other: A): Q073Operand = this
  def contextual(other: Q073Operand)(using String): Q073Operand = this
  def many(first: Q073Operand, second: Q073Operand): Q073Operand = this

object Q073OperandFixture:
  val left = Q073Operand("left")
  val right = Q073Operand("right")
  val context = "context"

/** Test-only Q073 single selected infix-name source role and reflected bridge. */
object Q073InfixSelectedNameProbe:
  private val ExistingNormalizedOperators = Set("+", "-", "*", "/", "%")
  private val Transport = "+++"
  private val LeftHole = "q073Left"
  private val RightHole = "q073Right"

  extension (inline sc: StringContext)
    transparent inline def q073qq(using q: Quotes) =
      ${ extractor('sc, 'q) }

  private def extractor(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        partExpressions.toList.map(_.valueOrAbort)
      case _ =>
        quotes.reflect.report.errorAndAbort(
          "Q073 test-only selected infix source requires a static StringContext",
          context
        )

    if parts != List("", " ", " ", "") then
      quotes.reflect.report.errorAndAbort(
        "Q073 test-only selected infix source requires exactly $left $operatorName $right",
        context
      )

    val effectiveSource = s"$$$LeftHole $Transport $$$RightHole"
    val compiled = QuasiPattern.term(effectiveSource).fold(
      failure =>
        quotes.reflect.report.errorAndAbort(
          "Q073 test-only selected infix source failed standard parsing: " + failure.message,
          context
        ),
      _.pattern
    )
    compiled match
      case TermPattern.Infix(
            TermPattern.Hole(LeftHole),
            Transport,
            TermPattern.Hole(RightHole)
          ) => ()
      case other =>
        quotes.reflect.report.errorAndAbort(
          "Q073 test-only selected infix source compiled to the wrong topology: " + other.render,
          context
        )

    '{
      Q073InfixSelectedNameProbe.testOnlyExtractor(using $callerQuotes)
    }

  private[quasiquotes] def testOnlyExtractor(using q: Quotes)
      : TermPatternProductExtractor[
        q.reflect.Term,
        (q.reflect.Term, SelectedMemberName, q.reflect.Term)
      ] =
    TermPatternProductExtractorFactory.productExtractor(target => matchSelected(target))

  private[quasiquotes] def matchSelected(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName, q.reflect.Term)] =
    if target == null then None
    else
      for
        raw <- TargetTermView.fromTerm(target).toOption
        decoded <- raw match
          case TargetTermView.Apply(
                TargetTermView.Select(_, name, _),
                _ :: Nil,
                _
              ) if ExistingNormalizedOperators(name) && !name.endsWith(":") => Some(name)
          case _ => None
        selected <- SelectedMemberName.from(decoded).toOption
        concrete = TermPattern.Infix(
          TermPattern.Hole(LeftHole),
          decoded,
          TermPattern.Hole(RightHole)
        )
        matched <- TermMatcher.matchTerm(concrete, target).toOption
        left <- matched.bindings.get(LeftHole)
        right <- matched.bindings.get(RightHole)
      yield (left, selected, right)

  def sourceRoleObservation: String =
    val source = "$left $operatorName $right"
    val mapped = PatternSource.synthesizeMapped(source).fold(
      error => throw new AssertionError(error.message),
      identity
    )
    val spans = mapped.occurrences.map(occurrence =>
      s"${occurrence.name}:${occurrence.originalSpan.start}-${occurrence.originalSpan.end}"
    )
    val publicClassification =
      SelectedMemberPatternSource.classifyIncludingRanked(List("", " ", " ", ""))
    val effective = QuasiPattern.term(s"$$$LeftHole $Transport $$$RightHole").fold(
      error => throw new AssertionError(error.message),
      identity
    )
    List(
      mapped.occurrences.map(_.name).mkString(","),
      spans.mkString(","),
      publicClassification == Right(None),
      effective.pattern.render,
      mapped.occurrences(1).originalSpan == quasiquotes.source.SourceSpan(6, 19)
    ).mkString("|")

  def lexicalObservation: String =
    val qPlus = SelectedMemberName.from("+").isRight
    val qColon = SelectedMemberName.from("::").isRight
    val qPlain = SelectedMemberName.from("combine").isRight
    val qSpaced = SelectedMemberName.from("safe name").isRight
    val qUnicode = SelectedMemberName.from("naïve").isLeft
    val nPlus = quasiquotes.definitions.DefinitionName.plain("+").isLeft
    val nPlain = quasiquotes.definitions.DefinitionName.plain("combine").isRight
    List(qPlus, qColon, qPlain, qSpaced, qUnicode, nPlus, nPlain).mkString(":")
