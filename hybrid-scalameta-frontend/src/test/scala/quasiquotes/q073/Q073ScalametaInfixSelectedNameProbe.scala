package quasiquotes.matching

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName
import quasiquotes.construct.hybrid.ScalametaTermFrontend
import quasiquotes.scalameta.TermFrontend

final class Q073ScalametaOperand(val label: String):
  infix def +(other: Q073ScalametaOperand): Q073ScalametaOperand = this
  infix def combine(other: Q073ScalametaOperand): Q073ScalametaOperand = this
  infix def ::(other: Q073ScalametaOperand): Q073ScalametaOperand = this

object Q073ScalametaOperandFixture:
  val left = Q073ScalametaOperand("left")
  val right = Q073ScalametaOperand("right")

/** Test-only Q073 Scalameta-primary source plan joined to the reflected bridge. */
object Q073ScalametaInfixSelectedNameProbe:
  private val LeftHole = "q073Left"
  private val RightHole = "q073Right"
  private val Transport = "+++"

  def sourceObservation: String =
    val source = s"$$$LeftHole $Transport $$$RightHole"
    val compiled = TermFrontend.compile(source).fold(
      failure => throw new AssertionError(failure.message),
      identity
    )
    val exact = ScalametaTermFrontend.validateExactCompiler(
      PatternSource.synthesizeMapped(source).fold(
        failure => throw new AssertionError(failure.message),
        _.patternSource.source
      )
    )
    List(
      compiled.engine == TermFrontend.Engine.Scalameta,
      compiled.primaryFailure.isEmpty,
      exact.isRight,
      compiled.pattern.render == "Infix(Hole($q073Left), +++, Hole($q073Right))"
    ).mkString(":")

  inline def reflectedObservation: String = ${ reflectedObservationImpl }

  inline def publicDynamicMatches: Boolean = ${ publicDynamicMatchesImpl }

  inline def publicFixedMatches: Boolean = ${ publicFixedMatchesImpl }

  inline def rightAssociatedRejected: Boolean = ${ rightAssociatedRejectedImpl }

  private def reflectedObservationImpl(using q: Quotes): Expr[String] =
    import q.reflect.*
    val source = s"$$$LeftHole $Transport $$$RightHole"
    val plan = TermFrontend.compile(source).fold(
      failure => report.errorAndAbort(failure.message),
      identity
    )
    if plan.engine != TermFrontend.Engine.Scalameta || plan.primaryFailure.nonEmpty then
      report.errorAndAbort("Q073 hybrid plan did not remain Scalameta-primary")
    val target = stripInlined(
      '{ Q073ScalametaOperandFixture.left + Q073ScalametaOperandFixture.right }.asTerm
    )
    val (originalLeft, originalRight) = target match
      case Apply(Select(left, "+"), right :: Nil) => (left, right)
      case other =>
        report.errorAndAbort(
          "Q073 hybrid + fixture changed shape: " + other.show(using Printer.TreeStructure)
        )

    val extractor: TermPatternProductExtractor[
      q.reflect.Term,
      (q.reflect.Term, SelectedMemberName, q.reflect.Term)
    ] = TermPatternProductExtractorFactory.productExtractor { candidate =>
      matchSelected(plan.pattern, candidate)
    }
    val (left, selected, right) = extractor.unapply(target).getOrElse(
      report.errorAndAbort("Q073 hybrid reflected bridge did not match")
    )
    Expr(
      s"${selected.decoded}:${left eq originalLeft}:${right eq originalRight}:" +
        s"${plan.engine == TermFrontend.Engine.Scalameta}"
    )

  private def publicDynamicMatchesImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq
    stripInlined(
      '{ Q073ScalametaOperandFixture.left + Q073ScalametaOperandFixture.right }.asTerm
    ) match
      case qq"$left $operatorName $right" => Expr(true)
      case _ => Expr(false)

  private def publicFixedMatchesImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq
    stripInlined(
      '{ Q073ScalametaOperandFixture.left + Q073ScalametaOperandFixture.right }.asTerm
    ) match
      case qq"$left + $right" => Expr(true)
      case _ => Expr(false)

  private def rightAssociatedRejectedImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    val source = s"$$$LeftHole $Transport $$$RightHole"
    val plan = TermFrontend.compile(source).fold(
      failure => report.errorAndAbort(failure.message),
      identity
    )
    val target = stripInlined(
      '{ Q073ScalametaOperandFixture.left :: Q073ScalametaOperandFixture.right }.asTerm
    )
    Expr(matchSelected(plan.pattern, target).isEmpty)

  private def matchSelected(using q: Quotes)(
      pattern: TermPattern,
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName, q.reflect.Term)] =
    if target == null then None
    else
      for
        raw <- TargetTermView.fromTerm(target).toOption
        decoded <- raw match
          case TargetTermView.Apply(
                TargetTermView.Select(_, name @ ("+" | "-" | "*" | "/" | "%"), _),
                _ :: Nil,
                _
              ) => Some(name)
          case _ => None
        selected <- SelectedMemberName.from(decoded).toOption
        concrete <- pattern match
          case TermPattern.Infix(left, Transport, right) =>
            Some(TermPattern.Infix(left, decoded, right))
          case _ => None
        result <- TermMatcher.matchTerm(concrete, target).toOption
        left <- result.bindings.get(LeftHole)
        right <- result.bindings.get(RightHole)
      yield (left, selected, right)

  private def stripInlined(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term match
      case Inlined(_, Nil, body) => stripInlined(body)
      case other => other
