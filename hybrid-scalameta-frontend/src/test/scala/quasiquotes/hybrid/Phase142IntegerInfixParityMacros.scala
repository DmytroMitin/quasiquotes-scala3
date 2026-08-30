package quasiquotes.hybrid

import scala.quoted.*

import _root_.quasiquotes.construct.Quasiquotes
import _root_.quasiquotes.matching.{MatchNormalizer, QuasiPattern, TargetTermView, TermMatcher}
import _root_.quasiquotes.neutral.ScalametaTermProjection
import _root_.quasiquotes.parser.TinyTermParser
import _root_.quasiquotes.scalameta.{ScalametaQuasiPattern, ScalametaQuasiquotes, TermFrontend}
import _root_.quasiquotes.construct.hybrid.ScalametaTermFrontend

object Phase142IntegerInfixParityMacros:
  final case class MatrixRow(
      id: String,
      source: String,
      expectedType: String,
      currentValue: String,
      scalametaValue: String,
      programmaticValue: String,
      currentHasExpectedType: Boolean,
      scalametaHasExpectedType: Boolean,
      programmaticHasExpectedType: Boolean,
      currentStructure: String,
      scalametaStructure: String,
      programmaticStructure: String,
      neutralShape: String,
      currentParserShape: String,
      engine: String,
      primaryFailureEmpty: Boolean
  )

  final case class MatchingEvidence(
      currentFixedSuccess: Boolean,
      scalametaFixedSuccess: Boolean,
      currentFixedMismatch: Boolean,
      scalametaFixedMismatch: Boolean,
      currentPatternMatchesScalametaTerm: Boolean,
      scalametaPatternMatchesCurrentTerm: Boolean,
      currentQqCapturesCurrentOriginals: Boolean,
      currentQqCapturesScalametaOriginals: Boolean,
      scalametaQqCapturesCurrentOriginals: Boolean,
      scalametaQqCapturesScalametaOriginals: Boolean,
      scalametaPatternEngine: String,
      scalametaPatternPrimaryFailureEmpty: Boolean
  )

  inline def matrixEvidence: List[MatrixRow] = ${ matrixEvidenceImpl }
  inline def matchingEvidence: MatchingEvidence = ${ matchingEvidenceImpl }

  private def matrixEvidenceImpl(using q: Quotes): Expr[List[MatrixRow]] =
    import q.reflect.*

    type Row = (String, String, String, Term)

    def currentTerms: List[Row] =
      import Quasiquotes.qr
      List(
        ("literal", "1", "Int", qr"1"),
        ("addition", "1 + 1", "Int", qr"1 + 1"),
        ("precedence", "1 + 2 * 3", "Int", qr"1 + 2 * 3"),
        ("negative-left", "-1 + 2", "Int", qr"-1 + 2"),
        ("subtraction", "7 - 3", "Int", qr"7 - 3"),
        ("division", "7 / 2", "Int", qr"7 / 2"),
        ("remainder", "7 % 4", "Int", qr"7 % 4"),
        ("equal", "1 == 1", "Boolean", qr"1 == 1"),
        ("not-equal", "1 != 2", "Boolean", qr"1 != 2"),
        ("less", "1 < 2", "Boolean", qr"1 < 2"),
        ("less-equal", "1 <= 1", "Boolean", qr"1 <= 1"),
        ("greater", "2 > 1", "Boolean", qr"2 > 1"),
        ("greater-equal", "2 >= 2", "Boolean", qr"2 >= 2")
      )

    def scalametaTerms: List[Row] =
      import ScalametaQuasiquotes.qr
      List(
        ("literal", "1", "Int", qr"1"),
        ("addition", "1 + 1", "Int", qr"1 + 1"),
        ("precedence", "1 + 2 * 3", "Int", qr"1 + 2 * 3"),
        ("negative-left", "-1 + 2", "Int", qr"-1 + 2"),
        ("subtraction", "7 - 3", "Int", qr"7 - 3"),
        ("division", "7 / 2", "Int", qr"7 / 2"),
        ("remainder", "7 % 4", "Int", qr"7 % 4"),
        ("equal", "1 == 1", "Boolean", qr"1 == 1"),
        ("not-equal", "1 != 2", "Boolean", qr"1 != 2"),
        ("less", "1 < 2", "Boolean", qr"1 < 2"),
        ("less-equal", "1 <= 1", "Boolean", qr"1 <= 1"),
        ("greater", "2 > 1", "Boolean", qr"2 > 1"),
        ("greater-equal", "2 >= 2", "Boolean", qr"2 >= 2")
      )

    def expectedType(term: Term, name: String): Boolean =
      if name == "Int" then term.tpe.widen =:= TypeRepr.of[Int]
      else term.tpe.widen =:= TypeRepr.of[Boolean]

    def runtimeValue(term: Term, name: String): Expr[String] =
      if name == "Int" then '{ ${term.asExprOf[Int]}.toString }
      else '{ ${term.asExprOf[Boolean]}.toString }

    val rows = currentTerms.zip(scalametaTerms).map { (current, scalameta) =>
      val (id, source, typeName, currentTerm) = current
      val (scalametaId, scalametaSource, scalametaType, scalametaTerm) = scalameta
      require((scalametaId, scalametaSource, scalametaType) == (id, source, typeName))

      val programmatic = TermFrontend
        .build(using q)(Seq(source), Nil)
        .fold(failure => throw new IllegalArgumentException(failure.message), identity)
      val neutralTree = ScalametaTermFrontend
        .parse(source)
        .fold(failure => throw new IllegalArgumentException(failure.message), identity)
      val neutralShape = ScalametaTermProjection
        .project(neutralTree)
        .fold(error => throw new IllegalArgumentException(error.message), _.shape.render)

      '{
        MatrixRow(
          ${Expr(id)},
          ${Expr(source)},
          ${Expr(typeName)},
          ${runtimeValue(currentTerm, typeName)},
          ${runtimeValue(scalametaTerm, typeName)},
          ${runtimeValue(programmatic.term, typeName)},
          ${Expr(expectedType(currentTerm, typeName))},
          ${Expr(expectedType(scalametaTerm, typeName))},
          ${Expr(expectedType(programmatic.term, typeName))},
          ${Expr(MatchNormalizer.normalizedTreeStructure(using q)(currentTerm))},
          ${Expr(MatchNormalizer.normalizedTreeStructure(using q)(scalametaTerm))},
          ${Expr(MatchNormalizer.normalizedTreeStructure(using q)(programmatic.term))},
          ${Expr(neutralShape)},
          ${Expr(TinyTermParser.parseOrThrow(source).shape.render)},
          ${Expr(programmatic.engine.toString)},
          ${Expr(programmatic.primaryFailure.isEmpty)}
        )
      }
    }

    Expr.ofList(rows)

  private def matchingEvidenceImpl(using q: Quotes): Expr[MatchingEvidence] =
    import q.reflect.*

    val currentTerm =
      import Quasiquotes.qr
      qr"1 + 2 * 3"
    val scalametaTerm =
      import ScalametaQuasiquotes.qr
      qr"1 + 2 * 3"

    val currentFixed = QuasiPattern.termOrThrow("1 + 2 * 3").pattern
    val currentMismatch = QuasiPattern.termOrThrow("1 + 2 * 4").pattern
    val scalametaFixed = TermFrontend
      .compile("1 + 2 * 3")
      .fold(failure => throw new IllegalArgumentException(failure.message), identity)
    val scalametaMismatch = TermFrontend
      .compile("1 + 2 * 4")
      .fold(failure => throw new IllegalArgumentException(failure.message), identity)

    def originalChildren(term: Term): (Term, Term) =
      MatchNormalizer.normalizedView(term) match
        case Right(TargetTermView.Infix(left, "+", right, _)) =>
          (left.original, right.original)
        case Right(other) =>
          throw new IllegalArgumentException(s"expected normalized infix, obtained ${other.render}")
        case Left(failure) => throw new IllegalArgumentException(failure.message)

    def sameIdentity(left: Term, expectedLeft: Term, right: Term, expectedRight: Term): Boolean =
      (left.asInstanceOf[AnyRef] eq expectedLeft.asInstanceOf[AnyRef]) &&
        (right.asInstanceOf[AnyRef] eq expectedRight.asInstanceOf[AnyRef])

    def currentQqIdentity(term: Term): Boolean =
      import QuasiPattern.qq
      val (expectedLeft, expectedRight) = originalChildren(term)
      term match
        case qq"$left + $right" => sameIdentity(left, expectedLeft, right, expectedRight)
        case _ => false

    def scalametaQqIdentity(term: Term): Boolean =
      import ScalametaQuasiPattern.qq
      val (expectedLeft, expectedRight) = originalChildren(term)
      term match
        case qq"$left + $right" => sameIdentity(left, expectedLeft, right, expectedRight)
        case _ => false

    val currentFixedSuccess =
      TermMatcher.matchTerm(using q)(currentFixed, currentTerm).isRight &&
        TermMatcher.matchTerm(using q)(currentFixed, scalametaTerm).isRight
    val scalametaFixedSuccess =
      TermMatcher.matchTerm(using q)(scalametaFixed.pattern, currentTerm).isRight &&
        TermMatcher.matchTerm(using q)(scalametaFixed.pattern, scalametaTerm).isRight
    val currentFixedMismatch =
      TermMatcher.matchTerm(using q)(currentMismatch, currentTerm).isLeft &&
        TermMatcher.matchTerm(using q)(currentMismatch, scalametaTerm).isLeft
    val scalametaFixedMismatch =
      TermMatcher.matchTerm(using q)(scalametaMismatch.pattern, currentTerm).isLeft &&
        TermMatcher.matchTerm(using q)(scalametaMismatch.pattern, scalametaTerm).isLeft

    '{
      MatchingEvidence(
        ${Expr(currentFixedSuccess)},
        ${Expr(scalametaFixedSuccess)},
        ${Expr(currentFixedMismatch)},
        ${Expr(scalametaFixedMismatch)},
        ${Expr(TermMatcher.matchTerm(using q)(currentFixed, scalametaTerm).isRight)},
        ${Expr(TermMatcher.matchTerm(using q)(scalametaFixed.pattern, currentTerm).isRight)},
        ${Expr(currentQqIdentity(currentTerm))},
        ${Expr(currentQqIdentity(scalametaTerm))},
        ${Expr(scalametaQqIdentity(currentTerm))},
        ${Expr(scalametaQqIdentity(scalametaTerm))},
        ${Expr(scalametaFixed.engine.toString)},
        ${Expr(scalametaFixed.primaryFailure.isEmpty)}
      )
    }
