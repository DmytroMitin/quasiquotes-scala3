package quasiquotes.matching

import quasiquotes.syntax.RankMarkerScanner.{unquotedDotRuns, hasSplitDoubleDot}
import scala.quoted.Quotes

private[matching] final case class RankedCompiledPattern(
    pattern: TermPattern,
    holeNames: Vector[String],
    sequenceHoleName: String
)

private[matching] final case class RankedTemplate(
    effectiveParts: Vector[String],
    holeNames: Vector[String],
    sequenceIndex: Option[Int]
):
  def source: String =
    effectiveParts.zipWithIndex.foldLeft(new StringBuilder) {
      case (builder, (part, index)) =>
        builder.append(part)
        holeNames.lift(index).foreach(name => builder.append('$').append(name))
        builder
    }.toString

private[quasiquotes] object RankedPatternSource:
  private[quasiquotes] def unsupportedFamilyRankDiagnostic(
      parts: Seq[String],
      family: String
  ): Option[String] =
    val runs = unquotedDotRuns(parts.toVector)
    if runs.exists(_._3 >= 3) then
      Some(s"rank-3 captures are not supported for $family patterns")
    else if runs.exists(_._3 == 2) || hasSplitDoubleDot(parts.toVector, runs) then
      Some(s"rank-2 captures are not supported for $family patterns")
    else None

  def classify(parts: List[String]): Either[String, RankedTemplate] =
    if parts.size <= 1 then
      Left("at least one term capture slot is required")
    else
      val partVector = parts.toVector
      val markers = unquotedDotRuns(partVector)
      if hasSplitDoubleDot(partVector, markers) then
        Left("orphan or malformed rank-marker spelling; `..` must be adjacent")
      else markers.find(_._3 >= 3) match
        case Some((partIndex, _, _)) =>
          Left(s"unsupported rank-3 `...` capture marker in literal part $partIndex")
        case None =>
          val rankTwo = markers.filter(_._3 == 2)
          val consumed = rankTwo.filter { case (partIndex, offset, _) =>
            partIndex < partVector.size - 1 && offset == partVector(partIndex).length - 2
          }
          if consumed.size > 1 then
            Left("only one rank-2 sequence-Term capture is supported in a complete qq pattern")
          else
            rankTwo.find(marker => !consumed.contains(marker)) match
              case Some((partIndex, _, _)) =>
                Left(s"orphan or malformed `..` rank marker in literal part $partIndex")
              case None =>
                val effective = consumed.headOption match
                  case Some((partIndex, _, _)) =>
                    partVector.updated(partIndex, partVector(partIndex).dropRight(2))
                  case None => partVector
                val holeNames = Vector.tabulate(parts.size - 1)(index => s"qqCapture$index")
                Right(RankedTemplate(effective, holeNames, consumed.headOption.map(_._1)))

  def compile(parts: List[String], sequenceIndex: Int): Either[String, RankedCompiledPattern] =
    classify(parts).flatMap { template =>
      if template.sequenceIndex != Some(sequenceIndex) then
        Left("ranked qq template classification changed before pattern compilation")
      else
        QuasiPattern.termLocated(template.source).left.map(_.diagnostic.message).flatMap { pattern =>
          val sequenceName = template.holeNames(sequenceIndex)
          validateSequencePosition(pattern.pattern, sequenceName).map(_ =>
            RankedCompiledPattern(pattern.pattern, template.holeNames, sequenceName)
          )
        }
    }

  def compileOrAbort(using q: Quotes)(
      parts: List[String],
      sequenceIndex: Int
  ): RankedCompiledPattern =
    compile(parts, sequenceIndex).fold(
      detail => q.reflect.report.errorAndAbort(s"Invalid qq term-pattern template: $detail"),
      identity
    )

  private def validateSequencePosition(
      pattern: TermPattern,
      sequenceName: String
  ): Either[String, Unit] =
    var totalOccurrences = 0
    var admittedOccurrences = 0

    def visit(current: TermPattern): Unit =
      current match
        case TermPattern.Hole(name) if name == sequenceName => totalOccurrences += 1
        case TermPattern.Lambda1(_, _, _, body) => visit(body)
        case TermPattern.Select(qualifier, _) => visit(qualifier)
        case TermPattern.Apply(function, arguments) =>
          visit(function)
          arguments.foreach {
            case TermPattern.Hole(name) if name == sequenceName =>
              totalOccurrences += 1
              admittedOccurrences += 1
            case argument => visit(argument)
          }
        case TermPattern.New(_, arguments) =>
          arguments.foreach {
            case TermPattern.Hole(name) if name == sequenceName =>
              totalOccurrences += 1
              admittedOccurrences += 1
            case argument => visit(argument)
          }
        case TermPattern.Infix(left, _, right) => visit(left); visit(right)
        case TermPattern.Unary(_, operand) => visit(operand)
        case TermPattern.InterpolatedString(_, _, arguments) => arguments.foreach(visit)
        case TermPattern.Typed(expression, _) => visit(expression)
        case TermPattern.Tuple(elements) => elements.foreach(visit)
        case TermPattern.If(condition, thenBranch, elseBranch) =>
          visit(condition); visit(thenBranch); visit(elseBranch)
        case TermPattern.Block(prefix, result) =>
          prefix.foreach {
            case term: TermPattern => visit(term)
            case BlockPatternStatement.LocalVal(_, _, _, initializer) => visit(initializer)
          }
          visit(result)
        case TermPattern.Parenthesized(inner) => visit(inner)
        case _ => ()

    visit(pattern)
    if totalOccurrences == 1 && admittedOccurrences == 1 then Right(())
    else if totalOccurrences == 0 then
      Left(
        "the rank-2 sequence-Term capture was not consumed as a direct ordinary Apply or fixed one-list New argument"
      )
    else
      Left(
        "rank-2 sequence-Term capture is supported only once as a direct ordinary Apply or fixed one-list New argument"
      )
