package quasiquotes.types

import quasiquotes.syntax.RankMarkerScanner

private[types] object TypeSequenceSource:
  private val rankError = "TYPE_SEQUENCE_RANK_MISMATCH: a Type sequence requires one adjacent `..` marker; scalar Type splices cannot carry rank markers."
  private val positionError = "UNSUPPORTED_TYPE_SEQUENCE_POSITION: expected exactly one constructor splice and one sequence in the complete application `$constructor[..$arguments]`."

  def scalarDiagnostic(parts: Seq[String]): Option[String] =
    if parts.exists(_ == null) then Some(rankError)
    else
      val runs = RankMarkerScanner.unquotedDotRuns(parts.toVector, includeInterpolationBodies = false)
      Option.when(runs.exists(_._3 >= 2) || RankMarkerScanner.hasSplitDoubleDot(parts.toVector, runs))(rankError)

  def validate(parts: Seq[String], additionalSequences: Boolean): Either[String, Unit] =
    if additionalSequences then
      Left("MULTIPLE_TYPE_SEQUENCE_SPLICES: exactly one Type argument sequence is supported.")
    else if parts.exists(_ == null) then Left(rankError)
    else
      val vector = parts.toVector
      val runs = RankMarkerScanner.unquotedDotRuns(vector, includeInterpolationBodies = false)
      val markers = runs.filter(_._3 >= 2)
      val orphanDot = runs.exists { (part, offset, length) =>
        length == 1 && ((part < vector.size - 1 && vector(part).substring(offset + 1).forall(_.isWhitespace)) ||
          (part > 0 && vector(part).substring(0, offset).forall(_.isWhitespace)))
      }
      val adjacentSequence = markers == Vector((1, vector.lift(1).fold(0)(_.length) - 2, 2))
      if orphanDot || RankMarkerScanner.hasSplitDoubleDot(vector, runs) ||
          markers.exists(_._3 != 2) || (markers.nonEmpty && !adjacentSequence) then Left(rankError)
      else if parts == Seq("", "[..", "]") then Right(())
      else if parts == Seq("", "[", "]") then Left(rankError)
      else Left(positionError)
