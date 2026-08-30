package quasiquotes.construct

/** A caller-universe-typed carrier for one repeated Term hole in `qr` construction.
  *
  * The carrier is deliberately distinct from both a single reflected Term and
  * an arbitrary `Seq`: callers must opt into sequence rank explicitly with
  * [[TermSequenceSplices.termSplice]], while the `..` source marker remains
  * mandatory at the interpolation site.
  */
final class TermSequenceSplice[+Term] private[construct] (
    private[construct] val terms: Vector[Term]
)

object TermSequenceSplices:
  /** Snapshots an ordered immutable sequence of caller-owned reflected Terms. */
  def termSplice[Term](terms: Seq[Term]): TermSequenceSplice[Term] =
    new TermSequenceSplice(terms.toVector)
