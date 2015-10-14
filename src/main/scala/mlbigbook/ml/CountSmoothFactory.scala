package mlbigbook.ml

/**
 * Module that defines and produces a type, Smoothing, that is able to perform
 * count smoothing on observed label-feature pairs from data.
 */
trait CountSmoothFactory[N] { factory =>

  /**
   * Evidence that N is a numeric type.
   */
  implicit val num: Numeric[N]

  /**
   * Creates a Smoothing instance from a counted feature map.
   */
  def apply[Label, Feature](
    fm: CountingNaiveBayes[N]#FeatureMap[Label, Feature]
  ): Smoothing[Label, Feature]

  /**
   * Performs count smoothing.
   */
  trait Smoothing[Label, Feature] {

    /**
     * A Smoothing's numeric instance is the same as its factory.
     */
    implicit final val num: Numeric[N] = factory.num

    /**
     * The pseudo count that this Smoothing type uses.
     */
    def sCount: N

    /**
     * The smoothed counts, aggregated on a per-label basis.
     */
    def sAggregateLabel: Map[Label, N]

    /**
     * The sum of all smoothed counts across all feature-label
     * co-occurrence pairs.
     */
    def sAggregateTotal: N
  }
}