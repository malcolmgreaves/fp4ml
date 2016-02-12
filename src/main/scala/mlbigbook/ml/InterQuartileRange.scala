package mlbigbook.ml

import breeze.linalg.Vector
import fif.Data
import fif.Data.ops._
import mlbigbook.math.{ NumericConversion, VectorOpsT }

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.ClassTag

object InterQuartileRange extends Serializable {

  // A container that helps us build the five number summary.
  private[this] case class BuildingFns[N: Numeric](
    min:    Option[N],
    q1:     Option[N],
    median: Option[N],
    q2:     Option[N],
    max:    Option[N]
  )

  // Provides a convenient function for creating a BuildingFns instance where
  // all members are None.
  private[this] object BuildingFns {
    def empty[N: Numeric] =
      BuildingFns[N](None, None, None, None, None)
  }

  /**
   * Evaluates to the first non-None element in the sequence of Options.
   * If there are no non-None elements, then the method evaluates to None.
   */
  def getFirst[T](options: Option[T]*): Option[T] =
    options.foldLeft(Option.empty[T]) {
      case (result, nextOpt) =>
        if (result isEmpty)
          nextOpt
        else
          result
    }

  def iqrForSingleFeature[D[_]: Data, N: Numeric: ClassTag](
    qi: QuartileIndicies
  )(
    reduced: D[N]
  ): FiveNumSummary[N] = {

    // This import allows us to reference the fields of the QuartileIndicies
    // instance. It makes it easier to check if a particular index matches one
    // of these indicies; we can use the `<value name>` syntax in the match
    // statement below.
    import qi._

    val sortedAscending = reduced.sortBy(identity)

    val buildingFns =
      sortedAscending
        .zipWithIndex
        .mapParition { bothPartIndices =>

          val partiallyBuildFns =
            bothPartIndices.foldLeft(BuildingFns.empty) {
              case (bFns, (value, index)) =>
                index match {
                  case 0L            => bFns.copy(min = Some(value))
                  case `q1Index`     => bFns.copy(q1 = Some(value))
                  case `medianIndex` => bFns.copy(median = Some(value))
                  case `q2Index`     => bFns.copy(q2 = Some(value))
                  case `maxIndex`    => bFns.copy(max = Some(value))
                  case _             => bFns
                }
            }
          Iterable(partiallyBuildFns)
        }
        .reduce {
          case (bFns1, bFns2) =>
            // Here we're using getFirst because we know that at most one
            // of the BuildingFns instances fields will be non-None.
            // This conclusion is due to the fact that we know there's only
            // one minimum, one q1, etc. in the entire data.
            BuildingFns(
              min = getFirst(bFns1.min, bFns2.min),
              q1 = getFirst(bFns1.q1, bFns2.q1),
              median = getFirst(bFns1.median, bFns2.median),
              q2 = getFirst(bFns1.q2, bFns2.q2),
              max = getFirst(bFns1.max, bFns2.max)
            )
        }

    // the following get calls are safe since we know:
    // (1) nElements > 0, meaning that each of the indicies are > 0 too
    // (2) (1) means that we'll be able to get some values, so at the
    //     end, the Options are going to be filled
    // (3) and since we know there's >=5 elements, we know that these
    //     values make sense
    FiveNumSummary(
      min = buildingFns.min.get,
      q1 = buildingFns.q1.get,
      median = buildingFns.median.get,
      q2 = buildingFns.q2.get,
      max = buildingFns.max.get
    )
  }

  def apply[D[_]: Data, V[_], N: Numeric: ClassTag](
    data: D[V[N]]
  )(implicit vops: VectorOpsT[N, V]): Seq[FiveNumSummary[N]] =
    data.headOption match {

      case Some(firstVector) =>
        val nElements = data.size
        if (nElements < 5l)
          Seq.empty[FiveNumSummary[N]]

        else {

          val fiveNumSum = iqrForSingleFeature[D, N](QuartileIndicies(nElements)) _
          val dimensionality = vops.size(firstVector)

          (0 until dimensionality).map { vectorIndex =>
            fiveNumSum(
              data.map { vector =>
                vops.valueAt(vector)(vectorIndex)
              }
            )
          }
        }

      case None =>
        Seq.empty[FiveNumSummary[N]]
    }

}