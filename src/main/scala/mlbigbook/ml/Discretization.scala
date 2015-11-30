package mlbigbook.ml

import fif.Data
import Data.ops._
import mlbigbook.math.VectorOpsT
import simulacrum.typeclass
import breeze.linalg.Vector

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.ClassTag

@typeclass trait Discretization[T] {

  def discretize[D[_]: Data, V[_] <: Vector[_], N: Numeric](
    t: T
  )(
    data:    D[V[N]],
    headers: Seq[String]
  )(
    implicit
    vops: VectorOpsT[N, V]
  ): (D[Seq[String]], Seq[Seq[String]])

}

object Iqr extends Serializable {

  case class FiveNumSummary[N: Numeric](
      min:    N,
      q1:     N,
      median: N,
      q2:     N,
      max:    N
  ) {

    override def toString =
      s"[Min: $min, Quartile 1: $q1, Median: $median, Quartile 2: $q2, Max: $max]"
  }

  object FiveNumSummary {

    def empty[N: Numeric]: FiveNumSummary[N] = {
      val zero = implicitly[Numeric[N]].zero
      FiveNumSummary(zero, zero, zero, zero, zero)
    }
  }

  private[this] case class BuildingFns[N: Numeric](
    min:    Option[N],
    q1:     Option[N],
    median: Option[N],
    q2:     Option[N],
    max:    Option[N]
  )

  private[this] object BuildingFns {
    def empty[N: Numeric] =
      BuildingFns[N](None, None, None, None, None)
  }

  def apply[D[_]: Data, V[_] <: Vector[_], N: Numeric: ClassTag](
    data: D[V[N]]
  )(
    implicit
    vops: VectorOpsT[N, V]
  ): Seq[FiveNumSummary[N]] =
    data.headOption match {

      case Some(first) =>
        val nElements = data.size
        if (nElements < 5l)
          Seq.empty[FiveNumSummary[N]]

        else {

          val dimensionality = first.size

          val q1Index = nElements / 4l // integer division OK
          val medianIndex = nElements / 2l // integer division OK
          val q2Index = 3l * q1Index // integer division OK
          val maxIndex = nElements - 1l

            def fiveNumSum(reduced: D[N]) = {

              val sortedAscending = reduced.sortBy(identity)

              val buildingFns =
                sortedAscending
                  .zipWithIndex
                  .mapParition { bothPartIndices =>

                    val partiallyBuildFns =
                      bothPartIndices
                        .foldLeft(BuildingFns.empty) {
                          case (bFns, (value, index)) =>

                            index match {
                              case 0L =>
                                bFns.copy(min = Some(value))

                              case `q1Index` =>
                                bFns.copy(q1 = Some(value))

                              case `medianIndex` =>
                                bFns.copy(median = Some(value))

                              case `q2Index` =>
                                bFns.copy(q2 = Some(value))

                              case `maxIndex` =>
                                bFns.copy(max = Some(value))

                              case _ =>
                                bFns
                            }
                        }
                    Iterable(partiallyBuildFns)
                  }
                  .reduce {
                    case (bFns1, bFns2) =>

                      def getFirst[T](options: Option[T]*): Option[T] =
                          options.foldLeft(Option.empty[T]) {
                            case (result, nextOpt) =>
                              if (result isEmpty)
                                nextOpt
                              else
                                result
                          }

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

          (0 until dimensionality)
            .map { vectorIndex =>
              fiveNumSum {
                data.map { vector =>
                  vops.valueAt(vector)(vectorIndex)
                }
              }
            }
        }

      case None =>
        Seq.empty[FiveNumSummary[N]]
    }

}

object IqrDiscretization {

  val discretizedValues =
    Seq("below_min", "min_q1", "q1_median", "median_q2", "q2_max", "above_or_equal_to_max")

  def apply[D[_]: Data, V[_] <: Vector[_], N: Numeric: ClassTag](
    data:    D[V[N]],
    headers: Seq[String]
  )(
    implicit
    vops: VectorOpsT[N, V]
  ): (D[Seq[String]], Seq[Seq[String]]) = {

    val fiveNumberSummaries = Iqr(data)
    if (fiveNumberSummaries isEmpty)
      (data.map(_ => Seq.empty[String]), Seq.empty[Seq[String]])

    else {

      val newDiscreteValuesPerFeat =
        headers.map { featureName =>
          discretizedValues.map { dv => s"$dv-$featureName" }
        }

      val discretizedData: D[Seq[String]] = {
        val lessThan = implicitly[Numeric[N]].lt _
        data
          .map { vector =>
            vops.toSeq(vector)
              .zip(fiveNumberSummaries)
              .map {
                case (value, fns) =>
                  // ordering of if statements below is _important_ !!
                  if (lessThan(value, fns.min))
                    discretizedValues.head

                  else if (lessThan(value, fns.q1))
                    discretizedValues(1)

                  else if (lessThan(value, fns.median))
                    discretizedValues(2)

                  else if (lessThan(value, fns.q2))
                    discretizedValues(3)

                  else if (lessThan(value, fns.max))
                    discretizedValues(4)

                  else
                    discretizedValues.last
              }
          }
      }

      (discretizedData, newDiscreteValuesPerFeat)
    }
  }

}