package mlbigbook.ml

import mlbigbook.data.{ UnlabeledData, Labeled }

/** Creates a Classifier from a Centers instance. */
object HardCluster {

  /**
   * Makes a classifier from a set of cluster centers. The returned class is equal
   * to one of the cluster center's id's.
   */
  def apply[T](d: Distance)(vcents: VectorizedCenters[T]): Classifier[T] =

    if (vcents.centers.isEmpty) {

      (ignore: T) => UnlabeledData.asLabled

    } else if (vcents.centers.size == 1) {

      val singleCenterId = Labeled(vcents.centers(1).id)
      (ignore: T) => singleCenterId

    } else {

      val nCenters = vcents.centers.size
      val compDistanceFromCenters = SoftCluster(d)(vcents)

      (input: T) => {

        val distCenters = compDistanceFromCenters(input)

        val minCenter =
          distCenters.slice(1, nCenters)
            .foldLeft(distCenters.head)({
              case (min @ (_, minDist), next @ (_, nextDist)) =>
                if (nextDist < minDist)
                  next
                else
                  min
            })._1

        Labeled(minCenter.id)

      }
    }

}
