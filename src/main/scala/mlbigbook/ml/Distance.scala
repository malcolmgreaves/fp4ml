/*
 * Trait, and implementations of, distance functions suitable for metric spaces.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.ml

import mlbigbook.data.Vector

/**
 * Type for a distance function.
 *
 * Evaluated value must be non-negative and obey the rules of a distance in a metric space.
 * These properties are:
 *   ** non-negativity : \forall x,y d(x,y) >= 0
 *   ** equality :  d(x,y) = 0 <==> x = y
 *   ** symmetry : \forall x,y d(x,y) = d(y,x)
 *   ** triangle inequality : \forall x,y,z d(x,y) <= d(x,z) + d(z,y)
 */
trait Distance extends ((Vector, Vector) => Double)

object Distance {

  @inline implicit def fn2distance(f: (Vector, Vector) => Double): Distance =
    new Distance {
      @inline override def apply(v1: Vector, v2: Vector): Double = f(v1, v2)
    }
}

case object Euclidian extends Distance {

  @inline override def apply(v1: Vector, v2: Vector): Double =
    Math.sqrt(
      v1.zip(v2)
        .foldLeft(0.0)({
          case (d, (_, value1, value2)) =>
            val difference = value1 - value2
            val squared = difference * difference
            d + squared
        })
    )
}

case object Manhattan extends Distance {

  @inline override def apply(v1: Vector, v2: Vector): Double =
    v1.zip(v2)
      .foldLeft(0.0)({
        case (d, (_, value1, value2)) =>
          val absDifference = Math.abs(value1 - value2)
          d + absDifference
      })
}

/**
 * Computes the cosine distance between two vectors, which is defined as:
 *
 *                  | v1 * v2 |
 *       1.0  -  -----------------
 *                  |v1| * |v2|
 *
 * where * is dot product and |...| is L1 norm.
 */
case object Cosine extends Distance {

  import mlbigbook.data.Vector._

  @inline override def apply(v1: Vector, v2: Vector): Double =
    1.0 - Math.abs(dotProduct(v1, v2)) / (absoluteValue(v1) * absoluteValue(v2))
}

case object Chebyshev extends Distance {

  @inline override def apply(v1: Vector, v2: Vector): Double =
    v1.zip(v2)
      .foldLeft(Option.empty[Double])({
        case (max, (_, value1, value2)) =>

          val absDiff = Math.abs(value1 - value2)
          max match {

            case m @ Some(maxValue) =>
              if (maxValue < absDiff)
                Some(absDiff)
              else
                m

            case None =>
              Some(absDiff)
          }
      }) match {

        case Some(maximumFound) =>
          maximumFound

        case None =>
          0.0
      }
}

case object BrayCurtis extends Distance {

  @inline override def apply(v1: Vector, v2: Vector): Double = {

    val (sumAbsPairwiseDiff, sumAbsPairwiseSum) =
      v1.zip(v2)
        .foldLeft((0.0, 0.0))({
          case ((absDiffSum, absPairSum), (_, value1, value2)) =>
            (absDiffSum + Math.abs(value1 - value2), absPairSum + Math.abs(value1 + value2))
        })

    sumAbsPairwiseDiff / sumAbsPairwiseSum
  }

}

case object Canberra extends Distance {

  @inline override def apply(v1: Vector, v2: Vector): Double =
    v1.zip(v2)
      .foldLeft(0.0)({
        case (sum, (_, value1, value2)) =>
          val absDiff = Math.abs(value1 - value2)
          val indivAbsSum = Math.abs(value1) + Math.abs(value2)
          sum + (absDiff / indivAbsSum)
      })
}

case object MinkowskiMaker {
  def apply(p: Int): Distance =
    new Distance {

      @inline private def raiseAbsDiffToP(v1: Double, v2: Double): Double =
        Math.pow(Math.abs(v1 - v2), p)

      private val pInv = 1.0 / p

      @inline private def raiseTo1OverP(x: Double): Double =
        Math.pow(x, pInv)

      @inline override def apply(v1: Vector, v2: Vector): Double =
        raiseTo1OverP(
          v1.zip(v2)
            .foldLeft(0.0)({
              case (sum, (_, value1, value2)) =>
                sum + raiseAbsDiffToP(value1, value2)
            })
        )
    }
}

// Not ready yet...
// Defined as:
//
//    1−(u−u¯)⋅(v−v¯)||(u−u¯)||2||(v−v¯)||2
//
//  where u~ is the mean of u
// ============================================================================
//case object CorrelationDist extends Distance {
//
//  @inline def computeMean(v: Vector): Double = {
//
//    val elementSum =
//      v.nonZeros.foldLeft(0.0)({
//        case (sum, (_, value)) =>
//          sum + value
//      })
//
//    elementSum / v.cardinality.toDouble
//  }
//
//  @inline override def apply(v1: Vector, v2: Vector): Double =
//    ???
//
//}