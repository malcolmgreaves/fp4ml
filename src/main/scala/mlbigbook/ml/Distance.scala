/*
 * Trait, and implementations of, distance functions suitable for metric spaces.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.ml

import mlbigbook.data.Vector
import mlbigbook.wordcount.Similarity

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

  implicit def fn2distance(f: (Vector, Vector) => Double): Distance =
    new Distance {
      override def apply(v1: Vector, v2: Vector): Double = f(v1, v2)
    }
}

case object Euclidian extends Distance {

  override def apply(v1: Vector, v2: Vector): Double =
    Math.sqrt(
      v1.zip(v2)
        .foldLeft(0.0)({
          case (d, (_, value1, value2)) => {
            val difference = value1 - value2
            val squared = difference * difference
            d + squared
          }
        })
    )
}

case object Manhattan extends Distance {

  override def apply(v1: Vector, v2: Vector): Double =
    v1.zip(v2)
      .foldLeft(0.0)({
        case (d, (_, value1, value2)) => {
          val difference = value1 - value2
          d + difference
        }
      })
}

case object Cosine extends Distance {

  override def apply(v1: Vector, v2: Vector): Double =
    1.0 - Similarity.cosine(v1, v2)
}
