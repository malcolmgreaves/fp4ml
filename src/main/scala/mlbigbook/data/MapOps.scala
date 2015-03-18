/*
 * Collection of classes, traits, and objects for manipulating Map types.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

import scala.collection.Map

/** Object for adding double and long maps together */
object AddMap {
  val Real = new AddMap[Double]
  val Whole = new AddMap[Long]
}

/** Class that supports operations for adding elements and combining maps */
class AddMap[@specialized(Byte, Int, Long, Float, Double) N: Numeric] {

  import scala.Numeric.Implicits._

  val empty: Map[String, N] = Map()

  def add(m: Map[String, N], k: String, v: N): Map[String, N] = {
    m.get(k) match {
      case Some(existing) => (m - k) + (k -> (existing + v))
      case None           => m + (k -> v)
    }
  }

  /**
   * Combines two maps. If maps m1 and m2 both have key k, then the resulting
   * map will have m1(k) + m2(k) for the value of k.
   */
  def combine(m1: Map[String, N], m2: Map[String, N]): Map[String, N] = {
    val (a, b) = if (m1.size < m2.size) (m1, m2) else (m2, m1)
    a.foldLeft(b)({
      case (aggmap, (k, v)) => aggmap.get(k) match {
        case Some(existing) => (aggmap - k) + (k -> (existing + v))
        case None           => aggmap + (k -> v)
      }
    })
  }
}

/** Class that supports operations on maps that indicate the presense of keys. */
object IndicatorMap extends AddMap[Long] {

  override def add(m: Map[String, Long], word: String, ignore: Long): Map[String, Long] = mark(m, word)

  /** Ensures that word is in the resulting map with a value of 1 */
  def mark(m: Map[String, Long], word: String): Map[String, Long] = {
    m.get(word) match {
      case Some(existing) => {
        if (existing == 1) {
          m
        } else {
          (m - word) + (word -> 1)
        }
      }
      case None => m + (word -> 1)
    }
  }

  /** Constructs a map that has all of the words from the input maps */
  override def combine(m1: Map[String, Long], m2: Map[String, Long]): Map[String, Long] = {
    val (a, b) = if (m1.size < m2.size) (m1, m2) else (m2, m1)
    a.foldLeft(b)({
      case (aggmap, (k, _)) => mark(aggmap, k)
    })
  }
}

/** Object for multiplying double maps together. */
object MultiplyMap {
  val Real: MultiplyMap[Double] = new MultiplyMap[Double]()
}

/** Class for multiplying maps together */
class MultiplyMap[@specialized(Long, Double) N: Numeric] {

  import scala.Numeric.Implicits._

  private val addmap = new AddMap[N]()
  private val empty: Map[String, N] = Map()

  /**
   * Constructs a mapping where all elements' values are multiplied together.
   * Note that only keys that appear in both maps will be present in the resulting mapping.
   * The keys that are not in the resulting mapping would have value 0.
   */
  def multiplyWith(larger: Map[String, N])(smaller: Map[String, N]): Map[String, N] = {
    smaller.aggregate(empty)(
      {
        case (aggmap, (k, v)) => larger.get(k) match {
          case Some(existing) => (aggmap - k) + (k -> (existing * v))
          case None           => aggmap // (anything) * 0 = 0
        }
      },
      addmap.combine
    )
  }
}
