package mlbigbook.wordcount

import org.apache.spark.rdd.RDD

import scala.collection.{ mutable, Map }
import scala.reflect.ClassTag

/**
 * Definitions of data structures. Includes definition of the distributed data trait DistData.
 * DistData defines methods for transforming and manipulating data of any size.
 * 
 * @author Malcolm Greaves 
 */

object Data {

  type Word = String

  /** A sentence is a sequence of words. */
  case class Sentence(words: Traversable[Word]) {
    override def toString = s"(${words.size})" + words.take(15).mkString(",") + (if (words.size > 15) "..." else "")
  }

  /** A document is a sequence of sentences. */
  case class Document(sentences: Traversable[Sentence]) {
    override def toString = {
      val x = sentences.foldLeft((1, List.empty[String]))({
        case ((i, a), s) => (i + 1, a :+ s"S$i:$s")
      })._2
      s"(${x.size} documents)" + x.take(5).mkString(";") + (if (x.size > 5) "..." else "")
    }
  }

  /** A corpus is a collection of documents. */ 
  type Corpus = DistData[Data.Document]

  /** The result of counting words from text. */
  type WordCount = Map[Word, Long]
  val EmptyWordCount: WordCount = Map()

  /** The result of performing some real-valued weighting of word counts. */
  type NormalizedWordCount = Map[Word, Double]
  val EmptyNormalizedWordCount: WordCount = Map()
}

/**
 * Trait that abstractly represents operations that can be performed on a dataset.
 * The implementation of DistData is suitable for both large-scale, distributed data
 * or in-memory structures.
 */
trait DistData[A] {

  /** Transform a dataset by applying f to each element. */
  def map[B: ClassTag](f: A => B): DistData[B]

  /** 
   * Starting from a defined zero value, perform an operation seqOp on each element
   * of a dataset. Combine results of seqOp using combOp for a final value.
   */
  def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B

  /** Sort the dataset using a function f that evaluates each element to an orderable type */
  def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): DistData[A]

  /** Construct a traversable for the first k elements of a dataset. Will load into main mem. */
  def take(k: Int): Traversable[A]

  /** Load all elements of the dataset into an array in main memory. */
  def collect(): Array[A]
}

/**
 * Collection of implicit conversions from common data types to DistData. Currently can
 * convert an RDD (from Spark) or anything that extends scala.collections.Traversable, which
 * includes Seq and List.
 */
object DistData {
  implicit def traversable2DistData[A: ClassTag](l: Traversable[A]): DistData[A] = TravDistData(l)
  implicit def rdd2DistData[A](d: RDD[A]): DistData[A] = RDDDistData(d)
}

/**
 * Wraps a Traversable as a DistData. 
 */
case class TravDistData[A: ClassTag](ls: Traversable[A]) extends DistData[A] {
  override def map[B: ClassTag](f: A => B): DistData[B] = new TravDistData(ls.map(f))
  override def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B = ls.aggregate(zero)(seqOp, combOp)
  override def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): DistData[A] = new TravDistData(ls.toSeq.sortBy(f))
  override def take(k: Int): Traversable[A] = ls.take(k)
  override def collect(): Array[A] = ls.toArray
}

/**
 * Wraps a Spark RDD as a DistData.
 */
case class RDDDistData[A](d: RDD[A]) extends DistData[A] {
  override def map[B: ClassTag](f: A => B) = new RDDDistData(d.map(f))
  override def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B = d.aggregate(zero)(seqOp, combOp)
  override def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): DistData[A] = new RDDDistData(d.sortBy(f))
  override def take(k: Int): Traversable[A] = d.take(k)
  override def collect(): Array[A] = d.collect()
}

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
  private val empty:Map[String, N] = Map()

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
