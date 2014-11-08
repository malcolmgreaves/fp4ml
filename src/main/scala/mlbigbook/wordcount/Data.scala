package mlbigbook.wordcount

import org.apache.spark.rdd.RDD

import scala.collection.{ mutable, Map }
import scala.reflect.ClassTag

object Data {
  type Word = String

  case class Sentence(words: Traversable[Word]) {
    override def toString = "(${words.size})" + words.take(15).mkString(",") + (if (words.size > 15) "..." else "")
  }

  case class Document(sentences: Traversable[Sentence]) {
    override def toString = {
      val x = sentences.foldLeft((1, List.empty[String]))({
        case ((i, a), s) => (i + 1, a :+ s"S$i:$s")
      })._2
      "(${x.size} documents)" + x.take(5).mkString(";") + (if (x.size > 5) "..." else "")
    }
  }

  type Corpus = DistData[Data.Document]

  type WordCount = Map[Word, Long]
  val EmptyWordCount: WordCount = Map()

  type NormalizedWordCount = Map[Word, Double]
  val EmptyNormalizedWordCount: WordCount = Map()
}

trait DistData[A] {

  def map[B: ClassTag](f: A => B): DistData[B]

  def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B

  def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): DistData[A]

  def take(k: Int): Traversable[A]

  def collect(): Array[A]
}

object DistData {
  implicit def traversable2DistData[A: ClassTag](l: Traversable[A]): DistData[A] = TravDistData(l)
  implicit def rdd2DistData[A](d: RDD[A]): DistData[A] = RDDDistData(d)
}

case class TravDistData[A: ClassTag](ls: Traversable[A]) extends DistData[A] {
  override def map[B: ClassTag](f: A => B): DistData[B] = new TravDistData(ls.map(f))

  override def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B = ls.aggregate(zero)(seqOp, combOp)

  override def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): DistData[A] = new TravDistData(ls.toSeq.sortBy(f))

  override def take(k: Int): Traversable[A] = ls.take(k)

  override def collect(): Array[A] = ls.toArray
}

case class RDDDistData[A](d: RDD[A]) extends DistData[A] {
  override def map[B: ClassTag](f: A => B) = new RDDDistData(d.map(f))

  override def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B = d.aggregate(zero)(seqOp, combOp)

  override def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): DistData[A] = new RDDDistData(d.sortBy(f))

  override def take(k: Int): Traversable[A] = d.take(k)

  override def collect(): Array[A] = d.collect()
}

object AddMap {
  val Real = new AddMap[Double]
  val Whole = new AddMap[Long]
}

class AddMap[@specialized(Byte, Int, Long, Float, Double) N: Numeric] {

  import scala.Numeric.Implicits._

  val empty: Map[String, N] = Map()

  def mark(m: Map[String, N], k: String, v: N): Map[String, N] = {
    m.get(k) match {
      case Some(existing) => (m - k) + (k -> (existing + v))
      case None           => m + (k -> v)
    }
  }

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

object IndicatorMap extends AddMap[Long] {

  override def mark(m: Map[String, Long], word: String, ignore: Long): Map[String, Long] = mark(m, word)

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

  override def combine(m1: Map[String, Long], m2: Map[String, Long]): Map[String, Long] = {
    val (a, b) = if (m1.size < m2.size) (m1, m2) else (m2, m1)
    a.foldLeft(b)({
      case (aggmap, (k, _)) => mark(aggmap, k)
    })
  }
}

object MultiplyMap {
  val Real: MultiplyMap[Double] = new MultiplyMap[Double]()
}

class MultiplyMap[@specialized(Long, Double) N: Numeric] {

  import scala.Numeric.Implicits._

  private val addmap = new AddMap[N]()

  def multiplyWith(larger: Map[String, N])(smaller: Map[String, N]): Map[String, N] = {
    smaller.aggregate(larger)(
      {
        case (aggmap, (k, v)) => aggmap.get(k) match {
          case Some(existing) => (aggmap - k) + (k -> (existing * v))
          case None           => aggmap // 0 * _ => 0
        }
      },
      addmap.combine
    )
  }
}
