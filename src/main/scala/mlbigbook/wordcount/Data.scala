package mlbigbook.wordcount

import org.apache.spark.rdd.RDD

import scala.collection.Map
import scala.reflect.ClassTag

object Data {
  type Word = String

  case class Sentence(words: IndexedSeq[Word])

  case class Document(sentences: IndexedSeq[Sentence])

  type Corpus = DistData[Data.Document]

  type WordCount = Map[Word, Long]
  val EmptyWordCount: WordCount = Map()

  type NormalizedWordCount = Map[Word, Double]
  val EmptyNormalizedWordCount: WordCount = Map()
}

trait DistData[A] {

  def map[B: ClassTag](f: A => B): DistData[B]

  def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B

  def foldLeft[B: ClassTag](zero: B)(seqOp: (B, A) => B): B

  def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): DistData[A]

  def take(k: Int): Traversable[A]
}

object DistData {

  implicit def traversable2DistData[A](l: Seq[A]): DistData[A] = TravDistData(l)

  case class TravDistData[A](ls: Seq[A]) extends DistData[A] {
    override def map[B: ClassTag](f: A => B) = new TravDistData(ls.map(f))

    override def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B = ls.aggregate(zero)(seqOp, combOp)

    override def foldLeft[B: ClassTag](zero: B)(seqOp: (B, A) => B): B = ls.foldLeft(zero)(seqOp)

    override def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): DistData[A] = ls.sortBy(f)

    override def take(k: Int): Traversable[A] = ls.take(k)
  }

  implicit def rdd2DistData[A](d: RDD[A]): DistData[A] = RDDDistData(d)

  case class RDDDistData[A](d: RDD[A]) extends DistData[A] {
    override def map[B: ClassTag](f: A => B) = new RDDDistData(d.map(f))

    override def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B = d.aggregate(zero)(seqOp, combOp)

    override def foldLeft[B: ClassTag](zero: B)(seqOp: (B, A) => B): B = d.foldLeft(zero)(seqOp)

    override def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): DistData[A] = d.sortBy(f)

    override def take(k: Int): Traversable[A] = d.take(k)
  }

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
