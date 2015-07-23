/*
 * Definition of the distributed data trait DistData.
 * DistData defines methods for transforming and manipulating data of any size
 * in a purely functional manner.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

import scala.reflect.ClassTag
import scala.util.Random

import mlbigbook.util.Sampling

/**
 * Trait that abstractly represents operations that can be performed on a dataset.
 * The implementation of DistData is suitable for both large-scale, distributed data
 * or in-memory structures.
 *
 * @author Malcolm Greaves
 * @author Marek Kolodziej
 */
trait DistData[A] {

  /** Transform a dataset by applying f to each element. */
  def map[B: ClassTag](f: A => B): DistData[B]

  /** This has type A as opposed to B >: A due to the RDD limitations */
  def reduceLeft(op: (A, A) => A): A

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
  def toSeq: Seq[A]

  def flatMap[B: ClassTag](f: A => TraversableOnce[B]): DistData[B]

  def groupBy[B: ClassTag](f: A => B): DistData[(B, Iterable[A])]

  def sample(withReplacement: Boolean, fraction: Double, seed: Long): DistData[A]

  def exactSample(fraction: Double, seed: Long): DistData[A]

  def size: Long

  def headOption: Option[A]

  def zipWithIndex: DistData[(A, Long)]

  def foreach(f: A => Unit): Unit
}

/**
 * Collection of implicit conversions from common data types to DistData. Currently can
 * convert an RDD (from Spark) or anything that extends scala.collections.Traversable, which
 * includes Seq and List.
 */
object DistData {
  /** Implicitly converts a Traversable into a DistData type. */
  @inline implicit def traversable2DistData[A: ClassTag](l: Traversable[A]): DistData[A] =
    TravDistData(l)

  /** Implicitly converts an RDD into a DistData type. */
  @inline implicit def rdd2DistData[A: ClassTag](d: RDD[A]): DistData[A] =
    RDDDistData(d)
}

/** Wraps a Traversable as a DistData. */
case class TravDistData[A: ClassTag](ls: Traversable[A]) extends DistData[A] {

  override def map[B: ClassTag](f: A => B): DistData[B] =
    new TravDistData(ls.map(f))

  override def reduceLeft(op: (A, A) => A): A =
    ls.reduceLeft(op)

  override def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B =
    ls.aggregate(zero)(seqOp, combOp)

  override def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): DistData[A] =
    new TravDistData(ls.toSeq.sortBy(f))

  override def take(k: Int): Traversable[A] =
    ls.take(k)

  override def toSeq: Seq[A] =
    ls.toSeq

  override def flatMap[B: ClassTag](f: A => TraversableOnce[B]): DistData[B] =
    new TravDistData(ls.flatMap(f))

  override def groupBy[B: ClassTag](f: A => B): DistData[(B, Iterable[A])] =
    new TravDistData(ls.groupBy(f).toTraversable.map({ case (b, iter) => (b, iter.toIterable) }))

  override def sample(withReplacement: Boolean, fraction: Double, seed: Long): DistData[A] =
    Sampling.sample(ls, math.round(fraction * ls.size).toInt, withReplacement, seed)

  override def exactSample(fraction: Double, seed: Long): DistData[A] =
    sample(withReplacement = false, fraction = fraction, seed = seed)

  override def size: Long =
    ls.size

  override def headOption: Option[A] =
    ls.headOption

  override def zipWithIndex: DistData[(A, Long)] =
    ls.toIndexedSeq.zipWithIndex.map(a => (a._1, a._2.toLong))

  override def foreach(f: A => Unit): Unit =
    ls.foreach(f)
}

/** Wraps a Spark RDD as a DistData. */
case class RDDDistData[A: ClassTag](d: RDD[A]) extends DistData[A] {

  override def map[B: ClassTag](f: A => B) =
    new RDDDistData(d.map(f))

  override def reduceLeft(op: (A, A) => A): A =
    d.reduce(op)


  override def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B =
    d.aggregate(zero)(seqOp, combOp)

  override def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): DistData[A] =
    new RDDDistData(d.sortBy(f))

  override def take(k: Int): Traversable[A] =
    d.take(k)

  override def toSeq: Seq[A] =
    d.collect().toSeq

  override def flatMap[B: ClassTag](f: A => TraversableOnce[B]): DistData[B] =
    new RDDDistData(d.flatMap(f))

  override def groupBy[B: ClassTag](f: A => B): DistData[(B, Iterable[A])] =
    d.groupBy(f)

  override def sample(withReplacement: Boolean, fraction: Double, seed: Long = Random.nextLong()): DistData[A] =
    d.sample(withReplacement, fraction, seed)

  override def exactSample(fraction: Double, seed: Long): DistData[A] = {

    val withIdx = d.zipWithIndex
    val modulo = math.round(1.0 / fraction).toInt
    val randNum = new Random(seed).nextInt(modulo)
    val filtered = withIdx.filter { case (datum, idx) => (idx + randNum) % modulo == 0 }
    val mapped = filtered.map { case (datum, idx) => datum }
    new RDDDistData(mapped)
  }

  /* This is a lazy val as opposed to a def, because the count() method on the RDD
     triggers a MapReduce job. Once we know the number of rows once, there's no point
     re-computing it since RDDs are immutable.
   */
  override lazy val size: Long = d.count()

  override def headOption: Option[A] =
    if (!d.isEmpty) Some(d.first()) else None

  override def zipWithIndex: DistData[(A, Long)] =
    d.zipWithIndex

  override def foreach(f: A => Unit): Unit =
    d.foreach(f)
}

/** Type that allows us to convert an interable sequence of data into a DistData type. */
trait DistDataContext {
  def from[T: ClassTag](data: Iterable[T]): DistData[T]
}

/** Implicit conversions to DistDataContext types. */
object DistDataContext {

  /** Implicitly converts a SparkContext into a DistDataContext type. */
  implicit def sparkContext2DistDataContext(sc: SparkContext): DistDataContext =
    SparkDistDataContext(sc)

  implicit val travDDContext: DistDataContext =
    TraversableDistDataContext
}

case class SparkDistDataContext(sc: SparkContext) extends DistDataContext {

  import DistData._

  @Deprecated
  override def from[T: ClassTag](data: Iterable[T]): DistData[T] =
    sc.parallelize(data.toSeq)
}

case object TraversableDistDataContext extends DistDataContext {

  import DistData._

  override def from[T: ClassTag](data: Iterable[T]): DistData[T] =
    data.toSeq
}

