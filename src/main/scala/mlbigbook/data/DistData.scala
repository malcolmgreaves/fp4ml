/*
 * Definition of the distributed data trait DistData.
 * DistData defines methods for transforming and manipulating data of any size
 * in a purely functional manner.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

import org.apache.spark.SparkContext
import org.apache.spark.rdd.{ PairRDDFunctions, RDD }

import scala.reflect.ClassTag

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
  def toSeq: Seq[A]

  def flatMap[B: ClassTag](f: A => TraversableOnce[B]): DistData[B]

  def groupBy[B: ClassTag](f: A => B): DistData[(B, Iterable[A])]
}

object DistData {

  /** Wraps a Traversable as a DistData. */
  implicit class TravDistData[A](val ls: Traversable[A]) extends DistData[A] {

    override def map[B: ClassTag](f: A => B): DistData[B] =
      new TravDistData(ls.map(f))

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
      new TravDistData(
        ls
          .groupBy(f)
          .toTraversable
          .map({ case (b, iter) => (b, iter.toIterable) })
      )
  }

  /** Wraps a Spark RDD as a DistData. */
  implicit class RDDDistData[A](d: RDD[A]) extends DistData[A] {

    override def map[B: ClassTag](f: A => B) =
      new RDDDistData(d.map(f))

    override def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B =
      d.aggregate(zero)(seqOp, combOp)

    //<<<<<<< HEAD
    override def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): DistData[A] =
      new RDDDistData(d.sortBy(f))

    override def take(k: Int): Traversable[A] =
      d.take(k)

    override def toSeq(): Seq[A] =
      d.collect().toSeq

    override def flatMap[B: ClassTag](f: A => TraversableOnce[B]): DistData[B] =
      new RDDDistData(d.flatMap(f))

    override def groupBy[B: ClassTag](f: A => B): DistData[(B, Iterable[A])] =
      new RDDDistData(
        new PairRDDFunctions(d.groupBy(f))
          .partitionBy(???)
      )
  }
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
