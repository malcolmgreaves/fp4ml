/*
 * Definition of the distributed data trait Data.
 * Data defines methods for transforming and manipulating data of any size
 * in a purely functional manner.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

import org.apache.spark.SparkContext
import org.apache.spark.rdd.{ PairRDDFunctions, RDD }

import scala.collection.immutable
import scala.reflect.ClassTag

/**
 * Trait that abstractly represents operations that can be performed on a dataset.
 * The implementation of Data is suitable for both large-scale, distributed data
 * or in-memory structures.
 */
trait Data[A] {

  /** Transform a dataset by applying f to each element. */
  def map[B: ClassTag](f: A => B): Data[B]

  def mapParition[B: ClassTag](f: Iterator[A] => Iterator[B]): Data[B]

  /** Apply a side-effecting function to each element. */
  def foreach(f: A => Any): Unit

  def foreachPartition(f: Iterator[A] => Any): Unit
  /**
   * Starting from a defined zero value, perform an operation seqOp on each element
   * of a dataset. Combine results of seqOp using combOp for a final value.
   */
  def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B

  /** Sort the dataset using a function f that evaluates each element to an orderable type */
  def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): Data[A]

  /** Construct a traversable for the first k elements of a dataset. Will load into main mem. */
  def take(k: Int): Traversable[A]

  /** Load all elements of the dataset into an array in main memory. */
  def toSeq: Seq[A]

  def flatMap[B: ClassTag](f: A => TraversableOnce[B]): Data[B]

  def groupBy[B: ClassTag](f: A => B): Data[(B, Iterable[A])]

  def as: Data[A] = this

  def reduce[A1 >: A](r: (A1, A1) => A1): A1

  def toMap[T, U](implicit ev: A <:< (T, U)): Map[T, U]

  def size: Long

  def isEmpty: Boolean

  def sum[N >: A](implicit num: Numeric[N]): N

  //  def zip[A1 >: A, B](that: Data[B]): Data[(A1,B)]

}

object Data {

  implicit def seq2data[A](s: Seq[A]): Data[A] =
    s.toTraversable

  implicit def indexedSeq2Data[A](s: IndexedSeq[A]): Data[A] =
    s.toTraversable

  implicit def array2Data[A](a: Array[A]): Data[A] =
    a.toTraversable

  def traversable2data[A](t: Traversable[A]): Data[A] = t

  /** Wraps a Traversable as a Data. */
  implicit class TravData[A](val ls: Traversable[A]) extends Data[A] {

    override def map[B: ClassTag](f: A => B): Data[B] =
      new TravData(ls.map(f))

    override def mapParition[B: ClassTag](f: Iterator[A] => Iterator[B]): Data[B] =
      f(ls.toIterator).toTraversable

    override def foreach(f: A => Any): Unit =
      ls.foreach(f)

    override def foreachPartition(f: Iterator[A] => Any): Unit = {
      val _ = f(ls.toIterator)
    }

    override def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B =
      ls.aggregate(zero)(seqOp, combOp)

    override def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): Data[A] =
      new TravData(ls.toSeq.sortBy(f))

    override def take(k: Int): Traversable[A] =
      ls.take(k)

    override def toSeq: Seq[A] =
      ls.toSeq

    override def flatMap[B: ClassTag](f: A => TraversableOnce[B]): Data[B] =
      new TravData(ls.flatMap(f))

    override def groupBy[B: ClassTag](f: A => B): Data[(B, Iterable[A])] =
      new TravData(
        ls
          .groupBy(f)
          .toTraversable
          .map({ case (b, iter) => (b, iter.toIterable) })
      )

    override def reduce[A1 >: A](r: (A1, A1) => A1): A1 =
      ls.reduce(r)

    override def toMap[T, U](implicit ev: A <:< (T, U)): Map[T, U] =
      ls.toMap

    override def size: Long =
      ls.size

    override def isEmpty: Boolean =
      ls.isEmpty

    override def sum[N >: A](implicit num: Numeric[N]): N =
      ls.sum(num)

    //    override def zip[A1 >: A, B](that: Data[B]): Data[(A1,B)] =
    //      ls.toIterable.zip(that)

  }

  /** Wraps a Spark RDD as a Data. */
  implicit class RddData[A](d: RDD[A]) extends Data[A] {

    override def map[B: ClassTag](f: A => B) =
      new RddData(d.map(f))

    override def mapParition[B: ClassTag](f: Iterator[A] => Iterator[B]): Data[B] =
      d.mapPartitions(f)

    override def foreach(f: A => Any): Unit =
      d.foreach(f)

    override def foreachPartition(f: Iterator[A] => Any): Unit = {
      val _ = d.foreachPartition(x => { val __ = f(x) })
    }

    override def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B =
      d.aggregate(zero)(seqOp, combOp)

    override def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): Data[A] =
      new RddData(d.sortBy(f))

    override def take(k: Int): Traversable[A] =
      d.take(k)

    override def toSeq: Seq[A] = {
      // force evaluated type
      // ( don't want to invoke implicit conversion
      //   from Array[A] -> Data[A] !! )                                                                 Changes not staged for commit:

      val a: Array[A] = d.collect()
      a.toIndexedSeq
    }

    override def flatMap[B: ClassTag](f: A => TraversableOnce[B]): Data[B] =
      new RddData(d.flatMap(f))

    override def groupBy[B: ClassTag](f: A => B): Data[(B, Iterable[A])] =
      ???
    //new RDDData(
    //  new PairRDDFunctions(d.groupBy(f))
    //    .partitionBy(???)
    //)

    override def reduce[A1 >: A](r: (A1, A1) => A1): A1 =
      d.reduce(r)

    override def toMap[T, U](implicit ev: A <:< (T, U)): Map[T, U] =
      d
        .mapPartitions(items => Iterator(items.toSeq.toMap(ev)))
        .reduce(_ ++ _)

    override def size: Long =
      d.count()

    override def isEmpty: Boolean =
      d.isEmpty()

    override def sum[N >: A](implicit num: Numeric[N]): N =
      reduce[N] { case (a, b) => num.plus(a, b) }

    //    override def zip[A1 >: A, B](that: Data[B]): Data[(A1, B)] =
    //      d.zip(that)
  }
}

/** Type that allows us to convert an interable sequence of data into a Data type. */
trait DataContext {
  def from[T: ClassTag](data: Iterable[T]): Data[T]
}

/** Implicit conversions to DataContext types. */
object DataContext {

  /** Implicitly converts a SparkContext into a DataContext type. */
  implicit def sparkContext2DataContext(sc: SparkContext): DataContext =
    SparkDataContext(sc)

  implicit val travDDContext: DataContext =
    TraversableDataContext
}

case class SparkDataContext(sc: SparkContext) extends DataContext {

  import Data._

  @Deprecated
  override def from[T: ClassTag](data: Iterable[T]): Data[T] =
    sc.parallelize(data.toSeq)
}

case object TraversableDataContext extends DataContext {

  import Data._

  override def from[T: ClassTag](data: Iterable[T]): Data[T] =
    data.toSeq
}
