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
sealed trait Data[A] {

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
  //    override def zip[A1 >: A, B
  def flatMap[B: ClassTag](f: A => TraversableOnce[B]): Data[B]

  def groupBy[B: ClassTag](f: A => B): Data[(B, Iterable[A])]

  def as: Data[A] = this

  def reduce[A1 >: A: ClassTag](r: (A1, A1) => A1): A1

  def toMap[T, U](implicit ev: A <:< (T, U)): Map[T, U]

  def size: Long

  def isEmpty: Boolean

  def sum[N >: A](implicit num: Numeric[N]): N

  def zip[A1 >: A: ClassTag, B: ClassTag, T[_]](that: T[B])(implicit view: T[B] => Data[B]): Data[(A1, B)]

}

/** Wraps a Traversable as a Data. */
case class TravData[A](ls: Traversable[A]) extends Data[A] {

  override def map[B: ClassTag](f: A => B): Data[B] =
    TravData(ls.map(f))

  override def mapParition[B: ClassTag](f: Iterator[A] => Iterator[B]): Data[B] =
    TravData(f(ls.toIterator).toTraversable)

  override def foreach(f: A => Any): Unit =
    ls.foreach(f)

  override def foreachPartition(f: Iterator[A] => Any): Unit = {
    val _ = f(ls.toIterator)
  }

  override def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B =
    ls.aggregate(zero)(seqOp, combOp)

  override def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): Data[A] =
    TravData(ls.toSeq.sortBy(f))

  override def take(k: Int): Traversable[A] =
    ls.take(k)

  override def toSeq: Seq[A] =
    ls.toSeq

  override def flatMap[B: ClassTag](f: A => TraversableOnce[B]): Data[B] =
    TravData(ls.flatMap(f))

  override def groupBy[B: ClassTag](f: A => B): Data[(B, Iterable[A])] =
    TravData(
      ls
        .groupBy(f)
        .toTraversable
        .map { case (b, iter) => (b, iter.toIterable) }
    )

  override def reduce[A1 >: A: ClassTag](r: (A1, A1) => A1): A1 =
    ls.reduce(r)

  override def toMap[T, U](implicit ev: A <:< (T, U)): Map[T, U] =
    ls.toMap

  override def size: Long =
    ls.size

  override def isEmpty: Boolean =
    ls.isEmpty

  override def sum[N >: A](implicit num: Numeric[N]): N =
    ls.sum(num)

  override def zip[A1 >: A: ClassTag, B: ClassTag, T[_]](that: T[B])(implicit view: T[B] => Data[B]): Data[(A1, B)] =
    view(that) match {

      case TravData(thatLs) =>
        TravData(ls.toIterable.zip(thatLs.toIterable).toTraversable)

      case RddData(thatD) =>
        val x: Seq[A1] = ls.toSeq
        RddData(thatD.sparkContext.parallelize(x).zip(thatD))
    }

}

/** Wraps a Spark RDD as a Data. */
case class RddData[A](d: RDD[A]) extends Data[A] {

  override def map[B: ClassTag](f: A => B): Data[B] =
    RddData(d.map(f))

  override def mapParition[B: ClassTag](f: Iterator[A] => Iterator[B]): Data[B] =
    RddData(d.mapPartitions(f))

  override def foreach(f: A => Any): Unit =
    d.foreach { a =>
      val _ = f(a)
    }

  override def foreachPartition(f: Iterator[A] => Any): Unit =
    d.foreachPartition { a =>
      val _ = f(a)
    }

  override def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B =
    d.aggregate(zero)(seqOp, combOp)

  override def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): Data[A] =
    RddData(d.sortBy(f))

  override def take(k: Int): Traversable[A] =
    d.take(k)

  override def toSeq: Seq[A] =
    d.collect().toIndexedSeq // toIndexedSeq makes a copy of the array, ensuring that it's immutable to the receiver

  override def flatMap[B: ClassTag](f: A => TraversableOnce[B]): Data[B] =
    RddData(d.flatMap(f))

  override def groupBy[B: ClassTag](f: A => B): Data[(B, Iterable[A])] =
    ???
  //new RDDData(
  //  new PairRDDFunctions(d.groupBy(f))
  //    .partitionBy(???)
  //)

  override def reduce[A1 >: A: ClassTag](r: (A1, A1) => A1): A1 = {
    d
      .map(_.asInstanceOf[A1])
      .reduce(r)
  }

  override def toMap[T, U](implicit ev: A <:< (T, U)): Map[T, U] =
    d
      .mapPartitions(items => Iterator(items.toSeq.toMap(ev)))
      .reduce(_ ++ _)

  override def size: Long =
    d.count()

  override def isEmpty: Boolean =
    d.isEmpty()

  override def sum[N >: A](implicit num: Numeric[N]): N =
    reduce[N] { case (a, b) => num.plus(a, b) } { ClassTag(num.zero.getClass) }

  override def zip[A1 >: A: ClassTag, B: ClassTag, T[_]](that: T[B])(implicit view: T[B] => Data[B]): Data[(A1, B)] =
    view(that) match {

      case TravData(thatLs) =>
        val x: RDD[B] = d.sparkContext.parallelize(thatLs.toSeq)
        RddData(d.zip(x).map { case (a, b) => (a.asInstanceOf[A1], b) })
      //        TravData(ls.toIterable.zip(thatLs.toIterable).toTraversable)

      case RddData(thatD) =>
        RddData(d.zip(thatD).map { case (a, b) => (a.asInstanceOf[A1], b) })
      //        val x: Seq[A1] = ls.toSeq
      //        RddData(thatD.sparkContext.parallelize(x).zip(thatD))
    }

}

//////////////

object Data {

  implicit def seq2data[A](s: Seq[A]): Data[A] =
    s.toTraversable

  implicit def indexedSeq2Data[A](s: IndexedSeq[A]): Data[A] =
    s.toTraversable

  implicit def array2Data[A](a: Array[A]): Data[A] =
    a.toTraversable

  implicit def traversable2data[A](t: Traversable[A]): Data[A] =
    TravData(t)

  implicit def rdd2data[A](d: RDD[A]): Data[A] =
    RddData(d)
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
