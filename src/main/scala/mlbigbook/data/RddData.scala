/*
 * Definition of the distributed data trait Data.
 * Data defines methods for transforming and manipulating data of any size
 * in a purely functional manner.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

import org.apache.spark.rdd.RDD

import scala.reflect.ClassTag

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

  override def zip[A1 >: A: ClassTag, B: ClassTag](that: Data[B]): Data[(A1, B)] =
    that match {

      case TravData(thatLs) =>
        val x: RDD[B] = d.sparkContext.parallelize(thatLs.toSeq)
        RddData(d.zip(x).map { case (a, b) => (a.asInstanceOf[A1], b) })

      case RddData(thatD) =>
        RddData(d.zip(thatD).map { case (a, b) => (a.asInstanceOf[A1], b) })

      case other =>
        other
          .zip(d.map[A1](_.asInstanceOf[A1]))(implicitly[ClassTag[B]], implicitly[ClassTag[A1]])
          .map { case (b, a1) => (a1, b) }
    }

}