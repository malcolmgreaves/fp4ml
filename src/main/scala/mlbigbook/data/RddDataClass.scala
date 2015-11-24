/*
 * Definition of the distributed data trait Data.
 * Data defines methods for transforming and manipulating data of any size
 * in a purely functional manner.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

import mlbigbook.util.Sampling
import org.apache.spark.rdd.RDD

import scala.reflect.ClassTag
import scala.util.Random

/** Wraps a Spark RDD as a Data. */
case class RddDataClass[A: ClassTag](d: RDD[A]) extends DataClass[A] {

  override def map[B: ClassTag](f: A => B): DataClass[B] =
    RddDataClass(d.map(f))

  override def mapParition[B: ClassTag](f: Iterator[A] => Iterator[B]): DataClass[B] =
    RddDataClass(d.mapPartitions(f))

  override def foreach(f: A => Any): Unit =
    d.foreach { a =>
      val _ = f(a)
    }

  override def foreachPartition(f: Iterator[A] => Any): Unit =
    d.foreachPartition { a =>
      val _ = f(a)
    }

  override def filter(f: A => Boolean): DataClass[A] =
    RddDataClass(d.filter(f))

  override def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B =
    d.aggregate(zero)(seqOp, combOp)

  override def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): DataClass[A] =
    RddDataClass(d.sortBy(f))

  override def take(k: Int): Traversable[A] =
    d.take(k)

  override def headOption: Option[A] =
    if (!d.isEmpty())
      Some(d.first())
    else
      None

  override def toSeq: Seq[A] =
    d.collect().toIndexedSeq // toIndexedSeq makes a copy of the array, ensuring that it's immutable to the receiver

  override def flatMap[B: ClassTag](f: A => TraversableOnce[B]): DataClass[B] =
    RddDataClass(d.flatMap(f))

  override def groupBy[B: ClassTag](f: A => B): DataClass[(B, Iterable[A])] =
    d.groupBy(f)

  override def reduce[A1 >: A: ClassTag](r: (A1, A1) => A1): A1 =
    d
      .map(_.asInstanceOf[A1])
      .reduce(r)

  override def reduceLeft(r: (A, A) => A): A =
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
    reduce[N] { case (a, b) => num.plus(a, b) } { ClassTag(num.zero.getClass) }

  override def zip[A1 >: A: ClassTag, B: ClassTag](that: DataClass[B]): DataClass[(A1, B)] =
    that match {

      case TravDataClass(thatLs) =>
        val x: RDD[B] = d.sparkContext.parallelize(thatLs.toSeq)
        RddDataClass(d.zip(x).map { case (a, b) => (a.asInstanceOf[A1], b) })

      case RddDataClass(thatD) =>
        RddDataClass(d.zip(thatD).map { case (a, b) => (a.asInstanceOf[A1], b) })

      case other =>
        other
          .zip(d.map[A1](_.asInstanceOf[A1]))(implicitly[ClassTag[B]], implicitly[ClassTag[A1]])
          .map { case (b, a1) => (a1, b) }
    }

  override def zipWithIndex: DataClass[(A, Long)] =
    d.zipWithIndex()

  override def sample(withReplacement: Boolean, fraction: Double, seed: Long = Random.nextLong()): DataClass[A] =
    d.sample(withReplacement, fraction, seed)

  override def exactSample(fraction: Double, seed: Long): DataClass[A] = {
    val modulo = math.round(1.0 / fraction).toInt
    val randNum = new Random(seed).nextInt(modulo)
    RddDataClass(
      d.zipWithIndex()
        .filter {
          case (datum, idx) => (idx + randNum) % modulo == 0
        }
        .map(_._1)
    )
  }

}