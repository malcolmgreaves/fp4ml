/*
 * Definition of the distributed data trait Data.
 * Data defines methods for transforming and manipulating data of any size
 * in a purely functional manner.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

import mlbigbook.util.Sampling

import scala.reflect.ClassTag

/** Wraps a Traversable as a Data. */
case class TravDataClass[A](ls: Traversable[A]) extends DataClass[A] {

  override def map[B: ClassTag](f: A => B): DataClass[B] =
    TravDataClass(ls.map(f))

  override def mapParition[B: ClassTag](f: Iterator[A] => Iterator[B]): DataClass[B] =
    TravDataClass(f(ls.toIterator).toTraversable)

  override def foreach(f: A => Any): Unit =
    ls.foreach(f)

  override def foreachPartition(f: Iterator[A] => Any): Unit = {
    val _ = f(ls.toIterator)
  }

  override def filter(f: A => Boolean): DataClass[A] =
    TravDataClass(ls.filter(f))

  override def aggregate[B: ClassTag](zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B =
    ls.aggregate(zero)(seqOp, combOp)

  override def sortBy[B: ClassTag](f: (A) ⇒ B)(implicit ord: math.Ordering[B]): DataClass[A] =
    TravDataClass(ls.toSeq.sortBy(f))

  override def take(k: Int): Traversable[A] =
    ls.take(k)

  override def headOption: Option[A] =
    ls.headOption

  override def toSeq: Seq[A] =
    ls.toSeq

  override def flatMap[B: ClassTag](f: A => TraversableOnce[B]): DataClass[B] =
    TravDataClass(ls.flatMap(f))

  override def groupBy[B: ClassTag](f: A => B): DataClass[(B, Iterable[A])] =
    TravDataClass(
      ls
        .groupBy(f)
        .toTraversable
        .map {
          case (b, iter) => (b, iter.toIterable)
        }
    )

  override def reduce[A1 >: A: ClassTag](r: (A1, A1) => A1): A1 =
    ls.reduce(r)

  override def reduceLeft(r: (A, A) => A): A =
    ls.reduce(r)

  override def toMap[T, U](implicit ev: A <:< (T, U)): Map[T, U] =
    ls.toMap

  override def size: Long =
    ls.size

  override def isEmpty: Boolean =
    ls.isEmpty

  override def sum[N >: A](implicit num: Numeric[N]): N =
    ls.sum(num)

  override def zip[A1 >: A: ClassTag, B: ClassTag](that: DataClass[B]): DataClass[(A1, B)] =
    that match {

      case TravDataClass(thatLs) =>
        TravDataClass(ls.toIterable.zip(thatLs.toIterable).toTraversable)

      case RddDataClass(thatD) =>
        val x: Seq[A1] = ls.toSeq
        RddDataClass(thatD.sparkContext.parallelize(x).zip(thatD))

      case other =>
        other
          .zip(ls.map(_.asInstanceOf[A1]))(implicitly[ClassTag[B]], implicitly[ClassTag[A1]])
          .map { case (b, a1) => (a1, b) }
    }

  override def zipWithIndex: DataClass[(A, Long)] =
    ls.toIndexedSeq.zipWithIndex.map(a => (a._1, a._2.toLong))

  override def sample(withReplacement: Boolean, fraction: Double, seed: Long): DataClass[A] =
    Sampling.sample(ls, math.round(fraction * ls.size).toInt, withReplacement, seed)

  override def exactSample(fraction: Double, seed: Long): DataClass[A] =
    sample(withReplacement = false, fraction = fraction, seed = seed)

}