///*
// * Definition of the distributed data trait Data.
// * Data defines methods for transforming and manipulating data of any size
// * in a purely functional manner.
// *
// * @author Malcolm Greaves
// */
//package mlbigbook.data
//
//import scala.reflect.ClassTag
//
///**
// * Trait that abstractly represents operations that can be performed on a dataset.
// * The implementation of Data is suitable for both large-scale, distributed data
// * or in-memory structures.
// */
//trait DataTypeclass[D[_]] {
//
//  /** Transform a dataset by applying f to each element. */
//  def map[B: ClassTag, A](d: D[A])(f: A => B): D[B]
//
//  def mapParition[B: ClassTag, A](d: D[A])(f: Iterator[A] => Iterator[B]): D[B]
//
//  /** Apply a side-effecting function to each element. */
//  def foreach[A](d: D[A])(f: A => Any): Unit
//
//  def foreachPartition[A](d: D[A])(f: Iterator[A] => Any): Unit
//  /**
//   * Starting from a defined zero value, perform an operation seqOp on each element
//   * of a dataset. Combine results of seqOp using combOp for a final value.
//   */
//  def aggregate[B: ClassTag, A](d: D[A])(zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B
//
//  /** Sort the dataset using a function f that evaluates each element to an orderable type */
//  def sortBy[B: ClassTag, A](d: D[A])(f: (A) ⇒ B)(implicit ord: math.Ordering[B]): D[A]
//
//  /** Construct a traversable for the first k elements of a dataset. Will load into main mem. */
//  def take[A](d: D[A])(k: Int): Traversable[A]
//
//  /** Load all elements of the dataset into an array Traversablein main memory. */
//  def toSeq[A](d: D[A]): Seq[A]
//
//  def flatMap[B: ClassTag, A](d: D[A])(f: A => TraversableOnce[B]): D[B]
//
//  def groupBy[B: ClassTag, A](d: D[A])(f: A => B): D[(B, Iterable[A])]
//
//  def reduce[A1 >: A, A](d: D[A])(r: (A1, A1) => A1): A1
//
//  def toMap[T, U, A](d: D[A])(implicit ev: A <:< (T, U)): Map[T, U]
//
//  def size(d: D[_]): Long
//
//  def isEmpty(d: D[_]): Boolean
//
//  def sum[N: Numeric](d: D[N]): N
//
//  def zip[B, A1 >: A, A](thisD: D[A], thatD: D[B]): D[(A1, B)]
//
//}
//
//object DataTypeclass {
//
//  def foo[A, T[_]: DataTypeclass](elements: T[A]) =
//    implicitly[DataTypeclass[T]].map(elements)(println)
//
//}
//
//object DataTypeclassImplicits {
//
//  implicit object TravDataTypeclass extends DataTypeclass[Traversable] {
//
//    override def map[B: ClassTag, A](ls: Traversable[A])(f: A => B) =
//      ls.map(f)
//
//    override def mapParition[B: ClassTag, A](ls: Traversable[A])(f: Iterator[A] => Iterator[B]) =
//      f(ls.toIterator).toTraversable
//
//    override def foreach[A](ls: Traversable[A])(f: A => Any): Unit =
//      ls.foreach(f)
//
//    override def foreachPartition[A](ls: Traversable[A])(f: Iterator[A] => Any): Unit = {
//      val _ = f(ls.toIterator)
//    } x
//
//    override def aggregate[B: ClassTag, A](ls: Traversable[A])(zero: B)(seqOp: (B, A) => B, combOp: (B, B) => B): B =
//      ls.aggregate(zero)(seqOp, combOp)
//
//    override def sortBy[B: ClassTag, A](ls: Traversable[A])(f: (A) ⇒ B)(implicit ord: math.Ordering[B]) =
//      ls.toSeq.sortBy(f)
//
//    override def take[A](ls: Traversable[A])(k: Int): Traversable[A] =
//      ls.take(k)
//
//    override def toSeq[A](ls: Traversable[A]): Seq[A] =
//      ls.toSeq
//
//    override def flatMap[B: ClassTag, A](ls: Traversable[A])(f: A => TraversableOnce[B]) =
//      ls.flatMap(f)
//
//    override def groupBy[B: ClassTag, A](ls: Traversable[A])(f: A => B) =
//      ls
//        .groupBy(f)
//        .toTraversable
//        .map({ case (b, iter) => (b, iter.toIterable) })
//
//    override def reduce[A1 >: A, A](ls: Traversable[A])(r: (A1, A1) => A1): A1 =
//      ls.reduce(r)
//
//    override def toMap[T, U, A](ls: Traversable[A])(implicit ev: A <:< (T, U)): Map[T, U] =
//      ls.toMap
//
//    override def size(ls: Traversable[_]): Long =
//      ls.size
//
//    override def isEmpty(ls: Traversable[_]): Boolean =
//      ls.isEmpty
//
//    override def sum[N: Numeric](ls: Traversable[N]): N =
//      ls.sum
//
//    override def zip[B, A1 >: A, A](ls: Traversable[A], that: Traversable[B]) =
//      ls.toIterable.zip(that.toIterable).toTraversable
//  }
//
//}
//
////case object TravDataTypeclass[] extends