package mlbigbook.data.imut

trait BoundedPriorityQueue[A] {

  type T

  val maxSize: Int

  val empty: T

  def peekMin(existing: T): Option[A]

  def takeMin(existing: T): Option[(A, T)]

  def merge(one: T, two: T): T

  def insert(item: A)(existing: T): T
}

object Modules {

  implicit class Piper[A](val x: A) extends AnyVal {
    def |>[B](f: A => B) = f(x)
  }

}

object BoundedPriorityQueue {

  val doubleOrdering = math.Ordering.Double

  def create[A](O: A => Double)(boundedMaximumSize: Int): BoundedPriorityQueue[A] =
    new BoundedPriorityQueue[A] {

      sealed trait T {
        val size: Int
      }

      case object Empty extends T {
        override val size = 0
      }

      case class Full(left: T, item: A, right: T) extends T {
        override val size = left.size + 1 + right.size
      }

      override val empty: T = Empty

      override val maxSize = boundedMaximumSize

      override def insert(item: A)(existing: T): T =
        insert_h(item, existing, existing.size)

      private def insert_h(item: A, existing: T, size: Int): T =
        existing match {

          case Empty =>
            if (size < maxSize)
              Full(Empty, item, Empty)
            else
              existing

          case f @ Full(left, heapItem, right) =>
            val cmp = doubleOrdering.compare(O(item), O(heapItem))
            if (cmp < 0)
              // item is "more minimum" than heap item:
              // push heap-item down
              if (right.size > left.size)
                Full(insert_h(heapItem, left, size), item, right)
              else
                Full(left, item, insert_h(heapItem, right, size))

            else // item is either "less minimum" or "the same priority" to the heap item:
            // continue down heap to find appropriate spot
            if (right.size > left.size)
              Full(insert_h(item, left, size), heapItem, right)
            else
              Full(left, heapItem, insert_h(item, right, size))
        }

      override def peekMin(existing: T): Option[A] =
        existing match {

          case Empty =>
            None

          case Full(_, item, _) =>
            Some(item)
        }

      @inline override def merge(left: T, right: T): T = {

        val tookLeft = peekMin(left)
        val tookRight = peekMin(right)

        tookLeft match {

          case None =>
            // no left subtree

            tookRight match {

              case None =>
                // no left nor right subtrees
                Empty

              case Some(_) =>
                // no left subtree, but a right one
                right
            }

          case Some(leftMin) =>
            // a left subtree

            tookRight match {

              case None =>
                // a left subtree, but no right one
                left

              case Some(rightMin) =>
                // left and right subtrees:
                // compare to see which one we should pull-up

                val cmp = doubleOrdering.compare(O(leftMin), O(rightMin))
                if (cmp < 0)
                  // left is "more minmum than right":
                  // promote left subtree
                  Full(
                    takeMin(left).map(_._2).getOrElse(Empty),
                    leftMin,
                    right
                  )
                else
                  // right is "more minimum" or "equal priority" to left:
                  // either case, promote right subtree
                  // item is "more minimum" than heap item
                  Full(
                    left,
                    rightMin,
                    takeMin(right).map(_._2).getOrElse(Empty)
                  )
            }

        }
      }

      @inline override def takeMin(existing: T): Option[(A, T)] =
        existing match {

          case Empty =>
            None

          case Full(left, item, right) =>
            Some((item, merge(left, right)))
        }
    }

}
