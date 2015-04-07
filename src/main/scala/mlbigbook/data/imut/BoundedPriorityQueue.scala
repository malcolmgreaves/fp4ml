package mlbigbook.data.imut

trait BoundedPriorityQueue[A] {

  type T

  val maxSize: Int

  def peekMin(existing: T): Option[A]

  def takeMin(existing: T): Option[(A, T)]

  def insert(item: A)(existing: T): T

}

object BoundedPriorityQueue {

  def create[A](boundedMaximumSize: Int, O: Ordering[A]): BoundedPriorityQueue[A] =
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

      override val maxSize: Int = boundedMaximumSize

      override def insert(item: A)(existing: T): T =
        existing match {

          case Empty =>
            if (existing.size < maxSize)
              Full(Empty, item, Empty)
            else
              existing

          case f @ Full(left, heapItem, right) =>
            val cmp = O.compare(item, heapItem)
            if (cmp < 0)
              // item is "more minimum" than heap item:
              // push heap-item down
              if (right.size > left.size)
                Full(insert(heapItem)(left), item, right)
              else
                Full(left, item, insert(heapItem)(right))

            else
            // item is either "less minimum" or "the same priority" to the heap item:
            // continue down heap to find appropriate spot
            if (right.size > left.size)
              Full(insert(item)(left), heapItem, right)
            else
              Full(left, heapItem, insert(item)(right))
        }

      override def peekMin(existing: T): Option[A] =
        existing match {

          case Empty =>
            None

          case Full(_, item, _) =>
            Some(item)
        }

      override def takeMin(existing: T): Option[(A, T)] =
        existing match {

          case Empty =>
            None

          case Full(left, item, right) =>

            val tookLeft = peekMin(left)
            val tookRight = peekMin(right)

            Some(
              tookLeft match {

                case None =>
                  // no left subtree

                  tookRight match {

                    case None =>
                      // no left nor right subtrees
                      (item, Empty)

                    case Some(_) =>
                      // no left subtree, but a right one
                      (item, right)
                  }

                case Some(leftMin) =>
                  // a left subtree

                  tookRight match {

                    case None =>
                      // a left subtree, but no right one
                      (item, left)

                    case Some(rightMin) =>
                      // left and right subtrees:
                      // compare to see which one we should pull-up

                      val cmp = O.compare(leftMin, rightMin)
                      if (cmp < 0)
                        // left is "more minmum than right":
                        // promote left subtree
                        (item,
                          Full(
                            takeMin(left).map(_._2).getOrElse(Empty),
                            leftMin,
                            right
                          )
                        )
                      else
                        // right is "more minimum" or "equal priority" to left:
                        // either case, promote right subtree
                        (
                          item,
              // item is "more minimum" than heap item
                          Full(
                            left,
                            rightMin,
                            takeMin(right).map(_._2).getOrElse(Empty)
                          )
                        )
                  }

              }
            )
        }
    }

}
