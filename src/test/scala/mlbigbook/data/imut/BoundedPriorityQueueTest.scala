package mlbigbook.data.imut

import org.scalatest.FunSuite

import scala.annotation.tailrec

class BoundedPriorityQueueTest extends FunSuite {

  import Modules._

  val intBindPQ = BoundedPriorityQueue.create((x: Int) => x.toDouble) _

  val Bound1PQ = intBindPQ(1)

  test("Simple queue of size 1") {
    val result = Bound1PQ.empty |> Bound1PQ.insert(3) |> Bound1PQ.insert(4) |> Bound1PQ.insert(1)

    val (min, rest) = Bound1PQ.takeMin(result).get

    assert(min == 1, "expected min to be 1")
    assert(Bound1PQ.takeMin(rest).isEmpty, "expected bounded 1 PQ to only have 1 element")
  }

  test("More values and ordering of P.Q. of size 1") {
    val values = Seq(9, 5, 10, 11, 24, 4, 3, 8, 4, 1, 2)

    val BoundValuePQ = intBindPQ(values.size)

    val result =
      values
        .foldLeft(BoundValuePQ.empty)({
          case (pq, v) => pq |> BoundValuePQ.insert(v)
        })

    @tailrec @inline def check(minSortedValues: List[Int], e: BoundValuePQ.T): Unit =

      BoundValuePQ.takeMin(e) match {

        case None =>
          assert(minSortedValues.isEmpty, "heap empty, expecting value list to be as well")

        case Some((min, restOfHeap)) =>

          minSortedValues match {

            case Nil =>
              fail("unexpected: ran out of values but still more in heap")

            case head :: tail =>
              assert(
                head == min,
                s"expecting sorted values to match heap takeMin... have $min expecting $head"
              )
              check(tail, restOfHeap)

          }
      }

    check(values.sorted.toList, result)
  }

}

//object BoundedPriorityQueueTest {
//
//  @tailrec @inline def check[T:BoundedPriorityQueue[Int]](minSortedValues:List[Int], e:T):Unit =
//
//    e.takeMin(e) match {
//
//      case None =>
//        assert(minSortedValues.isEmpty, "heap empty, expecting value list to be as well")
//
//      case Some((min, restOfHeap)) =>
//
//        minSortedValues match {
//
//          case Nil =>
//            fail("unexpected: ran out of values but still more in heap")
//
//          case head :: tail =>
//            assert(
//              head == min,
//              s"expecting sorted values to match heap takeMin... have $min expecting $head"
//            )
//            check(tail, restOfHeap)
//
//        }
//    }
//
//}
