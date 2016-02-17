package mlbigbook.ml

import org.scalatest.FunSuite

class EqualitySetTest extends FunSuite {

  import EqualityT.Implicits._

  test("Empty EqualitySet has nothing") {
    val s = EqualitySet.empty[String]
    assert(!s.contains(""))
    assert(!s.contains("21561261"))
    assert(!s.contains("1jklabsnagfbahkljbvfa"))
  }

  test("Equality set works for simple type: Boolean") {
    val s = EqualitySet.empty[Boolean]

    val sTrue = s + true
    assert(sTrue contains true)
    assert(!(sTrue contains false))

    val sFalse = s + false
    assert(!(sFalse contains true))
    assert(sFalse contains false)

    val sAll = sTrue + false
    assert(sAll contains true)
    assert(sAll contains false)
  }

  test("Equality set works for simple type: Int") {
    val s = EqualitySet.empty[Int]
    assert(!(s contains 0))

    add(s, Seq(1, 1, 2, 3, 4, 5, 5, 5, 6, 7, 7, 8, 9, 10))

    remove(
      EqualitySet[Int](1, 1, 2, 3, 4, 5, 5, 5, 6, 7, 7, 8, 9, 10),
      Seq(1, 2, 3)
    )
  }

  test("Equality set works for simple type: Long") {
    val s = EqualitySet.empty[Long]
    assert(!(s contains 0l))

    add(s, Seq(1, 1, 2, 3, 4, 5, 5, 5, 6, 7, 7, 8, 9, 10).map(_.toLong))

    remove(
      EqualitySet[Long](1l, 1l, 2l, 3l, 4l, 5l, 5l, 5l, 6l, 7l, 7l, 8l, 9l, 10l),
      Seq(1l, 2l, 3l)
    )
  }

  test("Equality set works for simple type: String") {
    val s = EqualitySet.empty[String]
    assert(!(s contains ""))

    add(s, Seq("hello", "hello", "hello", "hello", "world", "how", "are", "you"))

    remove(
      EqualitySet[String]("hello", "world", "how", "are", "you", "today"),
      Seq("today", "you", "how", "are", "hello", "universe")
    )
  }

  private[this] def contains[T: Equality](s: Set[T], elements: Seq[T]): Unit =
    elements.foreach { e =>
      assert(s.contains(e), s"element not in set: $e")
    }

  private[this] def add[T: Equality](s: Set[T], elements: Seq[T]): Unit =
    contains(
      elements.foldLeft(s) { case (set, e) => set + e },
      elements
    )

  private[this] def notContains[T: Equality](s: Set[T], elements: Seq[T]): Unit =
    elements.foreach { e =>
      assert(!s.contains(e), s"element in set that is not supposed to be there: $e")
    }

  private[this] def remove[T: Equality](s: Set[T], elements: Seq[T]): Unit =
    notContains(
      elements.foldLeft(s) { case (set, e) => set - e },
      elements
    )
}