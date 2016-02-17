package mlbigbook.ml

import org.scalatest.FunSuite

class EqualityMapTest extends FunSuite {

  import EqualityT.Implicits._

  test("Empty EqualityMap has nothing") {
    val s = EqualityMap.empty[String, Any]
    assert(!s.contains(""))
    assert(!s.contains("21561261"))
    assert(!s.contains("1jklabsnagfbahkljbvfa"))
  }

  test("Equality map works for simple type: Boolean") {
    val s = EqualityMap.empty[Boolean, String]

    val sTrue = s + (true -> "world")
    assert(sTrue contains true)
    assert(!(sTrue contains false))
    assert(sTrue(true) === "world")

    val sFalse = s + (false -> "hello")
    assert(!(sFalse contains true))
    assert(sFalse contains false)
    assert(sFalse(false) === "hello")

    val sAll = sTrue + (false -> "hello")
    assert(sAll contains true)
    assert(sAll contains false)
    assert(sAll(true) === "world")
    assert(sAll(false) === "hello")
  }

  test("Equality map works for simple type: Int") {
    val s = EqualityMap.empty[Int, String]
    assert(!(s contains 0))

    add(
      s,
      Seq(1, 1, 2, 3, 4, 5, 5, 5, 6, 7, 7, 8, 9, 10).map { v => (v, v.toString) }
    )

    remove(
      EqualityMap[Int, String](Seq(1, 1, 2, 3, 4, 5, 5, 5, 6, 7, 7, 8, 9, 10).map { v => (v, v.toString) }: _*),
      Seq(1, 2, 3).map { v => (v, v.toString) }
    )
  }

  test("Equality map works for simple type: Long") {
    val s = EqualityMap.empty[Long, String]
    assert(!(s contains 0l))

    add(
      s,
      Seq(1, 1, 2, 3, 4, 5, 5, 5, 6, 7, 7, 8, 9, 10).map { v => (v.toLong, v.toString) }
    )

    remove(
      EqualityMap[Long, String](Seq(1, 1, 2, 3, 4, 5, 5, 5, 6, 7, 7, 8, 9, 10).map(v => (v.toLong, v.toString)): _*),
      Seq(1l, 2l, 3l).map { v => (v, v.toString) }
    )
  }

  test("Equality map works for simple type: String") {
    val s = EqualityMap.empty[String, String]
    assert(!(s contains ""))

    add(
      s,
      Seq("hello", "hello", "hello", "hello", "world", "how", "are", "you")
        .map { v => (v, s"OMG_${v}_ASSHGH") }
    )

    remove(
      EqualityMap[String, String](
        Seq("hello", "hello", "hello", "hello", "world", "how", "are", "you")
          .map { v => (v, s"OMG_${v}_ASSHGH") }: _*
      ),
      Seq("today", "you", "how", "are", "hello", "universe")
        .map { v => (v, s"OMG_${v}_ASSHGH") }
    )
  }

  private[this] def contains[T: Equality](s: Map[T, String], elements: Seq[(T, String)]): Unit =
    elements.foreach {
      case (k, v) =>
        assert(s.contains(k))
        assert(s(k) === v)
        assert(s.get(k) === Some(v))
    }

  private[this] def add[T: Equality](s: Map[T, String], elements: Seq[(T, String)]): Unit =
    contains(
      elements.foldLeft(s) { case (m, (k, v)) => m + (k -> v) },
      elements
    )

  private[this] def notContains[T: Equality](s: Map[T, String], elements: Seq[(T, String)]): Unit =
    elements.foreach {
      case (k, v) =>
        assert(!s.contains(k))
        assert(s.get(k) === None)
    }

  private[this] def remove[T: Equality](s: Map[T, String], elements: Seq[(T, String)]): Unit =
    notContains(
      elements.foldLeft(s) { case (m, (k, _)) => m - k },
      elements
    )
}