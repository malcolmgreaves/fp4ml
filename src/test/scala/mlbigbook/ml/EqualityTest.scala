package mlbigbook.ml

import org.scalatest.FunSuite

class EqualityTest extends FunSuite {

  import Equality.ops._
  import EqualityT.Implicits._

  test("Equality type class works for simple type: Boolean") {
    testTypeclassEq(true, true)
    testTypeclassEq(false, false)

    testTypeclassNeq(false, true)
    testTypeclassNeq(true, false)
  }

  test("Equality type class works for simple type: Int") {
    testTypeclassEq(0, 0)
    testTypeclassEq(Int.MaxValue, Int.MaxValue)
    testTypeclassEq(661656128, 661656128)
    testTypeclassEq(-9926126, -9926126)

    testTypeclassNeq(0, 1)
    testTypeclassNeq(Int.MinValue, Int.MaxValue)
    testTypeclassNeq(626126123, -125612561)
  }

  test("Equality type class works for simple type: Long") {
    testTypeclassEq(0l, 0l)
    testTypeclassEq(Long.MaxValue, Long.MaxValue)
    testTypeclassEq(12516126126l, 12516126126l)
    testTypeclassEq(-9926126l, -9926126l)

    testTypeclassNeq(0l, 1l)
    testTypeclassNeq(Long.MinValue, Long.MaxValue)
    testTypeclassNeq(626126123l, -125612561l)
  }

  test("Equality type class works for simple type: String") {
    testTypeclassEq("", "")
    testTypeclassEq("hello world", "hello world")
    testTypeclassEq(
      "agskshjklasblskbfaklhbfhjkl1`2b51klb51ljk5bkl1gb5hjklabsf",
      "agskshjklasblskbfaklhbfhjkl1`2b51klb51ljk5bkl1gb5hjklabsf"
    )

    testTypeclassNeq("", "hello world")
    testTypeclassNeq(
      "agskshjklasblskbfaklhbfhjkl1`2b51klb51ljk5bkl1gb5hjklabsf",
      "hello world"
    )
  }

  def testTypeclassEq[T: Equality](actual: T, expected: T): Unit = {
    assert(actual.equalsE(expected))
    assert(actual.hashCodeE === expected.hashCodeE)
  }

  def testTypeclassNeq[T: Equality](actual: T, notExpected: T): Unit = {
    assert(!actual.equalsE(notExpected))
    assert(actual.hashCodeE !== notExpected.hashCodeE)
  }

}