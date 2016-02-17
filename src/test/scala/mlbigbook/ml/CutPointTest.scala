package mlbigbook.ml

import org.scalatest.FunSuite

class CutPointTest extends FunSuite {

  import fif.ImplicitCollectionsData._

  test("cut point is 0 when input data is empty") {
    val data = Seq.empty[(Double, Boolean)]
    assert(CutPoint(data) === 0.0)
  }

  test("evaluates to only element for cutpoint when given one element data") {
    val data = Seq((5.0, false))
    assert(CutPoint(data) === 5.0)
  }

  test("finds correct cut point") {
    val data = Seq(
      (5.0, false),
      (-12.0, true),
      (1515.0, false),
      (155.0, true),
      (17742.0, true),
      (12515.0, true),
      (-767823.0, false),
      (888.0, false),
      (66.0, false),
      (3838.0, false),
      (823.0, true),
      (9694.0, false),
      (8956494.0, true),
      (9965.0, true),
      (5050.0, true),
      (357.0, false),
      (11231.0, false),
      (78128.0, false),
      (383.0, true),
      (553125.0, false),
      (6687.0, false),
      (332245562.0, true),
      (1251.0, false),
      (1.0, false),
      (0.0, true)
    )
      .map { v => (v, math.random) }
      .sortBy { case (_, v) => v }
      .map { case (x, _) => x }

    assert(CutPoint(data) === 8956494.0)
  }

}