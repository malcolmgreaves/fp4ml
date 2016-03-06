package mlbigbook.math

import breeze.linalg.DenseVector
import org.scalatest.{ FunSpec, Matchers }

class MathVectorOpsTest extends FunSpec with Matchers {
  import MathVectorOpsTest._

  describe("DenseVector operations") {

    it("zeros") {
      dd.zeros(5)
        .foreach(x => x should be(0.0 +- tolerance))
    }

    it("ones") {
      dd.ones(5)
        .foreach(x => x should be(1.0 +- tolerance))
    }

    it("add vector") {
      val v1 = DenseVector(1.0, 2.0, 40.0)
      val v2 = DenseVector(-1.0, 2.0, 100.0)
      val r = dd.addV(v1, v2)

      r(0) should be(0.0 +- tolerance)
      r(1) should be(4.0 +- tolerance)
      r(2) should be(140.0 +- tolerance)
      r.size should be(3)
    }

    it("add scalar") {
      val v = DenseVector(1.0, 2.0, 40.0)
      val s = 10.0
      val r = dd.addS(v, s)

      r(0) should be(11.0 +- tolerance)
      r(1) should be(12.0 +- tolerance)
      r(2) should be(50.0 +- tolerance)
      r.size should be(3)
    }

    it("subtract vector") {
      val v1 = DenseVector(1.0, 2.0, 40.0)
      val v2 = DenseVector(-1.0, 2.0, 100.0)
      val r = dd.subV(v1, v2)

      r(0) should be(2.0 +- tolerance)
      r(1) should be(0.0 +- tolerance)
      r(2) should be(-60.0 +- tolerance)
      r.size should be(3)
    }

    it("subtract scalar") {
      val v = DenseVector(1.0, 2.0, 40.0)
      val s = 10.0
      val r = dd.subS(v, s)

      r(0) should be(-9.0 +- tolerance)
      r(1) should be(-8.0 +- tolerance)
      r(2) should be(30.0 +- tolerance)
      r.size should be(3)
    }

    it("dot product") {
      val v1 = DenseVector(1.0, 2.0, 40.0)
      val v2 = DenseVector(-1.0, 2.0, 100.0)
      val r = dd.dot(v1, v2)

      r should be(4003.0 +- tolerance)
    }

    it("multiply vector") {
      val v1 = DenseVector(1.0, 2.0, 40.0)
      val v2 = DenseVector(-1.0, 2.0, 100.0)
      val r = dd.mulV(v1, v2)

      r(0) should be(-1.0 +- tolerance)
      r(1) should be(4.0 +- tolerance)
      r(2) should be(4000.0 +- tolerance)
      r.size should be(3)
    }

    it("multiply scalar") {
      val v = DenseVector(1.0, 2.0, 40.0)
      val s = 10.0
      val r = dd.mulS(v, s)

      r(0) should be(10.0 +- tolerance)
      r(1) should be(20.0 +- tolerance)
      r(2) should be(400.0 +- tolerance)
      r.size should be(3)
    }

    it("divide vector") {
      val v1 = DenseVector(1.0, 2.0, 40.0)
      val v2 = DenseVector(-1.0, 2.0, 100.0)
      val r = dd.divV(v1, v2)

      r(0) should be(-1.0 +- tolerance)
      r(1) should be(1.0 +- tolerance)
      r(2) should be((2.0 / 5.0) +- tolerance)
      r.size should be(3)
    }

    it("divide scalar") {
      val v = DenseVector(1.0, 2.0, 40.0)
      val s = 10.0
      val r = dd.divS(v, s)

      r(0) should be(0.1 +- tolerance)
      r(1) should be(0.2 +- tolerance)
      r(2) should be(4.0 +- tolerance)
      r.size should be(3)
    }
  }

}

object MathVectorOpsTest {

  import MathVectorOps.Implicits._
  val dd = implicitly[MathVectorOps[Double, DenseVector]]

  val tolerance = 1e-6

}