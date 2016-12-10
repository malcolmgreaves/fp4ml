package mlbigbook.math

import org.scalatest.{FunSpec, Matchers}

import scala.language.higherKinds

abstract class AbstractMathVectorOpsT[N, V[_]] extends FunSpec with Matchers {

  val vops: MathVectorOps.Type[N, V]
  implicit lazy val nIsNumeric:Numeric[N] = vops.n

  def vals2vec(vs: N*): V[N]

  def dbl2num(d: Double): N
  def int2num(i: Int): N

  implicit class DblAsN(d: Double) {
    val n: N = dbl2num(d)
  }

  implicit class IntAsN(i: Int) {
    val n: N = int2num(i)
  }

  val tolerance: N


  describe("vector operations") {

    it("zeros") {
      vops.foreach { vops.zeros(5) } { x =>
          x should be(0.n +- tolerance)
        }
    }

    it("ones") {
      vops.foreach(vops.ones(5)) { x =>
        x should be(1.n +- tolerance)
      }
    }

    it("add vector") {
      val v1 = vals2vec(1.n, 2.n, 40.n)
      val v2 = vals2vec((-1).n, 2.n, 100.n)
      val r = vops.addV(v1, v2)

      vops(r)(0) should be(0.n +- tolerance)
      vops(r)(1) should be(4.n +- tolerance)
      vops(r)(2) should be(140.n +- tolerance)
      vops.size(r) should be(3)
    }

    it("add scalar") {
      val v = vals2vec(1.n, 2.n, 40.n)
      val s = 10.n
      val r = vops.addS(v, s)

      vops(r)(0) should be(11.n +- tolerance)
      vops(r)(1) should be(12.n +- tolerance)
      vops(r)(2) should be(50.n +- tolerance)
      vops.size(r) should be(3)
    }

    it("subtract vector") {
      val v1 = vals2vec(1.n, 2.n, 40.n)
      val v2 = vals2vec((-1).n, 2.n, 100.n)
      val r = vops.subV(v1, v2)

      vops(r)(0) should be(2.n +- tolerance)
      vops(r)(1) should be(0.n +- tolerance)
      vops(r)(2) should be((-60).n +- tolerance)
      vops.size(r) should be(3)
    }

    it("subtract scalar") {
      val v = vals2vec(1.n, 2.n, 40.n)
      val s = 10.n
      val r = vops.subS(v, s)

      vops(r)(0) should be((-9).n +- tolerance)
      vops(r)(1) should be((-8).n +- tolerance)
      vops(r)(2) should be(30.n +- tolerance)
      vops.size(r) should be(3)
    }

    it("dot product") {
      val v1 = vals2vec(1.n, 2.n, 40.n)
      val v2 = vals2vec((-1).n, 2.n, 100.n)
      val r = vops.dot(v1, v2)

      r should be(4003.n +- tolerance)
    }

    it("multiply vector") {
      val v1 = vals2vec(1.n, 2.n, 40.n)
      val v2 = vals2vec((-1).n, 2.n, 100.n)
      val r = vops.mulV(v1, v2)

      vops(r)(0) should be((-1).n +- tolerance)
      vops(r)(1) should be(4.n +- tolerance)
      vops(r)(2) should be(4000.n +- tolerance)
      vops.size(r) should be(3)
    }

    it("multiply scalar") {
      val v = vals2vec(1.n, 2.n, 40.n)
      val s = 10.n
      val r = vops.mulS(v, s)

      vops(r)(0) should be(10.n +- tolerance)
      vops(r)(1) should be(20.n +- tolerance)
      vops(r)(2) should be(400.n +- tolerance)
      vops.size(r) should be(3)
    }
  }
}