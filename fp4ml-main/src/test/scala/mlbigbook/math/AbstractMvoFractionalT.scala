package mlbigbook.math

import scala.language.higherKinds

trait AbstractMvoFractionalT[N, V[_]] extends AbstractMathVectorOpsT[N, V] {

  it("divide vector") {
    val v1 = vals2vec(1.n, 2.n, 40.n)
    val v2 = vals2vec((-1).n, 2.n, 100.n)
    val r = vops.divV(v1, v2)

    vops(r)(0) should be((-1).n +- tolerance)
    vops(r)(1) should be(1.n +- tolerance)
    vops(r)(2) should be((2.0 / 5.0).n +- tolerance)
    vops.size(r) should be(3)
  }

  it("divide scalar") {
    val v = vals2vec(1.n, 2.n, 40.n)
    val s = 10.n
    val r = vops.divS(v, s)

    vops(r)(0) should be(0.1.n +- tolerance)
    vops(r)(1) should be(0.2.n +- tolerance)
    vops(r)(2) should be(4.n +- tolerance)
    vops.size(r) should be(3)
  }

}
