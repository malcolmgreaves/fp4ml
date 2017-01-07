package mlbigbook.math

import breeze.linalg.DenseVector

class MathVectorOpsDenseDoubleTest
    extends AbstractMvoFractionalT[Double, DenseVector] {

  import MathVectorOps.Implicits._
  override val vops = implicitly[MathVectorOps.Type[Double, DenseVector]]
  override def int2num(i: Int) = i.toDouble
  override def dbl2num(d: Double) = d
  override def vals2vec(vs: Double*) = DenseVector(vs: _*)
  override val tolerance = 1e-6
}
