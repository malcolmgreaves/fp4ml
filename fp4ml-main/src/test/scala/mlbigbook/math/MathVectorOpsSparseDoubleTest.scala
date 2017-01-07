package mlbigbook.math

import breeze.linalg.SparseVector

class MathVectorOpsSparseDoubleTest
    extends AbstractMvoFractionalT[Double, SparseVector] {

  import MathVectorOps.Implicits._
  override val vops = implicitly[MathVectorOps.Type[Double, SparseVector]]
  override def int2num(i: Int) = i.toDouble
  override def dbl2num(d: Double) = d.toDouble
  override def vals2vec(vs: Double*) = SparseVector(vs: _*)
  override val tolerance = 1e-6
}
