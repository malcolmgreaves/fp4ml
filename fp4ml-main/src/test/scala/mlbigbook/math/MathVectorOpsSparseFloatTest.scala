package mlbigbook.math

import breeze.linalg.SparseVector

class MathVectorOpsSparseFloatTest
    extends AbstractMvoFractionalT[Float, SparseVector] {

  import MathVectorOps.Implicits._
  override val vops = implicitly[MathVectorOps.Type[Float, SparseVector]]
  override def int2num(i: Int) = i.toFloat
  override def dbl2num(d: Double) = d.toFloat
  override def vals2vec(vs: Float*) = SparseVector(vs: _*)
  override val tolerance = 1e-6f
}
