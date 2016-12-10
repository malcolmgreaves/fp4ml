package mlbigbook.math

import breeze.linalg.DenseVector

class MathVectorOpsDenseFloatTest
  extends AbstractMvoFractionalT[Float, DenseVector] {
  
  import MathVectorOps.Implicits._
  override val vops = implicitly[MathVectorOps.Type[Float, DenseVector]]
  override def int2num(i: Int) = i.toFloat
  override def dbl2num(d: Double) = d.toFloat
  override def vals2vec(vs: Float*) = DenseVector(vs:_*)
  override val tolerance = 1e-6f
}