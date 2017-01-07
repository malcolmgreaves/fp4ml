package mlbigbook.math

import breeze.linalg.DenseVector

class MathVectorOpsDenseIntTest
    extends AbstractMathVectorOpsT[Int, DenseVector] {

  import MathVectorOps.Implicits._
  override val vops = implicitly[MathVectorOps.Type[Int, DenseVector]]
  override def int2num(i: Int) = i
  override def dbl2num(d: Double) = d.toInt
  override def vals2vec(vs: Int*) = DenseVector(vs: _*)
  override val tolerance = 1
}
