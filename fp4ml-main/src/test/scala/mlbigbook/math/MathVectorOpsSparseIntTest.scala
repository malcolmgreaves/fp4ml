package mlbigbook.math

import breeze.linalg.SparseVector

class MathVectorOpsSparseIntTest
    extends AbstractMathVectorOpsT[Int, SparseVector] {

  import MathVectorOps.Implicits._
  override val vops = implicitly[MathVectorOps.Type[Int, SparseVector]]
  override def int2num(i: Int) = i
  override def dbl2num(d: Double) = d.toInt
  override def vals2vec(vs: Int*) = SparseVector(vs: _*)
  override val tolerance = 1
}
