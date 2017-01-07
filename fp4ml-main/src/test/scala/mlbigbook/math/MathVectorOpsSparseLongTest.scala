package mlbigbook.math

import breeze.linalg.SparseVector

class MathVectorOpsSparseLongTest
    extends AbstractMathVectorOpsT[Long, SparseVector] {

  import MathVectorOps.Implicits._
  override val vops = implicitly[MathVectorOps.Type[Long, SparseVector]]
  override def int2num(i: Int) = i.toLong
  override def dbl2num(d: Double) = d.toLong
  override def vals2vec(vs: Long*) = SparseVector(vs: _*)
  override val tolerance = 1l
}
