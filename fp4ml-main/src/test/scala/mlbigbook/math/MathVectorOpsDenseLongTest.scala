package mlbigbook.math

import breeze.linalg.DenseVector

class MathVectorOpsDenseLongTest
  extends AbstractMathVectorOpsT[Long, DenseVector] {
  
  import MathVectorOps.Implicits._
  override val vops = implicitly[MathVectorOps.Type[Long, DenseVector]]
  override def int2num(i: Int) = i.toLong
  override def dbl2num(d: Double) = d.toLong
  override def vals2vec(vs: Long*) = DenseVector(vs:_*)
  override val tolerance = 1l
}