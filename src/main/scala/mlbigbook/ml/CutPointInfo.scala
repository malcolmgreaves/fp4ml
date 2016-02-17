package mlbigbook.ml

import fif.Data
import fif.Data.ops._
import mlbigbook.math.VectorOpsT

import scala.language.higherKinds
import scala.reflect.ClassTag

case class CutPointInfo[D[_]: Data, N: Numeric](
  totalEntropy: Double,
  cutPoint:     N,
  infoGainOfCp: Double,
  s1:           D[(N, Boolean)],
  s1Entropy:    Double,
  s2:           D[(N, Boolean)],
  s2Entropy:    Double
)