package mlbigbook.ml

import fif.Data
import fif.Data.ops._
import mlbigbook.math.VectorOpsT

import scala.language.higherKinds
import scala.reflect.ClassTag

object Mdl {

  type Thresholds[N] = Seq[N]

  def apply[D[_]: Data, V[_], N: Numeric: ClassTag](data: D[(V[N], Boolean)])(
    implicit
    fs:   FeatureSpace,
    vops: VectorOpsT[N, V]
  ): Seq[Thresholds[N]] =
    if (data isEmpty)
      Seq.empty[Thresholds[N]]

    else
      (0 until fs.size).map { fIndex =>
        apply(
          data.map {
            case (vec, label) => (vops.valueAt(vec)(fIndex), label)
          }
        )
      }

  def apply[D[_]: Data, N: Numeric: ClassTag](
    data: D[(N, Boolean)]
  ): Thresholds[N] =
    CutPoint.rawCutPointAlgo(data)
      .map { cpInfo =>
        if (stoppingCriterion(data, cpInfo))
          Seq.empty[N]

        else {
          val thresholdsS1 = apply(cpInfo.s1)
          val thresholdsS2 = apply(cpInfo.s2)
          cpInfo.cutPoint +: (thresholdsS1 ++ thresholdsS2)
        }
      }
      .map { thresholds =>
        thresholds.sortBy(identity)
      }
      .getOrElse(Seq.empty[N])

  def stoppingCriterion[D[_]: Data, N: Numeric: ClassTag](
    data:   D[(N, Boolean)],
    cpInfo: CutPointInfo[D, N]
  ): Boolean = {

    val nExamples = data.size.toDouble

    val term1 =
      Information.log2(nExamples - 1.0) / nExamples

    val deltaATS = {

      val nc = calcNumClass(data)

      val a = Information.log2(math.pow(3.0, nc.toDouble) - 2.0)

      import EqualityT.Implicits._
      val b =
        nc * cpInfo.totalEntropy
      -(calcNumClass(cpInfo.s1) * cpInfo.s1Entropy)
      -(calcNumClass(cpInfo.s2) * cpInfo.s2Entropy)

      (a - b) / nExamples
    }

    cpInfo.infoGainOfCp > term1 + deltaATS
  }

  def calcNumClass[D[_]: Data, N: Numeric: ClassTag](data: D[(N, Boolean)]): Int = {
    val atLeastOnePos =
      if (!data.filter { case (_, label) => label }.isEmpty) 1
      else 0

    val atLeastOneNeg =
      if (!data.filter { case (_, label) => !label }.isEmpty) 1
      else 0

    atLeastOnePos + atLeastOneNeg
  }

}