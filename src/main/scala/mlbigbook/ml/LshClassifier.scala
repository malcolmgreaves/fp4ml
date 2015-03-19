package mlbigbook.ml

import java.util.Random

import mlbigbook.data._
import scala.reflect.ClassTag

object LshClassifier {

  import KnnClassifier._

  def apply[T: ClassTag](
    nLSHFuncs: Int,
    bandSize: Int)(
      dist: Distance,
      kNeighborhoodSize: Int,
      mkVec: VectorizerMaker[T],
      labeledCorpus: DistData[LabeledData[T]])(
        implicit ddContext: DistDataContext, rand: Random): Classifier[T] =

    KnnClassifier(
      LshRanker(nLSHFuncs, bandSize)(
        dist, kNeighborhoodSize, unlabeledVectorizerMaker(mkVec), labeledCorpus)(
          ddContext, rand)
    )

}
