package mlbigbook.ml

import mlbigbook.data._
import scala.reflect.ClassTag
import scala.util.Random

object LshClassifier {

  import LshRanker.createHashTablesForCorpus

  import Classifier._
  import Labeled.str2labeled
  import KnnClassifier._

  def apply[T: ClassTag](
    nLSHFuncs: Int,
    bandSize: Int)(
      dist: Distance,
      kNeighborhoodSize: Int,
      mkVec: VectorizerMaker[T],
      labeledCorpus: DistData[LabeledData[T]])(
        implicit ddContext: DistDataContext): Classifier[T] =

    KnnClassifier.apply(
      LshRanker(nLSHFuncs, bandSize)(dist, kNeighborhoodSize, unlabeledVectorizerMaker(mkVec), labeledCorpus)
    )

}
