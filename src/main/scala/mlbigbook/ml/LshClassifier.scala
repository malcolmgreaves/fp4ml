package mlbigbook.ml

import java.util.Random

import mlbigbook.data._
import scala.reflect.ClassTag

object LshClassifier {

  def apply[T: ClassTag](nLshFuncs: Int, nBins: Int)(n: NearNeighIn)(v: VectorDataIn[LabeledData[T]])(
    implicit ddContext: DataContext, rand: Random): Classifier[T] =

    KnnClassifier(LshRanker(nLshFuncs, nBins)(n)(v)(ddContext, rand))

}