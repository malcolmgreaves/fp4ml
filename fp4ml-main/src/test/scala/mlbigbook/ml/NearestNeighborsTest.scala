package mlbigbook.ml

import breeze.linalg.DenseVector
import mlbigbook.math.MathVectorOps
import org.scalatest.FunSuite

import scala.language.reflectiveCalls

class NearestNeighborsTest extends FunSuite {

  import NearestNeighborsTest._
  import fif.ImplicitCollectionsData._

  test("Sanity check: 1-NN on train set evaluates to input item") {
    val rank = nn.mkRanker(distance, stringVectorizer)(data)
    data foreach { item =>
      val retrieved = rank(1)(item)
      assert(retrieved.size === 1)
      assert(retrieved.head === item)
    }
  }

}

object NearestNeighborsTest {
  val nn = NearestNeighbors[String, Float, DenseVector](
    MathVectorOps.Implicits.FloatDenseVot
  )
  val data = KnnClassifierTest.data.map { case (ws, _) => ws }
  val stringVectorizer: nn.Vectorizer = KnnClassifierTest.stringVectorizer
  val distance: nn.Distance = KnnClassifierTest.distance
}
