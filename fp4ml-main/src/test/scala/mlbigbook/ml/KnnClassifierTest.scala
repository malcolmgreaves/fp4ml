package mlbigbook.ml

import breeze.linalg.DenseVector
import mlbigbook.math.MathVectorOps
import org.scalatest.FunSuite

import scala.language.reflectiveCalls

class KnnClassifierTest extends FunSuite {

  import KnnClassifierTest._
  import fif.ImplicitCollectionsData._

  test("Sanity Check: 1-NN Classification on train set is perfect") {

    val classify = knn.train((1, distance), stringVectorizer)(data)

    data foreach {
      case (item, label) =>
        val predicted = classify(item)
        assert(predicted === label)
    }
  }

}

object KnnClassifierTest {

  import ImplicitHashable._

  val knn = KnnClassifier[String, Boolean, Float, DenseVector](
    MathVectorOps.Implicits.FloatDenseVot,
    representsNoLabel = false
  )

  val data = Seq(
    ("becky wow", true),
    ("oh my lord", true),
    ("where is that", true),
    ("how now", false),
    ("how now brown cow", false),
    ("how how how how do you do it", false),
    ("how", false)
  )

  val words = data.flatMap { case (ws, _) => ws.split(" ") }.toSet

  val word2index = words.zipWithIndex.toMap

  val initial = word2index.map { case (_, index) => (index, 0.0f) }

  val stringVectorizer: knn.Vectorizer = new {

    lazy val vectorize = (s: String) =>
      DenseVector {
        val bothIndexValue = s
          .split(" ")
          .foldLeft(initial) {
            case (accum, word) =>
              val index = word2index(word)
              (accum - index) + (index -> (accum(index) + 1.0f))
          }

        (0 until nDimensions)
          .map { index => bothIndexValue.getOrElse(index, 0.0f) }
          .toArray
      }

    lazy val nDimensions = words.size
  }

  val distance: knn.Distance = (v1, v2) => {
    val r = knn.vops.subV(v1, v2)
    knn.vops.dot(r, r)
  }

}
