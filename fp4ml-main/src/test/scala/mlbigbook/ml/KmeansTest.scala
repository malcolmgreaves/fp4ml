package mlbigbook.ml

import breeze.linalg.DenseVector
import mlbigbook.math.{MathVectorOps, NumericConversion, RandoMut}
import org.scalatest.FunSuite

import scala.language.reflectiveCalls

class KmeansTest extends FunSuite {

  import KmeansTest._
  import fif.ImplicitCollectionsData._

  test("Simple run") {

    val initial =
      kmeans.initialize(conf.nClusters, stringVectorizer.nDimensions)
    println(
      s"""INITIAL with nClusters= ${conf.nClusters} & nDimensions= ${stringVectorizer.nDimensions}
         |# of clusters FROM INITIAL: ${initial.size}
         |
         |${initial.mkString("\n")}
         |
       """.stripMargin
    )

    val centers = kmeans.cluster(conf, distance, stringVectorizer)(data)

    centers foreach println
  }

}

object KmeansTest {

  val conf = ClusteringConf(
    nClusters = 2,
    tolerance = 0.001,
    maxIterations = 25
  )

  val kmeans: Kmeans.Type[String, Float, DenseVector] = {
    import NumericConversion.Implicits._
    Kmeans[String, Float, DenseVector](
      MathVectorOps.Implicits.FloatDenseVot,
      RandoMut.newSeedPerCall[Float]
    )
  }

  val data = Seq(
    "hello world",
    "hello hello",
    "how world",
    "hello how world world hello"
  )

  val words = "hello world how are you doing today fine great".split(" ").toSeq

  val word2index = words.zipWithIndex.toMap

  val initial = word2index.map { case (_, index) => (index, 0.0f) }

  val stringVectorizer: kmeans.Vectorizer = new {

    lazy val vectorize = (s: String) =>
      DenseVector {
        val bothIndexValue = s.split(" ").foldLeft(initial) {
          case (accum, word) =>
            val index = word2index(word)
            (accum - index) + (index -> (accum(index) + 1.0f))
        }

        (0 until nDimensions).map { index =>
          bothIndexValue.getOrElse(index, 0.0f)
        }.toArray
    }

    lazy val nDimensions = words.size
  }

  val distance: kmeans.Distance = (v1, v2) => {
    val r = kmeans.vops.subV(v1, v2)
    kmeans.vops.dot(r, r)
  }

}
