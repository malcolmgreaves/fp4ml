package mlbigbook.ml

import mlbigbook.data._
import mlbigbook.wordcount.LocalSparkContext
import org.scalatest.FunSuite

class NaiveBayesTest extends FunSuite {

  import NaiveBayesTest.Sentiment._

  test("classify simple sentiment") {

    val nbpe = NaiveBayes(bls, smooth)(vdata)
    val nbc = ProbabilityClassifier(nbpe)

    println(s"negs: ${nbpe.apply(negs)}")
    println(s"poss: ${nbpe.apply(poses)}")

    val shouldBeNeg = nbc(negs)
    val shouldBePos = nbc(poses)

    assert(shouldBeNeg.label === neg.label)
    assert(shouldBePos.label === pos.label)
  }

}

object NaiveBayesTest {

  object Sentiment {

    val neg = Labeled("negative_sentiment")

    val pos = Labeled("positive_sentiment")

    val bls = BinaryLS(neg, pos, 0.5)

    val smooth = new Smoothing { override def apply(): Double = 1.0 }

    def mkSimple(vals: Seq[Double]): Vector =
      new Vector {
        override def valueAt(dimension: Int): Double =
          vals(dimension)
        override val cardinality: Int =
          vals.size
        override val nonZeros: Traversable[(Int, Double)] =
          vals.zipWithIndex.map(x => (x._2, x._1))
      }

    val negs: Seq[Double] = (0 until 10).map(i => if (i % 2 == 0) 1.0 else 0.0)

    val poses: Seq[Double] = (0 until 10).map(i => if (i % 2 == 0) 0.0 else 1.0)

    import DistData.TravDistData
    val reviews: DistData[(LabeledData[Seq[Double]], Vector)] =
      Seq(
        (LabeledData(neg.label, negs), mkSimple(negs)),
        (LabeledData(pos.label, poses), mkSimple(poses))
      )

    val vdata: VectorDataIn[LabeledData[Seq[Double]]] =
      PreComputedVDIn(
        Vectorizer.Fn((l: LabeledData[Seq[Double]]) => mkSimple(l.example)),
        reviews
      )

  }

}

