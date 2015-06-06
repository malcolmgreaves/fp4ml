package mlbigbook.ml

import mlbigbook.data._
import org.scalatest.FunSuite

class NaiveBayesTest extends FunSuite {

  import NaiveBayesTest.Sentiment._

  test("classify simple sentiment") {

    val nbpe = NaiveBayes(smooth)(labels, vdata)
    val nbc = ProbabilityClassifier(nbpe)

    reviews
      .foreach {
        case (ld, vec) =>
          val predicted = nbc(ld.example)
          val estimate = nbpe(ld.example)
          println(s"""classifying a ${ld.label} as $predicted ,\nestimated probs: $estimate\n""")
      }

    reviews
      .foreach {
        case (ld, vec) =>
          assert(ld.label == nbc(ld.example).label)
      }
  }

}

object NaiveBayesTest {

  object Sentiment {

    val neg = Labeled("negative_sentiment")

    val pos = Labeled("positive_sentiment")

    val labels = BinaryLabels(yes = pos, no = neg)

    val smooth = () => 1.0

    def mkSimple(vals: Seq[Double]): Vector =
      new Vector {
        override def valueAt(dimension: Int): Double =
          vals(dimension)
        override val cardinality: Int =
          vals.size
        override val nonZeros: Traversable[(Int, Double)] =
          vals.zipWithIndex.map(x => (x._2, x._1))
      }

    val nEachClass = 20
    val nDimension = 5
    val rand = new scala.util.Random()

    val negs: Seq[Seq[Double]] = {
      val n = (0 until nDimension).map(i => if (i % 2 == 0) 1.0 else 0.0)
      IndexedSeq.fill(nEachClass)(Seq.empty[Double])
        .map(_ => n.map(_ * rand.nextGaussian()))
    }

    val negLabeled =
      negs
        .map(_.toSeq)
        .map(x => (LabeledData(neg.label, x), mkSimple(x)))

    val poses: Seq[Seq[Double]] = {
      val n = (0 until nDimension).map(i => if (i % 2 != 0) 1.0 else 0.0)
      IndexedSeq.fill(nEachClass)(Seq.empty[Double])
        .map(_ => n.map(_ * rand.nextGaussian()))
    }

    val posLabeled =
      poses
        .map(_.toSeq)
        .map(x => (LabeledData(pos.label, x), mkSimple(x)))

    import language.implicitConversions
    import DistData._

    val reviews: DistData[(LabeledData[Seq[Double]], Vector)] =
      posLabeled ++ negLabeled

    val vdata: VectorDataIn[LabeledData[Seq[Double]]] =
      PreComputedVDIn(
        Vectorizer.Fn((l: LabeledData[Seq[Double]]) => mkSimple(l.example)),
        reviews
      )

  }

}

