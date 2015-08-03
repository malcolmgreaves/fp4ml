package mlbigbook.ml

import org.scalatest.FunSuite

class CountingNaiveBayesTest extends FunSuite {

  import CountingNaiveBayesTest._

  test("test using small vocabulary, bag-of-words (both Long and Int manifestations)") {
    testSmallVocab(CountingNaiveBayes.Int)
    testSmallVocab(CountingNaiveBayes.Long)
  }

  def testSmallVocab[N: Numeric](c: CountingNaiveBayes[N]): Unit = {
    val nb = NaiveBayesModule(c.produce(training))
    val estimated = smallVocabData.map(nb.estimate).toSeq

    assert(estimated.size == 2)
    assert(estimated.head == estimated(1))
    assert(estimated.head == expectedSmallVocabDistribution)
  }

}

object CountingNaiveBayesTest {

  import mlbigbook.data.Data
  import Data._

  val smallVocabData: Data[Feature.Vector[String]] =
    Seq(
      "tom hello world how are you today".split(" "),
      "how hello today you are world tom".split(" ")
    )
      .map(array2Data)

  val smallVocabLabels: Data[String] =
    Seq(
      "positive",
      "negative"
    )

  val training: Learning[Feature.Vector[String], String]#TrainingData =
    smallVocabData.zip(smallVocabLabels)

  val expectedSmallVocabDistribution =
    DiscreteDistribution(smallVocabLabels.map(x => (x, 0.5)).toMap)

}