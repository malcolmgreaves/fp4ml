package mlbigbook.wordcount

import mlbigbook.wordcount.Data

import scala.collection.Map

trait Vector {
  val cardinality: Int

  def valueAt(dimension: Int): Double

  def zip(v: Vector): Traversable[(Double, Double)] = {
    assert(cardinality == v.cardinality)
    (0 until cardinality).map(dim => (valueAt(dim), v.valueAt(dim)))
  }
}

object Vector {

  def dotProduct(v1: Vector, v2: Vector): Double = {
    v1.zip(v2).foldLeft(0.0)({
      case (dpSum, (elementV1, elementV2)) => dpSum + elementV1 * elementV2
    })
  }

  def absoluteValue(v: Vector): Double = {
    (0 until v.cardinality).foldLeft(0.0)({
      case (absval, dimension) => absval + Math.abs(v.valueAt(dimension))
    })
  }

  type DistanceFn = (Vector, Vector) => Double

  def cosineSimilarity(v1: Vector, v2: Vector): Double = {
    Math.abs(dotProduct(v1, v2)) / (absoluteValue(v1) * absoluteValue(v2))
  }
}

abstract class CorpusCounter[@specialized(Long, Double) N: Numeric] {
  def apply(d: Data.Corpus): Map[Data.Word, N]
}

abstract class DocCounter[@specialized(Long, Double) N: Numeric] {
  def apply(d: Data.Document): Map[Data.Word, N]
}

object Counters {

  val WordCorpusCounter = new CorpusCounter[Long] {
    def apply(d: Data.Corpus): Data.WordCount = Count.wordcountCorpus(d)
  }

  val WordDocumentCounter = new DocCounter[Long] {
    def apply(d: Data.Document): Data.WordCount = Count.wordcountDocument(d)
  }

  val NormCorpusCounter = new CorpusCounter[Double] {
    def apply(d: Data.Corpus): Data.NormalizedWordCount = TFIDF(d)
  }

  def mkNormDocCounter(c: Data.Corpus): DocCounter[Double] = {
    new DocCounter[Double] {
      val docLevelTFIDF = TFIDF.docTFIDF(c)
      override def apply(d: Data.Document): Data.NormalizedWordCount = docLevelTFIDF(d)
    }
  }
}

object Vectorizer {

  import Numeric.Implicits._

  type Type = Data.Document => Vector

  type Maker = Data.Corpus => Type

  def apply[N: Numeric](corpCount: CorpusCounter[N], docCount: DocCounter[N])(documents: Data.Corpus): Type = {

    val index2word: IndexedSeq[Data.Word] = {

      val word2index = corpCount(documents).foldLeft((Data.EmptyWordCount, 0))({
        case ((word2count, nextIndex), (word, _)) => word2count.get(word) match {
          case Some(existingIndex) => (word2count, nextIndex)
          case None                => (word2count + (word -> nextIndex), nextIndex + 1)
        }
      })._1

      word2index.toSeq.sortBy(_._2).map(_._1).toIndexedSeq
    }

    (d: Data.Document) => {
      val countedD = docCount(d)
      new Vector {
        val cardinality = index2word.size
        def valueAt(dim: Int): Double = {
          if (dim >= 0 && dim < cardinality) {
            val word = index2word(dim)
            if (countedD.contains(word)) {
              countedD(word).toDouble
            } else {
              0.0
            }
          } else {
            0.0
          }
        }
      }
    }
  }
}

