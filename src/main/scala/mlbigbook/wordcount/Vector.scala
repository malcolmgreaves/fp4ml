package mlbigbook.wordcount

import scala.collection.Map

/**
 * Traits and type definitions related to representations of numerical vectors
 * and functions for operations on vectors.
 *
 * @author Malcolm Greaves
 */

/**
 * The Vector trait defines operations on data that behaves as a dense numerical vector.
 */
trait Vector {
  /** The size of the feature space that this vector is based on. */
  val cardinality: Int

  def valueAt(dimension: Int): Double

  /**
   * "zip" two vectors together. The resulting traversable will yield a pair
   * of values from each zipped vector. These values align 1-for-1 to the
   * same numerical dimension from each vector, starting from 1 until the
   * vector cardinality.
   *
   * This code fails with an assertion error if the vectors do not have the
   * same cardinality.
   */
  def zip(v: Vector): Traversable[(Double, Double)] = {
    assert(cardinality == v.cardinality)
    (0 until cardinality).map(dim => (valueAt(dim), v.valueAt(dim)))
  }
}

/**
 * Collection of vector operations and type definitions.
 */
object Vector {

  /**
   * Compute the dot-product of two vectors.
   *
   * Fails with assertion error if the vector cardinalities do not match up.
   */
  def dotProduct(v1: Vector, v2: Vector): Double = {
    v1.zip(v2).foldLeft(0.0)({
      case (dpSum, (elementV1, elementV2)) => dpSum + elementV1 * elementV2
    })
  }

  /**
   * Compute the sum of the absolute value of each element of the vector.
   */
  def absoluteValue(v: Vector): Double = {
    (0 until v.cardinality).foldLeft(0.0)({
      case (absval, dimension) => absval + Math.abs(v.valueAt(dimension))
    })
  }

  type Similarity = (Vector, Vector) => Double

  /**
   * Computes the cosine similairty between two vectors, which is defined as:
   *
   *                  | v1 * v2 |
   *               -----------------
   *                  |v1| * |v2|
   *
   */
  def cosineSimilarity(v1: Vector, v2: Vector): Double = {
    Math.abs(dotProduct(v1, v2)) / (absoluteValue(v1) * absoluteValue(v2))
  }
}

/**
 * Abstract definition of a class that has a function that produces
 * a word count mappinp (either whole or real numbered) from a corpus.
 */
abstract class CorpusCounter[@specialized(Long, Double) N: Numeric] {
  def apply(d: Data.Corpus): Map[Data.Word, N]
}

/**
 * Abstract definition of a class that has a function that produces
 * a word count mappinp (either whole or real numbered) from a document.
 */
abstract class DocCounter[@specialized(Long, Double) N: Numeric] {
  def apply(d: Data.Document): Map[Data.Word, N]
}

/**
 * Collection of word counting functions that operate on the document and sentence
 * level. The Word* counters use traditional word count. The Norm* counters use
 * TF-IDF weighting of word counts.
 */
object Counters {

  val WordCorpusCounter = new CorpusCounter[Long] {
    def apply(d: Data.Corpus): Data.WordCount = Count.wordcountCorpus(d)
  }

  val WordDocumentCounter = (ignored: Data.Corpus) => {
    new DocCounter[Long] {
      def apply(d: Data.Document): Data.WordCount = Count.wordcountDocument(d)
    }
  }

  val NormCorpusCounter = new CorpusCounter[Double] {
    def apply(d: Data.Corpus): Data.NormalizedWordCount = TFIDF(d)
  }

  val NormDocumentCounter = (c: Data.Corpus) => {
    new DocCounter[Double] {
      val docLevelTFIDF = TFIDF.docTFIDF(c)
      override def apply(d: Data.Document): Data.NormalizedWordCount = docLevelTFIDF(d)
    }
  }
}

/**
 * Type defintions for document vectorizers: a function that can compute a vector representation
 * of a document. The object's apply method construts such a function.
 */
object Vectorizer {

  import Numeric.Implicits._

  /**
   * A function that constructs a vector representation for a document.
   */
  type Type = Data.Document => Vector

  /**
   * A function that uses a corpus to construct a document vectorizer.
   */
  type Maker = Data.Corpus => Type

  /**
   * Constructs a document vectorizing funciton. The corpus and document counting functions are used
   * in the computation of document vectors. The input corpus is used as the basis for word count
   * computations and for the resulting document vectorizer.
   */
  def apply[N: Numeric](corpCount: CorpusCounter[N], mkDocCount: (Data.Corpus) => DocCounter[N])(documents: Data.Corpus): Type = {

    // each index corresponds to an individual word
    val index2word: IndexedSeq[Data.Word] = {
      corpCount(documents).aggregate(Set.empty[Data.Word])(
        { case (words, (word, _)) => words + word },
        _ ++ _
      ).toIndexedSeq
    }

    val docCount = mkDocCount(documents)

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

