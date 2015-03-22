package mlbigbook.wordcount

import mlbigbook.data._
import mlbigbook.ml.{ Ranker, RankerIn }

/** Produces a Ranker suitable for Data.Document instances. */
object DocRanker {

  /**
   * Produces a document ranker. Internally, the ranker uses cosine similarity to rank
   * documents against the query.
   *
   * @param docLimit Resulting Ranker will yield at most this many documents per invocation.
   * @param docData Corpus and document vectorization strategy.
   */
  def apply(docLimit: Int)(docData: VectorDataIn[Data.Document]): Ranker[Data.Document] =
    Ranker(RankerIn(cosineSimilarity, docLimit))(docData)

  import mlbigbook.data.Vector._

  /** Computes the cosine similarity of two vectors: cos(angle between v1 and v2). */
  private val cosineSimilarity =
    (v1: Vector, v2: Vector) =>
      Math.abs(dotProduct(v1, v2)) / (absoluteValue(v1) * absoluteValue(v2))

}