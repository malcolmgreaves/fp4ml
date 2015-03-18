package mlbigbook.wordcount

import mlbigbook.data._
import mlbigbook.ml.Ranker

/** Produces a Ranker suitable for Data.Document instances. */
object DocRanker {

  /**
   * Produces a document ranker. Internally, the ranker uses cosine similarity to rank
   * documents against the query.
   *
   * @param docLimit Resulting Ranker will yield at most this many documents per invocation.
   * @param mkVec Constructs a Vectorizer using the input corpus.
   * @param documents Corpus to build the vectorizer from and from where to rank documents.
   */
  def apply(docLimit: Int, mkVec: VectorizerMaker[Data.Document], documents: Data.Corpus): Ranker[Data.Document] =
    Ranker(Similarity.cosine, docLimit, mkVec, documents)
}