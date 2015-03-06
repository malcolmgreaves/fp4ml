package mlbigbook.wordcount

import scala.reflect.ClassTag

/**
 * The Rank object contains type definitions and the apply method for constructing a
 * document ranking function.
 *
 * @author Malcolm Greaves
 */
object Rank {

  type Score = Double

  type RankedDoc = (Double, Data.Document)

  type Type = Data.Document => Traversable[RankedDoc]

  /*
   * Prooduce a document ranking function using the input distance function (dist)
   * document vectorization strategy (mkVec). The integer docLimit is the upper bound
   * on the number of documents that are produced from the document ranking function.
   * The final parameter, documents, is the corpus from which were are ranking documents
   * against. 
   */
  def apply(
    dist: Vector.Distance,
    docLimit: Int,
    mkVec: Vectorizer.Maker,
    documents: Data.Corpus): Type = {

    val vectorizer = mkVec(documents)
    val vectorizedDocuments = documents.map(d => (d, vectorizer(d)))

    (inputDoc: Data.Document) => {

      val vecInputDoc = vectorizer(inputDoc)

      takeTopK(
        docLimit,
        vectorizedDocuments.map({ case (doc, vec) => (dist(vec, vecInputDoc), doc) })
      )
    }
  }

  /**
   * Evaluates to a Traversable containing the elements that have the largest associated values in the input. The
   * returned Traversable has at most limit items.
   */
  def takeTopK[T, N](limit: Int, elements: DistData[(N, T)])(implicit n: Numeric[N], c: ClassTag[N]): Traversable[(N, T)] =
    elements
      .sortBy(_._1)(c, n.reverse)
      .take(limit)

}
