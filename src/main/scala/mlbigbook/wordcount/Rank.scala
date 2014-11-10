package mlbigbook.wordcount

/**
 * The Rank object contains type definitions and the apply method for constructing a
 * document ranking function.
 *
 * @author Malcolm Greaves
 */
object Rank {

  type Score = Double

  type RankedDoc = (Score, Data.Document)

  type Type = Data.Document => Traversable[RankedDoc]

  /*
   * Prooduce a document ranking function using the input distance function (dist)
   * document vectorization strategy (mkVec). The integer docLimit is the upper bound
   * on the number of documents that are produced from the document ranking function.
   * The final parameter, documents, is the corpus from which were are ranking documents
   * against. 
   */
  def apply(
    dist: Vector.DistanceFn,
    docLimit: Int,
    mkVec: Vectorizer.Maker,
    documents: Data.Corpus): Type = {

    val vectorizer = mkVec(documents)
    val vectorizedDocuments = documents.map(d => (d, vectorizer(d)))

    (inputDoc: Data.Document) => {

      val vecInputDoc = vectorizer(inputDoc)
      vectorizedDocuments
        .map({ case (doc, vec) => (dist(vec, vecInputDoc), doc) })
        .sortBy(-_._1)
        .take(docLimit)
    }
  }
}
