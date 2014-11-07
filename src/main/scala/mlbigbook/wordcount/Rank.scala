package mlbigbook.wordcount

object Rank {

  type Score = Double

  type RankedDoc = (Score, Data.Document)

  type Type = Data.Document => Traversable[RankedDoc]

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
