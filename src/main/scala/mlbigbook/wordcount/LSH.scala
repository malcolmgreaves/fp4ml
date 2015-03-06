package mlbigbook.wordcount

trait LSH[T] {
  val partitionFuncs: IndexedSeq[T => Int]
}

object LSH {

  import DistData._

  def apply(config: LSH[Data.Document])(
    dist: Vector.Similarity,
    docLimit: Int,
    mkVec: Vectorizer.Maker,
    documents: Data.Corpus): Rank.Type = {

    val vectorizer = mkVec(documents)

    /* 

    -- need to make hash tables for each partition function:
        val table = documents
          .map(d => config.paritionFuncs.map(pf => pf(d)).toSeq)
          .aggregate(new table)(
            {
              case (table, particalFunctionResults) => update table with PF results
            },
            {
              case (t1,t2) => combine tables t1 and t2
            }
          )

    -- for new document:
        (d:Data.Document) => {
          config.partitionFuncs.flatMap(pf => {
            table(pf(new document)).map(d => (dist(d, new document), d)
          })
          .toSet.toSeq
          .sortBy(-_._1)
          .take(docLimit)
        }

    */

    throw new RuntimeException
  }
}
