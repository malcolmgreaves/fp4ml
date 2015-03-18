package mlbigbook.wordcount

import mlbigbook.data.Data
import mlbigbook.ml.Ranker

import org.scalatest.FunSuite

class RankerTest extends FunSuite {

  import WordcountTest._
  import RankerTest._

  test("[seq] rank documents from corpus") {

    val check = {
      val ranker = DocRanker(
        docLimit,
        VectorTest.wordcountVectorizer,
        corpus
      )
      checkInOutSame(ranker) _
    }

    check(docSanta)
    check(docFox)
  }
}

object RankerTest {

  val docLimit = 1

  def checkInOutSame(ranker: Ranker[Data.Document])(doc: Data.Document) = {
    val ranked = ranker(doc).toSeq.head._2
    assert(ranked == doc, s"expecting ranked: $ranked to be $doc")
  }

}
