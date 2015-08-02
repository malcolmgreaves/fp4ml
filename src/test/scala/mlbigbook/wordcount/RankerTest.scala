package mlbigbook.wordcount

import mlbigbook.data.{ NeedsApplicationVDIn, TextData }
import mlbigbook.ml.Ranker

import org.scalatest.FunSuite

class RankerTest extends FunSuite {

  import WordcountTest._
  import RankerTest._

  test("[seq] rank documents from corpus") {

    val check = {
      val ranker = DocRanker(docLimit)(NeedsApplicationVDIn(VectorTest.wordcountVectorizer, corpus))
      checkInOutSame(ranker) _
    }

    check(docSanta)
    check(docFox)
  }
}

object RankerTest {

  val docLimit = 1

  def checkInOutSame(ranker: Ranker[TextData.Document])(doc: TextData.Document) = {
    val ranked = ranker(doc).toSeq.head._1
    assert(ranked == doc, s"expecting ranked: $ranked to be $doc")
  }

}
