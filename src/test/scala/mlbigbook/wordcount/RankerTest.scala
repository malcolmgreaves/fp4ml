package mlbigbook.wordcount

import scala.collection.Map

import org.scalatest.{ BeforeAndAfterAll, Suite, FunSuite }

import org.apache.spark.SparkContext
import org.apache.log4j.{ Level, Logger }

class RankerTest extends FunSuite {

  import WordcountTest._
  import RankerTest._

  test("[seq] rank documents from corpus") {

    val check = {
      val ranker = Rank(
        Vector.cosineSimilarity,
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

  def checkInOutSame(ranker: Rank.Type)(doc: Data.Document) = {
    val ranked = ranker(doc).toSeq(0)._2
    assert(ranked == doc, s"expecting ranked: ${ranked} to be ${doc}")
  }

}
