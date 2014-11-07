package mlbigbook.wordcount

import scala.collection.Map

import org.scalatest.{ BeforeAndAfterAll, Suite, FunSuite }

import org.apache.spark.SparkContext
import org.apache.log4j.{ Level, Logger }

class VectorTest extends FunSuite {

  import WordcountTest._
  import VectorTest._

  test("[seq] vectorize documents from corpus, check zero/non-zero correctness") {
    val doc2vec = wordcountVectorizer(corpus)

    val vecFox = doc2vec(docFox)
    val vecSanta = doc2vec(docSanta)
    val vecBoth = doc2vec(docBoth)

    assert(vecFox.cardinality == vecBoth.cardinality
      && vecSanta.cardinality == vecFox.cardinality
      && vecFox.cardinality == knownCardinality,
      s"cardinality is not the same for computed document vectors")

    checkVec(nonzeroFox, vecFox)
    checkVec(nonzeroSanta, vecSanta)
    checkVec(nonzeroBoth, vecBoth)

    assert(vecFox.valueAt(-1) == 0.0)
    assert(vecFox.valueAt(knownCardinality) == 0.0)
  }
}

object VectorTest {

  val knownCardinality = 14
  val nonzeroFox = Set(0, 2, 4, 7, 8, 10, 12, 13)
  val nonzeroSanta = Set(1, 3, 5, 6, 9, 11)
  val nonzeroBoth = Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)

  val wordcountVectorizer = Vectorizer(Counters.WordCorpusCounter, Counters.WordDocumentCounter) _

  def checkVec(nonZero: Set[Int], v: Vector) = {
    (0 until knownCardinality).foreach(i => {
      v.valueAt(i) match {
        case 0.0 => assert(!nonZero(i), s"index $i was zero, should be non-zero")
        case _   => assert(nonZero(i), s"index $i was non-zero, should be zero")
      }
    })
  }
}
