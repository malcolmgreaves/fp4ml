package mlbigbook.wordcount

import org.scalatest.FunSuite

class VectorTest extends FunSuite {

  import mlbigbook.wordcount.VectorTest._
  import mlbigbook.wordcount.WordcountTest._

  test("[seq] [word count] vectorize documents from corpus, check zero/non-zero correctness") {
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

  test("[seq] [TFIDF] vectorize documents from corpus, check zero/non-zero correctness") {
    val doc2vec = tfidfVectorizer(corpus)

    val vecFox = doc2vec(docFox)
    val vecSanta = doc2vec(docSanta)
    val vecBoth = doc2vec(docBoth)

    assert(vecFox.cardinality == vecBoth.cardinality
      && vecSanta.cardinality == vecFox.cardinality
      && vecFox.cardinality == knownCardinality,
      s"cardinality is not the same for computed document vectors")

    // all values in TFIDF-vectorized vectors should be non-zero
    checkVec(nonzeroBoth, vecFox)
    checkVec(nonzeroBoth, vecSanta)
    checkVec(nonzeroBoth, vecBoth)

    assert(vecFox.valueAt(-1) == 0.0)
    assert(vecFox.valueAt(knownCardinality) == 0.0)
  }
}

object VectorTest {

  val nonzeroFox = Set(0, 2, 4, 7, 8, 10, 12, 13)
  val nonzeroSanta = Set(1, 3, 5, 6, 9, 11)
  val nonzeroBoth = nonzeroFox ++ nonzeroSanta
  val knownCardinality = nonzeroBoth.size

  val wordcountVectorizer = Vectorizer(Counters.WordCorpusCounter, Counters.WordDocumentCounter) _
  val tfidfVectorizer = Vectorizer(Counters.NormCorpusCounter, Counters.NormDocumentCounter) _

  def checkVec(nonZero: Set[Int], v: Vector) = {
    (0 until knownCardinality).foreach(i => {
      v.valueAt(i) match {
        case 0.0 => assert(!nonZero(i), s"index $i was zero, should be non-zero")
        case _   => assert(nonZero(i), s"index $i was non-zero, should be zero")
      }
    })
  }

  def stringify(v: Vector) = (0 until v.cardinality).map(i => v.valueAt(i)).mkString(",")
}
