package mlbigbook.wordcount

import mlbigbook.data.{ AddMap, Data }
import org.scalatest.FunSuite

import scala.collection.Map

class WordcountTest extends FunSuite {

  import mlbigbook.wordcount.WordcountTest._

  test("[seq] wordcount sentence") {
    assertCountsL(actualCounts(idFox), Count.wordcountSentence(sentFox))
    assertCountsL(actualCounts(idSanta), Count.wordcountSentence(sentSanta))
  }

  test("[seq] wordcount document") {
    assertCountsL(actualCounts(idFox), Count.wordcountDocument(docFox))
    assertCountsL(actualCounts(idSanta), Count.wordcountDocument(docSanta))
    assertCountsL(actualCounts(idBoth), Count.wordcountDocument(docBoth))
  }

  test("[seq] wordcount corpus") {
    assertCountsL(all, Count.wordcountCorpus(corpus))
  }
}

object WordcountTest {

  trait ID {
    def id(): Int
  }

  case class DocID(id: Int, doc: Data.Document) extends ID

  val sentFox = Data.Sentence(IndexedSeq("the", "quick", "brown", "fox", "jumped", "over", "the", "lazy", "dog"))
  val docFox = Data.Document(IndexedSeq(sentFox))
  val idFox = DocID(0, docFox)

  val sentSanta = Data.Sentence(IndexedSeq("santa", "claus", "is", "coming", "to", "town"))
  val docSanta = Data.Document(IndexedSeq(sentSanta))
  val idSanta = DocID(1, docSanta)

  val docBoth = Data.Document(IndexedSeq(sentFox, sentSanta))
  val idBoth = DocID(2, docBoth)

  val corpus = Seq(docFox, docSanta, docBoth)

  val actualCounts = Map(
    idFox -> Map(
      "the" -> 2L,
      "quick" -> 1L,
      "brown" -> 1L,
      "fox" -> 1L,
      "jumped" -> 1L,
      "over" -> 1L,
      "lazy" -> 1L,
      "dog" -> 1L
    ),
    idSanta -> Map(
      "santa" -> 1L,
      "claus" -> 1L,
      "is" -> 1L,
      "coming" -> 1L,
      "to" -> 1L,
      "town" -> 1L
    ),
    idBoth -> Map(
      "the" -> 2L,
      "quick" -> 1L,
      "brown" -> 1L,
      "fox" -> 1L,
      "jumped" -> 1L,
      "over" -> 1L,
      "lazy" -> 1L,
      "dog" -> 1L,
      "santa" -> 1L,
      "claus" -> 1L,
      "is" -> 1L,
      "coming" -> 1L,
      "to" -> 1L,
      "town" -> 1L
    )
  )

  val all = {
    val e: Map[String, Long] = Map()
    actualCounts.foldLeft(e)({
      case (m, (_, actual)) => actual.foldLeft(m)({
        case (a, (k, v)) => AddMap.Whole.add(a, k, v)
      })
    })
  }

  def assertCountsL(actual: Map[String, Long], counted: Map[String, Long]) = {
    counted.foreach({
      case (word, count) => assert(actual(word) == count,
        s"""\"$word\" is unexpected (has count $count)""")
    })

    assert(counted.size == actual.size, s"${counted.size} found, expecting ${actual.size} entries")

    val actualSum = actual.map(_._2).foldLeft(0L)(_ + _)
    val countedSum = counted.map(_._2).foldLeft(0L)(_ + _)
    assert(actualSum == countedSum, s"$countedSum total counts, expecting $actualSum total counts")
  }

}