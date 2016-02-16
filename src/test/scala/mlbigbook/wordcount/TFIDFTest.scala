package mlbigbook.wordcount

import mlbigbook.data.OLD_AddMap
import org.scalatest.FunSuite

import scala.collection.Map

class TFIDFTest extends FunSuite {

  import mlbigbook.wordcount.WordcountTest._

  private val emptyL: Map[String, Long] = Map()
  private val emptyD: Map[String, Double] = Map()

  def assertCountsD(actual: Map[String, Double], counted: Map[String, Double]) = {
    counted.foreach({
      case (word, count) => assert(
        actual(word) == count,
        s"$word is unexpected (has count $count)"
      )
    })

    assert(counted.size == actual.size, s"${counted.size} found, expecting ${actual.size} entries")

    val actualSum = actual.map(_._2).foldLeft(0.0)(_ + _)
    val countedSum = counted.map(_._2).foldLeft(0.0)(_ + _)
    assert(actualSum == countedSum, s"$countedSum total counts, expecting $actualSum total counts")
  }

  val expectedFoxTF = {
    val nWords = sentFox.words.size.toDouble
    sentFox.words.foldLeft(emptyD)({
      case (a, word) => OLD_AddMap.Real.add(a, word, 1.0 / nWords)
    })
  }

  test("[seq] term frequency sentence") {
    val observedFoxTF = TFIDF.termFreq(actualCounts(idFox))
    assertCountsD(expectedFoxTF, observedFoxTF)
  }

  ignore("[unk] TFIDF corpus") {
    val corpusNormCounts = TFIDF(corpus)
    corpusNormCounts.toSeq.sortBy(-_._2).foreach(println)
  }

  test("[seq] document frequency corpus") {
    val observedDF = TFIDF.docfreqCorpus(corpus)
    val sentFoxSet = sentFox.words.toSet
    val dfSum = observedDF.foldLeft(0L)({
      case (s, (word, df)) => {
        if (sentFoxSet(word)) {
          assert(
            df == 2,
            s"expecting DF of FOX sentence to be 2, actual $df"
          )
        } else {
          assert(
            df == 2,
            s"expecting DF of SANTA sentence to be 2, actual $df"
          )
        }
        s + df
      }
    })
    val expectedDFSum = {
      sentFox.words.size * 2 + // all words in FOX sentence are in 2 documents
        sentSanta.words.size * 2 - // all words in SANTA sentence are in 2 documents
        2 // "the" is double counted, it only has {0,1} DF count per document!
    }
    assert(
      dfSum == expectedDFSum,
      s"expecting DF sum $expectedDFSum actual $dfSum"
    )
  }

}