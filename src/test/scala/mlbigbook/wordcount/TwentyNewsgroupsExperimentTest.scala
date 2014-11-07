package mlbigbook.wordcount

import org.scalatest.FunSuite

class TwentyNewsgroupsExperimentTest extends FunSuite with LocalSparkContext {

  import TwentyNewsgroupsExperimentTest._

  lazy val trainDocs20Newsgroups: Data.Corpus = ??? // sc.textFile train 20 newsgroups data...
  lazy val testDocs20Newsgroups:Data.Corpus = ??? // sc.textFile test 20 newsgroups data...

  test("[experiment] 20 newsgroups document ranking, wordcount vs. TFIDF based vectorization") {
    doExperimentPrintResults(docLimit)(trainDocs20Newsgroups, testDocs20Newsgroups)
  }

  def doExperimentPrintResults(docLimit:Int)(train:DistData[Data.Document], test:DistData[Data.Document]):Unit = {

    val rankWC = Rank(
      Vector.cosineSimilarity,
      docLimit,
      VectorTest.wordcountVectorizer,
      train)

    val rankTFIDF = Rank(
      Vector.cosineSimilarity,
      docLimit,
      VectorTest.tfidfVectorizer,
      train)

    test.map(doc => {

      val topdocsWC = rankWC(doc)
      val topdocsTFIDF = rankTFIDF(doc)

      println(s"Query document: $doc")
      topdocsWC.toSeq.zip(topdocsTFIDF.toSeq).foreach({
        case (rdocWC, rdocTFIDF) => println(s"Ranked documents, WC: $rdocWC  TFIDF: $rdocTFIDF")
      })
      println("")
    })
  }

}

object TwentyNewsgroupsExperimentTest {

  val docLimit = 10
}