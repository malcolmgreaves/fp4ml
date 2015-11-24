package mlbigbook.wordcount

import java.io.File

import mlbigbook.data.{ NeedsApplicationVDIn, TextData, DataClass }
import mlbigbook.ml.Ranker
import org.apache.spark.SparkContext
import org.scalatest.FunSuite

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class TwentyNewsgroupsExperimentTestIgnore extends FunSuite {

  import DataClass._
  import mlbigbook.wordcount.TwentyNewsgroupsExperimentTestIgnore._

  lazy val sc: SparkContext = {
    class X extends FunSuite with LocalSparkContext
    new X().sc
  }

  lazy val (trainDocs20Newsgroups, testDocs20Newsgroups): (TextData.Corpus, TextData.Corpus) = {
    val lns = lines(None) _
    (
      { // train
        val a = dirTrainNG.listFiles()
        a.slice(0, a.size / 2).map(lns).map(mkDoc).toTraversable
      },
      { // test
        val a = dirTestNG.listFiles()
        a.slice(0, a.size / 2).map(lns).map(mkDoc).toTraversable
      }
    )
  }

  ignore("[experiment] 20 newsgroups document ranking, wordcount vs. TFIDF based vectorization") {
    println(s"Training corpus:  $dirTrainNG")
    println(s"Query corpus:     $dirTestNG")

    val start = System.currentTimeMillis()
    doExperimentPrintResults(docLimit)(trainDocs20Newsgroups, testDocs20Newsgroups)
    val end = System.currentTimeMillis()

    println(s"Finished in ${end - start}ms")
  }

  def doExperimentPrintResults(docLimit: Int)(train: DataClass[TextData.Document], test: DataClass[TextData.Document]): Unit = {

    val rankWC = DocRanker(docLimit)(NeedsApplicationVDIn(VectorTest.wordcountVectorizer, train))

    val rankTFIDF = DocRanker(docLimit)(NeedsApplicationVDIn(VectorTest.tfidfVectorizer, train))

    test.foreach(doc => {

      val topdocsWC = rankWC(doc)
      val topdocsTFIDF = rankTFIDF(doc)

      println(s"Query document: $doc\nRanked Documents, top $docLimit...")
      var i = 1
      topdocsWC.toSeq.zip(topdocsTFIDF.toSeq).foreach({
        case (rdocWC, rdocTFIDF) => {
          println(s"#$i\tWC: $rdocWC\n\tTFIDF: $rdocTFIDF")
          i += 1
        }
      })
      println("")
    })
  }

}

object TwentyNewsgroupsExperimentTestIgnore {

  def lines(scOpt: Option[SparkContext])(fi: File): DataClass[String] = scOpt match {
    case Some(sc) => sc.textFile(fi.toString).filter(_.size > 0)
    case None     => Source.fromFile(fi).getLines().filter(_.size > 0).foldLeft(List.empty[String])((a, line) => a :+ line)
  }

  val docLimit = 10

  def fileAt(parent: File)(parts: String*) = parts.foldLeft(parent)((f, p) => new File(f, p))

  val dataDir = fileAt(new File("."))("data")

  val dir20NG = fileAt(dataDir)("20newsgroups")
  val dirTrainNG = fileAt(dir20NG)("train")
  val dirTestNG = fileAt(dir20NG)("test")

  val newsgroups = Set(
    "alt.atheism",
    "comp.graphics",
    "comp.os.ms-windows.misc",
    "comp.sys.ibm.pc.hardware",
    "comp.sys.mac.hardware",
    "comp.windows.x",
    "misc.forsale",
    "rec.autos",
    "rec.motorcycles",
    "rec.sport.baseball",
    "rec.sport.hockey",
    "sci.crypt",
    "sci.electronics",
    "sci.med",
    "sci.space",
    "soc.religion.christian",
    "talk.politics.guns",
    "talk.politics.mideast",
    "talk.politics.misc",
    "talk.religion.misc"
  )

  def mkDoc(d: DataClass[String]) = TextData.Document({
    var ab = ArrayBuffer.empty[TextData.Sentence]
    d.map(convertSentence).map(s => {
      ab += s
    })
    ab.toTraversable
  })

  def convertSentence(s: String) = TextData.Sentence(s.split("\\\\s+"))
}