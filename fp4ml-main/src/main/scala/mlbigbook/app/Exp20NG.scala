package mlbigbook.app

import java.io.{ FileReader, BufferedReader, File }
import java.nio.charset.Charset
import java.nio.file.Files

import breeze.linalg.{ SparseVector, DenseVector }
import mlbigbook.math.MathVectorOps
import mlbigbook.ml.{ ImplicitHashable, KnnClassifier }

import scala.io.Source
import scala.util.Random

object Exp20NG extends App {

  lazy val normalizeLine: String => String =
    s => s.trim.toLowerCase

  lazy val filterLine: String => Boolean =
    s => s.nonEmpty &&
      headerPrefixes.forall { !s.startsWith(_) } &&
      headerSuffixes.forall { !s.endsWith(_) }

  lazy val labelTransform: String => String =
    label => {
      val i = label.indexOf(".")
      if (i >= 0)
        label.substring(0, i)
      else
        label
    }

  lazy val headerPrefixes: Seq[String] =
    """
    |Xref:
    |Path:
    |From:
    |Newsgroups:
    |Subject:
    |Summary:
    |Keywords:
    |Message-ID:
    |Date:
    |Expires:
    |Followup-To:
    |Distribution:
    |Organization:
    |Approved:
    |Supersedes:
    |Lines:
    |Archive-name:
    |Alt-atheism-archive-name:
    |Last-modified:
    |Version:
    |-----BEGIN PGP SIGNED MESSAGE-----
    |In article
    |From article
    |>
    |>>
    |References:
    |Email:
    |Sender:
    |NNTP-posting-host
    |NNTP-posting-user
    |--
    |: >:
    |: >
    | >
    |:
    |<
  """.stripMargin
      .trim
      .toLowerCase
      .split { "\n" }
      .toSeq

  lazy val headerSuffixes: Seq[String] =
    """
      |writes:
      |.com
    """.stripMargin
      .trim
      .toLowerCase
      .split { "\n" }
      .toSeq

  lazy val ngDirectory = new File("./20_newsgroups")
  println(s"Loading 20 Newsgroup Data from:\n${ngDirectory.getCanonicalPath}\n")

  import scala.collection.JavaConverters._
  lazy val loadNgFi: File => Seq[String] =
    fi => if (fi isFile)
      {
        val br = new BufferedReader(new FileReader(fi))
        val buf = new scala.collection.mutable.ArrayBuffer[String](420)
        var line: String = br.readLine()
        while (line != null) {
          buf.append(line)
          line = br.readLine()
        }
        buf.toSeq
      }
        .map { normalizeLine }
        .filter { filterLine }
    else
      Seq.empty

  lazy val loadNgData: File => Seq[(File, Seq[String])] =
    f => {
      if (f.isDirectory) {
        Option(f.listFiles())
          .map { _.toSeq }
          .getOrElse { Seq.empty }
          .flatMap { loadNgData }

      } else if (f.isFile)
        Seq((f, loadNgFi(f)))

      else
        Seq.empty
    }

  // // // // // // // // // // // // // // // // // // // // // // // // // //
  //
  //              S C R I P T
  //
  // // // // // // // // // // // // // // // // // // // // // // // // // //

  lazy val ng20 = ngDirectory.listFiles.filter(_ != null).toSeq
  println(s"There are ${ng20.size} newsgroup directories")

  val newsgroup2fileandcontent =
    ng20
      .map { ngDir =>
        println(s"loading data from the ${ngDir.getName} newsgroup ... ")
        val bothFiLines = loadNgData(ngDir)
        (ngDir.getName, bothFiLines)
      }
      .toMap

  type Document = String

  import ImplicitHashable._
  import fif.ImplicitCollectionsData._
  lazy val knn = KnnClassifier[Document, String, Float, SparseVector](
    MathVectorOps.Implicits.FloatSparseVot,
    representsNoLabel = ""
  )

  val stringVectorizer: knn.Vectorizer = new {

    val word2index: Map[String, Int] = {
      val words = {
        for {
          (_, data) <- newsgroup2fileandcontent
          (_, lines) <- data
          line <- lines
          word <- line.split(" ")
        } yield word
      }.toSet

      println(s"There are ${words.size} unique words")

      words.zipWithIndex.toMap
    }

    lazy val vectorize = (s: Document) =>
      SparseVector[Float](word2index.size)({
        val bothIndexValue = s
          .split(" ")
          .foldLeft(Map.empty[Int, Float]) {
            case (accum, word) =>

              if (word2index contains word) {
                val index = word2index(word)
                if (accum.contains(index))
                  (accum - index) + (index -> (accum(index) + 1.0f))
                else
                  accum + (index -> 1.0f)

              } else
                accum
          }

        bothIndexValue
          .map { case (index, count) => (index, math.log(count).toFloat) }
          .toSeq
      }: _*)

    lazy val nDimensions = word2index.size
  }

  val distance: knn.Distance = (v1, v2) => {
    val r = knn.vops.subV(v1, v2)
    knn.vops.dot(r, r)
  }

  val allLabeledData: Seq[(Document, String)] = for {
    (ng, bothFiAndData) <- newsgroup2fileandcontent.toSeq
    (_, lines) <- bothFiAndData
  } yield (lines.mkString("\n"), labelTransform(ng))

  println(s"total labeled data size: ${allLabeledData.size}")

  val (train, test): (Seq[(Document, String)], Seq[(Document, String)]) = {

    val shuffled: Seq[(Document, String)] = allLabeledData
      .map { x => (x, math.random) }
      .sortBy { case (_, rando) => rando }
      .map { case (x, _) => x }

    val si = (shuffled.size * .9).toInt

    (
      shuffled.slice(0, si),
      shuffled.slice(si + 1, shuffled.size)
    )
  }

  println(s"building kNN on ${train.size} examples")
  val classifier = knn.train((5, distance), stringVectorizer)(train)

  val nTake = 25
  println(s"grabbing $nTake random test example (from ${test.size} documents)")

  var nCorrect = 0
  test.take(nTake).foreach {
    case (testDoc, testLabel) =>
      val predicted = classifier(testDoc)
      println(s"predicted: $predicted actual: $testLabel")
      if (predicted == testLabel)
        nCorrect += 1
  }
  println(s"\n\nAccuracy:  $nCorrect / $nTake  = ${(nCorrect.toFloat / nTake.toFloat) * 100.0f} %")
}