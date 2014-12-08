package mlbigbook.wordcount

import org.apache.log4j.{ Level, Logger }
import org.apache.spark.SparkContext
import org.scalatest.{ BeforeAndAfterAll, FunSuite, Suite }

import scala.collection.Map
import scala.util.Random

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

class DataTest extends FunSuite {

  test("definition of indicator map"){
    val m:Map[String, Long] = Map()
    (0 until 25).foreach(_ => {
      assert(IndicatorMap.add(m, "hello", Random.nextLong()) == IndicatorMap.mark(m, "hello"))
    })
  }
}

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

class TFIDFTest extends FunSuite {

  import mlbigbook.wordcount.WordcountTest._

  private val emptyL: Map[String, Long] = Map()
  private val emptyD: Map[String, Double] = Map()

  def assertCountsD(actual: Map[String, Double], counted: Map[String, Double]) = {
    counted.foreach({
      case (word, count) => assert(actual(word) == count,
        s"$word is unexpected (has count $count)")
    })

    assert(counted.size == actual.size, s"${counted.size} found, expecting ${actual.size} entries")

    val actualSum = actual.map(_._2).foldLeft(0.0)(_ + _)
    val countedSum = counted.map(_._2).foldLeft(0.0)(_ + _)
    assert(actualSum == countedSum, s"$countedSum total counts, expecting $actualSum total counts")
  }

  val expectedFoxTF = {
    val nWords = sentFox.words.size.toDouble
    sentFox.words.foldLeft(emptyD)({
      case (a, word) => AddMap.Real.add(a, word, 1.0 / nWords)
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
          assert(df == 2,
            s"expecting DF of FOX sentence to be 2, actual $df")
        } else {
          assert(df == 2,
            s"expecting DF of SANTA sentence to be 2, actual $df")
        }
        s + df
      }
    })
    val expectedDFSum = {
      sentFox.words.size * 2 + // all words in FOX sentence are in 2 documents
        sentSanta.words.size * 2 - // all words in SANTA sentence are in 2 documents
        2 // "the" is double counted, it only has {0,1} DF count per document!
    }
    assert(dfSum == expectedDFSum,
      s"expecting DF sum $expectedDFSum actual $dfSum")
  }

}

class SparkWordcountTest extends FunSuite with LocalSparkContext {

  import mlbigbook.wordcount.WordcountTest._

  lazy val corpusRDD = sc.parallelize(corpus)

  test("[RDD] wordcount corpus") {
    assertCountsL(all, Count.wordcountCorpus(corpusRDD))
  }

}

trait LocalSparkContext extends BeforeAndAfterAll {
  self: Suite =>
  @transient var sc: SparkContext = _

  override def beforeAll() {
    SparkUtil.silenceSpark()
    sc = new SparkContext("local", "test")
    super.beforeAll()
  }

  override def afterAll() {
    if (sc != null) {
      sc.stop()
    }
    System.clearProperty("spark.driver.port")
    super.afterAll()
  }
}

object SparkUtil {
  def silenceSpark() {
    setLogLevels(Level.WARN, Seq("spark", "org.eclipse.jetty", "akka"))
  }

  def setLogLevels(level: org.apache.log4j.Level, loggers: TraversableOnce[String]) = {
    loggers.map {
      loggerName =>
        val logger = Logger.getLogger(loggerName)
        val prevLevel = logger.getLevel
        logger.setLevel(level)
        loggerName -> prevLevel
    }.toMap
  }

}