package mlbigbook.wordcount

import org.scalatest.FunSuite

class SparkWordcountTest extends FunSuite with LocalSparkContext {

  import mlbigbook.wordcount.WordcountTest._

  import mlbigbook.data.Data._

  test("[RDD] wordcount corpus") {
    val corpusRDD = sc.parallelize(corpus)
    assertCountsL(all, Count.wordcountCorpus(corpusRDD))
  }

}