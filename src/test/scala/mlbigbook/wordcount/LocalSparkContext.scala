package mlbigbook.wordcount

import org.apache.spark.{ SparkConf, SparkContext }
import org.scalatest.{ BeforeAndAfterAll, Suite }

trait LocalSparkContext extends BeforeAndAfterAll with Serializable {
  self: Suite =>

  @transient var sc: SparkContext = _

  override def beforeAll() = {
    sc = new SparkContext(
      new SparkConf()
        .setMaster("local[2]")
        .setAppName("unittests")
        .set("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
    )
    super.beforeAll()
  }

  override def afterAll() = {
    if (sc != null)
      sc.stop()
    System.clearProperty("spark.driver.port")
    super.afterAll()
  }
}