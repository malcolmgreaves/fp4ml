package mlbigbook.wordcount

import mlbigbook.data.IndicatorMap
import org.scalatest.FunSuite

import scala.collection.Map
import scala.util.Random

class DataTest extends FunSuite {

  test("definition of indicator map") {
    val m: Map[String, Long] = Map()
    (0 until 25).foreach(_ => {
      assert(IndicatorMap.add(m, "hello", Random.nextLong()) == IndicatorMap.mark(m, "hello"))
    })
  }
}

