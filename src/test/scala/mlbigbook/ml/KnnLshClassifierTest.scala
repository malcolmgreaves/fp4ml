package mlbigbook.ml

import mlbigbook.data.Labeled
import mlbigbook.wordcount.LocalSparkContext
import org.scalatest.FunSuite

class KnnLshClassifierTest extends FunSuite with LocalSparkContext {

  import KnnLshClassifierTest._

  ignore("classify simple addresses") {
    fail("unimplemented")
  }

}

object KnnLshClassifierTest {

  import NearestNeighborsLSHTest._

  def classificationTest[T](c: Learning[T, Labeled]#Classifier, input: T, expected: Labeled): Err = {
    val actual = c(input)
    if (actual.label != expected.label)
      Some(s"Expected and actual labels dont match. Expecting: ${actual.label} . Actual: ${actual.label}")
    else
      None
  }

}

