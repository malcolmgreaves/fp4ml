package mlbigbook.ml

import mlbigbook.wordcount.LocalSparkContext
import org.scalatest.FunSuite

class NearestNeighborsLSHTest extends FunSuite with LocalSparkContext {

  import NearestNeighborsLSHTest._

  ignore("nearest neighbors addresses, k=3") {
    fail("unimmplemented")
  }

  ignore("LSH modified NN addresses, k=3, nBins=5") {
    fail("unimplemented")
  }

}

object NearestNeighborsLSHTest {

  type Err = Option[String]

  def nnTest[T](nn: Ranker[T], input: T, expected: Traversable[(T, Double)]): Err = {

    val actual = nn(input)

    val errors =
      expected.toSeq.zip(actual.toIndexedSeq).foldLeft(List.empty[String])({
        case (sum, (e, a)) =>
          if (e != a)
            sum :+ s"""Expecting: $e | Actual: $a"""
          else
            sum
      })

    if(errors.nonEmpty)
      Some(s"""Found ${errors.length} differences: ${errors.mkString("\n")}""")
    else
      None
  }

  lazy val nnConfig = NearNeighIn(Manhattan, 3)

  lazy val nLshFuncs = 5

  lazy val lshConfig = LshIn(???, nLshFuncs)

}

