package mlbigbook.math

import breeze.linalg.DenseVector
import org.scalatest.{ FunSpec, Matchers }

import mlbigbook.ml.Stats

import scala.util.Random

/**
 * @author Marek Kolodziej
 * @author Malcolm Greaves
 */
class OnlineMeanVarianceTest extends FunSpec with Matchers {

  implicit private[this] val t = fif.TravData

  //
  //  lazy val rand = new Random(42L)
  //  lazy val numEl = 1e6.toInt
  //  lazy val localData = Seq.fill(numEl)(rand.nextDouble() + 1e9)
  //  lazy val parData = sc.parallelize(localData).repartition(4)
  //  lazy val localEst = OnlineMeanVariance(localData)
  //  lazy val sparkEst = OnlineMeanVariance(parData)
  //  lazy val expected = 0.083382083
  //  lazy val tolerance = 1e-6
  //
  //  describe("Welford's online variance calculation") {
  //
  //    it("should get a correct variance estimate for Scala collections") {
  //      localEst should be(expected +- tolerance)
  //    }
  //
  //    it("should get a correct variance estimate for Spark RDDs") {
  //      sparkEst should be(expected +- tolerance)
  //    }
  //
  //    it("local Scala collections and Spark estimates should be close to each other") {
  //      sparkEst should be(localEst +- tolerance)
  //    }
  //  }

  describe("Welford's online variance calculation using generalized vectors") {

    it("should work") {

      val data: Traversable[DenseVector[Double]] = Seq(DenseVector(12.0), DenseVector(-1.0), DenseVector(55.0))

      val expectedStats =
        Stats(
          count = 3l,
          mean = DenseVector(22.0),
          variance = DenseVector(859.0)
        )

      lazy val rand = new Random(42L)
      lazy val numEl = 1e6.toInt
      lazy val tolerance = 1e-6

      import VectorOpsT.Implicits._
      import NumericConversion.Implicits._

      OnlineMeanVariance.batch(data) match {
        case s @ Stats(count, mean, variance) =>
          count should be(expectedStats.count)
          mean(0) should be(expectedStats.mean(0) +- tolerance)
          variance(0) should be(expectedStats.variance(0) +- tolerance)
      }

    }

  }

}