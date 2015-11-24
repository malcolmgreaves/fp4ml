package mlbigbook.math

import mlbigbook.data.DataClass
import mlbigbook.wordcount.LocalSparkContext
import org.scalatest.{ FunSpec, Matchers }

import scala.util.Random

/**
 * Tests for Welford's online variance cacluation.
 *
 * @author Marek Kolodziej
 * @author Malcolm Greaves
 */
class FeatureScalingTest extends FunSpec with Matchers with LocalSparkContext {

  lazy val rand = new Random(42L)
  lazy val tolerance = 1e-6

  object DataVarianceLargeNumber {
    lazy val numEl = 1e6.toInt
    lazy val localData = Seq.fill(numEl)(rand.nextDouble() + 1e9)
    lazy val parData = sc.parallelize(localData).repartition(4)
    lazy val localEst = FeatureScaling.welfordsMethodVariance(localData)
    lazy val sparkEst = FeatureScaling.welfordsMethodVariance(parData)
    lazy val expected = 0.083382083
  }

  object DataVarianceSmall {
    lazy val numEl = 3
    lazy val data: DataClass[Double] = Seq(-1.0, 12.0, 55.0)
    lazy val estimate = FeatureScaling.welfordsMethodVariance(data)
    lazy val expected = 859.0
  }

  describe("Welford's Online Variance, Large Number") {
    import DataVarianceLargeNumber._
    it("should get a correct variance estimate for Scala collections") {
      localEst should be(expected +- tolerance)
    }
    it("should get a correct variance estimate for Spark RDDs") {
      sparkEst should be(expected +- tolerance)
    }
    it("local Scala collections and Spark estimates should be close to each other") {
      sparkEst should be(localEst +- tolerance)
    }
  }

  describe("Welford's Online Variance, Small Data") {
    import DataVarianceSmall._
    it("should be equal to naive calculation") {
      estimate should be(expected +- tolerance)
    }
  }

}