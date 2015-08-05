package mlbigbook.ml

import mlbigbook.optimization.OptimAlgos
import mlbigbook.util.FeatureScaling
import mlbigbook.wordcount.LocalSparkContext
import org.scalatest.{ Matchers, FunSpec }
import scala.util.Random

/**
 * @author Marek Kolodziej
 */
class FeatureScalingTest extends FunSpec with Matchers with LocalSparkContext {

  lazy val rand = new Random(42L)
  lazy val numEl = 1e6.toInt
  lazy val localData = Seq.fill(numEl)(rand.nextDouble() + 1e9)
  lazy val parData = sc.parallelize(localData).repartition(4)
  lazy val localEst = FeatureScaling.welfordsMethodVariance(localData)
  lazy val sparkEst = FeatureScaling.welfordsMethodVariance(parData)
  lazy val expected = 0.083382083
  lazy val tolerance = 1e-6

  describe("Welford's online variance calculation") {

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

}