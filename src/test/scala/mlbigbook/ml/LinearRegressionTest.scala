package mlbigbook.ml

import breeze.linalg.DenseVector
import mlbigbook.data.{ Datum, DataClass, VectorizedData }
import mlbigbook.optimization.Optimizer.optimize
import mlbigbook.optimization.Types.WeightUpdate
import mlbigbook.optimization._
import mlbigbook.util.DataConversions.toVectorizedData
import mlbigbook.wordcount.LocalSparkContext
import org.scalatest._

import scala.util.Random

class LinearRegressionTest extends FunSpec with Matchers with LocalSparkContext {

  val rand = new Random(42L)
  val numExamples = 1000
  val (intercept, slope) = (3.0D, 10.0D)
  val feature = Seq.fill(numExamples)(rand.nextDouble())
  val targets = feature.map(i => intercept + slope * i + rand.nextDouble() / 100)
  val data =
    targets.
      zip(feature).
      map {
        // merge target and feature, add intercept to feature vector
        case (y, x) => Datum(y, DenseVector[Double](1.0D, x))
      }

  // These lazy vals are important due to initialization issues (Spark needs to start up first)
  lazy val rdd: DataClass[VectorizedData] =
    toVectorizedData(data = sc.parallelize(data), numExamplesPerGroup = 10)

  lazy val commonParams = optimize(
    iter = 250,
    seed = 123L,
    initAlpha = 0.3,
    momentum = 0.9,
    gradFn = Gradients.linearRegressionGradient,
    costFn = CostFunctions.linearRegressionCost,
    _: WeightUpdate,
    miniBatchFraction = 0.01,
    weightInitializer = WeightInitializer.gaussianInit,
    data = rdd
  )

  describe("linear regression") {

    val plusMinus = 0.01

    it("should converge correctly using SGD") {

      val sgd = commonParams(OptimAlgos.sgd)
      val lastWeights = sgd.weights.last
      lastWeights(0) should be(intercept +- plusMinus)
      lastWeights(1) should be(slope +- plusMinus)
    }

    it("should converge correctly using Adagrad") {

      val adaGrad = commonParams(OptimAlgos.adaGrad)
      val lastWeights = adaGrad.weights.last
      lastWeights(0) should be(intercept +- plusMinus)
      lastWeights(1) should be(slope +- plusMinus)
    }
  }

}

