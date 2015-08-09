package mlbigbook.ml

import breeze.linalg.DenseVector
import mlbigbook.util.{VectorOpsT, OnlineMeanVariance, FeatureScaling}
import mlbigbook.wordcount.LocalSparkContext
import org.scalatest.{FunSpec, Matchers}

import scala.util.Random

/**
 * @author Marek Kolodziej
 * @author Malcolm Greaves
 */
class OnlineMeanVarianceTest extends FunSpec with Matchers {
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

  import mlbigbook.util.NumericConversion.Implicits._


  ignore("Welford's online variance calculation using generalized vectors"){

    it("should work") {

      import mlbigbook.data.Data
      import Data._

//      val data: Data[DenseVector[Double]] = Seq(DenseVector(12.0), DenseVector(-1.0), DenseVector(55.0))
      // mean:     20
      // variance: 20


      lazy val rand = new Random(42L)
      lazy val numEl = 1e6.toInt
      lazy val data: Data[DenseVector[Double]] = Seq.fill(numEl)(DenseVector(rand.nextDouble() + 1e9))
      lazy val expected = 0.083382083
      lazy val tolerance = 1e-6



      import VectorOpsT._

      OnlineMeanVariance(data) match {
        case s @ OnlineMeanVariance.Stats(count, mean, variance) =>
          println(s"STATS: $s")
          assert(count == numEl)
          variance(0) should be(expected +- tolerance)
//          assert(mean(0))
//          assert(count == 3l)
//          assert(mean(0) == 2.0)
//          assert(variance(0) == 2.0)
      }

    }

  }

}