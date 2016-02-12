package mlbigbook.ml

import breeze.linalg.{ QuasiTensor, DenseVector }
import mlbigbook.math.{ NumericConversion, VectorOpsT }
import org.scalatest.FunSuite

class DiscretizationTest extends FunSuite {

  import DiscretizationTest._
  import fif.ImplicitCollectionsData._
  import VectorOpsT.Implicits._
  import NumericConversion.Implicits._
  import MathOps.Implicits._

  test("testing that IQR computation is correct") {
    val newFiveNumSums = InterQuartileRange(dataForDiscretization)
    assert(newFiveNumSums.size === 3)
    assert(newFiveNumSums.head === dim0_expectedFiveNumSum)
    assert(newFiveNumSums(1) === dim1_expectedFiveNumSum)
    assert(newFiveNumSums.last === dim2_expectedFiveNumSum)

    (0 until 3)
      .foreach { index =>
        checkDataWithIqr(
          newFiveNumSums(index),
          dataForDiscretization.map((v: QuasiTensor[Int, Int]) => v(index))
        )
      }
  }

  def checkDataWithIqr(fns: FiveNumSummary[Int], data: Seq[Int]) = {

    val (belowMin, minQ1, q1Median, medianQ2, q2Max, aboveMax) =
      data
        .foldLeft((0, 0, 0, 0, 0, 0)) {
          case (counts @ (nBMin, nMinQ1, nQ1Median, nMedianQ2, nQ2Max, nAMax), value) =>
            if (value < fns.min)
              counts.copy(_1 = nBMin + 1)
            else if (value < fns.q1)
              counts.copy(_2 = nMinQ1 + 1)
            else if (value < fns.median)
              counts.copy(_3 = nQ1Median + 1)
            else if (value < fns.q2)
              counts.copy(_4 = nMedianQ2 + 1)
            else if (value < fns.max)
              counts.copy(_5 = nQ2Max + 1)
            else
              counts.copy(_6 = nAMax + 1)
        }

    val expected = data.size / 4

    assert(belowMin === 0, ": below min wrong")
    assert(minQ1 === expected, ": min-q1 wrong")
    assert(q1Median === expected, ": q1-median wrong")
    assert(medianQ2 === expected, ": median-q2 wrong")
    assert(q2Max === expected, ": q2-max wrong")
    assert(aboveMax === data.size / 100, ": above or equal to max wrong")

  }

  test("Testing IQR based discretization") {

    val (newData, newFs) = {
      implicit val _ = oldFs
      Discretization(dataForDiscretization, IqrDiscretization)
    }

    assert(newFs.isCategorical.forall(identity))
    assert(newFs.categorical2values.keys.toSet === newFs.features.toSet)

    val newDiscretizedFeatureValues =
      newFs.categorical2values.toSeq
        .map {
          case (featureName, newValues) =>
            (newFs.feat2index(featureName), newValues)
        }
        .sortBy { case (featIndex, _) => featIndex }
        .map { case (_, newValues) => newValues }

    assert(newData.size === dataForDiscretization.size)
    newData.foreach { values =>
      assert(values.size == newDiscretizedFeatureValues.size)
    }
    verifyNewDiscretizedValue(newDiscretizedFeatureValues)
    verifyData(newData)
  }

  def verifyNewDiscretizedValue(newValuesPerFeature: Seq[Seq[String]]) =
    newValuesPerFeature
      .zipWithIndex
      .foreach {
        case (grouped, index) =>
          assert(grouped.head === s"${IqrDiscretization.below_min}--dimension_$index")
          assert(grouped(1) === s"${IqrDiscretization.min_q1}--dimension_$index")
          assert(grouped(2) === s"${IqrDiscretization.q1_median}--dimension_$index")
          assert(grouped(3) === s"${IqrDiscretization.median_q2}--dimension_$index")
          assert(grouped(4) === s"${IqrDiscretization.q2_max}--dimension_$index")
          assert(grouped.last === s"${IqrDiscretization.above_or_equal_to_max}--dimension_$index")
      }

  // check data
  def verifyData(data: Seq[Seq[String]]) = {

    val mult = data.head.size
    val expected = (data.size / 4) * mult

    val (belowMin, minQ1, q1Median, medianQ2, q2Max, aboveMax) =
      data
        .foldLeft((0, 0, 0, 0, 0, 0)) {
          case (c, values) =>
            values.foldLeft(c) {
              case (counts @ (nBMin, nMinQ1, nQ1Median, nMedianQ2, nQ2Max, nAMax), value) =>
                if (value.startsWith(IqrDiscretization.below_min))
                  counts.copy(_1 = nBMin + 1)
                else if (value.startsWith(IqrDiscretization.min_q1))
                  counts.copy(_2 = nMinQ1 + 1)
                else if (value.startsWith(IqrDiscretization.q1_median))
                  counts.copy(_3 = nQ1Median + 1)
                else if (value.startsWith(IqrDiscretization.median_q2))
                  counts.copy(_4 = nMedianQ2 + 1)
                else if (value.startsWith(IqrDiscretization.q2_max))
                  counts.copy(_5 = nQ2Max + 1)
                else
                  counts.copy(_6 = nAMax + 1)
            }
        }

    assert(belowMin === 0, s": ${IqrDiscretization.below_min} wrong")
    assert(minQ1 === expected, s": ${IqrDiscretization.min_q1}  wrong")
    assert(q1Median === expected, s": ${IqrDiscretization.q1_median}  wrong")
    assert(medianQ2 === expected, s": ${IqrDiscretization.median_q2}  wrong")
    assert(q2Max === expected, s": ${IqrDiscretization.q2_max}  wrong")
    assert(aboveMax === (data.size / 100) * mult, s": ${IqrDiscretization.above_or_equal_to_max}  wrong")
  }

}

object DiscretizationTest {

  val dim0_expectedFiveNumSum = FiveNumSummary(
    min = -50,
    q1 = -25,
    median = 0,
    q2 = 25,
    max = 50
  )

  val dim1_expectedFiveNumSum = FiveNumSummary(
    min = 0,
    q1 = 25,
    median = 50,
    q2 = 75,
    max = 100
  )

  val dim2_expectedFiveNumSum = FiveNumSummary(
    min = 0,
    q1 = 250,
    median = 500,
    q2 = 750,
    max = 1000
  )

  val dataForDiscretization: Seq[DenseVector[Int]] = {
    (0 to 100)
      .flatMap { value =>
        Seq(
          DenseVector(value - 50, value, value * 10),
          DenseVector(value - 50, value, value * 10),
          DenseVector(value - 50, value, value * 10)
        )
      }
      .map { vector => (vector, math.random) }
      .sortBy { _._2 }
      .map { _._1 }
      .toSeq
  }

  val oldFs = FeatureSpace.allReal(
    Seq("dimension_0", "dimension_1", "dimension_2")
  )

}

