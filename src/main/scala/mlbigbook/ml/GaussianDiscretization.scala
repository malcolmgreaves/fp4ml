package mlbigbook.ml

import breeze.linalg.Vector
import fif.Data
import mlbigbook.math.{ NumericConversion, VectorOpsT }

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.ClassTag

object GaussianDiscretization extends RuleProducer {

  val below_neg3_sdev = "below_neg_3_sdev"
  val between_neg3_inclusive_and_neg2_exclusive = "between_neg3_inclusive_and_neg2_exclusive"
  val between_neg2_inclusive_and_neg1_exclusive = "between_neg2_inclusive_and_neg1_exclusive "
  val between_neg1_inclusive_and_mean_exlcusive = "between_neg1_inclusive_and_mean_exlcusive"
  val between_mean_inclusive_and_pos1_exlcusive = "between_mean_inclusive_and_pos1_exlcusive"
  val between_pos1_inclusive_and_pos2_exlcusive = "between_pos1_inclusive_and_pos2_exlcusive"
  val between_pos2_inclusive_and_pos3_exlcusive = "between_pos2_inclusive_and_pos3_exlcusive"
  val above_pos3_sdev = "above_pos3_sdev"

  val gaussianDiscretizedValueBases = Seq(
    below_neg3_sdev,
    between_neg3_inclusive_and_neg2_exclusive,
    between_neg2_inclusive_and_neg1_exclusive,
    between_neg1_inclusive_and_mean_exlcusive,
    between_mean_inclusive_and_pos1_exlcusive,
    between_pos1_inclusive_and_pos2_exlcusive,
    between_pos2_inclusive_and_pos3_exlcusive,
    above_pos3_sdev
  )

  override def apply[D[_]: Data, V[_] <: Vector[_], N: NumericConversion: ClassTag](
    data: D[V[N]]
  )(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ): Seq[Rule[N]] = {


    import Data.ops._
    import NumericConversion.Implicits._
    import MathOps.Implicits._
    NumericConversion[N] match {


      case FloatC  =>
        Gaussian.estimate(data)
          .map(g => gaussianRule(g))

      case DoubleC =>
        implicit val _ = NumericConversion[N].numeric
        Gaussian.estimate(data)
          .map(g => gaussianRule(g))

      case LongC | IntC =>
        implicit val _0 = ClassTag.Double
        implicit val _1: ClassTag[V[Double]] = vops.runtimeClassTag[Double]
        val converted = data.map { vector =>
          vops.map(vector)(NumericConversion[N].numeric.toDouble)
        }
        import MathOps.Implicits.DoubleMo
        Gaussian.estimate(converted)
          .map{ gDbl =>
            val gN = Gaussian[N](
              mean = NumericConversion[N].fromDouble(gDbl.mean),
              variance = NumericConversion[N].fromDouble(gDbl.variance),
              stddev = NumericConversion[N].fromDouble(gDbl.stddev)
            )
            gaussianRule(gN)
          }

      case ShortC | ByteC =>
        // convert to floats
        // convert to doubles
        implicit val _0 = ClassTag.Float
        implicit val _1: ClassTag[V[Float]] = vops.runtimeClassTag[Float]
        val converted = data.map { vector =>
          vops.map(vector)(NumericConversion[N].numeric.toFloat)
        }
        Gaussian.estimate(converted)
          .map { gFlt =>
            val gN = Gaussian[N](
              mean = NumericConversion[N].fromDouble(gFlt.mean),
              variance = NumericConversion[N].fromDouble(gFlt.variance),
              stddev = NumericConversion[N].fromDouble(gFlt.stddev)
            )
            gaussianRule(gN)
          }
    }
  }

  def gaussianRule[N: Numeric: MathOps](g: Gaussian[N]): Rule[N] = new Rule[N] {

    val num = implicitly[Numeric[N]]

    val lessThan = num.lt _

    // pre-calculate the values of 2, 3 times the standard deviation
    val sdev2 = num.times(g.stddev, num.plus(num.one, num.one))
    val sdev3 = num.times(sdev2, g.stddev)

    // pre-calculate the threshold points for 1, 2, 3 times the
    // standard deviation minus the mean
    val neg1 = num.minus(g.mean, g.stddev)
    val neg2 = num.minus(g.mean, sdev2)
    val neg3 = num.minus(g.mean, sdev3)

    // pre-calculate the threshold points for 1, 2, 3 times the
    // standard deviation plus the mean
    val pos1 = num.plus(g.mean, g.stddev)
    val pos2 = num.plus(g.mean, sdev2)
    val pos3 = num.plus(g.mean, sdev3)

    override def apply(value: N): String =
      //
      // ORDERING OF if STATEMENTS IS CRITICAL!
      //
      if (lessThan(value, neg3)) below_neg3_sdev
      else if (lessThan(value, neg2)) between_neg3_inclusive_and_neg2_exclusive
      else if (lessThan(value, neg1)) between_neg2_inclusive_and_neg1_exclusive
      else if (lessThan(value, g.mean)) between_neg1_inclusive_and_mean_exlcusive
      else if (lessThan(value, pos1)) between_mean_inclusive_and_pos1_exlcusive
      else if (lessThan(value, pos2)) between_pos1_inclusive_and_pos2_exlcusive
      else if (lessThan(value, pos3)) between_pos2_inclusive_and_pos3_exlcusive
      else above_pos3_sdev

    override val discretizedValueBases =
      gaussianDiscretizedValueBases
  }
}