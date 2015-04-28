package mlbigbook.ml

import mlbigbook.data._

import scala.reflect.ClassTag

trait Smoothing {
  def apply(): Double
}

// values.map(_._2).sum == 1
sealed trait Distribution {
  def labelSet: LabelSet
  def values: Seq[Double]
}

case class BinaryDist(override val labelSet: BinaryLS, yesValue: Double) extends Distribution {
  lazy override val values = Seq(1.0 - yesValue, yesValue)
  lazy val noValue = values.head
}

case class MultiDist(override val labelSet: MultiLS, override val values: Seq[Double]) extends Distribution

object NaiveBayes {

  // count features
  // incorporate smoothing
  // for each vector, v:
  //    c = v.class()
  //    for each feature, f:
  //      if v(f) is nonzero:
  //        featurevaluecount[c][f] += v(f) // by class
  //    classcount[c] += 1
  //
  // likelihood[c][f] = LOG ( featurevaluecount[c][f] / sum c' { featurevalucecount[c'][f] } )
  // posterior[c] = LOG ( classcount[c] / sum c' { classcount[c'] } )
  //
  // for new one:
  //    for each feature *f* in new one:
  //      s += likelihood[c'][*f*]
  //    posterior[c'] + s

  def apply[T: ClassTag](ls: LabelSet, smoothing: Smoothing)(vdata: VectorDataIn[LabeledData[T]]): ProbabilityEstimator[T] = {

    val mkDist = ls match {
      case bls: BinaryLS =>
        (vs: Seq[Double]) => BinaryDist(bls, vs.head)

        case mls: MultiLS =>
        (vs: Seq[Double]) => MultiDist(mls, vs)
    }

    val (vectorizer, data) = vdata()

    val (labelC, featureValC) =
      data.aggregate((Map.empty[Labeled, Long], Map.empty[Labeled, Map[Int, Double]]))(
        {
          case ((lc, fvc), (ld, vec)) =>

            val l = Labeled(ld.label)

            val updatedLc = lc.get(l) match {

              case Some(existing) =>
                (lc - l) + (l -> (existing + 1L))

              case None =>
                lc + (l -> 1L)

            }

            val updatedFvc = fvc.get(l) match {

              case Some(featureCountsForL) =>
                val x =
                  vec.nonZeros.foldLeft(featureCountsForL)({
                    case (m, (index, value)) =>
                      m.get(index) match {
                        case Some(v) =>
                          (m - index) + (index -> (v + value))
                        case None =>
                          m + (index -> value)
                      }
                  })
                (fvc - l) + (l -> x)

              case None =>
                fvc + (l -> vec.nonZeros.toMap)
            }

            (updatedLc, updatedFvc)
        },
        {
          case ((lc1, fvc1), (lc2, fvc2)) =>
            (
              lc1.foldLeft(lc2)({
                case (m, (label, value)) =>
                  m.get(label) match {

                    case Some(existing) =>
                      (m - label) + (label -> (existing + value))

                    case None =>
                      m + (label -> value)

                  }
              }),

              fvc1.foldLeft(fvc2)({
                case (m, (label, fvsForLabel)) =>
                  val update =
                    fvsForLabel.foldLeft(m.getOrElse(label, Map.empty[Int, Double]))({
                      case (m2, (index, value)) =>
                        m2.get(index) match {

                          case Some(existing) =>
                            (m2 - index) + (index -> (existing + value))

                          case None =>
                            m2 + (index -> value)
                        }
                    })

                  m.get(label) match {

                    case Some(_) =>
                      (m - label) + (label -> update)

                    case None =>
                      m + (label -> update)
                  }
              })
            )
        }
      )

    val logPosterior = {
      val classCountSum = labelC.foldLeft(0.0)({
        case (s, (_, v)) => s + v
      })
      labelC.foldLeft(Map.empty[Labeled, Double])({
        case (p, (labeled, long)) =>
          p + (labeled -> math.log(long / classCountSum))
      })
    }

    val logPosteriorsInOrder = ls.labels.map(label => (label, logPosterior(label)))

    val logLikelihood = {
      val classFeatureValSums =
        featureValC
          .foldLeft(Map.empty[Labeled, Double])({
            case (m, (label, featureValuesForLabel)) =>
              m + (label -> featureValuesForLabel.values.sum)
          })
      featureValC.foldLeft(Map.empty[Labeled, Map[Int, Double]])({
        case (logLike, (label, featureValuesForLabel)) =>
          val sumForLabel = classFeatureValSums(label)
          val x =
            featureValuesForLabel
              .foldLeft(Map.empty[Int, Double])({
                case (m, (index, value)) =>
                  m + (index -> math.log(value / sumForLabel))
              })
          logLike + (label -> x)
      })
    }

    import ProbabilityEstimator.Fn
    (input: T) => {
      val vecInput = vectorizer(UnlabeledData(input))
      mkDist(
        logPosteriorsInOrder
          .map({
            case (label, labelPosterior) =>
              val fvsForLabel = logLikelihood(label)
              vecInput.nonZeros
                .foldLeft(labelPosterior)({
                  case (s, (index, inputValue)) =>
                    inputValue * fvsForLabel.getOrElse(index, smoothing())
                })
          })
      )
    }
  }
}

trait ProbabilityEstimator[T] extends (T => Distribution)

object ProbabilityEstimator {

  implicit class Fn[T](f: T => Distribution) extends ProbabilityEstimator[T] {
    override def apply(x: T) = f(x)
  }
}

object ProbabilityClassifier {

  import Classifier.Fn

  def apply[T](pe: ProbabilityEstimator[T]): Classifier[T] =
    (input: T) =>
      pe(input) match {

        case MultiDist(MultiLS(labels), values) =>
          val labelsAndEstimates = labels.zip(values)

          labelsAndEstimates.slice(1, labelsAndEstimates.size)
            .foldLeft(labelsAndEstimates.head)({
              case ((maxLabeled, maxEstimate), (nextLabeled, nextEstimate)) =>
                if (nextEstimate > maxEstimate)
                  (nextLabeled, nextEstimate)
                else
                  (maxLabeled, maxEstimate)
            })
            ._1

        case BinaryDist(BinaryLS(no, yes, threshold), yesValue) =>
          if (yesValue > threshold)
            yes
          else
            no
      }
}
