package mlbigbook.ml

import mlbigbook.data._

import scala.reflect.ClassTag

object BadNaiveBayes {

  // count features
  // incorporate smoothing
  // for each vector, v:
  //    c = v.class()                                             assert(shouldBeNeg.)
  //    for each feature, f:
  //      if v(f) is nonzero:
  //        featurevalu
  // by class
  //    classcount[c] += 1
  //
  // likelihood[c][f] = LOG ( featurevaluecount[c][f] / sum c' { featurevalucecount[c'][f] } )
  // posterior[c] = LOG ( classcount[c] / sum c' { classcount[c'] } )
  //
  // for new one:
  //    for each feature *f* in new one:
  //      s += likelihood[c'][*f*]
  //    posterior[c'] + s

  def apply[T: ClassTag](smoothing: Smoothing.Fn)(
    ls: Labels,
    data: DistData[LabeledData[T]]): ProbabilityEstimator[T] = {

    val (labelCount, featureValCount) =
      data.aggregate((Map.empty[String, Long], Map.empty[String, Map[T, Double]]))(
        {
          case ((lc, fvc), ld) =>


            val updatedLc =
              lc.get(ld.label) match {

                case Some(existing) =>
                  (lc - ld.label) + (ld.label -> (existing + 1L))

                case None =>
                  lc + (ld.label -> 1L)
              }

            val updatedFvc =
              fvc.get(ld.label) match {

                case Some(featureCountsForL) =>

                  val x = featureCountsForL.get(ld.example) match {
                    case Some(v) =>
                      (featureCountsForL - ld.example) + (ld.example -> (v+1.0))
                    case None =>
                      featureCountsForL + (ld.example -> 1.0)
                  }

                  (fvc - ld.label) + (ld.label -> x)

                case None =>
                  fvc + (ld.label -> Map(ld.example -> 1.0))
              }

            (updatedLc, updatedFvc)
        },
        {
          case ((lc1, fvc1), (lc2, fvc2)) =>
            (
              lc1.foldLeft(lc2) {
                case (m, (label, value)) =>
                  m.get(label) match {

                    case Some(existing) =>
                      (m - label) + (label -> (existing + value))

                    case None =>
                      m + (label -> value)

                  }
              },

              fvc1.foldLeft(fvc2)({
                case (m, (label, fvsForLabel)) =>
                  val update =
                    fvsForLabel
                      .foldLeft(m.getOrElse(label, Map.empty[Int, Double])) {
                        case (m2, (index, value)) =>
                          m2.get(index) match {

                            case Some(existing) =>
                              (m2 - index) + (index -> (existing + value))

                            case None =>
                              m2 + (index -> value)
                          }
                      }

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

    val logPrior: Map[Labeled, Double] = {
      val classCountSum =
        labelCount.foldLeft(0.0)({
          case (s, (_, v)) => s + v
        })
      labelCount
        .foldLeft(Map.empty[Labeled, Double]) {
          case (p, (labeled, v)) =>
            p + (
              labeled ->
              (if (classCountSum == 0 || v == 0) 0.0 else math.log(v / classCountSum))
            )
        }
    }

    val logLabelPriorsInOrder: Seq[(Labeled, Double)] =
      ls.labels
        .map(label => (label, logPrior(label)))

    val logLikelihood: Map[Labeled, Map[Int, Double]] = {

      val classFeatureValSums =
        featureValCount
          .foldLeft(Map.empty[Labeled, Double]) {
            case (m, (label, featureValuesForLabel)) =>
              m + (label -> featureValuesForLabel.values.sum)
          }

      featureValCount
        .foldLeft(Map.empty[Labeled, Map[Int, Double]]) {
          case (logLike, (label, featureValuesForLabel)) =>
            val sumForLabel = classFeatureValSums(label)
            val x =
              featureValuesForLabel
                .foldLeft(Map.empty[Int, Double]) {
                  case (m, (index, value)) =>
                    m + (index -> {
                      val temp = math.log(value / sumForLabel)
                      if (java.lang.Double.isNaN(temp) || java.lang.Double.isInfinite(temp))
                        0.0
                      else
                        temp
                    })
                }
            logLike + (label -> x)
        }
    }

    val mkDist = Distribution.make(ls)

    (input: T) => {
      val vecInput = vectorizer(UnlabeledData(input))

      val logPosteriors =
        logLabelPriorsInOrder
          .map {
            case (label, logLabelPrior) =>
              val fvsForLabel = logLikelihood(label)
              vecInput
                .nonZeros
                .foldLeft(logLabelPrior) {
                  case (s, (index, inputValue)) =>
                    s + fvsForLabel.getOrElse(index, math.log(smoothing())) * inputValue
                }
          }

      mkDist(normalize(logPosteriors))
        .getOrElse(throw new IllegalStateException("unexpected "))
    }
  }

  @inline def normalize(vs: Seq[Double]): Seq[Double] = {
    val s: Double = vs.sum
    vs.map(v => v / s)
  }
}






