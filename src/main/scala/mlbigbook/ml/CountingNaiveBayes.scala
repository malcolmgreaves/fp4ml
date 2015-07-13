package mlbigbook.ml

import mlbigbook.data._

import scala.reflect.ClassTag

object CountingNaiveBayes {

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



  import mlbigbook.wordcount.NumericMap

  type State[Label, Event] = Map[Label, NumericMap[Long]#M[Event]]

  def empty[Label,Event]: State[Label,Event] =
    Map.empty[Label, NumericMap[Long]#M[Event]]

  def apply[Label: ClassTag, T: ClassTag](nm: NumericMap[Long])(data: DistData[(Label, T)]):State[Label,T] =
    apply(nm, empty)(data)

  trait Vectorizeable[T] {
    def as(x: T):Vector
  }

  def apply[Label : ClassTag, T: ClassTag : Vectorizeable](nm: NumericMap[Long], s: State[Label, T])(data: DistData[(Label, T)]):State[Label, T] =  {
    data
      .aggregate(empty)(
        {
          case (st, (label, instance)) =>
            val v = implicitly[Vectorizeable[T]].as(instance)
            v.nonZeros
              .foldLeft(st) {
                case (s, (featureIndex, value)) =>
                  nm.increment(s, featureIndex, value)
              }
        },
        {}
       )


  }

  def apply[T: ClassTag](smoothing: Smoothing.Fn)(
    ls: Labels,
    vdata: VectorDataIn[LabeledData[T]]): ProbabilityEstimator[T] = {

    val (vectorizer, data) = vdata()

    val (labelCount, featureValCount) =
      data.aggregate((Map.empty[Labeled, Long], Map.empty[Labeled, Map[Int, Double]]))(
        {
          case ((lc, fvc), (ld, vec)) =>

            val l = Labeled(ld.label)

            val updatedLc =
              lc.get(l) match {

                case Some(existing) =>
                  (lc - l) + (l -> (existing + 1L))

                case None =>
                  lc + (l -> 1L)
              }

            val updatedFvc =
              fvc.get(l) match {

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

trait ProbabilityEstimator[T] extends (T => Distribution)

object ProbabilityEstimator {

  implicit class Fn[T](f: T => Distribution) extends ProbabilityEstimator[T] {
    override def apply(x: T) = f(x)
  }
}

object ProbabilityClassifier {

  def apply[T](pe: ProbabilityEstimator[T]): Classifier[T] =
    (input: T) => {
      val dist = pe(input)
      val zipLabelProb = dist.labels.zip(dist.values)

      zipLabelProb.slice(1, zipLabelProb.size)
        .foldLeft(zipLabelProb.head) {
          case (max @ (_, maxP), next @ (_, nextP)) =>
            if (nextP > maxP)
              next
            else
              max
        }._1
    }
}
