package mlbigbook.ml

import fif.{ SeqData, Data, TravData }

import scala.language.{ higherKinds, postfixOps }

object EntropyBasedTreeLearning {

  import Discretization.DiscretizedVector
  import fif.Data.ops._

  def apply[D[_]: Data](
    dtModule:       DecisionTree.Type[Boolean, Seq[String]],
    data:           D[(DiscretizedVector, Boolean)],
    importantFeats: FeatureImportance.Type[String, Boolean]
  )(implicit fs: CategoricalFeatureSpace): Option[dtModule.Node] =
    if (fs.size > 0 && fs.isCategorical.forall(identity))
      learn(data, fs.features.indices.toSet)(
        implicitly[Data[D]],
        fs,
        dtModule,
        importantFeats
      )
    else
      None

  private[this] def learn[D[_]: Data](
    data:         D[(DiscretizedVector, Boolean)],
    featuresLeft: Set[Int]
  )(
    implicit
    fs:             CategoricalFeatureSpace,
    dtModule:       DecisionTree.Type[Boolean, DiscretizedVector],
    importantFeats: FeatureImportance.Type[String, Boolean]
  ): Option[dtModule.Node] =

    if (data isEmpty)
      None

    else {

      val (nPos, nNeg) =
        data.aggregate((0l, 0l))(
          {
            case ((nP, nN), (_, label)) =>
              if (label)
                (nP + 1l, nN)
              else
                (nP, nN + 1l)
          },
          {
            case ((nP1, nN1), (nP2, nN2)) =>
              (nP1 + nP2, nN1 + nN2)
          }
        )

      val majorityDecision = nPos > nNeg

      if (featuresLeft isEmpty)
        Some(dtModule.Leaf(majorityDecision))

      else {

        (nPos, nNeg) match {

          case (0l, _) =>
            Some(dtModule.Leaf(false))

          case (_, 0l) =>
            Some(dtModule.Leaf(true))

          case (_, _) =>
            {
              implicit val v = TupleVal2[String]
              implicit val sd = SeqData
              Argmax(importantFeats(data))
            }
              .map {
                case (nameOfMostImportantFeat, _) =>
                  makeNodeForFeature(
                    data,
                    featuresLeft,
                    majorityDecision,
                    nameOfMostImportantFeat
                  )
              }
        }
      }
    }

  private[this] def makeNodeForFeature[D[_]: Data](
    data:                    D[(DiscretizedVector, Boolean)],
    featuresLeft:            Set[Int],
    majorityDecision:        Boolean,
    nameOfMostImportantFeat: String
  )(
    implicit
    fs:             CategoricalFeatureSpace,
    dtModule:       DecisionTree.Type[Boolean, DiscretizedVector],
    importantFeats: FeatureImportance.Type[String, Boolean]
  ): dtModule.Node = {

    // partition data according to the discrete values of each
    val distinctValues = fs.categorical2values(nameOfMostImportantFeat)
    val indexOfMinEntropyFeat = fs.feat2index(nameOfMostImportantFeat)
    val newFeaturesLeft = featuresLeft - indexOfMinEntropyFeat

    val partitionedByDistinctValues: Seq[(String, D[(DiscretizedVector, Boolean)])] =
      distinctValues
        .map { distinct =>

          val partitionedForDistinct =
            data.filter {
              case (catFeats, _) =>
                catFeats(indexOfMinEntropyFeat) == distinct
            }

          (distinct, partitionedForDistinct)
        }

    //
    // Final steps to make the parent node:
    //
    // (1) recursively apply learn() to each partition
    // (2) when not none, add as a child
    // (3) if all none, then turn into a leaf w/ decision = majority vote of
    //     data
    // (4) in parent's test, if given a bad feature vector or one whose feature
    //     value maps to a learn() call that resulted in None, then default to
    //     a leaf node with decision = majority vote

    val childrenResults =
      partitionedByDistinctValues
        .map {
          case (_, partitionedForDistinct) =>
            learn(partitionedForDistinct, newFeaturesLeft)
        }
        .zipWithIndex

    val defaultToMajDecision = dtModule.Leaf(majorityDecision)

    if (childrenResults.forall { case (m, _) => m.isEmpty })
      defaultToMajDecision

    else {

      val distinct2child =
        childrenResults
          .collect {
            case (Some(child), index) =>
              (distinctValues(index), child)
          }
          .toMap

      val children = distinct2child.values.toSeq

      dtModule.Parent(
        (fv: DiscretizedVector) => {
          val valueOfMinEntropyFeat = fv(indexOfMinEntropyFeat)
          if (distinct2child.contains(valueOfMinEntropyFeat))
            distinct2child(valueOfMinEntropyFeat)
          else
            defaultToMajDecision
        },
        children
      )
    }
  }

}