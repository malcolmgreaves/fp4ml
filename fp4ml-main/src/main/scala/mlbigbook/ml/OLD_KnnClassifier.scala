//package mlbigbook.ml
//
//import mlbigbook.data._
//
//import scala.reflect.ClassTag
//
//object KnnClassifier {
//
//  /**
//   * Creates a k-Nearest Neighbors classifier.
//   *
//   * Uses NnRanker.apply underneath to perform the nearest neighbors search.
//   */
//  def apply[T: ClassTag](n: NearNeighIn)(vdata: VectorDataIn[LabeledData[T]]): Learning[T, Labeled]#Classifier =
//    apply(NnRanker(n)(vdata))
//
//  def apply[T: ClassTag](nearestNeighborsRanker: Ranker[LabeledData[T]]): Learning[T, Labeled]#Classifier =
//    (input: T) => {
//      val neighborhood =
//        nearestNeighborsRanker(UnlabeledData(input))
//          .map(_._1.label)
//
//      Labeled(takeLargest(countVotes(neighborhood)))
//    }
//
//  /**
//   * Counts the number of times each element occurs in neighborhood.
//   * Returns this information as a mapping.
//   */
//  def countVotes(neighborhood: Traversable[String]): Map[String, Int] =
//    neighborhood.foldLeft(Map.empty[String, Int])(
//      (m, label) =>
//        if (m.contains(label)) {
//          val newCount = m(label) + 1
//          (m - label) + (label -> newCount)
//        } else {
//          m + (label -> 1)
//        }
//    )
//
//  /**
//   * Evaluates to the String associated with the largest value (of Numeric type N). If the input
//   * elements is empty, evaluates to the empty string ("").
//   */
//  @inline def takeLargest[N](elements: Map[String, N])(implicit n: Fractional[N]): String =
//    takeLargest(elements.toIndexedSeq)
//
//  /**
//   * Evaluates to the String associated with the largest value (of Numeric type N). If the input
//   * elements is empty, evaluates to the empty string ("").
//   *
//   */
//  def takeLargest[N](elements: IndexedSeq[(String, N)])(implicit n: Fractional[N]): String =
//    elements.size match {
//
//      case 0 =>
//        ""
//
//      case 1 =>
//        elements.head._1
//
//      case _ =>
//        elements.slice(1, elements.size)
//          .foldLeft(elements.head)({
//            case ((maxLabel, maxValue), (label, value)) =>
//              if (n.gt(value, maxValue))
//                (label, value)
//              else
//                (maxLabel, maxValue)
//          })._1
//
//    }
//
//}

// RANKING

///**
//  * Evaluates to a Traversable containing the elements that have the largest associated values in the input. The
//  * returned Traversable has at most limit items.
//  */
//def takeTopK[T, N](limit: Int, elements: DataClass[(T, N)])(
//  implicit
//  n: Fractional[N], c: ClassTag[N]
//): Traversable[(T, N)] =
//  elements
//    .sortBy(_._2)(c, n.reverse)
//    .take(limit)