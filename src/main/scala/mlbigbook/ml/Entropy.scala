package mlbigbook.ml

import fif.Data
import mlbigbook.data.AddMap

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.ClassTag

object Entropy {

  import Data.ops._

  def apply[D[_]: Data, Label: Equality: ClassTag, Ignore](
    data: D[(Ignore, Label)]
  ): Double =
    apply(data.map { case (_, label) => label })

  def apply[D[_]: Data, Label: Equality](data: D[Label]): Double = {

    val am = AddMap[Label, Long]

    val label2count =
      data.aggregate(EqualityMap.empty[Label, Long])(
        { case (map, label) => am.add(map, label, 1) },
        { case (map1, map2) => am.combine(map1, map2) }
      )

    val total = data.size.toDouble

    label2count
      .foldLeft(0.0) {
        case (sum, (label, count)) =>
          val probability = count / total
          val info = probability * Information.log2(probability)
          sum - info
      }
  }

}