package mlbigbook.ml

sealed abstract class FeatureSpace {

  val features: Seq[String]
  val isCategorical: Seq[Boolean]
  val feat2index: Map[String, Int]
  val categorical2values: Map[String, Seq[String]]

  lazy val size = features.size

  lazy val (realIndices, catIndices) =
    isCategorical
      .zipWithIndex
      .foldLeft((Seq.empty[Int], Seq.empty[Int])) {
        case ((ri, ci), (isCategory, index)) =>
          if (isCategory)
            (ri, ci :+ index)
          else
            (ri :+ index, ci)
      }

  lazy val (realFeatNames, catFeatNames) = (
    realIndices.map { index => features(index) },
    catIndices.map { index => features(index) }
  )

  /*

     [PEDAGOGICAL NOTES for book writing]

     Show first:

     Example of doing this for one case:

      val realIndices =
       fs.isCategorical
         .zipWithIndex
         .flatMap {
           case (isCategory, index) =>
             if (isCategory)
               None
             else
               Some(index)
         }


     // analogus case

     val catIndices =
       fs.isCategorical
         .zipWithIndex
         .flatMap {
           case (isCategory, index) =>
             if(isCategory)
               Some(index)
             else
               None
         }

     // we're reapeating ourselves!
     // maybe put into a function and apply twice?
     // ...
     // let's give that a few seconds' thought
     // ...
     // are we really going to _reuse_ this function?
     // no
     // it's also not as efficient: we're going through the feature space twice
     // what if we fold our way through the space, accumulating both sequences?
     //
     // <enter actual solution>

  */

}

case class RealFeatureSpace(override val features: Seq[String])
    extends FeatureSpace {

  override val isCategorical = Seq.fill(features.size)(false)
  override val feat2index = features.zipWithIndex.toMap
  override val categorical2values = Map.empty[String, Seq[String]]
}

object RealFeatureSpace {
  val empty = RealFeatureSpace(features = Seq.empty[String])
}

case class CategoricalFeatureSpace(
    override val features:           Seq[String],
    override val categorical2values: Map[String, Seq[String]]
) extends FeatureSpace {

  override val isCategorical = Seq.fill(features.size)(true)
  override val feat2index = features.zipWithIndex.toMap
}

object CategoricalFeatureSpace {
  val empty = CategoricalFeatureSpace(
    features = Seq.empty[String],
    categorical2values = Map.empty[String, Seq[String]]
  )
}

case class MixedFeatureSpace(
  override val features:           Seq[String],
  override val isCategorical:      Seq[Boolean],
  override val feat2index:         Map[String, Int],
  override val categorical2values: Map[String, Seq[String]]
) extends FeatureSpace

object MixedFeatureSpace {
  val empty = new MixedFeatureSpace(
    features = Seq.empty[String],
    isCategorical = Seq.empty[Boolean],
    feat2index = Map.empty[String, Int],
    categorical2values = Map.empty[String, Seq[String]]
  )
}

object FeatureSpace {
  val empty: FeatureSpace = MixedFeatureSpace.empty
  val emptyCategorical: CategoricalFeatureSpace = CategoricalFeatureSpace.empty
  val emptyReal: RealFeatureSpace = RealFeatureSpace.empty
}