package mlbigbook.ml

import scala.language.{ higherKinds, postfixOps }

object FeatureVectorSupport {

  sealed trait Value

  case class Categorical(v: String) extends Value

  case class Real(v: Double) extends Value

  type FeatVec = Seq[Value]

  case class FeatureSpace(
      features:           Seq[String],
      isCategorical:      Seq[Boolean],
      feat2index:         Map[String, Int],
      categorical2values: Map[String, Seq[String]]
  ) {

    val size = features.size

    val (realIndices, catIndices) =
      isCategorical
        .zipWithIndex
        .foldLeft((Seq.empty[Int], Seq.empty[Int])) {
          case ((ri, ci), (isCategory, index)) =>
            if (isCategory)
              (ri, ci :+ index)
            else
              (ri :+ index, ci)
        }

    val (realFeatNames, catFeatNames) = (
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

}