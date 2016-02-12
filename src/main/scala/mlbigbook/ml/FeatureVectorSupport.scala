package mlbigbook.ml

object FeatureVectorSupport {

  sealed trait Value

  case class Categorical(v: String) extends Value

  case class Real(v: Double) extends Value

  type FeatVec = Seq[Value]

}