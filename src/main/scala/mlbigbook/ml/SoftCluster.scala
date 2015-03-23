package mlbigbook.ml

trait SoftCluster[T] extends (T => IndexedSeq[(Center, Double)])

object SoftCluster {

  def apply[T](d: Distance)(vcents: VectorizedCenters[T]): SoftCluster[T] =
    (input: T) => {
      val vecInput = vcents.v(input)
      vcents.centers.map(center => (center, d(center.mean, vecInput)))
    }

  implicit def fn2soft[T](f: (T => IndexedSeq[(Center, Double)])): SoftCluster[T] =
    new SoftCluster[T] {
      override def apply(x: T): IndexedSeq[(Center, Double)] = f(x)
    }

}