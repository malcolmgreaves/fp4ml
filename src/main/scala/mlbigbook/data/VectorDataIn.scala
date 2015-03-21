package mlbigbook.data

sealed trait VectorDataIn[T] extends (() => (Vectorizer[T], DistData[(T, Vector)]))

case class NeedsApplicationVDIn[T](mkVec: VectorizerMaker[T], data: DistData[T]) extends VectorDataIn[T] {
  override def apply() = {
    val vectorizer = mkVec(data)
    (vectorizer, data.map(d => (d, vectorizer(d))))
  }
}

case class PreComputedVDIn[T](v: Vectorizer[T], data: DistData[(T, Vector)]) extends VectorDataIn[T] {
  override val apply = (v, data)
}

object VectorDataIn {

  implicit def computeNeedy[T](needy: NeedsApplicationVDIn[T]): PreComputedVDIn[T] = {
    val (vectorizer, vectors) = needy()
    PreComputedVDIn(vectorizer, vectors)
  }
}

