package mlbigbook.data

sealed trait VectorDataIn[T] extends (() => (Vectorizer[T], Data[(T, Vector)]))

case class NeedsApplicationVDIn[T](mkVec: VectorizerMaker[T], data: Data[T]) extends VectorDataIn[T] {
  override def apply() = {
    val vectorizer = mkVec(data)
    (vectorizer, data.map(d => (d, vectorizer(d))))
  }
}

case class PreComputedVDIn[T](v: Vectorizer[T], data: Data[(T, Vector)]) extends VectorDataIn[T] {
  override val apply = (v, data)
}

object VectorDataIn {

  implicit def computeNeedy[T](needy: NeedsApplicationVDIn[T]): PreComputedVDIn[T] = {
    val (vectorizer, vectors) = needy()
    PreComputedVDIn(vectorizer, vectors)
  }
}

