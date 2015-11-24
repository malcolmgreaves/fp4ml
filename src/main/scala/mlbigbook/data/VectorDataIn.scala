package mlbigbook.data

sealed trait VectorDataIn[T] extends (() => (Vectorizer[T], DataClass[(T, OldVector)]))

case class NeedsApplicationVDIn[T](mkVec: VectorizerMaker[T], data: DataClass[T]) extends VectorDataIn[T] {
  override def apply() = {
    val vectorizer = mkVec(data)
    (vectorizer, data.map(d => (d, vectorizer(d))))
  }
}

case class PreComputedVDIn[T](v: Vectorizer[T], data: DataClass[(T, OldVector)]) extends VectorDataIn[T] {
  override val apply = (v, data)
}

object VectorDataIn {

  implicit def computeNeedy[T](needy: NeedsApplicationVDIn[T]): PreComputedVDIn[T] = {
    val (vectorizer, vectors) = needy()
    PreComputedVDIn(vectorizer, vectors)
  }
}

