package mlbigbook.data

sealed trait VectorDataIn[T] extends (() => (OLD_Vectorizer[T], DataClass[(T, OLD_Vector)]))

case class NeedsApplicationVDIn[T](mkVec: OLD_VectorizerMaker[T], data: DataClass[T]) extends VectorDataIn[T] {
  override def apply() = {
    val vectorizer = mkVec(data)
    (vectorizer, data.map(d => (d, vectorizer(d))))
  }
}

case class PreComputedVDIn[T](v: OLD_Vectorizer[T], data: DataClass[(T, OLD_Vector)]) extends VectorDataIn[T] {
  override val apply = (v, data)
}

object VectorDataIn {

  implicit def computeNeedy[T](needy: NeedsApplicationVDIn[T]): PreComputedVDIn[T] = {
    val (vectorizer, vectors) = needy()
    PreComputedVDIn(vectorizer, vectors)
  }
}

