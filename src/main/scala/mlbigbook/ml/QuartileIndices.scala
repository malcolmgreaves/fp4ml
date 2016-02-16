package mlbigbook.ml

case class QuartileIndicies(
  q1Index:     Long,
  medianIndex: Long,
  q3Index:     Long,
  maxIndex:    Long
)

object QuartileIndices {

  def apply(nElements: Long): QuartileIndicies = {
    val q1Index: Long = nElements / 4l // integer division OK
    val medianIndex: Long = nElements / 2l // integer division OK
    val q3Index: Long = 3l * q1Index // integer division OK
    val maxIndex: Long = nElements - 1l
    QuartileIndicies(q1Index, medianIndex, q3Index, maxIndex)
  }

}