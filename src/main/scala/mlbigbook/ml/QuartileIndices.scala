package mlbigbook.ml

case class QuartileIndices(
  q1Index:     Long,
  medianIndex: Long,
  q3Index:     Long,
  maxIndex:    Long
)

object QuartileIndices {

  def from(nElements: Long): QuartileIndices = {
    val q1Index: Long = nElements / 4l // integer division OK
    val medianIndex: Long = nElements / 2l // integer division OK
    val q3Index: Long = 3l * q1Index // integer division OK
    val maxIndex: Long = nElements - 1l
    QuartileIndices(q1Index, medianIndex, q3Index, maxIndex)
  }
}