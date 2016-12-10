package mlbigbook

import scala.reflect.ClassTag

package object util {

  @inline
  def copyToSeq[@specialized A: ClassTag](src: Array[A]): Seq[A] =
    if (src == null || src.isEmpty)
      Seq.empty[A]
    else {
      val s = new Array[A](src.length)
      System.arraycopy(src, 0, s, 0, src.length)
      s.toSeq
    }

}