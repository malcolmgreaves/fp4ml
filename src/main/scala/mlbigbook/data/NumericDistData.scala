/*
 * Definition of the distributed data trait DistData.
 * DistData defines methods for transforming and manipulating data of any size
 * in a purely functional manner.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

object NumericDistData {

  object Implicits {

    implicit class IsNumericDistData[N: Numeric](d: DistData[N]) extends NumericDistData[N] {

      override def sum()(implicit ev: N => Double) =
        d.reduce[N] {
          case (a, b) =>
            implicitly[Numeric[N]].plus(a, b)
        }
    }

  }

}

trait NumericDistData[A] {
  def sum()(implicit ev: A => Double): Double //N forSome { type N = Numeric[A] }
}

