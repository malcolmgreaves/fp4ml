/*
 * Definition of the distributed data trait DistData.
 * DistData defines methods for transforming and manipulating data of any size
 * in a purely functional manner.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

trait NumericDistData[A] {
  def sum[N >: A](implicit num: Numeric[N]): N
  def product[N >: A](implicit num : Numeric[N]): N
}

object NumericDistData {

  object Implicits {

    implicit class IsNumericDistData[A](d: DistData[A]) extends NumericDistData[A] {

      override def sum[N >: A](implicit num: Numeric[N]): N =
        d.reduce[N] { case (a, b) => num.plus(a, b) }


      override def product[N >: A](implicit num: Numeric[N]): N =
        d.reduce[N]{ case (a,b) => num.times(a,b)}
    }

  }

}
