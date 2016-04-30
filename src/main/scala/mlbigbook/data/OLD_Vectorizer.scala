/*
 * Type definitions for things that can create vectors from objects (Vectorizer) and
 * thing that can create a Vectorizer, if given a data source (VectorizerMaker).
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

/** Type that represents something that can convert an arbitrary instance of T into a Vector. */
trait OLD_Vectorizer[T] extends (T => OLD_Vector)

object OLD_Vectorizer {

  implicit class Fn[T](val f: T => OLD_Vector) extends OLD_Vectorizer[T] {
    override def apply(x: T) = f(x)
  }
}

/** Type that can construct a vectorizer, if given a distributed data source. */
trait OLD_VectorizerMaker[T] extends (DataClass[T] => OLD_Vectorizer[T])

object OLD_VectorizerMaker {

  implicit class Fn[T](val f: DataClass[T] => OLD_Vectorizer[T]) extends OLD_VectorizerMaker[T] {
    override def apply(x: DataClass[T]) = f(x)
  }
}