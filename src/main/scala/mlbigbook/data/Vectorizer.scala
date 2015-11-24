/*
 * Type definitions for things that can create vectors from objects (Vectorizer) and
 * thing that can create a Vectorizer, if given a data source (VectorizerMaker).
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

/** Type that represents something that can convert an arbitrary instance of T into a Vector. */
trait Vectorizer[T] extends (T => OldVector)

object Vectorizer {

  implicit class Fn[T](val f: T => OldVector) extends Vectorizer[T] {
    override def apply(x: T) = f(x)
  }
}

/** Type that can construct a vectorizer, if given a distributed data source. */
trait VectorizerMaker[T] extends (DataClass[T] => Vectorizer[T])

object VectorizerMaker {

  implicit class Fn[T](val f: DataClass[T] => Vectorizer[T]) extends VectorizerMaker[T] {
    override def apply(x: DataClass[T]) = f(x)
  }
}