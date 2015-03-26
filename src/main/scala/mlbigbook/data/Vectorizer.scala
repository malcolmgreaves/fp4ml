/*
 * Type definitions for things that can create vectors from objects (Vectorizer) and
 * thing that can create a Vectorizer, if given a data source (VectorizerMaker).
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

/** Type that represents something that can convert an arbitrary instance of T into a Vector. */
trait Vectorizer[T] extends (T => Vector)

object Vectorizer {

  @inline implicit def fn2vectorizer[T](f: T => Vector): Vectorizer[T] =
    new Vectorizer[T] {
      @inline override def apply(x: T): Vector = f(x)
    }

}

/** Type that can construct a vectorizer, if given a distributed data source. */
trait VectorizerMaker[T] extends (DistData[T] => Vectorizer[T])

object VectorizerMaker {

  @inline implicit def fn2maker[T](f: DistData[T] => Vectorizer[T]): VectorizerMaker[T] =
    new VectorizerMaker[T] {
      @inline override def apply(x: DistData[T]): Vectorizer[T] = f(x)
    }
}