/*
 * Type definitions for things that can create vectors from objects (Vectorizer) and
 * thing that can create a Vectorizer, if given a data source (VectorizerMaker).
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

import breeze.math.Semiring
import breeze.storage.Zero
import fif.Data
import mlbigbook.math.MathVectorOps

import scala.language.higherKinds

trait Vectorizer {

  def apply[D[_]: Data, N: Numeric: Semiring: Zero, V[_], T](data: D[T])(implicit vops: MathVectorOps[N, V]): T => V[N]

}