package mlbigbook.ml

import breeze.math.Semiring
import breeze.storage.Zero
import mlbigbook.math.MathVectorOps

import scala.language.{ higherKinds, postfixOps, reflectiveCalls }
import scala.reflect.ClassTag

trait ItemNumVecModule {

  type Item
  type N
  type V[_]

  // vops serves as a type class for numeric vector operations
  // having an instance of type MathVectorOps[N,V] implies constraints on N and V
  val vops: MathVectorOps.Type[N, V]

  // we can get these type classes for N
  implicit lazy final val nFrac: Fractional[N] = vops.n
  implicit lazy final val nSr: Semiring[N] = vops.s
  implicit lazy final val nZero: Zero[N] = vops.z

  // Class tag support for abstract types
  implicit val ctN: ClassTag[N]
  implicit val ctI: ClassTag[Item]
  // support for the numerical vector type
  implicit val ctVn: ClassTag[V[N]]

}