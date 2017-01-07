package mlbigbook.math

/** Type class for giving a value to a type `X`. */
trait Val[-X] {

  type N
  implicit def n: Numeric[N]

  def valueOf(a: X): N
}

object Val {

  def apply[V: Val]: Val[V] = implicitly[Val[V]]

  def inverse[V: Val]: Val[V] = {
    val original = Val[V]
    new Val[V] {
      override type N = original.N
      override lazy val n = original.n
      @inline override def valueOf(a: V) =
        original.n.minus(original.n.zero, original.valueOf(a))
    }
  }

  object Implicits {

    implicit def identityVal[X: Numeric] = {
      val evidence = implicitly[Numeric[X]]
      new Val[X] {
        override type N = X
        override implicit lazy val n = evidence
        @inline override def valueOf(a: X) = a
      }
    }

    implicit def tupleValIn1st[Num: Numeric, X] = {
      val evidence = implicitly[Numeric[Num]]
      new Val[(Num, X)] {
        override type N = Num
        override implicit lazy val n = evidence
        @inline override def valueOf(a: (Num, X)) = a._1
      }
    }

    implicit def tupleValIn2nd[X, Num: Numeric] = {
      val evidence = implicitly[Numeric[Num]]
      new Val[(X, Num)] {
        override type N = Num
        override implicit lazy val n = evidence
        @inline override def valueOf(a: (X, Num)) = a._2
      }
    }

  }
}
