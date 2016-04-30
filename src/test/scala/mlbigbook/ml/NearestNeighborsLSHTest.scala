package mlbigbook.ml

import mlbigbook.data.mut.DenseVector
import mlbigbook.data.{ OLD_VectorizerMaker$, OLD_Vectorizer$ }
import mlbigbook.wordcount.LocalSparkContext
import org.scalatest.FunSuite

class NearestNeighborsLSHTest extends FunSuite with LocalSparkContext {

  import NearestNeighborsLSHTest._

  ignore("nearest neighbors addresses, k=3") {
    fail("unimmplemented")
  }

  ignore("LSH modified NN addresses, k=3, nBins=5") {
    fail("unimplemented")
  }

}

object NearestNeighborsLSHTest {

  type Err = Option[String]

  def nnTest[T](nn: Ranker[T], input: T, expected: Traversable[(T, Double)]): Err = {

    val actual = nn(input)

    val errors =
      expected.toSeq.zip(actual.toIndexedSeq).foldLeft(List.empty[String])({
        case (sum, (e, a)) =>
          if (e != a)
            sum :+ s"""Expecting: $e | Actual: $a"""
          else
            sum
      })

    if (errors.nonEmpty)
      Some(s"""Found ${errors.length} differences: ${errors.mkString("\n")}""")
    else
      None
  }

  lazy val nnConfig = NearNeighIn(Manhattan, 3)

  lazy val nLshFuncs = 5

  def lshConfig: LshIn = ???
  //LshIn(???, nLshFuncs)

  import AddressData._

  val apartments = Seq(
    Address(Location(1, 2), Some("apartment A")),
    Address(Location(0, 0), Some("apartment B")),
    Address(Location(3, 2), Some("apartment C")),
    Address(Location(5, 2), Some("apartment D")),
    Address(Location(4, 5), Some("apartment E")),
    Address(Location(0, 5), Some("apartment F")),
    Address(Location(4, 4), Some("apartment G")),
    Address(Location(3, 2), Some("apartment H")),
    Address(Location(2, 1), Some("apartment I")),
    Address(Location(5, 3), Some("apartment J"))
  )

  import OLD_Vectorizer._

  def addressVectorizer[N: Numeric]: OLD_Vectorizer[Address[N]] =
    (a: Address[N]) =>
      DenseVector(Array(implicitly[Numeric[N]].toDouble(a.loc.x), implicitly[Numeric[N]].toDouble(a.loc.y)))

  def mkAddressVectorizer[N](implicit n: Numeric[N]): OLD_VectorizerMaker[Address[N]] =
    ???

}

