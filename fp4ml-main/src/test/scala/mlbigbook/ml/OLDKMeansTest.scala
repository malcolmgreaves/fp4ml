//package mlbigbook.ml
//
//import mlbigbook.wordcount.LocalSparkContext
//import org.scalatest.FunSuite
//
//class OLDKMeansTest extends FunSuite with LocalSparkContext {
//
//  ignore("classify simple addresses") {
//    fail("unimplemented")
//  }
//
//}
//
//object OLDKMeansTest {
//
//  import NearestNeighborsLSHTest._
//
//  def softClusterTest[T](sc: SoftCluster[T], input: T, expected: IndexedSeq[(OLD_Center, Double)]): Err = {
//    val actual = sc(input)
//
//    val errors =
//      expected.zip(actual).foldLeft(List.empty[String])({
//        case (sum, (e, a)) =>
//          if (e != a)
//            sum :+ s"""Expecting: $e | Actual: $a"""
//          else
//            sum
//      })
//
//    if (errors.nonEmpty)
//      Some(s"""Found ${errors.length} differences: ${errors.mkString("\n")}""")
//    else
//      None
//  }
//
//}
//
