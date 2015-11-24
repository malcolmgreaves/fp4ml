package mlbigbook.ml

import fif.TravData
import org.scalatest.FunSuite

class DecisionTreeTest extends FunSuite {

  import DecisionTreeTest._

  test("Decision tree, pre-made, on synthetic categorical data") {

    val decisionTree = {
      val trueChild = dtModule.Leaf(true)
      val falseChild = dtModule.Leaf(false)

      dtModule.Parent(
        (fv: Seq[String]) =>
          if (fv.head == "foo")
            trueChild
          else
            falseChild,
        Seq(trueChild, falseChild)
      )
    }

    val classifier = dtModule.decide(decisionTree)

    syntheticLabeledData
      .foreach {
        case (feats, label) =>
          assert(classifier(feats) == label)
      }
  }

  test("ID3 learning on synthetic categorical data") {
    import TravData.Implicits._
    Id3LearningSimpleFv(dtModule, syntheticLabeledData) match {

      case Some(decisionTree) =>

        val classifier = dtModule.decide(decisionTree)
        syntheticLabeledData
          .foreach {
            case (feats, label) =>
              assert(classifier(feats) == label)
          }

      case None =>
        fail(s"Expecting to learn a decision tree on synthetic data. Algorithm resulted in None.")
    }
  }

  test("Decision tree toString is properly nested.") {

    val decisionTree = {
      val trueChild = dtModule.Leaf(true)
      val falseChild = dtModule.Leaf(false)
      val test = (ignored: Seq[String]) => trueChild
      val nested = dtModule.Parent(
        test,
        Seq(trueChild, falseChild)
      )
      dtModule.Parent(
        test,
        Seq(trueChild, falseChild, nested)
      )
    }

    assert(decisionTree.toString == """Parent(
                                      |  Leaf(decision=true)
                                      |  Leaf(decision=false)
                                      |  Parent(
                                      |    Leaf(decision=true)
                                      |    Leaf(decision=false)
                                      |  )
                                      |)""".stripMargin)
  }

}

object DecisionTreeTest {

  val dtModule = DecisionTree[Boolean, Seq[String]]

  val syntheticData: Traversable[Seq[String]] =
    (0 until 20).map { _ => Seq("foo") } ++ (0 until 10).map { _ => Seq("bar") }

  val syntheticLabeledData: Traversable[(Seq[String], Boolean)] =
    syntheticData
      .map { feats =>
        (feats, feats.head == "foo")
      }

  implicit val syntheticFeatureSpace: FeatureVectorSupport.FeatureSpace =
    FeatureVectorSupport.FeatureSpace(
      features = Seq("foobar"),
      isCategorical = Seq(true),
      feat2index = Map("foobar" -> 0),
      categorical2values = Map("foobar" -> Seq("foo", "bar"))
    )

}