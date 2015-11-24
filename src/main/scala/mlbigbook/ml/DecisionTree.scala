package mlbigbook.ml

import scala.annotation.tailrec
import scala.language.{ postfixOps, higherKinds }

trait DecisionTree {

  /**
   * Abstract type. A stand-in for the overall decision type of an entire
   * decision tree. Common cases include String (e.g.
   */
  type Decision

  /**
   * Abstract type. A stand-in for the feature vector that the decision tree
   * uses during learning and classification.
   */
  type FeatureVector

  /**
   * The children of a parent node. Must always be non-empty.
   */
  final type Children = Seq[Node]

  /**
   * A parent node's feature test. Functions of this type accept a non-empty
   * sequence of children and a feature vector. The test will inspect values
   * from the vector to determine which of the children to output.
   */
  final type Test = FeatureVector => Node

  /**
   * A node of a decision tree. By itself, a Node instance is a fully
   * functional decision tree. One may grow a decision tree from a single node,
   * use an existing decision tree as a subtree of another, or prune a decision
   * tree by selectively removing nodes.
   *
   * The Node abstract data type has two concrete instantiations:
   * Parent and Leaf.
   */
  sealed trait Node {

    override def toString =
      nodeToString(this, 0)

    private[this] def nodeToString(n: Node, depth: Int): String = {
      val depthString = (0 until depth).map { _ => "  " }.mkString("")
      n match {

        case Parent(_, children) =>
          val childrenStr =
            children.map { child => nodeToString(child, depth + 1) }.mkString("\n")
          s"""${depthString}Parent(
                              |$childrenStr
              |$depthString)""".stripMargin

        case Leaf(decision) =>
          s"${depthString}Leaf(decision=$decision)"
      }
    }

  }

  /**
   * A Parent is a Node sub-type that makes up a decision tree. Parents contain
   * one or more child nodes and a Test function. The Test function is used in
   * the decision making process. When presented with an input feature vector,
   * one uses a Parent node's Test function to select from one of its own
   * children.
   */
  sealed case class Parent(t: Test, c: Children) extends Node

  /**
   * A Leaf is a Node sub-type that makes up a decision tree. Leaves are the
   * final decision making aspect of a decision tree. The decision process
   * stops once a Leaf is encountered. At such a point, the Decision instance
   * contained within the Leaf is returned as the decision tree's response.
   */
  sealed case class Leaf(d: Decision) extends Node

  /**
   * The function type for making decisions using a decision tree node.
   */
  final type Decider = Node => FeatureVector => Decision

  /**
   * Implementation of the decision making process using a decision tree.
   * Is efficient and guaranteed to not incur a stack overflow.
   */
  val decide: Decider =
    decisionTree => featureVector => {

        @tailrec def descend(n: Node): Decision =
          n match {

            case Parent(test, _) =>
              descend(test(featureVector))

            case Leaf(d) =>
              d
          }

      descend(decisionTree)
    }
}

object DecisionTree {

  def apply[D, FV]: DecisionTree { type Decision = D; type FeatureVector = FV } =
    new DecisionTree {
      override type Decision = D
      override type FeatureVector = FV
    }

}